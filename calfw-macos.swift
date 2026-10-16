// calfw-macos.swift --- Read-only EventKit bridge for calfw.
// SPDX-License-Identifier: GPL-3.0-or-later
// Inspired by https://github.com/haji-ali/maccalfw's EventKit bridge.
import Foundation
import EventKit
import AppKit

func output(_ value: [String: Any]) {
    do {
        let data = try JSONSerialization.data(withJSONObject: value, options: [.sortedKeys])
        FileHandle.standardOutput.write(data)
        FileHandle.standardOutput.write(Data([10]))
    } catch {
        fail("Cannot encode calendar response: \(error.localizedDescription)")
    }
}

func fail(_ message: String, code: Int32 = 1) -> Never {
    FileHandle.standardError.write(Data((message + "\n").utf8))
    exit(code)
}

func authorization() -> String {
    let status = EKEventStore.authorizationStatus(for: .event)
    // Full access was named `authorized` before macOS 14 (same raw value).
    switch status.rawValue {
    case 0: return "not-determined"
    case 1: return "restricted"
    case 2: return "denied"
    case 3: return "full-access"
    case 4: return "write-only"
    default: return "unknown"
    }
}

func requireAccess() {
    guard authorization() == "full-access" else {
        fail("Calendar read access is \(authorization()). Run M-x cfw:macos-request-access first.", code: 2)
    }
}

func color(_ calendar: EKCalendar) -> String {
    guard let cgColor = calendar.cgColor,
          let rgb = NSColor(cgColor: cgColor)?.usingColorSpace(.deviceRGB) else {
        return "#4682b4"
    }
    return String(format: "#%02x%02x%02x", Int((rgb.redComponent * 255).rounded()),
                  Int((rgb.greenComponent * 255).rounded()), Int((rgb.blueComponent * 255).rounded()))
}

func calendarData(_ calendar: EKCalendar) -> [String: Any] {
    return ["id": calendar.calendarIdentifier, "title": calendar.title,
            "source": calendar.source?.title ?? "", "color": color(calendar),
            "subscribed": calendar.isSubscribed]
}

let arguments = Array(CommandLine.arguments.dropFirst())
guard let command = arguments.first else {
    fail("Usage: calfw-calendar status | authorize | calendars | events START END CALENDAR_ID ...")
}

switch command {
case "status":
    output(["version": 1, "authorization": authorization()])
case "authorize":
    if authorization() == "full-access" {
        output(["version": 1, "authorization": "full-access"])
        exit(0)
    }
    let store = EKEventStore()
    let completion: (Bool, Error?) -> Void = { granted, error in
        if granted {
            output(["version": 1, "authorization": "full-access"])
            exit(0)
        }
        fail(error?.localizedDescription ?? "Calendar access was not granted. Enable full access in System Settings > Privacy & Security > Calendars.", code: 2)
    }
    if #available(macOS 14.0, *) {
        store.requestFullAccessToEvents(completion: completion)
    } else {
        store.requestAccess(to: .event, completion: completion)
    }
    // Keep the main run loop alive for the system permission dialog.
    withExtendedLifetime(store) {
        RunLoop.main.run(until: Date(timeIntervalSinceNow: 120))
    }
    fail("Calendar authorization timed out; retry M-x cfw:macos-request-access.", code: 2)
case "calendars":
    requireAccess()
    let store = EKEventStore()
    let calendars = store.calendars(for: .event).sorted {
        (($0.source?.title ?? ""), $0.title, $0.calendarIdentifier) <
        (($1.source?.title ?? ""), $1.title, $1.calendarIdentifier)
    }
    output(["version": 1, "calendars": calendars.map(calendarData)])
case "events":
    guard arguments.count >= 3, let start = Double(arguments[1]),
          let end = Double(arguments[2]), start.isFinite, end.isFinite, start < end else {
        fail("events expects an increasing finite START END range in Unix seconds.")
    }
    // An empty selection means no events, never all calendars.
    let identifiers = Array(arguments.dropFirst(3))
    if identifiers.isEmpty {
        output(["version": 1, "events": []])
        exit(0)
    }
    requireAccess()
    let store = EKEventStore()
    let calendars = identifiers.map { identifier -> EKCalendar in
        guard let calendar = store.calendar(withIdentifier: identifier) else {
            fail("A selected calendar no longer exists. Reopen the calendar list and select it again.")
        }
        return calendar
    }
    let begin = Date(timeIntervalSince1970: start)
    let finish = Date(timeIntervalSince1970: end)
    let predicate = store.predicateForEvents(withStart: begin, end: finish, calendars: calendars)
    // EventKit expands recurrences in the requested range. Keep every occurrence.
    let events = store.events(matching: predicate).filter { event in
        event.status != .canceled && event.startDate < finish &&
        (event.endDate > begin || (event.startDate == event.endDate && event.startDate >= begin))
    }.sorted {
        ($0.startDate, $0.endDate, $0.title ?? "") < ($1.startDate, $1.endDate, $1.title ?? "")
    }
    let result: [[String: Any]] = events.map { event in
        ["id": event.eventIdentifier ?? "", "uid": event.calendarItemExternalIdentifier ?? "",
         "calendar": event.calendar.calendarIdentifier, "title": event.title ?? "",
         "start": event.startDate.timeIntervalSince1970, "end": event.endDate.timeIntervalSince1970,
         "allDay": event.isAllDay, "location": event.location ?? "", "notes": event.notes ?? ""]
    }
    output(["version": 1, "events": result])
default:
    fail("Unknown command: \(command)")
}
