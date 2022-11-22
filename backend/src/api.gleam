import gleam/io
import gleam/dynamic
import gleam/json
import gleam/result
import utils

pub type PageEvent {
  PageEvent(page: String, uuid: String, event: Event)
}

pub type Event {
  Daily(event: DailyEvent)
  Training(event: TrainingEvent)
}

pub type DailyEvent {
  DailyEvent(
    progress: String,
    result: String,
    attempts: Int,
    mistakes: Int,
    rata: Int,
  )
}

pub type TrainingEvent {
  TrainingEvent(
    dicts_active: List(String),
    attempts: Int,
    correct: Int,
    retries: Int,
  )
}

// DECODERS

pub fn page_event_to_json(page_event: PageEvent) -> json.Json {
  json.object([
    #("page", json.string(page_event.page)),
    #("uuid", json.string(page_event.uuid)),
    #("event", event_to_json(page_event.event)),
  ])
}

pub fn event_to_json(event: Event) -> json.Json {
  case event {
    Daily(e) -> daily_event_to_json(e)
    Training(e) -> training_event_to_json(e)
  }
}

pub fn daily_event_to_json(daily_event: DailyEvent) -> json.Json {
  json.object([
    #("progress", json.string(daily_event.progress)),
    #("result", json.string(daily_event.result)),
    #("attempts", json.int(daily_event.attempts)),
    #("mistakes", json.int(daily_event.mistakes)),
    #("rata", json.int(daily_event.rata)),
  ])
}

pub fn training_event_to_json(training_event: TrainingEvent) -> json.Json {
  json.object([
    #("dicts-active", json.array(training_event.dicts_active, json.string)),
    #("attempts", json.int(training_event.attempts)),
    #("correct", json.int(training_event.correct)),
    #("retries", json.int(training_event.retries)),
  ])
}

pub fn decode_page_event(str: String) -> Result(PageEvent, String) {
  let daily_event_decoder =
    dynamic.decode5(
      fn(a, b, c, d, e) { Daily(event: DailyEvent(a, b, c, d, e)) },
      dynamic.field("progress", of: dynamic.string),
      dynamic.field("result", of: dynamic.string),
      dynamic.field("attempts", of: dynamic.int),
      dynamic.field("mistakes", of: dynamic.int),
      dynamic.field("rata", of: dynamic.int),
    )

  let training_event_decoder =
    dynamic.decode4(
      fn(a, b, c, d) { Training(event: TrainingEvent(a, b, c, d)) },
      dynamic.field("dicts-active", of: dynamic.list(dynamic.string)),
      dynamic.field("correct", of: dynamic.int),
      dynamic.field("attempts", of: dynamic.int),
      dynamic.field("retries", of: dynamic.int),
    )

  let event_decoder = dynamic.any([daily_event_decoder, training_event_decoder])

  dynamic.decode3(
    PageEvent,
    dynamic.field("page", of: dynamic.string),
    dynamic.field("uuid", of: dynamic.string),
    dynamic.field("event", of: event_decoder),
  )
  |> json.decode(from: str)
  |> utils.map_error_string("parse error")
}
