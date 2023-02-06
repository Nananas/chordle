import gleam/dynamic
import gleam/json
import utils

pub type PageEvent {
  PageEvent(page: String, uuid: String, event: Event)
}

pub type Event {
  Daily(event: DailyEvent)
  TrainingFinished(event: TrainingFinishedEvent)
  TrainingRound(event: TrainingRoundEvent)
  NumbersRound(event: NumbersRoundEvent)
}

pub type DailyEvent {
  DailyEvent(
    dicts_active: List(String),
    progress: String,
    result: String,
    attempts: Int,
    mistakes: Int,
    rata: Int,
  )
}

pub type TrainingFinishedEvent {
  TrainingFinishedEvent(
    dicts_active: List(String),
    attempts: Int,
    correct: Int,
    retries: Int,
  )
}

pub type TrainingRoundEvent {
  TrainingRoundEvent(
    dicts_active: List(String),
    nr_words_found: Int,
    success: Bool,
    mistakes: Int,
  )
}

pub type NumbersRoundEvent {
  NumbersRoundEvent(number: Int, mode: String)
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
    TrainingFinished(e) -> training_finished_event_to_json(e)
    TrainingRound(e) -> training_round_event_to_json(e)
    NumbersRound(e) -> numbers_round_event_to_json(e)
  }
}

pub fn daily_event_to_json(daily_event: DailyEvent) -> json.Json {
  json.object([
    #("dicts-active", json.array(daily_event.dicts_active, json.string)),
    #("progress", json.string(daily_event.progress)),
    #("result", json.string(daily_event.result)),
    #("attempts", json.int(daily_event.attempts)),
    #("mistakes", json.int(daily_event.mistakes)),
    #("rata", json.int(daily_event.rata)),
  ])
}

pub fn training_finished_event_to_json(
  training_event: TrainingFinishedEvent,
) -> json.Json {
  json.object([
    #("dicts-active", json.array(training_event.dicts_active, json.string)),
    #("attempts", json.int(training_event.attempts)),
    #("correct", json.int(training_event.correct)),
    #("retries", json.int(training_event.retries)),
  ])
}

pub fn training_round_event_to_json(
  training_event: TrainingRoundEvent,
) -> json.Json {
  json.object([
    #("dicts-active", json.array(training_event.dicts_active, json.string)),
    #("nr-words-found", json.int(training_event.nr_words_found)),
    #("success", json.bool(training_event.success)),
    #("mistakes", json.int(training_event.mistakes)),
  ])
}

pub fn numbers_round_event_to_json(
  numbers_event: NumbersRoundEvent,
) -> json.Json {
  json.object([
    #("number", json.int(numbers_event.number)),
    #("mode", json.string(numbers_event.mode)),
  ])
}

pub fn decode_page_event(str: String) -> Result(PageEvent, String) {
  let daily_event_decoder =
    dynamic.decode6(
      fn(a, b, c, d, e, f) { Daily(event: DailyEvent(a, b, c, d, e, f)) },
      dynamic.field("dicts-active", of: dynamic.list(dynamic.string)),
      dynamic.field("progress", of: dynamic.string),
      dynamic.field("result", of: dynamic.string),
      dynamic.field("attempts", of: dynamic.int),
      dynamic.field("mistakes", of: dynamic.int),
      dynamic.field("rata", of: dynamic.int),
    )

  let training_finished_event_decoder =
    dynamic.decode4(
      fn(a, b, c, d) {
        TrainingFinished(event: TrainingFinishedEvent(a, b, c, d))
      },
      dynamic.field("dicts-active", of: dynamic.list(dynamic.string)),
      dynamic.field("correct", of: dynamic.int),
      dynamic.field("attempts", of: dynamic.int),
      dynamic.field("retries", of: dynamic.int),
    )

  let training_round_event_decoder =
    dynamic.decode4(
      fn(a, b, c, d) { TrainingRound(event: TrainingRoundEvent(a, b, c, d)) },
      dynamic.field("dicts-active", of: dynamic.list(dynamic.string)),
      dynamic.field("nr-words-found", of: dynamic.int),
      dynamic.field("success", of: dynamic.bool),
      dynamic.field("mistakes", of: dynamic.int),
    )

  let numbers_round_event_decoder =
    dynamic.decode2(
      fn(a, b) { NumbersRound(event: NumbersRoundEvent(a, b)) },
      dynamic.field("number", of: dynamic.int),
      dynamic.field("mode", of: dynamic.string),
    )

  let event_decoder =
    dynamic.any([
      daily_event_decoder,
      training_finished_event_decoder,
      training_round_event_decoder,
      numbers_round_event_decoder,
    ])

  dynamic.decode3(
    PageEvent,
    dynamic.field("page", of: dynamic.string),
    dynamic.field("uuid", of: dynamic.string),
    dynamic.field("details", of: event_decoder),
  )
  |> json.decode(from: str)
  |> utils.map_json_error_to_string
}
