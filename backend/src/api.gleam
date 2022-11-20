import gleam/dynamic
import gleam/json
import gleam/result
import utils

pub type PageEvent {
  PageEvent(page: String, event: Event)
}

pub type Event {
  Event(
    progress: String,
    result: String,
    attempts: Int,
    mistakes: Int,
    rata: Int,
  )
}

type CreateUserRequest {
  CreateUserRequest(username: String, password: String, email: String)
}

// DECODERS

pub fn page_event_to_json(page_event: PageEvent) -> json.Json {
  json.object([
    #("page", json.string(page_event.page)),
    #("event", event_to_json(page_event.event)),
  ])
}

pub fn event_to_json(event: Event) -> json.Json {
  json.object([
    #("progress", json.string(event.progress)),
    #("result", json.string(event.result)),
    #("attempts", json.int(event.attempts)),
    #("mistakes", json.int(event.mistakes)),
    #("rata", json.int(event.rata)),
  ])
}

pub fn decode_page_event(str: String) -> Result(PageEvent, String) {
  let event_decoder =
    dynamic.decode5(
      Event,
      dynamic.field("progress", of: dynamic.string),
      dynamic.field("result", of: dynamic.string),
      dynamic.field("attempts", of: dynamic.int),
      dynamic.field("mistakes", of: dynamic.int),
      dynamic.field("rata", of: dynamic.int),
    )

  let page_event_decoder =
    dynamic.decode2(
      PageEvent,
      dynamic.field("page", of: dynamic.string),
      dynamic.field("event", of: event_decoder),
    )
  json.decode(from: str, using: page_event_decoder)
  |> utils.map_error_string("parse error")
}

fn decode_createuser(str: String) -> Result(CreateUserRequest, String) {
  let decoder =
    dynamic.decode3(
      CreateUserRequest,
      dynamic.field("username", of: dynamic.string),
      dynamic.field("password", of: dynamic.string),
      dynamic.field("email", of: dynamic.string),
    )
  json.decode(from: str, using: decoder)
  |> utils.map_error_string("auth parse error")
}
