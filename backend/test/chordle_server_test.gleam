import gleeunit
import gleeunit/should
import gleam/result
import gleam/json
import db
import api
import gleam/io

pub fn main() {
  gleeunit.main()
}

pub fn db_page_event_insert_test() {
  let page_event =
    api.PageEvent(
      page: "db-test",
      event: api.Event(
        progress: "test",
        result: "test",
        attempts: -1,
        mistakes: -1,
        rata: -1,
      ),
    )
  try dbc = db.connect()
  db.insert_event(dbc, page_event)
  |> should.be_ok

  db.cleanup(dbc)
}

pub fn api_decode_page_event_test() {
  let str =
    "{\"page\":\"daily\",\"event\":{\"progress\":\"First\",\"result\":\"Success\",\"attempts\":8,\"mistakes\":3,\"rata\":123456}}"

  str
  |> api.decode_page_event
  |> result.map(api.page_event_to_json)
  |> result.map(json.to_string)
  |> io.debug
  |> should.equal(Ok(str))
}
