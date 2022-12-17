import gleam/pgo
import gleam/option.{Some}
import gleam/io
import gleam/dynamic
import gleam/list
import gleam/int
import gleam/json
import api.{PageEvent}
import utils.{map_error_string}

pub fn insert_event(
  db: pgo.Connection,
  page_event: PageEvent,
) -> Result(Nil, String) {
  let insert_sql =
    "insert into page_events(page, uuid, event) values ($1, $2, $3);"
  let event_json_str =
    api.event_to_json(page_event.event)
    |> json.to_string

  try _ =
    pgo.execute(
      insert_sql,
      db,
      [
        pgo.text(page_event.page),
        pgo.text(page_event.uuid),
        pgo.text(event_json_str),
      ],
      dynamic.dynamic,
    )
    |> map_error_string("Database error")
  Ok(Nil)
}

pub fn connect() -> Result(pgo.Connection, String) {
  let db_config =
    pgo.Config(
      ..pgo.default_config(),
      host: "chordle-postgres",
      database: "chordle",
      user: "postgres",
      pool_size: 5,
      password: Some("chordle-pass123"),
    )
  io.debug(db_config)
  let db = pgo.connect(db_config)

  try r =
    pgo.execute(
      "select count(*) from page_events",
      db,
      [],
      dynamic.element(0, dynamic.int),
    )
    |> map_error_string("Database error: could not select all page_events")

  try count =
    list.first(r.rows)
    |> map_error_string("Incorrect DB result")

  io.println("Database connected:")
  io.println("\tPage events: " <> int.to_string(count))
  Ok(db)
}

pub fn cleanup(dbc) {
  pgo.execute(
    "DELETE FROM page_events where page = 'db-test'",
    dbc,
    [],
    dynamic.dynamic,
  )
  |> map_error_string("Database cleanup error")
}
