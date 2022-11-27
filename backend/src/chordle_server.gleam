import gleam/bit_builder
import gleam/bit_string
import gleam/erlang/process.{Subject}
import gleam/http/request.{Request}
import gleam/http/response
import gleam/http
import gleam/string
import mist
import gleam/result
import gleam/io
import mist/file as mfile
import gleam/int
import mist/handler.{HandlerResponse, Response}
import mist/http.{BitBuilderBody, Body, FileBody} as mhttp
import gleam/option.{None, Option, Some}
import utils.{map_error_string}
import db
import api

const static_dir = "static/"

// MIDDLEWARE
fn check_valid_content_type_header(req, next) {
  let content_type_header = request.get_header(req, "content-type")
  case content_type_header {
    Ok("application/json") -> next()
    Ok(_) -> error_response("Invalid content-type header")
    Error(_) -> error_response("Missing content-type header")
  }
}

/// MIDDLEWARE
fn parse_page_event(req, next) {
  let r_page_event =
    req
    |> mhttp.read_body
    |> map_error_string("decode error")
    |> result.then(fn(req_bits) {
      bit_string.to_string(req_bits.body)
      |> map_error_string("invalid UTF-8")
    })
    |> map_error_string("bit-string error")
    |> result.then(api.decode_page_event)

  case r_page_event {
    Ok(page_event) -> next(page_event)
    Error(err) -> error_response(err)
  }
}

fn error_response(txt) {
  response.new(400)
  |> response.set_body(BitBuilderBody(bit_builder.from_string(txt)))
  |> Response
}

fn string_response(txt) {
  response.new(200)
  |> response.set_body(BitBuilderBody(bit_builder.from_string(txt)))
  |> Response
}

fn not_found() {
  response.new(404)
  |> response.set_body(BitBuilderBody(bit_builder.new()))
  |> Response
}

pub fn main() {
  assert Ok(db) = db.connect()

  assert Ok(_) =
    mist.serve(
      8080,
      handler.with_func(fn(req: Request(Body)) -> HandlerResponse {
        io.debug(request.path_segments(req))
        case req.method, request.path_segments(req) {
          http.Get, ["main.js"] -> serve_file("main.js")
          http.Get, ["words.json"] -> serve_file("words.json")
          http.Get, ["favicon.ico"] -> serve_file("favicon.ico")
          http.Get, [] -> serve_file("index.html")
          http.Post, ["event"] -> {
            use <- check_valid_content_type_header(req)
            use page_event <- parse_page_event(req)
            assert Ok(_) = db.insert_event(db, page_event)
            response.new(200)
            |> response.set_body(BitBuilderBody(bit_builder.from_string("OK")))
            |> Response
          }
          _, _ -> not_found()
        }
      }),
    )

  process.sleep_forever()
}

pub fn serve_file(filename: String) -> HandlerResponse {
  io.println("Serve file static/" <> filename)
  let file_path = bit_string.from_string(static_dir <> filename)

  let size = mfile.size(file_path)
  assert Ok(fd) = mfile.open(file_path)
  response.new(200)
  |> response.set_header(
    "Content-Type",
    case string.ends_with(filename, ".json") {
      True -> "application/json"
      False -> ""
    },
  )
  |> response.set_body(FileBody(fd, int.to_string(size), 0, size))
  |> Response
}
