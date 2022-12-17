import gleam/result
import gleam/list
import gleam/json

pub external fn base64_encode(bit_str) -> String =
  "base64" "encode"

pub fn map_error_string(something, str) {
  use err <- result.map_error(something)
  str
}

pub fn map_json_error_to_string(r) {
  use err <- result.map_error(r)
  case err {
    json.UnexpectedEndOfInput -> "UnexpectedEndOfInput"
    json.UnexpectedByte(byte, position) -> "UnexpectedByte"
    json.UnexpectedSequence(byte, position) -> "UnexpectedSequence"
    json.UnexpectedFormat(err) -> "UnexpectedFormat"
  }
}

pub fn test() {
  let a = [1, 2, 3]
  let b = {
    use el <- list.map(a)
    el + 1
  }
  let c = b
  c
}
