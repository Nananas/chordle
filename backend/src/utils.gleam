import gleam/int
import gleam/string
import gleam/result
import gleam/list

pub external fn base64_encode(bit_str) -> String =
  "base64" "encode"

pub fn map_error_string(something, str) {
  use err <- result.map_error(something)
  str
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
