open Core
open Httpaf
open Import

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let cookies_parser =
  let open Angstrom in
  let cookie_name =
    take_while (function
      | '\000' .. '\021' | '\127' | ';' | '=' -> false
      | _ -> true)
  in
  let cookie_value =
    let cookie_value_chars =
      take_while (function
        | '\000' .. '\021' | '\127' | ';' -> false
        | _ -> true)
    in
    char '"' *> cookie_value_chars <* char '"' <|> cookie_value_chars
  in
  let sep = string "; " in
  sep_by sep
    ( both (cookie_name <* char '=' <* take_while is_whitespace) cookie_value
    >>| fun (name, value) -> (String.rstrip name ~drop:is_whitespace, value) )

let extract headers =
  Headers.get headers "Cookie"
  |> Option.bind ~f:(fun value ->
         Angstrom.parse_string ~consume:All cookies_parser value |> Result.ok)
  |> Option.value ~default:[]
