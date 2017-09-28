open Core.Std

let camel_case word =
  let buf = Buffer.create (String.length word) in
  String.fold word
    ~init:`Upper
    ~f:(fun flag c ->
        match flag, c with
        | _, '_' -> `Upper
        | `Upper, c ->
          Buffer.add_char buf @@ Char.uppercase c;
          `Lower
        | `Lower, c ->
          Buffer.add_char buf @@ Char.lowercase c;
          `Lower)
  |> ignore;
  Buffer.contents buf

module Format = struct

  type placeholder =
    | Text of char
    | Int
    | Float
    | String

  type t = placeholder list

  let parse s =
    let rec parse_format cs accu =
      match cs with
      | [] -> List.rev accu
      | '%' :: cs -> parse_field cs accu
      | c :: cs -> parse_format cs (Text c :: accu)
    and parse_field cs accu =
      match cs with
      | [] -> List.rev (Text '%' :: accu)
      | 'd' :: cs -> parse_format cs (Int :: accu)
      | 's' :: cs -> parse_format cs (String :: accu)
      | '%' :: cs -> parse_format cs (Text '%' :: accu)
      | c :: cs -> parse_format cs (Text c :: Text '%' :: accu)
    in
    parse_format (String.to_list s) []

  let params fmt =
    List.filter fmt ~f:(fun place ->
        match place with
        | Text _ -> false
        | _ -> true)

end
