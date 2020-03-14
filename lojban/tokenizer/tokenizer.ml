open Core

type token =
  | Brivla of string
  | Ma'ovla of string
  | Cmevla of string

let consonants = "bcdfgjklmnprstvxz'"

let vowels = "aeiou"

let is_consonant = String.contains consonants

let is_vowel = String.contains vowels

let is_valid_letter c = is_consonant c || is_vowel c

let extract_string predicate code =
  let rec aux acc = function
    | [] -> (acc, [])
    | hd :: tl when predicate hd -> aux (hd :: acc) tl
    | _ :: tl -> (acc, tl)
  in
  let res = aux [] code in
  (fst res |> List.rev, snd res)

let rec is_brivla = function
  | [] -> false
  | hd1 :: hd2 :: _ when is_consonant hd1 && is_consonant hd2 -> true
  | _ :: tl -> is_brivla tl

let is_cmevla ident =
  let hd = List.rev ident |> List.hd in
  Option.value hd ~default:'a' |> is_consonant

let is_ma'ovla ident = not (is_brivla ident || is_cmevla ident)

let rec tokenize code =
  match code with
  | [] -> []
  | ' ' :: tl -> tokenize tl
  | hd :: _ when is_valid_letter hd ->
      let ident, rest = extract_string is_valid_letter code in
      let str = String.of_char_list ident in
      let token =
        match ident with
        | i when is_brivla i -> Brivla str
        | i when is_ma'ovla i -> Ma'ovla str
        | i when is_cmevla i -> Cmevla str
        | _ -> failwith "unreachable"
      in
      token :: tokenize rest
  | _ -> failwith "invalid char"

let f code = String.to_list code |> tokenize