open Core

type token =
  | Brivla of string
  | Ma'ovla of string
  | Cmevla of string
  | String of string

let string_of_token = function
  | Brivla s -> s
  | Ma'ovla s -> s
  | Cmevla s -> s
  | String s -> s

let ( = ) a b =
  match (a, b) with
  | Brivla s1, Brivla s2 when String.equal s1 s2 -> true
  | Ma'ovla s1, Ma'ovla s2 when String.equal s1 s2 -> true
  | Cmevla s1, Cmevla s2 when String.equal s1 s2 -> true
  | String s1, String s2 when String.equal s1 s2 -> true
  | _, _ -> false

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

let rec classify code =
  match code with
  | [] -> []
  | ' ' :: tl -> classify tl
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
      token :: classify rest
  | _ -> failwith "invalid char"

let rec tokenize words =
  let rec aux acc = function
    | [] -> (acc, [])
    | hd :: tl when hd = Ma'ovla "li'u" -> (acc, tl)
    | hd :: tl -> (
        match String.length acc with
        | 0 -> aux (string_of_token hd) tl
        | _ -> aux (acc ^ " " ^ string_of_token hd) tl )
  in
  match words with
  | [] -> []
  | hd :: tl when hd = Ma'ovla "lu" ->
      let str, rest = aux "" tl in
      String str :: tokenize rest
  | hd :: tl -> hd :: tokenize tl

let f code = String.to_list code |> classify |> tokenize
