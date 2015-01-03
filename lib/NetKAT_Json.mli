open NetKAT_Types
open Yojson.Basic

val from_json : json -> policy
(** Note: errors may occur when converting between 64-bit values and
    JSON-representable integers. *)

val to_json : policy -> json

val from_json_string : string -> policy

val to_json_string : policy -> string