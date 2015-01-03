open Core.Std
open NetKAT_Types
open Yojson.Basic
open Yojson.Basic.Util

let to_json_value (h : header_val) : json = match h with
  | Switch n
  | EthSrc n
  | EthDst n -> `Int (Int64.to_int_exn n)
  | Location (Physical n) -> `Assoc [("type", `String "physical");
                                     ("port", `Int (Int32.to_int_exn n))]
  | Location (Pipe s) -> `Assoc [("type", `String "pipe");
                                 ("name", `String s)]
  | Location (Query s) -> `Assoc [("type", `String "query");
                                  ("name", `String s)]
  | Vlan n
  | VlanPcp n
  | EthType n
  | IPProto n
  | TCPSrcPort n
  | TCPDstPort n -> `Int n
  | IP4Src (addr, mask)
  | IP4Dst (addr, mask) -> `Assoc [("addr", `Int (Int32.to_int_exn addr));
                                   ("mask", `Int (Int32.to_int_exn mask))]

let to_json_header (h : header_val) : json =
  let str = match h with
    | Switch _ -> "switch"
    | Location _ -> "location"
    | EthSrc _ -> "ethsrc"
    | EthDst _ -> "ethdst"
    | Vlan _ -> "vlan"
    | VlanPcp _ -> "vlanpcp"
    | EthType _ -> "ethtype"
    | IPProto _ -> "ipproto"
    | IP4Src _ -> "ip4src"
    | IP4Dst _ -> "ip4dst"
    | TCPSrcPort _ -> "tcpsrcport"
    | TCPDstPort _ -> "tcpdstport" in
  `String str


let rec to_json_pred (pred : pred) : json = match pred with
  | True -> `Assoc [("type", `String "true")]
  | False -> `Assoc [("type", `String "false")]
  | Test h -> `Assoc [("type", `String "test");
                      ("header", to_json_header h);
                      ("value", to_json_value h)]
  | And (a, b) -> `Assoc [("type", `String "and");
                          ("lhs", to_json_pred a);
                          ("rhs", to_json_pred b)]
  | Or (a, b) -> `Assoc [("type", `String "or");
                         ("lhs", to_json_pred a);
                         ("rhs", to_json_pred b)]
  | Neg a -> `Assoc [("type", `String "neg");
                     ("pred", to_json_pred a)]

let rec to_json_pol (pol : policy) : json = match pol with
  | Filter a -> `Assoc [("type", `String "filter");
                        ("pred", to_json_pred a)]
  | Mod h -> `Assoc [("type", `String "mod");
                     ("header", to_json_header h);
                     ("value", to_json_value h)]
  | Union (p, q) -> `Assoc [("type", `String "union");
                            ("lhs", to_json_pol p);
                            ("rhs", to_json_pol q)]
  | Seq (p, q) -> `Assoc [("type", `String "seq");
                          ("lhs", to_json_pol p);
                          ("rhs", to_json_pol q)]
  | Star p -> `Assoc [("type", `String "star");
                      ("pol", to_json_pol p)]
  | Link (sw1, pt1, sw2, pt2) ->
    `Assoc [("type", `String "link");
            ("sw1", `Int (Int64.to_int_exn sw1));
            ("pt1", `Int (Int32.to_int_exn pt1));
            ("sw2", `Int (Int64.to_int_exn sw2));
            ("pt2", `Int (Int32.to_int_exn pt2))]

let from_json_header_val (json : json) : header_val =
  let value = json |> member "value" in
  match json |> member "header" |> to_string with
  | "switch" -> Switch (value |> to_int |> Int64.of_int)
  | "location" ->
    let value = match value |> member "type" |> to_string with
      | "physical" -> Physical (value |> member "port" |>
                                to_int |> Int32.of_int_exn)
      | "pipe" -> Pipe (value |> member "name" |> to_string)
      | "query" -> Query (value |> member "name" |> to_string)
      | str -> raise (Invalid_argument ("invalid location type " ^ str))
    in Location value
  | "ethsrc" -> EthSrc (value |> to_int |> Int64.of_int)
  | "ethdst" -> EthDst (value |> to_int |> Int64.of_int)
  | "vlan" -> Vlan (value |> to_int)
  | "vlanpcp" -> VlanPcp (value |> to_int)
  | "ethtype" -> EthType (value |> to_int)
  | "ipproto" -> IPProto (value |> to_int)
  | "ip4src" -> IP4Src (value |> member "addr" |> to_int |> Int32.of_int_exn,
                        value |> member "mask" |> to_int |> Int32.of_int_exn)
  | "ip4dst" -> IP4Dst (value |> member "addr" |> to_int |> Int32.of_int_exn,
                        value |> member "mask" |> to_int |> Int32.of_int_exn)
  | "tcpsrcport" -> TCPSrcPort (value |> to_int)
  | "tcpdstport" -> TCPDstPort (value |> to_int)
  | str -> raise (Invalid_argument ("invalid header " ^ str))

let rec from_json_pred (json : json) : pred =
  match json |> member "type" |> to_string with
  | "true" -> True
  | "false" -> False
  | "test" -> Test (from_json_header_val json)
  | "and" -> And (json |> member "lhs" |> from_json_pred,
                  json |> member "rhs" |> from_json_pred)
  | "or" -> Or (json |> member "lhs" |> from_json_pred,
                json |> member "rhs" |> from_json_pred)
  | "neg" -> Neg (json |> member "pred" |> from_json_pred)
  | str -> raise (Invalid_argument ("invalid predicate type " ^ str))

let rec from_json_pol (json : json) : policy =
   match json |> member "type" |> to_string with
   | "filter" -> Filter (json |> member "pred" |> from_json_pred)
   | "mod" -> Mod (from_json_header_val json)
   | "union" -> Union (json |> member "lhs" |> from_json_pol,
                       json |> member "rhs" |> from_json_pol)
   | "seq" -> Seq (from_json_pol (json |> member "lhs"),
                   from_json_pol (json |> member "rhs"))
   | "star" -> Star (from_json_pol (json |> member "pol"))
   | "link" -> Link (json |> member "sw1" |> to_int |> Int64.of_int,
                     json |> member "pt1" |> to_int |> Int32.of_int_exn,
                     json |> member "sw2" |> to_int |> Int64.of_int,
                     json |> member "pt2" |> to_int |> Int32.of_int_exn)
   | str -> raise (Invalid_argument ("invalid policy type " ^ str))

let to_json = to_json_pol

let from_json = from_json_pol

(* by default, Yojson produces non-standard JSON *)
let to_json_string (pol : policy) : string =
  Yojson.Basic.to_string ~std:true (to_json pol)

let from_json_string (str : string) : policy = from_json (from_string str)

