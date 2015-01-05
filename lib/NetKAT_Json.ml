let int_to_uint32 = Int32.of_int

open Core.Std
open SDN_Types
open NetKAT_Types
open Yojson.Basic
open Optimize


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
                          ("preds", `List [to_json_pred a; to_json_pred b])]
  | Or (a, b) -> `Assoc [("type", `String "or");
                         ("preds", `List [to_json_pred a; to_json_pred b])]
  | Neg a -> `Assoc [("type", `String "neg");
                     ("pred", to_json_pred a)]

let rec to_json_pol (pol : policy) : json = match pol with
  | Filter a -> `Assoc [("type", `String "filter");
                        ("pred", to_json_pred a)]
  | Mod h -> `Assoc [("type", `String "mod");
                     ("header", to_json_header h);
                     ("value", to_json_value h)]
  | Union (p, q) -> `Assoc [("type", `String "union");
                            ("pols", `List [to_json_pol p; to_json_pol q])]
  | DisjointUnion (p, q) ->
    `Assoc [("type", `String "disjoint");
            ("pols", `List [to_json_pol p; to_json_pol q])]
  | Seq (p, q) ->
    `Assoc [("type", `String "seq");
           ("pols", `List [to_json_pol p; to_json_pol q])]
  | Star p -> `Assoc [("type", `String "star");
                      ("pol", to_json_pol p)]
  | Link (sw1, pt1, sw2, pt2) ->
    `Assoc [("type", `String "link");
            ("sw1", `Int (Int64.to_int_exn sw1));
            ("pt1", `Int (Int32.to_int_exn pt1));
            ("sw2", `Int (Int64.to_int_exn sw2));
            ("pt2", `Int (Int32.to_int_exn pt2))]

let from_json_header_val (json : json) : header_val =
  let open Yojson.Basic.Util in
  let value = json |> member "value" in
  match json |> member "header" |> to_string with
  | "switch" -> Switch (value |> to_int |> Int64.of_int)
  | "location" ->
    let value = match value |> member "type" |> to_string with
      | "physical" -> Physical (value |> member "port" |>
                                to_int |> int_to_uint32)
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
  | "ip4src" -> IP4Src (value |> member "addr" |> to_int |> int_to_uint32,
                        value |> member "mask" |> to_int |> int_to_uint32)
  | "ip4dst" -> IP4Dst (value |> member "addr" |> to_int |> int_to_uint32,
                        value |> member "mask" |> to_int |> int_to_uint32)
  | "tcpsrcport" -> TCPSrcPort (value |> to_int)
  | "tcpdstport" -> TCPDstPort (value |> to_int)
  | str -> raise (Invalid_argument ("invalid header " ^ str))

let rec from_json_pred (json : json) : pred =
  let open Yojson.Basic.Util in
  match json |> member "type" |> to_string with
  | "true" -> True
  | "false" -> False
  | "test" -> Test (from_json_header_val json)
  | "and" -> mk_big_and (json |> member "preds" |> to_list
                         |> List.map ~f:from_json_pred)
  | "or" -> mk_big_or (json |> member "preds" |> to_list
                       |> List.map ~f:from_json_pred)
  | "neg" -> Neg (json |> member "pred" |> from_json_pred)
  | str -> raise (Invalid_argument ("invalid predicate type " ^ str))

let rec from_json_pol (json : json) : policy =
  let open Yojson.Basic.Util in
   match json |> member "type" |> to_string with
   | "filter" -> Filter (json |> member "pred" |> from_json_pred)
   | "mod" -> Mod (from_json_header_val json)
   | "union" -> mk_big_union (json |> member "pols" |> to_list
                              |> List.map ~f:from_json_pol)
   | "seq" -> mk_big_seq (json |> member "pols" |> to_list
                          |> List.map ~f:from_json_pol)
   | "disjoint" ->
     mk_big_disjoint_union (json |> member "pols" |> to_list
                            |> List.map ~f:from_json_pol)
   | "star" -> Star (from_json_pol (json |> member "pol"))
   | "link" -> Link (json |> member "sw1" |> to_int |> Int64.of_int,
                     json |> member "pt1" |> to_int |> int_to_uint32,
                     json |> member "sw2" |> to_int |> Int64.of_int,
                     json |> member "pt2" |> to_int |> int_to_uint32)
   | str -> raise (Invalid_argument ("invalid policy type " ^ str))

let to_json = to_json_pol

let from_json = from_json_pol

(* by default, Yojson produces non-standard JSON *)
let to_json_string (pol : policy) : string =
  Yojson.Basic.to_string ~std:true (to_json pol)

let from_json_string (str : string) : policy = from_json (from_string str)

let policy_from_json_channel (chan : In_channel.t) : policy =
  from_json (from_channel chan)

let pattern_to_json (p:Pattern.t) : Yojson.Safe.json =
  let open Pattern in
  let i = function
    | None ->
       `Null
    | Some (a,m) ->
       `Tuple [`Intlit(Packet.string_of_ip a);
               `Intlit(Int32.to_string m)] in
  let o f = function None -> `Null | Some x -> `Intlit(f x) in
  `Assoc [
     ("dlSrc", o Packet.string_of_mac p.dlSrc);
     ("dlDst", o Packet.string_of_mac p.dlDst);
     ("dlTyp", o string_of_int p.dlTyp);
     ("dlVlan", o string_of_int p.dlVlan);
     ("dlVlanPcp", o string_of_int p.dlVlanPcp);
     ("nwSrc", i p.nwSrc);
     ("nwDst", i p.nwDst);
     ("nwProto", o string_of_int p.nwProto);
     ("tpSrc", o string_of_int p.tpSrc);
     ("tpDst", o string_of_int p.tpDst);
     ("inPort", o Int32.to_string p.inPort) ]

let pseudoport_to_json (p:pseudoport) : Yojson.Safe.json =
  match p with
  | Physical p ->
     `List[`String "Physical"; `Intlit (Int32.to_string p)]
  | InPort ->
     `String "InPort"
  | Table ->
     `String "Table"
  | Normal ->
     `String "Normal"
  | Flood ->
     `String "Flood"
  | All ->
     `String "All"
  | Controller(n) ->
     `List[`String "Controller"; `Intlit (string_of_int n)]
  | Local ->
     `String "Local"

let modify_to_json (m:modify) : Yojson.Safe.json =
  match m with
  | SetEthSrc m ->
     `List [`String "SetDlSrc"; `Intlit (Packet.string_of_mac m)]
  | SetEthDst m ->
     `List [`String "SetDlDst"; `Intlit (Packet.string_of_mac m)]
  | SetVlan o ->
     `List [`String "SetVlan"; `Intlit (match o with None -> "0xffff" | Some n -> string_of_int n)]
  | SetVlanPcp n ->
     `List [`String "SetVlanPcp"; `Intlit (string_of_int n)]
  | SetEthTyp n ->
     `List [`String "SetDlTyp"; `Intlit (string_of_int n)]
  | SetIPProto n ->
     `List [`String "SetNwProto"; `Intlit (string_of_int n)]
  | SetIP4Src n ->
     `List [`String "SetNwSrc"; `Intlit (Packet.string_of_ip n)]
  | SetIP4Dst n ->
     `List [`String "SetNwDst"; `Intlit (Packet.string_of_ip n)]
  | SetTCPSrcPort n ->
     `List [`String "SetTpSrc"; `Intlit (string_of_int n)]
  | SetTCPDstPort n ->
     `List [`String "SetTpDst"; `Intlit (string_of_int n)]

let action_to_json (a:action) : Yojson.Safe.json =
  match a with
  | Output p ->
     `List [`String "Output"; pseudoport_to_json p]
  | Enqueue (p,q) ->
     `List [`String "Enqueue"; `Intlit (Int32.to_string p); `Intlit (Int32.to_string q)]
  | Modify m ->
     `List [`String "Modify"; modify_to_json m]

let seq_to_json (s:seq) : Yojson.Safe.json =
  `List (List.map ~f:action_to_json s)

let par_to_json (p:par) : Yojson.Safe.json =
  `List (List.map ~f:seq_to_json p)

let action_to_json (g:group) : Yojson.Safe.json =
  match g with
  | [p] -> par_to_json p
  | _ -> failwith "group with more than 1 element"

let timeout_to_json (t:timeout) : Yojson.Safe.json =
  match t with
  | Permanent ->
     `String "Permanent"
  | ExpiresAfter n ->
     `List [`String "ExpiresAfter"; `Intlit (string_of_int n)]

let flow_to_json (n:int) (f:flow) : Yojson.Safe.json =
  `Assoc [
     ("priority", `Intlit (string_of_int n));
     ("pattern", pattern_to_json f.pattern);
     ("action", action_to_json f.action);
     ("cookie", `Intlit (Int64.to_string f.cookie));
     ("idle_timeout", timeout_to_json f.idle_timeout);
     ("hard_timeout", timeout_to_json f.hard_timeout)
   ]

let flowtable_to_json (t:flowTable) : Yojson.Safe.json =
  let l,_ =
    List.fold_right t ~init:([], 65535)
      ~f:(fun f (acc,i) -> (flow_to_json i f::acc, i - 1))
  in `List l

let flowtable_to_json_string (t:flowTable) : string =
  Yojson.Safe.to_string (flowtable_to_json t)

