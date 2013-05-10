open List
open Misc
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module Pattern = NetCoreAction.NetCoreAction.Pattern

type pattern = Pattern.t

type pred =
| PrHdr of pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

type pol =
| PoAction of NetCoreAction.NetCoreAction.t
| PoFilter of pred
| PoUnion of pol * pol
| PoSeq of pol * pol

type value =
| Pkt of switchId * NetCoreAction.NetCoreAction.port * packet
   * (bufferId, bytes) sum

(** val value_rect :
    (switchId -> NetCoreAction.NetCoreAction.port -> packet -> (bufferId,
    bytes) sum -> 'a1) -> value -> 'a1 **)

(** val match_pred : pred -> switchId -> Pattern.port -> packet -> bool **)

let rec match_pred pr sw pt pk =
  match pr with
  | PrHdr pat -> Pattern.match_packet pt pk pat
  | PrOnSwitch sw' -> if Word64.eq_dec sw sw' then true else false
  | PrOr (p1, p2) -> (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrAnd (p1, p2) -> (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrNot p' -> not (match_pred p' sw pt pk)
  | PrAll -> true
  | PrNone -> false

(** val serialize_pkt : packet -> bytes **)

let serialize_pkt = Packet_Parser.serialize_packet

(** val eval_action :
    value -> NetCoreAction.NetCoreAction.t -> value list **)

let eval_action inp act =
  let Pkt (sw, pt, pk, buf) = inp in
  map (fun ptpk -> let (pt', pk') = ptpk in Pkt (sw, pt', pk', buf))
    (NetCoreAction.NetCoreAction.apply_action act (pt, pk))

(** val classify : pol -> value -> value list **)

let rec classify p inp =
  match p with
  | PoAction action -> eval_action inp action
  | PoFilter pred0 ->
    let Pkt (sw, pt, pk, buf) = inp in
    if match_pred pred0 sw pt pk then inp :: [] else []
  | PoUnion (p1, p2) -> (classify p1 inp) @ (classify p2 inp)
  | PoSeq (p1, p2) -> concat_map (classify p2) (classify p1 inp)

