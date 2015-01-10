open NetKAT_Types
open Optimize

(** Local and global program counters.
    A program counter identifies a location in a given program uniquely. We will write a program
    counter to a packet in order to keep track of the packet's current location in the program,
    i.e. it's state of execution.
    Local program counters are used to identify and recod the state of the execution within a
    switch, while global program counters are used to identify and record the state of the execution
    when a packet crosses a link.
    Since we never need to maintain both the local and the global execution state simultaneously,
    we can map both program counters to the same phyiscal header field; we use the vlan field.
    Note that since the local program counter is maintained only within a switch, all tests and
    modifications of it will be optimized away by the local compiler. *)
let final_local_pc = 0
let initial_local_pc = final_local_pc + 1

(** Note that 0xffff = null. Using null as the inital global pc is a convenient hack with the result
    that the vlan field is only used if it is actually needed, i.e. if there are several global
    program counters *)
let initial_global_pc = 0xffff

let match_pc pc = Filter (Test (Vlan pc))

let set_pc pc = Mod (Vlan pc)

let unset_pc = Mod (Vlan 0xffff)

let match_location (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt)))))

(** link ends are identified by a global program counter modulo a switch and a port *)
let match_link_end sw pt pc =
  let t1 = Test (Vlan pc) in
  let t2 = Test (Switch sw) in
  let t3 = Test (Location(Physical(pt))) in
  Filter (mk_big_and [t1; t2; t3])

let rec link_free p =
  match p with
  | Filter _ | Mod _ -> true
  | Union (q,r) | Seq (q,r) -> link_free q && link_free r
  | Star q -> link_free q
  | Link _ -> false

(** Conceptually, the CPS translation transforms a given global program (that may contain links)
    to a big disjoint union of "CPS atoms" iterated by the kleene star. A CPS atom is a link-free
    policy that is guarded by a test of the program counter (and is hence only applied to packets
    with matching program location). After the guard, the atom executes the program identified by
    the program counter, and finally sets the program counter to the continuation, i.e. the program
    that immediately succeeds the program encapsulated by the atom.
    This conceptual idea is slightly adapted to achieve correctness and to allow for a
    differentiation of local and global programs counters. The big union is partitioned into
    Entrance, Local, and Exit CPS atoms. The Entrance atoms are those executed immediately
    when entering a switch, the Exit atoms are thosed executed immediately before leaving a switch,
    and the Local atoms are all those executed in between (basically the local component of the
    global program).
    An Entrance atom has a global program counter and a continuation with local program
    counter (i.e. a Local atom); a Local atom has a local program counter and a continuation with a
    local program counter (i.e. a Local atom or an Exit atom); and an Exit atom has a local program
    counter and a continuation with a global program counter (i.e. an Entrance atom).

    The CPS function computes the sets of Entrance, Local, and Exit atoms.

    The compiler returns the policy ENTRANCE; LOCAL*; EXIT, where ENTRANCE, LOCAL, and EXIT denote
    the union of all Entrance, Local, and Exit atoms, respectively. *)

type cps_policy =
| Entrance of policy
| Local of policy
| Exit of policy

let rec flatten_union (pol : policy) : policy list = match pol with
  | Union (p, q) -> flatten_union p @ flatten_union q
  | _ -> [pol]

let cps (p : policy) =
  let module Tbl = Core.Std.Hashtbl.Poly in
  let local_pc_ref = ref final_local_pc in
  let global_pc_tbl = Tbl.create () in
  let next_local_pc () =
    local_pc_ref := (!local_pc_ref + 1); !local_pc_ref in
  let next_global_pc sw pt =
    let pc = Tbl.find global_pc_tbl (sw, pt) |> Core.Std.Option.value ~default:initial_global_pc in
    Tbl.replace global_pc_tbl ~key:(sw, pt) ~data:(pc-1); pc in
  let rec cps' p pc k =
    match p with
    | Filter _ | Mod _ ->
       [Local (mk_big_seq [match_pc pc; p; set_pc k])]
    | p when link_free p ->
       [Local (mk_big_seq [match_pc pc; p; set_pc k])]
    | Union _ ->
       let pols = flatten_union p in
       let pcs = List.map (fun _ -> next_local_pc ()) pols in
       let jump_lst = mk_big_union (List.map (fun pc -> set_pc pc) pcs) in
       let rest = List.flatten (List.map2 (fun pol pc -> cps' pol pc k) pols pcs) in
       Local (mk_big_seq [match_pc pc; jump_lst]) :: rest
    | Seq (q,r) ->
       let pc' = next_local_pc () in
       (cps' q pc pc') @ (cps' r pc' k)
    | Star q ->
       let pc_q = next_local_pc () in
       Local (mk_big_seq [match_pc pc ; mk_big_union [set_pc pc_q; set_pc k]]) :: (cps' q pc_q pc)
    | Link (sw1,pt1,sw2,pt2) ->
       let gpc = next_global_pc sw2 pt2 in
       [Exit (mk_big_seq [match_link_end sw1 pt1 pc; set_pc gpc]);
        Entrance (mk_big_seq [match_link_end sw2 pt2 gpc; set_pc k])] in
  cps' p (next_local_pc ()) final_local_pc

let split_cps (cps : cps_policy list) =
  let clasify_cps_policy (entrance, local, exit) = function
    | Entrance p -> (p::entrance, local, exit)
    | Local p -> (entrance, p::local, exit)
    | Exit p -> (entrance, local, p::exit) in
  List.fold_left clasify_cps_policy ([],[],[]) cps

let compile (ingress : pred) (egress : pred) (p : policy) =
  let (entrance, local, exit) = split_cps (cps p) in
  let pre = mk_big_seq [Filter ingress; set_pc initial_local_pc] in
  let post = mk_big_seq [match_pc final_local_pc; Filter egress; unset_pc] in
  mk_big_seq [mk_big_union (pre::entrance); mk_star (mk_big_union local); mk_big_union (post::exit)]
