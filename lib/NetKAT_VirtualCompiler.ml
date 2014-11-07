open NetKAT_Types
open Optimize

(* physical location *)
type ploc = switchId * portId

(* virtual location *)
type vloc = vswitchId * vportId

(* node type *)
type ('a, 'b) node_type =
  | InPort of 'a * 'b
  | OutPort of 'a * 'b

(* port wrapper *)
type extendedPort =
  | RealPort of portId
  | Loop

let equal = (=)
let hash = Hashtbl.hash

(* virtual vertex *)
module V_virt = struct
  type t = (vswitchId, vportId) node_type
  let compare = compare
  let equal = equal
  let hash  = hash
end

(* physical vertex *)
module V_phys = struct
  type t = (switchId, extendedPort) node_type
  let compare = compare
  let equal = equal
  let hash = hash
end

module MakeGraph (Params : sig
  type switch
  type port
  val locs_from_pred : pred -> (switch * port) list
  val links_from_topo : policy -> (switch * port * switch * port) list
end) (Vlabel : Graph.Sig.COMPARABLE with type t = (Params.switch, Params.port) node_type) = struct
  include Params
  module G = Graph.Persistent.Digraph.Concrete(Vlabel)
  include G

  let make_graph (ingress : pred) (egress : pred) (topo : policy) =
    let add_vertex v g = add_vertex g v in
    let add_edge v1 v2 g = add_edge g v1 v2 in
    let add_loc (sw, pt) g =
      let in_pt = G.V.create (InPort (sw, pt)) in
      let out_pt = G.V.create (OutPort (sw, pt)) in
      g |> add_vertex in_pt
        |> add_vertex out_pt
        |> add_edge in_pt out_pt in
    let add_link (sw1, pt1, sw2, pt2) g =
      g |> add_loc (sw1, pt1)
        |> add_loc (sw2, pt2)
        |> add_edge (G.V.create (OutPort (sw1, pt1))) (G.V.create (InPort (sw2, pt2))) in
    let connect_switch_ports' v1 v2 g =
      match V.label v1, V.label v2 with
      | InPort (sw, _), OutPort (sw', _) when sw=sw' -> add_edge v1 v2 g
      | _ -> g in
    let connect_switch_ports g =
      fold_vertex (fun v1 g' -> fold_vertex (fun v2 g' -> connect_switch_ports' v1 v2 g') g g') g g
    in
    G.empty |> List.fold_right add_link (links_from_topo topo)
            |> List.fold_right add_loc (locs_from_pred ingress)
            |> List.fold_right add_loc (locs_from_pred egress)
            |> connect_switch_ports
end

module G_virt = MakeGraph (struct
  type switch = vswitchId
  type port = vportId
  let rec locs_from_pred pred =
    match pred with
    | And (Test (VSwitch vsw), Test (VPort vpt)) -> [(vsw, vpt)]
    | Or (p1, p2) -> locs_from_pred p1 @ locs_from_pred p2
    | _ -> failwith "Virtual Compiler: not a valid ingress/egress predicate"
  let rec links_from_topo vtopo =
    match vtopo with
    | VLink (vsw1,vpt1,vsw2,vpt2) -> [(vsw1,vpt1,vsw2,vpt2)]
    | Union (t1, t2) -> links_from_topo t1 @ links_from_topo t2
    | _ -> failwith "Virtual Compiler: not a valid virtual topology"
end) (V_virt)

module G_phys = MakeGraph (struct
  type switch = switchId
  type port = extendedPort
  let rec locs_from_pred pred =
    match pred with
    | And (Test (Switch sw), Test (Location (Physical pt))) -> [(sw, RealPort pt)]
    | Or (p1, p2) -> locs_from_pred p1 @ locs_from_pred p2
    | _ -> failwith "Virtual Compiler: not a valid ingress/egress predicate"
  let rec links_from_topo topo =
    match topo with
    | Link (sw1,pt1,sw2,pt2) -> [(sw1, RealPort pt1, sw2, RealPort pt2)]
    | Union (t1, t2) -> links_from_topo t1 @ links_from_topo t2
    | _ -> failwith "Virtual Compiler: not a valid virtual topology"
end) (V_phys)

(* product vertex *)
type product = (G_virt.V.t * G_phys.V.t)
type product_vertex =
  | ConsistentIn of product
  | InconsistentOut of product
  | ConsistentOut of product
  | InconsistentIn of product

module V_prod = struct
  type t = product_vertex
  let compare = compare
  let equal = equal
  let hash = hash
end

module G_prod = Graph.Persistent.Digraph.Concrete(V_prod)

module Tbl = Core.Std.Hashtbl.Poly

(* aux list functions *)
let inters xs ys = List.find_all (fun x -> List.mem x ys) xs
let product xs ys =
  List.map (fun x -> List.fold_right (fun y xys -> (x,y)::xys) ys []) xs
  |> List.flatten

let parse_vrel (vrel : pred) =
  let rec parse_physical pred alist =
    match pred with
    | Or (p1, p2) -> parse_physical p1 alist |> parse_physical p2
    | And (Test (Switch sw), Test (Location (Physical pt))) -> (sw, RealPort pt) :: alist
    | _ -> failwith "Virtual Compiler: not a valid virtual relation"
  in
  let rec parse pred alist =
    match pred with
    | Or (p1, p2) ->
       parse p1 alist |> parse p2
    | And (And (Test (VSwitch vsw), Test (VPort vpt)), physical) ->
       ((vsw, vpt), parse_physical physical []) :: alist
    | _ ->
       failwith "Virtual Compiler: not a valid virtual relation"
  in
  match Tbl.of_alist (parse vrel []) with
  | `Ok map -> map
  | `Duplicate_key (_, _) -> failwith "Virtual Compiler: virtual relation contains duplicate key"

let make_product_graph (vgraph : G_virt.t) (pgraph : G_phys.t) (ving : pred) (vrel : pred) =
begin

  let vrel = parse_vrel vrel in

  let vrel (vsw, vpt) = Tbl.find vrel (vsw, vpt) |> Core.Std.Option.value ~default:[] in

  let vrel' vvertex =
    match G_virt.V.label vvertex with
    | InPort (vsw, vpt) ->
       vrel (vsw, vpt) |> List.map (fun (sw, pt) -> G_phys.V.create (InPort (sw, pt)))
    | OutPort (vsw, vpt) ->
       vrel (vsw, vpt) |> List.map (fun (sw, pt) -> G_phys.V.create (OutPort (sw, pt)))
  in

  let pgraph_closure =
    let module Op = Graph.Oper.P(G_phys) in
    Op.transitive_closure ~reflexive:false pgraph
  in

  let virt_ing =
    List.map (fun (vsw, vpt) -> InPort (vsw, vpt) |> G_virt.V.create) (G_virt.locs_from_pred ving)
  in

  let prod_ing =
    List.map (fun vv -> product [vv] (vrel' vv)) virt_ing
    |> List.flatten
    |> List.map (fun (vv, pv) -> G_prod.V.create (ConsistentIn (vv, pv)))
  in

  let loop_of pvertex =
    match G_phys.V.label pvertex with
    | InPort (sw, _) -> OutPort (sw, Loop) |> G_phys.V.create
    | OutPort (sw, _) -> InPort (sw, Loop) |> G_phys.V.create
  in

  let step product_vertex =
    begin match G_prod.V.label product_vertex with
    | ConsistentIn (vvertex, pvertex)  ->
       let virtual_sucs = G_virt.succ vgraph vvertex in
       List.map (fun vv -> InconsistentOut (vv, pvertex) |> G_prod.V.create) virtual_sucs
    | InconsistentOut (vvertex, pvertex) ->
       let physical_sucs =
         match vrel' vvertex with
         (* SJS: This is a hack. We interpret [] as true, although to be consistent we would have
                 to interpret it as false *)
         | [] -> G_phys.succ pgraph_closure pvertex
         | logical_sucs -> inters logical_sucs (G_phys.succ pgraph_closure pvertex) in
       List.map (fun psuc -> ConsistentOut (vvertex, psuc) |> G_prod.V.create) physical_sucs
    | ConsistentOut (vvertex, pvertex) ->
       (* SJS: check that if there are no successors, we have reached the egress *)
       let virtual_sucs = G_virt.succ vgraph vvertex in
       List.map (fun vsuc -> InconsistentIn (vsuc, pvertex) |> G_prod.V.create) virtual_sucs
    | InconsistentIn (vvertex, pvertex) ->
       let physical_sucs =
         match vrel' vvertex with
         (* SJS: This is a hack. We interpret [] as true, although to be consistent we would have
                 to interpret it as false *)
         | [] -> G_phys.succ pgraph_closure pvertex
         | logical_sucs -> inters logical_sucs (G_phys.succ pgraph_closure pvertex) in
       List.map (fun pv -> ConsistentIn (vvertex, pv) |> G_prod.V.create) physical_sucs
    end
  in

  let rec make work_list edges g =
    begin match work_list with
    | [] -> List.fold_left (fun g (v1, v2) -> G_prod.add_edge g v1 v2) g edges
    | v::vs ->
       if G_prod.mem_vertex g v then
         make vs edges g
       else
         let g' = G_prod.add_vertex g v in
         let sucs = step v in
         let edges' = List.fold_left (fun edges suc -> (v, suc)::edges) edges sucs in
         make (sucs@work_list) edges' g'
    end
  in

  make prod_ing [] (G_prod.empty)

end


(* module WEIGHT = struct
  type label = unit
  type t = int
  let weight _ = 1
  let compare = compare
  let add x y = x + y
  let zero = 0
end

module Dijkstra = Graph.Path.Dijkstra(G)(WEIGHT) *)

let match_ploc (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt)))))
let match_vloc (vsw,vpt) = Filter (And (Test(VSwitch vsw), Test(VPort vpt)))
let set_vloc (vsw,vpt) = mk_seq (Mod (VSwitch vsw)) (Mod (VPort vpt))

let generate_union (indices : 'a list) (f : 'a -> policy) =
  mk_big_union (List.map f indices)

let dedup xs =
  xs |> Core.Core_list.of_list |> Core.Core_list.dedup |> Core.Core_list.to_list

let get_vlocs (p : policy) =
  let open Core.Std.List in
  let rec get = function
    | Filter (And (Test (VSwitch vsw), Test (VPort vpt))) -> [(vsw, vpt)]
    | Filter _ | Mod _ | Link _ -> []
    | Union (q, r) | Seq (q, r) -> (get q) @ (get r)
    | Star q -> get q in
  to_list (dedup (get p))

(*

  out_fabric/in_fabric have to be of the form
     UNION_{vl : Vloc}{
      match_vloc vl; UNION_{pl : Ploc}{
        match_ploc pl; path(vl, pl)
      }
    }
  where path(vl, pl) is a path in the physical topology ending with a link
    (i) going out of pl in the case of out_fabric, or
    (ii) going into pl in the case of in_fabric

  Note that the framework allows out_fabric/in_fabric to be specified by hand in NetKAT,
  but we could also generate them automatically, e.g. from a virtualization relation and
  the physical topology using Dijkstra.

 ---------------------------

  Vingress defines the virtual ingress. Examples:

   1) The physical ingress is {(1,1)} (i.e. packets can enter the network only
      through port 1 of switch 1), and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   2) The physical ingress is {(1,1), (2,2)} (i.e. packets can enter the network only
      through port 1 of switch 1 and port 2 of switch 2),
      and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   3) The physical ingress is {(1,1)} and we want packets to enter the virtual network
      at both vport 3 of vswitch 3 and vport 4 of switch 4. This is encoded as
        vingress =
          (vswitch := 3; vport := 3) + (vswitch := 4; vport := 4)

   4) The physical ingress is {(1,1), (2,2)} and we want packets from (1,1) to
      enter the virtual network at vport 3 of vswitch 3, and packet from (2,2)
      shall enter at vport 4 of vswitch 4. This is encoded as
        vingress =
          switch = 1; port = 1; vswitch := 3; vport := 3
        + switch = 2; port = 2; vswitch := 4; vport := 4

   5) I just realized that the framework can even handle more complicated virtual ingress
      specifications as the ones Arjun mentioned in our last meeting, e.g.
        vingress =
          IPProto = tcp; vswitch := 1; vport := 1
        + IPProto = ucp; vswitch := 2; vport := 1
      This is super awesome!!


  To gurantee correctness we will have to do some sort of "type checking", i.e. we have to make sure
  certain pre conditions are met.

*)

let compile (vpolicy : policy) (vtopo : policy) (vingress : policy)
(out_fabric : policy) (in_fabric : policy) =
  let fout = generate_union (get_vlocs out_fabric)
    (fun vl -> mk_big_seq [match_vloc vl; out_fabric; set_vloc vl]) in
  let fin = generate_union (get_vlocs in_fabric)
    (fun vl -> mk_big_seq [match_vloc vl; in_fabric; set_vloc vl]) in
  let ing = mk_seq vingress fin in
  let p = mk_seq vpolicy fout in
  let t = mk_seq vtopo fin in
  (* ing; p; (t;p)^*  *)
  mk_big_seq [ing; p; mk_star (mk_seq t p)]

  