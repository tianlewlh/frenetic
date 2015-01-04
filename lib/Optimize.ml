let mk_and pr1 pr2 =
  match pr1, pr2 with
    | NetKAT_Types.True, _ ->
      pr2
    | _, NetKAT_Types.True ->
      pr1
    | NetKAT_Types.False, _ ->
      NetKAT_Types.False
    | _, NetKAT_Types.False ->
      NetKAT_Types.False
    | _ ->
      NetKAT_Types.And(pr1, pr2)

let mk_or pr1 pr2 =
  match pr1, pr2 with
    | NetKAT_Types.True, _ ->
      NetKAT_Types.True
    | _, NetKAT_Types.True ->
      NetKAT_Types.True
    | NetKAT_Types.False, _ ->
      pr2
    | _, NetKAT_Types.False ->
      pr1
    | _ ->
      NetKAT_Types.Or(pr1, pr2)

let mk_not pat =
  match pat with
    | NetKAT_Types.False -> NetKAT_Types.True
    | NetKAT_Types.True -> NetKAT_Types.False
    | _ -> NetKAT_Types.Neg(pat)

let mk_filter pr =
  NetKAT_Types.Filter (pr)

let mk_union pol1 pol2 =
  match pol1, pol2 with
    | NetKAT_Types.Filter NetKAT_Types.False, _ ->
      pol2
    | _, NetKAT_Types.Filter NetKAT_Types.False ->
      pol1
    | _ ->
      NetKAT_Types.Union(pol1,pol2)

let mk_seq pol1 pol2 =
  match pol1, pol2 with
    | NetKAT_Types.Filter NetKAT_Types.True, _ ->
      pol2
    | _, NetKAT_Types.Filter NetKAT_Types.True ->
      pol1
    | NetKAT_Types.Filter NetKAT_Types.False, _ ->
      pol1
    | _, NetKAT_Types.Filter NetKAT_Types.False ->
      pol2
    | _ ->
      NetKAT_Types.Seq(pol1,pol2)

let mk_star pol =
  match pol with
    | NetKAT_Types.Filter NetKAT_Types.True ->
      pol
    | NetKAT_Types.Filter NetKAT_Types.False ->
      NetKAT_Types.Filter NetKAT_Types.True
    | NetKAT_Types.Star(pol1) -> pol
    | _ -> NetKAT_Types.Star(pol)

let specialize_pred sw pr =
  let rec loop pr k =
    match pr with
      | NetKAT_Types.True ->
        k pr
      | NetKAT_Types.False ->
        k pr
      | NetKAT_Types.Neg pr1 ->
        loop pr1 (fun pr -> k (mk_not pr))
      | NetKAT_Types.Test (NetKAT_Types.Switch v) ->
        if v = sw then
          k NetKAT_Types.True
        else
          k NetKAT_Types.False
      | NetKAT_Types.Test _ ->
        k pr
      | NetKAT_Types.And (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_and p1 p2)))
      | NetKAT_Types.Or (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_or p1 p2))) in
  loop pr (fun x -> x)

let specialize_policy sw pol =
  let rec loop pol k =
    match pol with
      | NetKAT_Types.Filter pr ->
        k (NetKAT_Types.Filter (specialize_pred sw pr))
      | NetKAT_Types.Mod hv ->
        k pol
      | NetKAT_Types.Union (pol1, pol2) ->
        loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_union p1 p2)))
      | NetKAT_Types.Seq (pol1, pol2) ->
        loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_seq p1 p2)))
      | NetKAT_Types.Star pol ->
        loop pol (fun p -> k (mk_star p))
      | NetKAT_Types.DisjointUnion (pol1, pol2) ->
        loop pol1 (fun p1 ->
          loop pol2 (fun p2 ->
            (* TODO(arjun): need disjoint union axioms to implement optimizations *)
            k (DisjointUnion (p1, p2))))
      | NetKAT_Types.Link(sw,pt,sw',pt') ->
	failwith "Not a local policy" in
  loop pol (fun x -> x)

let mk_big_and = List.fold_left mk_and NetKAT_Types.True

let mk_big_union = List.fold_left mk_union NetKAT_Types.drop

let mk_big_seq = List.fold_left mk_seq NetKAT_Types.id
