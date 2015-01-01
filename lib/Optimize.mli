open NetKAT_Types

val mk_and : pred -> pred -> pred
val mk_or : pred -> pred -> pred
val mk_not :  pred -> pred
val mk_filter : pred -> policy
val mk_union : policy -> policy -> policy
val mk_seq : policy -> policy -> policy
val mk_star : policy -> policy
val specialize_pred : switchId -> pred -> pred
val specialize_policy : switchId -> policy -> policy
val mk_big_and : pred list -> pred
val mk_big_union : policy list -> policy
val mk_big_seq : policy list -> policy
