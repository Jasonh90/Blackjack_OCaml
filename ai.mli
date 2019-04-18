(** 
   Contains functions that contribute to configuring the algorithms for
   dealer and AI player.
*)

(** [dealer state] determines the moves for dealer. When dealer has cards 
    totaling to 16 or less, dealer calls hit. If not, dealer calls check. *)
val dealer : State.t -> State.t
