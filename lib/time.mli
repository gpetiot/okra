type t = Days of float | Hours of float

val of_string : float -> string -> t option
val nil : t
val days : float -> t
val hours : float -> t
val equal : t -> t -> bool
val add : t -> t -> t

val ( +. ) : t -> t -> t
(** Alias for [add]. *)

val pp : t Fmt.t
