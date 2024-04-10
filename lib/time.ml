type t = Days of float | Hours of float

let of_string x = function
  | ("d" | "day" | "days") when Float.is_integer @@ Float.div x 0.125 ->
      Some (Days x)
  | ("h" | "hour" | "hours") when Float.is_integer x -> Some (Hours x)
  | _ -> None

let to_days = function Days x -> x | Hours x -> x *. 0.125
let equal x y = Float.equal (to_days x) (to_days y)
let nil = Hours 0.
let days x = Days x
let hours x = Hours x

let add x y =
  match (x, y) with
  | Hours x, Hours y -> Hours (x +. y)
  | x, y -> Days (to_days x +. to_days y)

let ( +. ) = add

let pp fs x =
  let pp_float fs f =
    if classify_float (fst (modf f)) = FP_zero then Fmt.pf fs "%.0f" f
    else if Float.is_integer @@ Float.div f 0.1 then Fmt.pf fs "%.1f" f
    else if Float.is_integer @@ Float.div f 0.01 then Fmt.pf fs "%.2f" f
    else Fmt.pf fs "%.3f" f
  in
  let data, unit =
    match x with Days x -> (x, "day") | Hours x -> (x, "hour")
  in
  if Float.equal data 1. then Fmt.pf fs "1 %s" unit
  else Fmt.pf fs "%a %ss" pp_float data unit
