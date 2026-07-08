open! Core

module Build_list = struct
  (* [acc @ [ x ]] copies the whole accumulator each step -> O(n^2)
     allocation. *)
  let silly xs =
    let rec silly_helper acc xs =
      match xs with
      | x :: excess -> silly_helper (acc @ [ x ]) excess
      | [] -> acc
    in
    silly_helper [] xs
  ;;

  (* Prepend (O(1) per step) then reverse once -> O(n) allocation. Same
     result. *)
  let non_silly xs =
    let rec silly_helper acc xs =
      match xs with
      | x :: excess -> silly_helper (x :: acc) excess
      | [] -> acc
    in
    silly_helper [] xs |> List.rev
  ;;
end

module First_match = struct
  (* Allocate a fresh list of *every* match, then throw all but the head
     away. *)
  let silly xs ~f =
    match List.filter xs ~f with head :: _ -> Some head | [] -> None
  ;;

  (* Stop at the first match; allocate nothing but the returned [Some]. *)
  let non_silly xs ~f = List.find xs ~f
end
