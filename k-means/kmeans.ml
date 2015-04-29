(** kmeans.ml --- K-means clustering

    [MIT License] Copyright (C) 2015 Akinori ABE
*)

open Format

module Array = struct
  include Array

  let iter2 f x y = iteri (fun i xi -> f xi y.(i)) x

  let foldi f init x =
    snd (fold_left (fun (i, acc) xi -> (i+1, f i acc xi)) (0, init) x)

  let min f x =
    foldi
      (fun i (i0, v0) xi -> let v = f xi in if v0 > v then (i, v) else (i0, v0))
      (-1, max_float) x

  let fold_left2 f init x y = foldi (fun i acc xi -> f acc xi y.(i)) init x

  let map2_sum f = fold_left2 (fun acc xi yi -> acc +. f xi yi) 0.0
end

(** [distance x y] returns the square of the L2 norm of the distance between
    vectors [x] and [y], i.e., [||x - y||^2]. *)
let distance = Array.map2_sum (fun xi yi -> let diff = xi -. yi in diff *. diff)

(** [kmeans k xs] performs [k]-means clustering algorithm for data set [xs].
    @return [(means, cs)] where [means] is an array of mean vectors, and [cs] is
    an array such that the [i]-th element is the class number of [xs.(i)]. *)
let kmeans k xs =
  let d = Array.length xs.(0) in (* the dimension of a sample *)
  let calc_means cs = (* Compute the mean of each class *)
    let z = Array.init k (fun _ -> (ref 0, Array.make d 0.0)) in
    let sum_up ci xi =
      let (n, sum) = z.(ci) in
      Array.iteri (fun j xij -> sum.(j) <- sum.(j) +. xij) xi; (* sum += xi *)
      incr n
    in
    let normalize (n, sum) =
      let c = 1.0 /. float !n in
      Array.map (( *. ) c) sum
    in
    Array.iter2 sum_up cs xs;
    Array.map normalize z
  in
  let update means cs = (* Update class assignment *)
    Array.foldi (fun i updated xi ->
      let ci', _ = Array.min (distance xi) means in
      if cs.(i) <> ci' then (cs.(i) <- ci' ; true) else updated)
    false xs
  in
  let m = Array.length xs in (* the number of samples *)
  let cs = Array.init m (fun i -> i mod k) in (* class assignment *)
  let rec loop () =
    let means = calc_means cs in
    if update means cs then loop () else (means, cs)
  in
  loop () (* loop until convergence *)

let show_result k xs cs =
  let ys = Array.map snd Dataset.samples in (* answers *)
  let tbl = Array.make_matrix k k 0 in
  Array.iter2 (fun ci yi -> tbl.(ci).(yi) <- succ tbl.(ci).(yi)) cs ys;
  for prd = 0 to k - 1 do
    for ans = 0 to k - 1 do
      printf "Prediction = %d, Answer = %d: %d points@\n"
        prd ans tbl.(prd).(ans)
    done
  done

let () =
  let k = Dataset.n_classes in
  let xs = Array.map fst Dataset.samples in
  let (means, cs) = kmeans k xs in
  printf "mean vectors:@\n";
  Array.iteri (fun i mi ->
      printf "[%d]" i;
      Array.iter (printf " %.2f") mi;
      print_newline ())
    means;
  show_result k xs cs
