(** qr_gram_schmidt.ml --- QR decompisition by Gram-Schmidt orthonormalization

    [MIT License] Copyright (C) 2015 Akinori ABE
*)

open Format

module Array = struct
  include Array

  let init_matrix m n f = init m (fun i -> init n (f i))

  let matrix_size a =
    let m = length a in
    let n = if m = 0 then 0 else length a.(0) in
    (m, n)

  let map2 f x y = mapi (fun i xi -> f xi y.(i)) x

  let fold_left2 f init x y =
    let acc = ref init in
    for i = 0 to length x - 1 do acc := f !acc x.(i) y.(i) done;
    !acc

  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
end

(** [foldi f init i j] is [f (... (f (f init i) (i+1)) ...) j]. *)
let foldi f init i j =
  let acc = ref init in
  for k = i to j do acc := f !acc k done;
  !acc

(** [sumi f i j] is [f i +. f (i+1) +. ... +. f j] *)
let sumi f i j = foldi (fun acc k -> acc +. f k) 0.0 i j

(** Dot product of two vectors *)
let dot = Array.fold_left2 (fun acc xi yi -> acc +. xi *. yi) 0.0
let scal c = Array.map (( *. ) c)

let transpose x =
  let m, n = Array.matrix_size x in
  Array.init_matrix m n (fun i j -> x.(j).(i))

(** Gram-Schmidt orthonormalization *)
let orthonormalize at =
  let orth_aux x qt acc j = Array.map2 (-.) acc (scal (dot x qt.(j)) qt.(j)) in
  let orth x qt i = foldi (orth_aux x qt) x 0 (i-1) in
  let a = transpose at in
  let n = Array.length a in
  let qt = Array.make n [||] in
  for i = 0 to n-1 do
    let qi = orth a.(i) qt i in
    let nrm = dot qi qi in
    qt.(i) <- if nrm > 1e-6 then scal (1. /. sqrt nrm) qi else Array.make n 0.
  done;
  let q = transpose qt in
  q

(** Compute a right triangular matrix *)
let calc_r a q =
  let n = Array.length a in
  let r = Array.make_matrix n n 0.0 in
  for i = 0 to n-1 do
    for j = i to n-1 do
      r.(i).(j) <- sumi (fun k -> q.(k).(i) *. a.(k).(j)) 0 (n-1)
    done
  done;
  r

(** Matrix multiplication *)
let gemm x y =
  let m, k = Array.matrix_size x in
  let k', n = Array.matrix_size y in
  assert(k = k');
  Array.init_matrix m n
    (fun i j -> sumi (fun l -> x.(i).(l) *. y.(l).(j)) 0 (k - 1))

let print_mat label x =
  printf "%s =@\n" label;
  Array.iter (fun xi ->
      Array.iter (printf "  %5.2f") xi;
      print_newline ()) x

let () =
  let a =
    [|
      [| 3.; 5.; 0.; 0.; 1.|];
      [| 0.; 2.; 3.; 0.; 9.|];
      [|-1.; 1.; 4.; 2.; 3.|];
      [| 6.; 0.;-9.; 1.; 0.|];
      [|-8.; 3.; 1.;-5.; 2.|];
    |] in
  let q = orthonormalize a in
  let r = calc_r a q in
  print_mat "Q [orthogonal matrix]" q;
  print_mat "R [right triangular matrix]" r;
  print_mat "Q * R" (gemm q r)
