(** fft.ml --- Cooley-Tukey fast Fourier transform algorithm *)

open Format
open Complex

(** [get_n_bits n] returns the number of bits of [n]. *)
let get_n_bits =
  let rec aux n i =
    if i = 1 then n
    else if i > 0 && i land 1 = 0 then aux (n + 1) (i lsr 1)
    else invalid_arg "invalid input length"
  in
  aux 0

(** [bitrev n i] bit-reverses [n]-digit integer [i]. *)
let bitrev =
  let rec aux acc n i =
    if n = 0 then acc else aux ((acc lsl 1) lor (i land 1)) (n - 1) (i lsr 1)
  in
  aux 0

let ( +! ) = add
let ( -! ) = sub
let ( *! ) = mul
let ( /! ) = div

let make_twiddle_factors len =
  let pi = 3.14159265358979 in
  let c = ~-. 2.0 *. pi /. float len in
  Array.init (len / 2) (fun i -> exp { re = 0.; im = c *. float i })

let fft x =
  let len = Array.length x in
  let n_bits = get_n_bits len in
  let w = make_twiddle_factors len in
  let y = Array.init len (fun i -> x.(bitrev n_bits i)) in
  let butterfly m n ofs =
    for i = 0 to n / 2 - 1 do
      let j, k = ofs + i, ofs + i + n / 2 in
      let a, b = y.(j), y.(k) in
      y.(j) <- a +! w.(i * m) *! b;
      y.(k) <- a -! w.(i * m) *! b;
    done
  in
  for nb = 1 to n_bits do
    let n = 1 lsl nb in
    let m = 1 lsl (n_bits - nb) in
    for i = 0 to m - 1 do butterfly m n (n * i) done
  done;
  y

let ifft x =
  let c = 1.0 /. float (Array.length x) in
  let normalize z = { re = c *. z.re; im = ~-. c *. z.im } in
  fft (Array.map normalize x)

let main () =
  let x = [|
    1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0;
    9.0; 10.0; 11.0; 12.0; 13.0; 14.0; 15.0; 16.0;
  |]
    |> Array.map (fun x -> { re = x; im = 0.0 }) in
  let y = fft x in
  printf "FFT =@\n  @[";
  Array.iteri (fun i yi -> printf "[%d] %f %+fi@\n" i yi.re yi.im) y;
  printf "@]@\n";
  let z = ifft y in
  printf "IFFT =@\n  @[";
  Array.iteri (fun i zi -> printf "[%d] %f %+fi@\n" i zi.re zi.im) z;
  printf "@]@."

let () = main ()
