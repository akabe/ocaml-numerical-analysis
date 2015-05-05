(** wav.mli, wav.ml --- A lightweight WAV reader/writer

    [MIT License] Copyright (C) 2015 Akinori ABE *)

open Format

(* WAVE (linear PCM) data format

   +-----+-------+---------------------------------------------+
   | pos | bytes | Meaning                                     |
   +-----+-------+---------------------------------------------+
   |   0 |     4 | RIFF header "RIFF"                          |
   |   4 |     4 | #bytes of file - 8                          |
   |   8 |     4 | WAVE header "WAVE"                          |
   |  12 |     4 | FMT chunk "fmt "                            |
   |  16 |     4 | #bytes of FMT chunk (16 in linear PCM)      |
   |  20 |     2 | Format ID (1 in linear PCM)                 |
   |  22 |     2 | #channels (monoral = 1, stereo = 2)         |
   |  24 |     4 | Sampling rate [Hz]                          |
   |  28 |     4 | Data speed [Byte/sec]                       |
   |  32 |     2 | Block size [Byte/(sample * #channels)]      |
   |  34 |     2 | #bits per one sample [bit/sample] (8 or 16) |
   |  36 |     4 | DATA chunk "data"                           |
   |  40 |     4 | #bytes of DATA chunk                        |
   |  44 |     ? | Data                                        |
   +-----+-------+---------------------------------------------+ *)

type wav_data =
  | MONORAL of float array
  | STEREO of (float * float) array

(** Output little-endian unsigned 16-bit integer. *)
let output_le_u16 oc n =
  output_byte oc (n land 0xff);
  output_byte oc ((n land 0xff00) lsr 8)

(** Output little-endian unsigned 31-bit integer. *)
let output_le_u31 oc n =
  output_le_u16 oc (n land 0xffff);
  output_le_u16 oc ((n land 0x7fff0000) lsr 16)

let output_le_s8 oc n = output_byte oc (if n >= 0 then n else n + 0x100)
let output_le_s16 oc n = output_le_u16 oc (if n >= 0 then n else n + 0x10000)
let output_le_s8f oc x = output_le_s8 oc (int_of_float (x *. 128.))
let output_le_s16f oc x = output_le_s16 oc (int_of_float (x *. 32768.))

let save ?(sampling_bits = 16) ~sampling_rate filename x =
  let channels, n = match x with
    | MONORAL x -> 1, Array.length x
    | STEREO x -> 2, Array.length x in
  let output_pt = match sampling_bits with
    | 8 -> output_le_s8f
    | 16 -> output_le_s16f
    | _ -> invalid_arg "Invalid sampling bits (8 or 16 is supported)" in
  let block_size = channels * (sampling_bits / 8) in
  let data_bytes = n * block_size in
  let oc = open_out_bin filename in
  output_string oc "RIFF";
  output_le_u31 oc (36 + data_bytes); (* #bytes of DATA chunk *)
  output_string oc "WAVEfmt ";
  output_le_u31 oc 16; (* #bytes of FMT chunk *)
  output_le_u16 oc 1; (* format ID (linear PCM) *)
  output_le_u16 oc channels; (* #channels *)
  output_le_u31 oc sampling_rate; (* sampling rate *)
  output_le_u31 oc (block_size * sampling_rate); (* data speed *)
  output_le_u16 oc block_size; (* block size *)
  output_le_u16 oc sampling_bits; (* #bits per one sample *)
  output_string oc "data"; (* DATA chunk *)
  output_le_u31 oc data_bytes; (* #bytes of DATA chunk *)
  begin match x with
    | MONORAL x -> Array.iter (output_pt oc) x
    | STEREO x -> Array.iter (fun (l, r) -> output_pt oc l ; output_pt oc r) x
  end;
  close_out oc

(** Input little-endian unsigned 16-bit integer. *)
let input_le_u16 ic =
  let n1 = input_byte ic in
  let n2 = input_byte ic in
  n1 lor (n2 lsl 8)

(** Input little-endian unsigned 31-bit integer. *)
let input_le_u31 ic =
  let n1 = input_le_u16 ic in
  let n2 = input_le_u16 ic in
  n1 lor (n2 lsl 16)

let signed_of_unsigned m n = if n < m then n else n - m * 2

let input_le_s8 ic = signed_of_unsigned 0x80 (input_byte ic)
let input_le_s16 ic = signed_of_unsigned 0x8000 (input_le_u16 ic)
let input_le_s8f ic = float (input_le_s8 ic) *. (1. /. 128.)
let input_le_s16f ic = float (input_le_s16 ic) *. (1. /. 32768.)

let check_format ic =
  if really_input_string ic 4 <> "RIFF" then failwith "Not RIFF format";
  seek_in ic 8;
  if really_input_string ic 8 <> "WAVEfmt " then failwith "Not WAVE format";
  let fmt_bytes = input_le_u31 ic in
  let fmt_id = input_le_u16 ic in
  if fmt_bytes <> 16 || fmt_id <> 1 then failwith "Not linear PCM format";
  seek_in ic 36;
  if really_input_string ic 4 <> "data" then failwith "No DATA chunk"

let load_points ~data_bytes ~sampling_bits ~channels ic =
  let n = data_bytes / (channels * sampling_bits / 8) in
  let input_pt = if sampling_bits = 8 then input_le_s8f else input_le_s16f in
  let input_pt2 ic =
    let x = input_pt ic in
    let y = input_pt ic in
    (x, y)
  in
  match channels with
  | 1 -> MONORAL (Array.init n (fun _ -> input_pt ic))
  | 2 -> STEREO (Array.init n (fun _ -> input_pt2 ic))
  | _ -> failwith "Unexpected #channels"

let load filename =
  let ic = open_in_bin filename in
  check_format ic;
  seek_in ic 22;
  let channels = input_le_u16 ic in
  let sampling_rate = input_le_u31 ic in
  seek_in ic 34;
  let sampling_bits = input_le_u16 ic in
  seek_in ic 40;
  let data_bytes = input_le_u31 ic in
  let x = load_points ~data_bytes ~sampling_bits ~channels ic in
  close_in ic;
  (sampling_rate, x)
