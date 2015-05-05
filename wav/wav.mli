(** wav.mli, wav.ml --- A lightweight WAV reader/writer

    [MIT License] Copyright (C) 2015 Akinori ABE *)

type wav_data =
  | MONORAL of float array
  | STEREO of (float * float) array

(** [load filename] loads a wav file of path [filename].
    @return [(fs, x)] where [fs] is the sampling rate and [x] is wav data. *)
val load : string -> int * wav_data

(** [save ?sampling_bits ~sampling_rate filename x] saves data [x] into a wav
    file of path [filename].
    @param sampling_bits quantization bits ([8] or [16]).
    @param sampling_rate the sampling rate of [data] (Hz). *)
val save : ?sampling_bits:int -> sampling_rate:int -> string -> wav_data -> unit
