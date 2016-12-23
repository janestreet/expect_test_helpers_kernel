open! Core_kernel.Std

include Helpers_intf

module Make (Print : Print) = struct

  open Print

  let hide_positions_in_string =
    let module Re = Re_pcre in
    let expanders = lazy (
      [ (* This first pattern has an alphabetic prefix because we want to match exceptions
           and their file positions without also matching timestamps.  However, [Re_pcre]
           doesn't implement back-references, precluding a simple substitution.  Instead,
           we provide a length of matched data to copy into the output, effectively acting
           like a back-reference in this special case. *)
        "[a-zA-z]:[0-9]+:[0-9]+" , 1, ":LINE:COL"
      ; "line [0-9]+, characters [0-9]+-[0-9]+" , 0, "line LINE, characters C1-C2" ]
      |> List.map ~f:(fun (pattern, prefix_len, expansion) ->
        let rex = Re.regexp pattern in
        fun string ->
          Re.substitute ~rex string ~subst:(fun orig ->
            String.concat [ String.prefix orig prefix_len; expansion])))
    in
    fun string ->
      List.fold (force expanders) ~init:string ~f:(fun ac expander -> expander ac)
  ;;

  let maybe_hide_positions_in_string ?(hide_positions = false) string =
    if hide_positions
    then hide_positions_in_string string
    else string
  ;;

  let sexp_to_string ?hide_positions sexp =
    let string = Sexp_pretty.Pretty_print.sexp_to_string sexp in
    maybe_hide_positions_in_string ?hide_positions string
  ;;

  let print_s ?hide_positions sexp =
    print_string (sexp_to_string ?hide_positions sexp);
  ;;

  let require ?(cr = "CR") ?hide_positions ?if_false_then_print_s here bool =
    if not bool
    then (
      print_endline
        (String.concat
           [ "(* ";cr;" require-failed: "
           ; here |> Source_code_position.to_string
           ; ".\n"
           ; "   Do not 'X' this CR; instead make the required property true,\n"
           ; "   which will make the CR disappear.  For more information, see\n"
           ; "   [Expect_test_helpers.Helpers.require]. *)"
           ]
         |> maybe_hide_positions_in_string ?hide_positions);
      (match if_false_then_print_s with
       | None -> ()
       | Some sexp -> print_s ?hide_positions (force sexp)));
  ;;

  let require_does_not_raise ?cr ?hide_positions here f =
    match f () with
    | _             -> ()
    | exception exn ->
      require ?cr ?hide_positions here false
        ~if_false_then_print_s:(lazy [%message "unexpectedly raised" ~_:(exn : exn)])
  ;;

  let bigstring_for_print_bin_ios = ref (Bigstring.create 1024)

  let print_bin_ios_internal (type a)
        ?cr
        ?hide_positions
        ?require_bin_io_length_doesn't_exceed
        (module M : Print_bin_ios_arg with type t = a)
        (all : a list) =
    print_s [%message "" ~bin_shape_digest:(
      Bin_prot.Shape.eval_to_digest_string M.bin_shape_t : string)];
    let module Failure = struct
      type t =
        { value         : M.t
        ; length        : int
        ; serialization : string }
      [@@deriving sexp_of]
    end in
    let failures =
      List.fold all ~init:[] ~f:(fun failures t ->
        let size = M.bin_writer_t.size t in
        if size > Bigstring.length !bigstring_for_print_bin_ios
        then bigstring_for_print_bin_ios := Bigstring.create (Int.ceil_pow2 size);
        let bigstring = !bigstring_for_print_bin_ios in
        (* We use [M.bin_writer_t.write] rather than [Bigstring.write_bin_prot] in order to
           minimize diffs in pre-existing tests.  This matches [Iobuf.fill_bin_prot]. *)
        let len = M.bin_writer_t.write bigstring t ~pos:0 in
        assert (len <= size);
        let serialization = Bigstring.to_string bigstring ~len in
        print_endline (sprintf "%S" serialization);
        match require_bin_io_length_doesn't_exceed with
        | None -> failures
        | Some (maximum, _) ->
          if len <= maximum
          then failures
          else { Failure. value = t; length = len; serialization } :: failures)
    in
    match require_bin_io_length_doesn't_exceed with
    | None -> ()
    | Some (maximum, here) ->
      require ?cr ?hide_positions here (List.is_empty failures)
        ~if_false_then_print_s:(lazy [%message
                                 "Maximum binable length exceeded"
                                   (maximum : int)
                                   (failures : Failure.t list)])
  ;;

  let print_bin_ios m ts = print_bin_ios_internal m ts

  let print_bin_ios_with_max
        (type a)
        ?cr ?hide_positions here
        (module M : Print_bin_ios_with_max_arg with type t = a)
        ts =
    print_bin_ios_internal (module M) ts ?cr ?hide_positions
      ~require_bin_io_length_doesn't_exceed:(M.max_binable_length, here)
  ;;

  module type Int63able = sig
    type t
    val to_int63     : t -> Int63.t
    val of_int63_exn : Int63.t -> t
  end

  let print_and_check_stable_internal
        (type a) ?cr ?hide_positions ?max_binable_length here
        (module M : Stable_without_comparator with type t = a)
        (int63able : (module Int63able with type t = a) option)
        list =
    let equal = [%compare.equal: M.t] in
    print_s ?hide_positions [%message
      "" ~bin_shape_digest:(Bin_prot.Shape.eval_to_digest_string M.bin_shape_t : string)];
    require_does_not_raise ?cr ?hide_positions here (fun () ->
      List.iter list ~f:(fun original ->
        let sexp   = M.sexp_of_t                  original in
        let bin_io = Binable.to_string (module M) original in
        let int63  = Option.map int63able ~f:(fun (module I) -> I.to_int63 original) in
        print_s ?hide_positions [%message
          ""
            (sexp   : Sexp.t)
            (bin_io : string)
            (int63  : Int63.t sexp_option)];
        let sexp_roundtrip = M.t_of_sexp sexp in
        require ?cr ?hide_positions here (equal original sexp_roundtrip)
          ~if_false_then_print_s:
            (lazy [%message
              "sexp serialization failed to round-trip"
                (original       : M.t)
                (sexp           : Sexp.t)
                (sexp_roundtrip : M.t)]);
        let bin_io_roundtrip = Binable.of_string (module M) bin_io in
        require ?cr ?hide_positions here (equal original bin_io_roundtrip)
          ~if_false_then_print_s:
            (lazy [%message
              "bin-io serialization failed to round-trip"
                (original         : M.t)
                (bin_io           : string)
                (bin_io_roundtrip : M.t)]);
        begin
          match max_binable_length with
          | None -> ()
          | Some max_binable_length ->
            let bin_io_length = String.length bin_io in
            require ?cr ?hide_positions here (bin_io_length <= max_binable_length)
              ~if_false_then_print_s:
                (lazy [%message
                  "bin-io serialization exceeds max binable length"
                    (original           : M.t)
                    (bin_io             : string)
                    (bin_io_length      : int)
                    (max_binable_length : int)])
        end;
        begin
          match int63able with
          | None            -> ()
          | Some (module I) ->
            let int63           = Option.value_exn int63 in
            let int63_roundtrip = I.of_int63_exn   int63 in
            require ?cr ?hide_positions here (equal original int63_roundtrip)
              ~if_false_then_print_s:
                (lazy [%message
                  "int63 serialization failed to round-trip"
                    (original        : M.t)
                    (int63           : Int63.t)
                    (int63_roundtrip : M.t)])
        end))
  ;;

  let print_and_check_stable_type
        (type a) ?cr ?hide_positions ?max_binable_length here
        (module M : Stable_without_comparator with type t = a)
        list =
    print_and_check_stable_internal ?cr ?hide_positions ?max_binable_length
      here (module M) None list
  ;;

  let print_and_check_stable_int63able_type
        (type a) ?cr ?hide_positions ?max_binable_length here
        (module M : Stable_int63able with type t = a)
        list =
    print_and_check_stable_internal ?cr ?hide_positions ?max_binable_length
      here (module M) (Some (module M)) list
  ;;

  let show_raise (type a) ?hide_positions (f : unit -> a) =
    try
      ignore (f () : a);
      print_s [%message "did not raise"]
    with exn ->
      print_s ?hide_positions [%message "raised" (exn : exn)]
  ;;

  (* We disable inlining for [show_allocation] so the GC stats and the call to [f] are
     never rearranged. *)
  let [@inline never] show_allocation f =
    let minor_words_before = Gc.minor_words () in
    let major_words_before = Gc.major_words () in
    (* We wrap [f ()] with [Sys.opaque_identity] to prevent the return value from being
       optimized away. *)
    let x = Sys.opaque_identity (f ()) in
    let minor_words_after = Gc.minor_words () in
    let major_words_after = Gc.major_words () in
    print_s [%message
      "allocated"
        ~minor_words:(minor_words_after - minor_words_before : int)
        ~major_words:(major_words_after - major_words_before : int)];
    x
  ;;

  (* We disable inlining for [require_no_allocation] so the GC stats and the call to [f]
     are never rearranged. *)
  let [@inline never] require_no_allocation ?cr ?hide_positions src f =
    let minor_words_before = Gc.minor_words () in
    let major_words_before = Gc.major_words () in
    (* We wrap [f ()] with [Sys.opaque_identity] to prevent the return value from being
       optimized away. *)
    let x = Sys.opaque_identity (f ()) in
    let minor_words_after = Gc.minor_words () in
    let major_words_after = Gc.major_words () in
    require src ?cr ?hide_positions
      (minor_words_before = minor_words_after
       && major_words_before = major_words_after)
      ~if_false_then_print_s:
        (lazy [%message
          "allocated"
            ~minor_words:(minor_words_after - minor_words_before : int)
            ~major_words:(major_words_after - major_words_before : int)]);
    x
  ;;
end

include Make (struct
    open Core_kernel.Std

    let print_endline = print_endline

    let print_string string =
      print_string string;
      Out_channel.(flush stdout);
    ;;
  end)

module Expect_test_config = struct
  include Expect_test_config

  let run f =
    try
      f ();
    with exn ->
      print_s [%message
        "\
A top-level expression in [let%expect] raised -- consider using [show_raise]"
          ~_:(exn : exn)]
  ;;
end
