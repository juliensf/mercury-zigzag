%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 Julien Fischer.
% See the file COPYING for license details.
%---------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test harness for the mercury-zigzag library.
%
%---------------------------------------------------------------------------%

:- module test_zigzag.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module zigzag.

:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module random.sfc64.
:- import_module random.system_rng.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    open_system_rng(MaybeSystemRNG, !IO),
    (
        MaybeSystemRNG = ok(SystemRNG),
        system_rng.generate_uint64(SystemRNG, SeedA, !IO),
        system_rng.generate_uint64(SystemRNG, SeedB, !IO),
        system_rng.generate_uint64(SystemRNG, SeedC, !IO),
        close_system_rng(SystemRNG, !IO),
        sfc64.seed(SeedA, SeedB, SeedC, RNG, RNGState),
        make_io_urandom(RNG, RNGState, IO_RNG, !IO),
        run_tests(IO_RNG,!IO)
    ;
        MaybeSystemRNG = error(ErrorMsg),
        io.stderr_stream(Stderr, !IO),
        io.format(Stderr, "Error: %s\n", [s(ErrorMsg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_tests(RNG::in, io::di, io::uo) is det <= urandom(RNG, io).

run_tests(RNG, !IO) :-
    % For 8-, 16- and 32-bit integers we just exhaustively test every value.
    test_all_int8s(!IO),
    test_all_int16s(!IO),
    test_all_int32s(!IO),

    % Exhaustive testing is not feasible for 64-bit integers.
    % We test a list of known values.
    test_int64s(RNG, !IO),
    test_ints(!IO).


%---------------------------------------------------------------------------%

:- pred test_all_int8s(io::di, io::uo) is det.

test_all_int8s(!IO) :-
    test_all_int8s_loop(min_int8, 0, NumFailures, !IO),
    ( if NumFailures = 0 then
        io.print_line("PASSED: all int8 tests.", !IO)
    else
        true
    ).

:- pred test_all_int8s_loop(int8::in, int::in, int::out, io::di, io::uo)
    is det.

test_all_int8s_loop(N, !NumFailures, !IO) :-
    EncodedN = encode_int8(N),
    DecodedN = decode_int8(EncodedN),
    ( if N \= DecodedN then
        io.format(
            "FAILED: encode_int8(%di8) = %uu8; decode_int8(%uu8) = %di8\n",
            [i8(N), u8(EncodedN), u8(EncodedN), i8(DecodedN)], !IO),
        !:NumFailures = !.NumFailures + 1
    else
        true
    ),
    ( if N = max_int8 then
        true
    else
        test_all_int8s_loop(N + 1i8, !NumFailures, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_all_int16s(io::di, io::uo) is det.

test_all_int16s(!IO) :-
    test_all_int16s_loop(min_int16, 0, NumFailures, !IO),
    ( if NumFailures = 0 then
        io.print_line("PASSED: all int16 tests.", !IO)
    else
        true
    ).

:- pred test_all_int16s_loop(int16::in, int::in, int::out, io::di, io::uo)
    is det.

test_all_int16s_loop(N, !NumFailures, !IO) :-
    EncodedN = encode_int16(N),
    DecodedN = decode_int16(EncodedN),
    ( if N \= DecodedN then
        io.format(
            "FAILED: encode_int16(%di16) = %uu16; decode_int16(%uu16) = %di16\n",
            [i16(N), u16(EncodedN), u16(EncodedN), i16(DecodedN)], !IO),
        !:NumFailures = !.NumFailures + 1
    else
        true
    ),
    ( if N = max_int16 then
        true
    else
        test_all_int16s_loop(N + 1i16, !NumFailures, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_all_int32s(io::di, io::uo) is det.

test_all_int32s(!IO) :-
    test_all_int32s_loop(min_int32, 0, NumFailures, !IO),
    ( if NumFailures = 0 then
        io.print_line("PASSED: all int32 tests.", !IO)
    else
        true
    ).

:- pred test_all_int32s_loop(int32::in, int::in, int::out, io::di, io::uo)
    is det.

test_all_int32s_loop(N, !NumFailures, !IO) :-
    EncodedN = encode_int32(N),
    DecodedN = decode_int32(EncodedN),
    ( if N \= DecodedN then
        io.format(
            "FAILED: encode_int32(%di32) = %uu32; decode_int32(%uu32) = %di32\n",
            [i32(N), u32(EncodedN), u32(EncodedN), i32(DecodedN)], !IO),
        !:NumFailures = !.NumFailures + 1
    else
        true
    ),
    ( if N = max_int32 then
        true
    else
        test_all_int32s_loop(N + 1i32, !NumFailures, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_int64s(RNG::in, io::di, io::uo) is det <= urandom(RNG, io).

test_int64s(RNG, !IO) :-
    test_non_random_int64s(0, NumNonRandomFailures, !IO),
    ( if NumNonRandomFailures = 0 then
        io.print_line("PASSED: non-random int64 tests.", !IO)
    else
        true
    ),
    test_random_int64s(RNG, num_random_tests, 0, NumRandomFailures, !IO),
    ( if NumRandomFailures = 0 then
        io.print_line("PASSED: random int64 tests.", !IO)
    else
        true
    ).

:- pred test_non_random_int64s(int::in, int::out, io::di, io::uo) is det.

test_non_random_int64s(!NumFailures, !IO) :-
    list.foldl2(do_test_int64, int64s, !NumFailures, !IO).

:- pred do_test_int64(int64::in, int::in, int::out, io::di, io::uo) is det.

do_test_int64(N, !NumFailures, !IO) :-
    EncodedN = encode_int64(N),
    DecodedN = decode_int64(EncodedN),
    ( if N \= DecodedN then
        io.format(
        "FAILED: encode_int64(%di64) = %uu64; decode_int64(%uu64) = %di64\n",
        [i64(N), u64(EncodedN), u64(EncodedN), i64(DecodedN)], !IO),
        !:NumFailures = !.NumFailures + 1
    else
        true
    ).

:- func int64s = list(int64).

int64s = [
    min_int64,
    -1i64,
    0i64,
    1i64,
    max_int64
].

:- pred test_random_int64s(RNG::in, uint::in, int::in, int::out, io::di, io::uo)
    is det <= urandom(RNG, io).

test_random_int64s(RNG, TestN, !NumFailures, !IO) :-
    random.generate_uint64(RNG, U64, !IO),
    N = cast_from_uint64(U64),
    do_test_int64(N, !NumFailures, !IO),
    ( if TestN = 0u then
        true
    else
        test_random_int64s(RNG, TestN - 1u, !NumFailures, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_ints(io::di, io::uo) is det.

test_ints(!IO) :-
    some [!NumFailures] (
        !:NumFailures = 0,
        test_non_random_ints(!NumFailures, !IO),
        ( if !.NumFailures = 0 then
            io.print_line("PASSED: non-random int tests.", !IO)
        else
            true
        )
    ).

:- pred test_non_random_ints(int::in, int::out, io::di, io::uo) is det.

test_non_random_ints(!NumFailures, !IO) :-
    list.foldl2(do_test_int, ints, !NumFailures, !IO).

:- pred do_test_int(int::in, int::in, int::out, io::di, io::uo) is det.

do_test_int(N, !NumFailures, !IO) :-
    EncodedN = encode_int(N),
    DecodedN = decode_int(EncodedN),
    ( if N \= DecodedN then
        io.format("FAILED: encode_int(%d) = %uu; decode_int(%uu) = %d\n",
            [i(N), u(EncodedN), u(EncodedN), i(DecodedN)], !IO),
        !:NumFailures = !.NumFailures + 1
    else
        true
    ).

:- func ints = list(int).

ints = [
    min_int,
    -1,
    0,
    1,
    max_int
].

%---------------------------------------------------------------------------%

:- func num_random_tests = uint.

num_random_tests = 1_000_000u.

%---------------------------------------------------------------------------%
:- end_module test_zigzag.
%---------------------------------------------------------------------------%
