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
:- import_module int8.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    % For 8-, 16- and 32-bit integers we just exhaustively test every value.
    test_all_int8s(!IO),
    test_all_int16s(!IO),
    test_all_int32s(!IO),

    % Exhaustive testing is not feasible for 64-bit integers.
    do_test64(-9223372036854775808i64, !IO),
    do_test64(-1i64, !IO),
    do_test64(0i64, !IO),
    do_test64(1i64, !IO),
    do_test64(9223372036854775807i64, !IO),
    io.nl(!IO),

    do_test(-2147483648, !IO),
    do_test(-1, !IO),
    do_test(0, !IO),
    do_test(1, !IO),
    do_test(2147483647, !IO).

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
        io.format("FAILED: encode_int8(%di8) = %uu8; decode_int8(%uu8) = %di8\n",
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
        io.format("FAILED: encode_int16(%di16) = %uu16; decode_int16(%uu16) = %di16\n",
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
        io.format("FAILED: encode_int32(%di32) = %uu32; decode_int32(%uu32) = %di32\n",
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

:- pred do_test64(int64::in, io::di, io::uo) is det.

do_test64(N, !IO) :-
    EncodedN = encode_int64(N),
    DecodedN = decode_int64(EncodedN),
    io.format("encode_int64(%d) = %u; decode_int64(%u) = %d\n",
        [i64(N), u64(EncodedN), u64(EncodedN), i64(DecodedN)], !IO).

:- pred do_test(int::in, io::di, io::uo) is det.

do_test(N, !IO) :-
    EncodedN = encode_int(N),
    DecodedN = decode_int(EncodedN),
    io.format("encode_int(%d) = %u; decode_int(%u) = %d\n",
        [i(N), u(EncodedN), u(EncodedN), i(DecodedN)], !IO).

%---------------------------------------------------------------------------%
:- end_module test_zigzag.
%---------------------------------------------------------------------------%
