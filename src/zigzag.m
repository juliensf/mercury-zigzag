%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 Julien Fischer.
% See the file COPYING for license details.
%---------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides functions for Zigzag encoding Mercury's signed integer
% types as their corresponding unsigned integer types.
%
%---------------------------------------------------------------------------%

:- module zigzag.
:- interface.

%---------------------------------------------------------------------------%

:- func encode_int8(int8) = uint8.
:- func decode_int8(uint8) = int8.

:- func encode_int16(int16) = uint16.
:- func decode_int16(uint16) = int16.

:- func encode_int32(int32) = uint32.
:- func decode_int32(uint32) = int32.

:- func encode_int64(int64) = uint64.
:- func decode_int64(uint64) = int64.

:- func encode_int(int) = uint.
:- func decode_int(uint) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module uint.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module uint8.

% We use the fast encode / decode methods by Daniel Lemire described in
% <https://lemire.me/blog/2022/11/25/making-all-your-integers-positive-with-zigzag-encoding/>.

%---------------------------------------------------------------------------%

encode_int8(X) =
    cast_from_int8((2i8 * X) `xor` (X >> 7)).

decode_int8(X) =
    cast_from_uint8(X >> 1) `xor` (-(cast_from_uint8(X /\ 1u8))).

%---------------------------------------------------------------------------%

encode_int16(X) =
    cast_from_int16((2i16 * X) `xor` (X >> 15)).

decode_int16(X) =
    cast_from_uint16(X >> 1) `xor` (-(cast_from_uint16(X /\ 1u16))).

%---------------------------------------------------------------------------%

encode_int32(X) =
    cast_from_int32((2i32 * X) `xor` (X >> 31)).

decode_int32(X) =
    cast_from_uint32(X >> 1) `xor` (-(cast_from_uint32(X /\ 1u32))).

%---------------------------------------------------------------------------%

encode_int64(X) =
    cast_from_int64((2i64 * X) `xor` (X >> 63)).

decode_int64(X) =
    cast_from_uint64(X >> 1) `xor` (-(cast_from_uint64(X /\ 1u64))).

%---------------------------------------------------------------------------%

encode_int(X) =
    cast_from_int((2i * X) `xor` (X >> (bits_per_int - 1))).

decode_int(X) =
    cast_to_int(X >> 1) `xor` (-(cast_to_int(X /\ 1u))).

%---------------------------------------------------------------------------%
:- end_module zigzag.
%---------------------------------------------------------------------------%
