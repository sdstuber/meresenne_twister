CREATE OR REPLACE PACKAGE sds.sfmt
IS
    -- PL/SQL Implementation of the SIMD oriented Fast Mersenne Twister(SFMT)
    -- Conversion to PL/SQL by Sean D. Stuber

    --                    .///.
    --                   (0 o)
    ---------------0000--(_)--0000---------------
    --
    --  Sean D. Stuber
    --  sean.stuber@gmail.com
    --
    --             oooO      Oooo
    --------------(   )-----(   )---------------
    --             \ (       ) /
    --              \_)     (_/

    --Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
    --University. All rights reserved.
    --
    --Redistribution and use in source and binary forms, with or without
    --modification, are permitted provided that the following conditions are
    --met:
    --
    --    * Redistributions of source code must retain the above copyright
    --      notice, this list of conditions and the following disclaimer.
    --    * Redistributions in binary form must reproduce the above
    --      copyright notice, this list of conditions and the following
    --      disclaimer in the documentation and/or other materials provided
    --      with the distribution.
    --    * Neither the name of the Hiroshima University nor the names of
    --      its contributors may be used to endorse or promote products
    --      derived from this software without specific prior written
    --      permission.
    --
    --THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    --"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    --LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    --A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    --OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    --SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    --LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    --DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    --THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    --(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    --OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------

    -- PL/SQL doesn't have a 32-bit or 64-bit unsigned integer type
    -- Nor is there a way to declare a subtype with that constraint
    -- So we just have to fake it with regular INTEGER type
    -- Within the package, the code ensures no values execeed the bit limit.
    -- Inputs will be validated procedurally before accepting.
    SUBTYPE t_uint32 IS INTEGER;

    SUBTYPE t_uint64 IS INTEGER;

    TYPE t_uint32_array IS TABLE OF t_uint32
                               INDEX BY BINARY_INTEGER;

    TYPE t_uint64_array IS TABLE OF t_uint64
                               INDEX BY BINARY_INTEGER;

    -- The random number generator constructs an internal array of 32 bit values
    -- 64bit values are returned by combining 2 of these values
    -- If the offset into the array is an odd number this exception will be raised
    -- This exception should only occur if both 32bit and 64bit generation is
    -- used for a single initialization.  To avoid this error, avoid using both
    -- types of generation, or be sure to invoke 32bit generation in pairs
    -- between 64bit calls.
    exc_invalid_64bit_offset EXCEPTION;

    -- Before the number generation can begin it must be initialized
    -- with a seed number or array of seed values
    -- Failure to do so will raise this exception
    exc_not_initialized      EXCEPTION;

    -- If initialization value isn't a valid unsigned 32bit integer
    -- then this exception will be raised
    exc_invalid_uint32       EXCEPTION;

    -- If an invalid Mersenne Exponent is specified then
    -- this exception will be raised
    -- valid MEXP values are: (607,1279,2281,4253,11213,19937,44497,86243,132049,216091)
    exc_invalid_mexp         EXCEPTION;

    -- Determines the period of the pseudo-random number generator
    -- Setting this defines several other parameters
    -- The original c-code defined these at compile time
    -- p_mexp must be one of the following
    -- 607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091
    -- Changing the MEXP requires a reinitialization of the generator
    -- The default MEXP is initialized to 19937 during package initialization
    PROCEDURE set_mersenne_exponent(p_mexp IN INTEGER DEFAULT 19937);

    -- Simple initialization
    -- The MEXP (Mersenne Exponent) must be set prior to calling this
    PROCEDURE init_gen_rand(p_seed IN t_uint32);

    -- The initialization array must have 0..p_key_length-1 elements populated
    -- if key length is null, or unspecified the length will be p_init_key.count
    -- So, length isn't strictly necessary but the original api is maintained
    -- The MEXP (Mersenne Exponent) must be set prior to calling this
    PROCEDURE init_by_array(
        p_init_key     IN OUT NOCOPY t_uint32_array,
        p_key_length                 INTEGER DEFAULT NULL
    );

    -- Return a single unsigned 32-bit integer pseudo-random number
    -- subsequent calls will return new values
    -- The generator must be initialized prior to calling this
    FUNCTION gen_rand32
        RETURN t_uint32;

    -- Return a single unsigned 64-bit integer pseudo-random number
    -- subsequent calls will return new values
    -- The generator must be initialized prior to calling this
    FUNCTION gen_rand64
        RETURN t_uint64;

    -- Fills a 0-based array with pseudo-random unsigned 32-bit integers
    -- The generator must be initialized prior to calling this
    PROCEDURE fill_array32(
        p_array   IN OUT NOCOPY t_uint32_array,
        p_size    IN            INTEGER
    );

    -- Fills a 0-based array with pseudo-random unsigned 64-bit integers
    -- The generator must be initialized prior to calling this
    PROCEDURE fill_array64(
        p_array   IN OUT NOCOPY t_uint64_array,
        p_size    IN            INTEGER
    );

    /* These real versions are due to Isaku Wada */
    -- generates a random number on [0,1]-real-interval
    FUNCTION genrand_real1
        RETURN NUMBER;

    -- generates a random number on [0,1)-real-interval
    FUNCTION genrand_real2
        RETURN NUMBER;

    -- generates a random number on (0,1)-real-interval
    FUNCTION genrand_real3
        RETURN NUMBER;

    -- generates a random number on [0,1) with 53-bit resolution
    FUNCTION genrand_res53
        RETURN NUMBER;

    -- generates a random number on [0,1) with 53-bit resolution
    -- value is generated from from two 32 bit integers
    FUNCTION genrand_res53_mix
        RETURN NUMBER;

    -- Replicate the checking processes of the original for whatever
    -- MEXP you want.  Normally the results will be printed to
    -- the DBMS_OUTPUT buffer.
    -- If the directory and file name are provided the output will
    -- also be written via UTL_FILE to the specified file and location.
    PROCEDURE check32(
        p_mexp        IN INTEGER DEFAULT 19937,
        p_file_dir    IN VARCHAR2 DEFAULT NULL,
        p_file_name   IN VARCHAR2 DEFAULT NULL
    );

    PROCEDURE check64(
        p_mexp        IN INTEGER DEFAULT 19937,
        p_file_dir    IN VARCHAR2 DEFAULT NULL,
        p_file_name   IN VARCHAR2 DEFAULT NULL
    );

    -- Run speed tests
    PROCEDURE speed32(p_count IN INTEGER, p_block IN INTEGER);

    PROCEDURE speed64(p_count IN INTEGER, p_block IN INTEGER);
END sfmt;
/

CREATE OR REPLACE PACKAGE BODY sds.sfmt
IS
    -- PL/SQL Implementation of the SIMD oriented Fast Mersenne Twister(SFMT)
    -- Conversion to PL/SQL by Sean D. Stuber

    --                    .///.
    --                   (0 o)
    ---------------0000--(_)--0000---------------
    --
    --  Sean D. Stuber
    --  sean.stuber@gmail.com
    --
    --             oooO      Oooo
    --------------(   )-----(   )---------------
    --             \ (       ) /
    --              \_)     (_/

    --Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
    --University. All rights reserved.
    --
    --Redistribution and use in source and binary forms, with or without
    --modification, are permitted provided that the following conditions are
    --met:
    --
    --    * Redistributions of source code must retain the above copyright
    --      notice, this list of conditions and the following disclaimer.
    --    * Redistributions in binary form must reproduce the above
    --      copyright notice, this list of conditions and the following
    --      disclaimer in the documentation and/or other materials provided
    --      with the distribution.
    --    * Neither the name of the Hiroshima University nor the names of
    --      its contributors may be used to endorse or promote products
    --      derived from this software without specific prior written
    --      permission.
    --
    --THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    --"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    --LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    --A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    --OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    --SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    --LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    --DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    --THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    --(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    --OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    c_8hexdigits  CONSTANT VARCHAR2(8) := RPAD('x', 8, 'x');
    c_16hexdigits CONSTANT VARCHAR2(16) := RPAD('x', 16, 'x');
    c_32bit_mask  CONSTANT INTEGER
                               := TO_NUMBER(RPAD('f', 8, 'f'), c_8hexdigits) ;
    c_64bit_mask  CONSTANT INTEGER
        := TO_NUMBER(RPAD('f', 16, 'f'), c_16hexdigits) ;

    ---- Pseudo-constants
    ---- These define the period of the generator
    ---- They are initialized once before processing
    ---- and remain constant thereafter
    ---- In the original c-code they were defined at compile time
    g_mexp                 INTEGER;
    g_pos1                 INTEGER;
    g_sl1                  INTEGER;
    g_sl2                  INTEGER;
    g_sr1                  INTEGER;
    g_sr2                  INTEGER;
    g_msk1                 INTEGER;
    g_msk2                 INTEGER;
    g_msk3                 INTEGER;
    g_msk4                 INTEGER;
    g_parity1              INTEGER;
    g_parity2              INTEGER;
    g_parity3              INTEGER;
    g_parity4              INTEGER;
    g_idstr                VARCHAR2(100);
    g_n                    INTEGER;
    g_n32                  INTEGER;
    g_n64                  INTEGER;
    g_period_set           BOOLEAN := FALSE;

    -----------------------------------------------------------------------------------

    TYPE t_w128 IS RECORD(u t_uint32_array);

    -- Ideally this would be a varray, but we can't declare it
    -- until the MEXP is defined so we'll use the more flexible
    -- associative array

    TYPE t_w128_array IS TABLE OF t_w128
                             INDEX BY PLS_INTEGER;

    g_sfmt                 t_w128_array;

    g_idx                  INTEGER;
    g_initialized          BOOLEAN := FALSE;

    g_parity               t_uint32_array;

    -- PL/SQL doesn't have syntax corresponding to the pointer
    -- tricks that c does with multi-typed pointers
    -- So, SET/GET routines to the nested array simulate them
    PROCEDURE psfmt32set(p_index IN INTEGER, p_value IN INTEGER)
    IS
    BEGIN
        g_sfmt(FLOOR(p_index / 4)).u(MOD(p_index, 4)) := p_value;
    END psfmt32set;

    FUNCTION psfmt32get(p_index IN INTEGER)
        RETURN INTEGER
    IS
    BEGIN
        RETURN g_sfmt(FLOOR(p_index / 4)).u(MOD(p_index, 4));
    END psfmt32get;

    -- combines the functions of the GET and SET
    -- used where the value within the array is adjusted by some other
    -- value with 32bit roll over
    -- rather than simply assigning a new 32 bit value
    -- Statements like this:  psfmt32[i] += r;
    PROCEDURE psfmt32delta(p_index IN INTEGER, p_delta IN INTEGER)
    IS
        x INTEGER;
    BEGIN
        x := g_sfmt(FLOOR(p_index / 4)).u(MOD(p_index, 4)) + p_delta;

        IF x < POWER(2, 32)
        THEN
            g_sfmt(FLOOR(p_index / 4)).u(MOD(p_index, 4)) := x;
        ELSE
            -- Rollover values greater than 32bits
            g_sfmt(FLOOR(p_index / 4)).u(MOD(p_index, 4)) := x - POWER(2, 32);
        END IF;
    END psfmt32delta;

    -- Determines the period of the pseudo-random number generator
    -- Setting this defines several other parameters
    -- The original c-code defined these at compile time
    -- p_mexp must be one of the following
    -- 607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091
    PROCEDURE set_mersenne_exponent(p_mexp IN INTEGER DEFAULT 19937)
    IS
    BEGIN
        IF p_mexp IN
               (607,
                1279,
                2281,
                4253,
                11213,
                19937,
                44497,
                86243,
                132049,
                216091)
        THEN
            -- If we change the exponent
            -- then the generator must be reinitalized
            g_initialized := FALSE;

            g_mexp := p_mexp;
            g_n := FLOOR(g_mexp / 128) + 1;
            g_n32 := g_n * 4;
            g_n64 := g_n * 2;

            IF p_mexp = 607
            THEN
                g_pos1 := 2;
                g_sl1 := 15;
                g_sl2 := 3;
                g_sr1 := 13;
                g_sr2 := 3;
                g_msk1 := TO_NUMBER('fdff37ff', c_8hexdigits);
                g_msk2 := TO_NUMBER('ef7f3f7d', c_8hexdigits);
                g_msk3 := TO_NUMBER('ff777b7d', c_8hexdigits);
                g_msk4 := TO_NUMBER('7ff7fb2f', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity4 := TO_NUMBER('5986f054', c_8hexdigits);
                g_idstr :=
                    'SFMT-607:2-15-3-13-3:fdff37ff-ef7f3f7d-ff777b7d-7ff7fb2f';
            ELSIF p_mexp = 1279
            THEN
                g_pos1 := 7;
                g_sl1 := 14;
                g_sl2 := 3;
                g_sr1 := 5;
                g_sr2 := 1;
                g_msk1 := TO_NUMBER('f7fefffd', c_8hexdigits);
                g_msk2 := TO_NUMBER('7fefcfff', c_8hexdigits);
                g_msk3 := TO_NUMBER('aff3ef3f', c_8hexdigits);
                g_msk4 := TO_NUMBER('b5ffff7f', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity4 := TO_NUMBER('20000000', c_8hexdigits);

                g_idstr :=
                    'SFMT-1279:7-14-3-5-1:f7fefffd-7fefcfff-aff3ef3f-b5ffff7f';
            ELSIF p_mexp = 2281
            THEN
                g_pos1 := 12;
                g_sl1 := 19;
                g_sl2 := 1;
                g_sr1 := 5;
                g_sr2 := 1;
                g_msk1 := TO_NUMBER('bff7ffbf', c_8hexdigits);
                g_msk2 := TO_NUMBER('fdfffffe', c_8hexdigits);
                g_msk3 := TO_NUMBER('f7ffef7f', c_8hexdigits);
                g_msk4 := TO_NUMBER('f2f7cbbf', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity4 := TO_NUMBER('41dfa600', c_8hexdigits);

                g_idstr :=
                    'SFMT-2281:12-19-1-5-1:bff7ffbf-fdfffffe-f7ffef7f-f2f7cbbf';
            ELSIF p_mexp = 4253
            THEN
                g_pos1 := 17;
                g_sl1 := 20;
                g_sl2 := 1;
                g_sr1 := 7;
                g_sr2 := 1;
                g_msk1 := TO_NUMBER('9f7bffff', c_8hexdigits);
                g_msk2 := TO_NUMBER('9fffff5f', c_8hexdigits);
                g_msk3 := TO_NUMBER('3efffffb', c_8hexdigits);
                g_msk4 := TO_NUMBER('fffff7bb', c_8hexdigits);
                g_parity1 := TO_NUMBER('a8000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('af5390a3', c_8hexdigits);
                g_parity3 := TO_NUMBER('b740b3f8', c_8hexdigits);
                g_parity4 := TO_NUMBER('6c11486d', c_8hexdigits);

                g_idstr :=
                    'SFMT-4253:17-20-1-7-1:9f7bffff-9fffff5f-3efffffb-fffff7bb';
            ELSIF p_mexp = 11213
            THEN
                g_pos1 := 68;
                g_sl1 := 14;
                g_sl2 := 3;
                g_sr1 := 7;
                g_sr2 := 3;
                g_msk1 := TO_NUMBER('effff7fb', c_8hexdigits);
                g_msk2 := TO_NUMBER('ffffffef', c_8hexdigits);
                g_msk3 := TO_NUMBER('dfdfbfff', c_8hexdigits);
                g_msk4 := TO_NUMBER('7fffdbfd', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('e8148000', c_8hexdigits);
                g_parity4 := TO_NUMBER('d0c7afa3', c_8hexdigits);

                g_idstr :=
                    'SFMT-11213:68-14-3-7-3:effff7fb-ffffffef-dfdfbfff-7fffdbfd';
            ELSIF p_mexp = 19937
            THEN
                g_pos1 := 122;
                g_sl1 := 18;
                g_sl2 := 1;
                g_sr1 := 11;
                g_sr2 := 1;
                g_msk1 := TO_NUMBER('dfffffef', c_8hexdigits);
                g_msk2 := TO_NUMBER('ddfecb7f', c_8hexdigits);
                g_msk3 := TO_NUMBER('bffaffff', c_8hexdigits);
                g_msk4 := TO_NUMBER('bffffff6', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity4 := TO_NUMBER('13c9e684', c_8hexdigits);

                g_idstr :=
                    'SFMT-19937:122-18-1-11-1:dfffffef-ddfecb7f-bffaffff-bffffff6';
            ELSIF p_mexp = 44497
            THEN
                g_pos1 := 330;
                g_sl1 := 5;
                g_sl2 := 3;
                g_sr1 := 9;
                g_sr2 := 3;
                g_msk1 := TO_NUMBER('effffffb', c_8hexdigits);
                g_msk2 := TO_NUMBER('dfbebfff', c_8hexdigits);
                g_msk3 := TO_NUMBER('bfbf7bef', c_8hexdigits);
                g_msk4 := TO_NUMBER('9ffd7bff', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('a3ac4000', c_8hexdigits);
                g_parity4 := TO_NUMBER('ecc1327a', c_8hexdigits);

                g_idstr :=
                    'SFMT-44497:330-5-3-9-3:effffffb-dfbebfff-bfbf7bef-9ffd7bff';
            ELSIF p_mexp = 86243
            THEN
                g_pos1 := 366;
                g_sl1 := 6;
                g_sl2 := 7;
                g_sr1 := 19;
                g_sr2 := 1;
                g_msk1 := TO_NUMBER('fdbffbff', c_8hexdigits);
                g_msk2 := TO_NUMBER('bff7ff3f', c_8hexdigits);
                g_msk3 := TO_NUMBER('fd77efff', c_8hexdigits);
                g_msk4 := TO_NUMBER('bf9ff3ff', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity4 := TO_NUMBER('e9528d85', c_8hexdigits);

                g_idstr :=
                    'SFMT-86243:366-6-7-19-1:fdbffbff-bff7ff3f-fd77efff-bf9ff3ff';
            ELSIF p_mexp = 132049
            THEN
                g_pos1 := 110;
                g_sl1 := 19;
                g_sl2 := 1;
                g_sr1 := 21;
                g_sr2 := 1;
                g_msk1 := TO_NUMBER('ffffbb5f', c_8hexdigits);
                g_msk2 := TO_NUMBER('fb6ebf95', c_8hexdigits);
                g_msk3 := TO_NUMBER('fffefffa', c_8hexdigits);
                g_msk4 := TO_NUMBER('cff77fff', c_8hexdigits);
                g_parity1 := TO_NUMBER('00000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('00000000', c_8hexdigits);
                g_parity3 := TO_NUMBER('cb520000', c_8hexdigits);
                g_parity4 := TO_NUMBER('c7e91c7d', c_8hexdigits);

                g_idstr :=
                    'SFMT-132049:110-19-1-21-1:ffffbb5f-fb6ebf95-fffefffa-cff77fff';
            ELSIF p_mexp = 216091
            THEN
                g_pos1 := 627;
                g_sl1 := 11;
                g_sl2 := 3;
                g_sr1 := 10;
                g_sr2 := 1;
                g_msk1 := TO_NUMBER('bff7bff7', c_8hexdigits);
                g_msk2 := TO_NUMBER('bfffffff', c_8hexdigits);
                g_msk3 := TO_NUMBER('bffffa7f', c_8hexdigits);
                g_msk4 := TO_NUMBER('ffddfbfb', c_8hexdigits);
                g_parity1 := TO_NUMBER('f8000001', c_8hexdigits);
                g_parity2 := TO_NUMBER('89e80709', c_8hexdigits);
                g_parity3 := TO_NUMBER('3bd2b64b', c_8hexdigits);
                g_parity4 := TO_NUMBER('0c64b1e4', c_8hexdigits);

                g_idstr :=
                    'SFMT-216091:627-11-3-10-1:bff7bff7-bfffffff-bffffa7f-ffddfbfb';
            END IF;

            g_parity(0) := g_parity1;
            g_parity(1) := g_parity2;
            g_parity(2) := g_parity3;
            g_parity(3) := g_parity4;
        ELSE
            RAISE exc_invalid_mexp;
        END IF;
    END set_mersenne_exponent;

    -- Return an integer that is a result of OR-ing each bit of a and b
    FUNCTION bitor(p_a IN INTEGER, p_b IN INTEGER)
        RETURN INTEGER
    IS
    BEGIN
        RETURN (p_a + p_b) - BITAND(p_a, p_b);
    END bitor;

    -- Return an integer that is a result of XOR-ing each bit of a and b
    -- enforce 32bit result
    FUNCTION bitxor(p_a IN INTEGER, p_b IN INTEGER)
        RETURN INTEGER
    IS
    BEGIN
        RETURN BITAND((p_a + p_b) - BITAND(p_a, p_b) * 2, c_32bit_mask);
    END bitxor;

    -- Shift bits right
    FUNCTION bitshr(p_bits IN INTEGER, p_shift IN INTEGER)
        RETURN INTEGER
    IS
    BEGIN
        RETURN FLOOR(p_bits / (2 ** p_shift));
    END bitshr;

    -- Shift bits left, and enforce 32bit or 64bit results
    FUNCTION bitshl(
        p_bits     IN INTEGER,
        p_shift    IN INTEGER,
        p_length   IN INTEGER DEFAULT 32
    )
        RETURN INTEGER
    IS
    BEGIN
        IF p_length = 32
        THEN
            RETURN BITAND(p_bits * (2 ** p_shift), c_32bit_mask);
        ELSE
            RETURN BITAND(p_bits * (2 ** p_shift), c_64bit_mask);
        END IF;
    END bitshl;

    PROCEDURE rshift128(
        p_out     IN OUT NOCOPY t_w128,
        p_in      IN            t_w128,
        p_shift   IN            INTEGER
    )
    IS
        th INTEGER;
        tl INTEGER;
        oh INTEGER;
        ol INTEGER;
    BEGIN
        th := bitor(bitshl(p_in.u(3), 32, 64), p_in.u(2));
        tl := bitor(bitshl(p_in.u(1), 32, 64), p_in.u(0));

        oh := bitshr(th, p_shift * 8);
        ol := bitshr(tl, p_shift * 8);
        ol := bitor(ol, bitshl(th, 64 - p_shift * 8, 64));
        p_out.u(1) := BITAND(bitshr(ol, 32), c_32bit_mask);
        p_out.u(0) := BITAND(ol, c_32bit_mask);
        p_out.u(3) := BITAND(bitshr(oh, 32), c_32bit_mask);
        p_out.u(2) := BITAND(oh, c_32bit_mask);
    END rshift128;

    PROCEDURE lshift128(
        p_out     IN OUT NOCOPY t_w128,
        p_in      IN            t_w128,
        p_shift   IN            INTEGER
    )
    IS
        th INTEGER;
        tl INTEGER;
        oh INTEGER;
        ol INTEGER;
    BEGIN
        th := bitor(bitshl(p_in.u(3), 32, 64), p_in.u(2));
        tl := bitor(bitshl(p_in.u(1), 32, 64), p_in.u(0));

        oh := bitshl(th, p_shift * 8, 64);
        ol := bitshl(tl, p_shift * 8, 64);
        oh := bitor(oh, bitshr(tl, 64 - p_shift * 8));
        p_out.u(1) := BITAND(bitshr(ol, 32), c_32bit_mask);
        p_out.u(0) := BITAND(ol, c_32bit_mask);
        p_out.u(3) := BITAND(bitshr(oh, 32), c_32bit_mask);
        p_out.u(2) := BITAND(oh, c_32bit_mask);
    END lshift128;

    PROCEDURE do_recursion(
        p_r   IN OUT NOCOPY t_w128,
        p_a   IN            t_w128,
        p_b   IN            t_w128,
        p_c   IN            INTEGER,
        p_d   IN            INTEGER
    )
    IS
        x t_w128;
        y t_w128;
    BEGIN
        lshift128(x, p_a, g_sl2);
        rshift128(y, g_sfmt(p_c), g_sr2);

        p_r.u(0) :=
            bitxor(
                bitxor(
                    bitxor(
                        bitxor(p_a.u(0), x.u(0)),
                        BITAND(bitshr(p_b.u(0), g_sr1), g_msk1)
                    ),
                    y.u(0)
                ),
                bitshl(g_sfmt(p_d).u(0), g_sl1)
            );

        p_r.u(1) :=
            bitxor(
                bitxor(
                    bitxor(
                        bitxor(p_a.u(1), x.u(1)),
                        BITAND(bitshr(p_b.u(1), g_sr1), g_msk2)
                    ),
                    y.u(1)
                ),
                bitshl(g_sfmt(p_d).u(1), g_sl1)
            );
        p_r.u(2) :=
            bitxor(
                bitxor(
                    bitxor(
                        bitxor(p_a.u(2), x.u(2)),
                        BITAND(bitshr(p_b.u(2), g_sr1), g_msk3)
                    ),
                    y.u(2)
                ),
                bitshl(g_sfmt(p_d).u(2), g_sl1)
            );
        p_r.u(3) :=
            bitxor(
                bitxor(
                    bitxor(
                        bitxor(p_a.u(3), x.u(3)),
                        BITAND(bitshr(p_b.u(3), g_sr1), g_msk4)
                    ),
                    y.u(3)
                ),
                bitshl(g_sfmt(p_d).u(3), g_sl1)
            );
    END do_recursion;

    PROCEDURE gen_rand_all
    IS
        i  INTEGER;
        r1 INTEGER;
        r2 INTEGER;
    BEGIN
        r1 := g_n - 2;
        r2 := g_n - 1;
        i := 0;

        WHILE i < g_n - g_pos1
        LOOP
            do_recursion(
                g_sfmt(i),
                g_sfmt(i),
                g_sfmt(i + g_pos1),
                r1,
                r2
            );
            r1 := r2;
            r2 := i;
            i := i + 1;
        END LOOP;

        WHILE i < g_n
        LOOP
            do_recursion(
                g_sfmt(i),
                g_sfmt(i),
                g_sfmt(i + g_pos1 - g_n),
                r1,
                r2
            );
            r1 := r2;
            r2 := i;
            i := i + 1;
        END LOOP;
    END gen_rand_all;

    FUNCTION func1(p_num IN INTEGER)
        RETURN INTEGER
    IS
        x INTEGER := BITAND(p_num, c_32bit_mask);
    BEGIN
        RETURN BITAND(bitxor(x, bitshr(x, 27)) * 1664525, c_32bit_mask);
    END func1;

    FUNCTION func2(p_num IN INTEGER)
        RETURN INTEGER
    IS
    BEGIN
        RETURN BITAND(
                   bitxor(p_num, bitshr(p_num, 27)) * 1566083941,
                   c_32bit_mask
               );
    END func2;

    PROCEDURE period_certification
    IS
        v_inner INTEGER := 0;
        v_work  INTEGER;
        v_shift INTEGER;
    BEGIN
        FOR i IN 0 .. 3
        LOOP
            v_inner := bitxor(v_inner, BITAND(psfmt32get(i), g_parity(i)));
        END LOOP;

        v_shift := 16;

        WHILE v_shift > 0
        LOOP
            v_inner := bitxor(v_inner, bitshr(v_inner, v_shift));
            v_shift := FLOOR(v_shift / 2);
        END LOOP;

        v_inner := BITAND(v_inner, 1);

        IF (v_inner = 1)
        THEN
            RETURN;
        END IF;

        FOR i IN 0 .. 3
        LOOP
            v_work := 1;

            FOR j IN 0 .. 31
            LOOP
                IF (BITAND(v_work, g_parity(i)) != 0)
                THEN
                    psfmt32set(i, bitxor(psfmt32get(i), v_work));
                    RETURN;
                END IF;

                v_work := BITAND(v_work * 2, c_32bit_mask);
            END LOOP;
        END LOOP;
    END period_certification;

    FUNCTION get_idstring
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN g_idstr;
    END get_idstring;

    FUNCTION gen_rand32
        RETURN t_uint32
    IS
        r INTEGER;
    BEGIN
        IF NOT g_initialized
        THEN
            RAISE exc_not_initialized;
        END IF;

        IF (g_idx >= g_n32)
        THEN
            gen_rand_all;
            g_idx := 0;
        END IF;

        r := psfmt32get(g_idx);
        g_idx := g_idx + 1;
        RETURN r;
    END gen_rand32;

    FUNCTION gen_rand64
        RETURN t_uint64
    IS
        v_lo INTEGER;
        v_hi INTEGER;
    BEGIN
        IF NOT g_initialized
        THEN
            RAISE exc_not_initialized;
        END IF;

        IF MOD(g_idx, 2) != 0
        THEN
            RAISE exc_invalid_64bit_offset;
        END IF;

        IF (g_idx >= g_n32)
        THEN
            gen_rand_all;
            g_idx := 0;
        END IF;

        v_lo := psfmt32get(g_idx);
        v_hi := psfmt32get(g_idx + 1);
        g_idx := g_idx + 2;
        RETURN bitor(bitshl(v_hi, 32, 64), v_lo);
    END gen_rand64;

    -- Array filling in the original c-code relied on pointer manipulation for
    -- rapid array population.  No corresponding facility exists for pl/sql
    -- so the array is populated by simple iteration.
    -- As a side effect of this, some of the extra assertions on the
    -- array limitations aren't necessary in this implementation
    PROCEDURE fill_array32(
        p_array   IN OUT NOCOPY t_uint32_array,
        p_size    IN            INTEGER
    )
    IS
        v_temp_w128 t_w128_array;
    BEGIN
        IF NOT g_initialized
        THEN
            RAISE exc_not_initialized;
        END IF;

        p_array.delete;

        FOR i IN 0 .. p_size - 1
        LOOP
            p_array(i) := gen_rand32;
        END LOOP;
    END fill_array32;

    PROCEDURE fill_array64(
        p_array   IN OUT NOCOPY t_uint64_array,
        p_size    IN            INTEGER
    )
    IS
        v_temp_w128 t_w128_array;
        v_x         INTEGER;
        v_y         INTEGER;
    BEGIN
        IF NOT g_initialized
        THEN
            RAISE exc_not_initialized;
        END IF;

        p_array.delete;

        FOR i IN 0 .. p_size - 1
        LOOP
            v_x := gen_rand32;
            v_y := gen_rand32;
            p_array(i) := bitor(bitshl(v_y, 32, 64), v_x);
        --
        END LOOP;
    END fill_array64;

    PROCEDURE init_gen_rand(p_seed IN t_uint32)
    IS
    BEGIN
        -- Validate the seed value is within the
        -- unsigned 32bit integer range 0..2^32-1
        IF p_seed < 0 OR p_seed >= POWER(2, 32) OR p_seed IS NULL
        THEN
            RAISE exc_invalid_uint32;
        END IF;

        g_sfmt.delete;
        psfmt32set(0, p_seed);

        FOR i IN 1 .. g_n32 - 1
        LOOP
            psfmt32set(
                i,
                BITAND(
                    1812433253
                    * bitxor(
                          psfmt32get(i - 1),
                          bitshr(psfmt32get(i - 1), 30)
                      )
                    + i,
                    c_32bit_mask
                )
            );
        END LOOP;

        g_idx := g_n32;
        period_certification;
        g_initialized := TRUE;
    END init_gen_rand;

    PROCEDURE init_by_array(
        p_init_key     IN OUT NOCOPY t_uint32_array,
        p_key_length                 INTEGER DEFAULT NULL
    )
    IS
        i        INTEGER;
        j        INTEGER;
        v_count  INTEGER;
        v_r      INTEGER;
        v_lag    INTEGER;
        v_mid    INTEGER;
        v_size   INTEGER := g_n * 4;
        v_length INTEGER := NVL(p_key_length, p_init_key.COUNT);
    BEGIN
        -- Validate the initialization array is fully populated
        -- on the required range.  Also confirm each element
        -- is within the 0-2^32-1 unsigned 32-bit integer range.
        FOR i IN 0 .. v_length - 1
        LOOP
            IF p_init_key(i) < 0
            OR  p_init_key(i) >= POWER(2, 32)
            OR  p_init_key(i) IS NULL
            THEN
                RAISE exc_invalid_uint32;
            END IF;
        END LOOP;

        g_sfmt.delete;

        IF v_size >= 623
        THEN
            v_lag := 11;
        ELSIF v_size >= 68
        THEN
            v_lag := 7;
        ELSIF v_size >= 39
        THEN
            v_lag := 5;
        ELSE
            v_lag := 3;
        END IF;

        v_mid := FLOOR((v_size - v_lag) / 2);

        -- quasi-memset
        FOR p IN 0 .. g_n - 1
        LOOP
            FOR q IN 0 .. 4
            LOOP
                g_sfmt(p).u(q) := TO_NUMBER('8b8b8b8b', c_8hexdigits);
            END LOOP;
        END LOOP;

        IF v_length + 1 > g_n32
        THEN
            v_count := v_length + 1;
        ELSE
            v_count := g_n32;
        END IF;

        v_r :=
            func1(
                bitxor(
                    bitxor(psfmt32get(0), psfmt32get(v_mid)),
                    psfmt32get(g_n32 - 1)
                )
            );

        psfmt32delta(v_mid, v_r);
        v_r := v_r + v_length;
        psfmt32delta(v_mid + v_lag, v_r);

        psfmt32set(0, v_r);
        v_count := v_count - 1;

        i := 1;
        j := 0;

        WHILE j < v_count AND j < v_length
        LOOP
            v_r :=
                func1(
                    bitxor(
                        bitxor(
                            psfmt32get(i),
                            psfmt32get(MOD(i + v_mid, g_n32))
                        ),
                        psfmt32get(MOD(i + g_n32 - 1, g_n32))
                    )
                );

            psfmt32delta(MOD(i + v_mid, g_n32), v_r);

            v_r := v_r + p_init_key(j) + i;
            psfmt32delta(MOD(i + v_mid + v_lag, g_n32), v_r);
            psfmt32set(i, v_r);
            i := MOD(i + 1, g_n32);
            j := j + 1;
        END LOOP;

        WHILE j < v_count
        LOOP
            v_r :=
                func1(
                    bitxor(
                        bitxor(
                            psfmt32get(i),
                            psfmt32get(MOD(i + v_mid, g_n32))
                        ),
                        psfmt32get(MOD(i + g_n32 - 1, g_n32))
                    )
                );
            psfmt32delta(MOD(i + v_mid, g_n32), v_r);
            v_r := v_r + i;
            psfmt32delta(MOD(i + v_mid + v_lag, g_n32), v_r);
            psfmt32set(i, v_r);
            i := MOD(i + 1, g_n32);
            j := j + 1;
        END LOOP;

        j := 0;

        WHILE j < g_n32
        LOOP
            v_r :=
                func2(
                    BITAND(
                        BITAND(
                            psfmt32get(i) + psfmt32get(MOD(i + v_mid, g_n32)),
                            c_32bit_mask
                        )
                        + psfmt32get(MOD(i + g_n32 - 1, g_n32)),
                        c_32bit_mask
                    )
                );

            psfmt32set(
                MOD(i + v_mid, g_n32),
                bitxor(psfmt32get(MOD(i + v_mid, g_n32)), v_r)
            );

            v_r := v_r - i;
            psfmt32set(
                MOD(i + v_mid + v_lag, g_n32),
                bitxor(psfmt32get(MOD(i + v_mid + v_lag, g_n32)), v_r)
            );
            psfmt32set(i, v_r);
            i := MOD(i + 1, g_n32);
            j := j + 1;
        END LOOP;

        g_idx := g_n32;
        period_certification;
        g_initialized := TRUE;
    END init_by_array;

    -- generates a random number on [0,1]-real-interval
    FUNCTION genrand_real1
        RETURN NUMBER
    IS
    BEGIN
        RETURN gen_rand32 / 4294967295;
    END genrand_real1;

    -- generates a random number on [0,1)-real-interval
    FUNCTION genrand_real2
        RETURN NUMBER
    IS
    BEGIN
        RETURN gen_rand32 / 4294967296;
    END genrand_real2;

    -- generates a random number on (0,1)-real-interval
    FUNCTION genrand_real3
        RETURN NUMBER
    IS
    BEGIN
        RETURN (gen_rand32 + 0.5) / 4294967296;
    END genrand_real3;

    FUNCTION genrand_res53
        RETURN NUMBER
    IS
    BEGIN
        RETURN gen_rand64() / 18446744073709551616;
    END genrand_res53;

    FUNCTION genrand_res53_mix
        RETURN NUMBER
    IS
    BEGIN
        -- Functionally this is the same as genrand_res53
        -- replicated here for to mimic origional c api
        RETURN bitor(gen_rand32, bitshl(gen_rand32, 32, 64))
               / 18446744073709551616;
    END genrand_res53_mix;

    PROCEDURE check32(
        p_mexp        IN INTEGER DEFAULT 19937,
        p_file_dir    IN VARCHAR2 DEFAULT NULL,
        p_file_name   IN VARCHAR2 DEFAULT NULL
    )
    IS
        v_out1 t_uint32_array;
        v_out2 t_uint32_array;
        v_init t_uint32_array;
        v_str  VARCHAR2(1000);
        v_file UTL_FILE.file_type;
        v_r    INTEGER;

        PROCEDURE p(v_str IN VARCHAR2, p_newline IN BOOLEAN DEFAULT FALSE)
        IS
        BEGIN
            IF UTL_FILE.is_open(v_file)
            THEN
                IF p_newline
                THEN
                    UTL_FILE.put_line(v_file, v_str);
                ELSE
                    UTL_FILE.put(v_file, v_str);
                END IF;
            END IF;

            IF p_newline
            THEN
                DBMS_OUTPUT.put_line(v_str);
            ELSE
                DBMS_OUTPUT.put(v_str);
            END IF;
        END;
    BEGIN
        set_mersenne_exponent(p_mexp);

        IF p_file_dir IS NOT NULL AND p_file_name IS NOT NULL
        THEN
            v_file :=
                UTL_FILE.fopen(
                    p_file_dir,
                    p_file_name,
                    'w',
                    1000
                );
        END IF;

        p(get_idstring, TRUE);
        p('32 bit generated randoms', TRUE);
        p('init_gen_rand__________', TRUE);
        init_gen_rand(1234);
        fill_array32(v_out1, 10000);
        fill_array32(v_out2, 10000);
        init_gen_rand(1234);

        FOR i IN 0 .. 9999
        LOOP
            IF i < 1000
            THEN
                p(LPAD(v_out1(i), 10) || ' ');

                IF MOD(i, 5) = 4
                THEN
                    p(NULL, TRUE);
                END IF;
            END IF;

            v_r := gen_rand32;

            IF v_r != v_out1(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out1:'
                    || TO_CHAR(v_out1(i), c_8hexdigits)
                    || ' gen:'
                    || TO_CHAR(v_r, c_8hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        FOR i IN 0 .. 699
        LOOP
            v_r := gen_rand32;

            IF v_r != v_out2(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out2:'
                    || TO_CHAR(v_out2(i), c_8hexdigits)
                    || ' gen:'
                    || TO_CHAR(v_r, c_8hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        p(CHR(10) || 'init_by_array__________', TRUE);

        v_init(0) := TO_NUMBER('1234', 'xxxx');
        v_init(1) := TO_NUMBER('5678', 'xxxx');
        v_init(2) := TO_NUMBER('9abc', 'xxxx');
        v_init(3) := TO_NUMBER('def0', 'xxxx');
        init_by_array(v_init, 4);
        fill_array32(v_out1, 10000);
        fill_array32(v_out2, 10000);
        init_by_array(v_init, 4);

        FOR i IN 0 .. 9999
        LOOP
            IF i < 1000
            THEN
                p(LPAD(v_out1(i), 10) || ' ');

                IF MOD(i, 5) = 4
                THEN
                    p(NULL, TRUE);
                END IF;
            END IF;

            v_r := gen_rand32;

            IF v_r != v_out1(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out1:'
                    || TO_CHAR(v_out1(i), c_8hexdigits)
                    || ' gen:'
                    || TO_CHAR(v_r, c_8hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        FOR i IN 0 .. 699
        LOOP
            v_r := gen_rand32;

            IF v_r != v_out2(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out2:'
                    || TO_CHAR(v_out2(i), c_8hexdigits)
                    || ' gen:'
                    || TO_CHAR(v_r, c_8hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        IF UTL_FILE.is_open(v_file)
        THEN
            UTL_FILE.fclose(v_file);
        END IF;
    END check32;

    PROCEDURE check64(
        p_mexp        IN INTEGER DEFAULT 19937,
        p_file_dir    IN VARCHAR2 DEFAULT NULL,
        p_file_name   IN VARCHAR2 DEFAULT NULL
    )
    IS
        v_out1 t_uint64_array;
        v_out2 t_uint64_array;
        v_init t_uint32_array;
        v_str  VARCHAR2(1000);
        v_file UTL_FILE.file_type;
        v_r    INTEGER;

        PROCEDURE p(v_str IN VARCHAR2, p_newline IN BOOLEAN DEFAULT FALSE)
        IS
        BEGIN
            IF UTL_FILE.is_open(v_file)
            THEN
                IF p_newline
                THEN
                    UTL_FILE.put_line(v_file, v_str);
                ELSE
                    UTL_FILE.put(v_file, v_str);
                END IF;
            END IF;

            IF p_newline
            THEN
                DBMS_OUTPUT.put_line(v_str);
            ELSE
                DBMS_OUTPUT.put(v_str);
            END IF;
        END;
    BEGIN
        set_mersenne_exponent(p_mexp);

        IF p_file_dir IS NOT NULL AND p_file_name IS NOT NULL
        THEN
            v_file :=
                UTL_FILE.fopen(
                    p_file_dir,
                    p_file_name,
                    'w',
                    1000
                );
        END IF;

        p(get_idstring, TRUE);
        p('64 bit generated randoms', TRUE);
        p('init_gen_rand__________', TRUE);
        init_gen_rand(4321);
        fill_array64(v_out1, 5000);
        fill_array64(v_out2, 5000);
        init_gen_rand(4321);

        FOR i IN 0 .. 4999
        LOOP
            IF i < 1000
            THEN
                p(LPAD(v_out1(i), 20) || ' ');

                IF MOD(i, 3) = 2
                THEN
                    p(NULL, TRUE);
                END IF;
            END IF;

            v_r := gen_rand64;

            IF v_r != v_out1(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out1:PRI'
                    || TO_CHAR(v_out1(i), c_16hexdigits)
                    || ' gen:PRI'
                    || TO_CHAR(v_r, c_16hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        FOR i IN 0 .. 699
        LOOP
            v_r := gen_rand64;

            IF v_r != v_out2(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out2:PRI'
                    || TO_CHAR(v_out2(i), c_16hexdigits)
                    || ' gen:PRI'
                    || TO_CHAR(v_r, c_16hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        p(CHR(10) || 'init_by_array__________', TRUE);

        v_init(0) := 5;
        v_init(1) := 4;
        v_init(2) := 3;
        v_init(3) := 2;
        v_init(4) := 1;

        init_by_array(v_init, 5);
        fill_array64(v_out1, 5000);
        fill_array64(v_out2, 5000);
        init_by_array(v_init, 5);

        FOR i IN 0 .. 4999
        LOOP
            IF i < 1000
            THEN
                p(LPAD(v_out1(i), 20) || ' ');

                IF MOD(i, 3) = 2
                THEN
                    p(NULL, TRUE);
                END IF;
            END IF;

            v_r := gen_rand64;

            IF v_r != v_out1(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out1:'
                    || TO_CHAR(v_out1(i), c_16hexdigits)
                    || ' gen:'
                    || TO_CHAR(v_r, c_16hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        FOR i IN 0 .. 699
        LOOP
            v_r := gen_rand64;

            IF v_r != v_out2(i)
            THEN
                p(
                       CHR(10)
                    || 'mismatch at '
                    || i
                    || ' v_out2:PRI'
                    || TO_CHAR(v_out2(i), c_16hexdigits)
                    || ' gen:PRI'
                    || TO_CHAR(v_r, c_16hexdigits),
                    TRUE
                );
                RETURN;
            END IF;
        END LOOP;

        IF UTL_FILE.is_open(v_file)
        THEN
            UTL_FILE.fclose(v_file);
        END IF;
    END check64;

    PROCEDURE speed32(p_count IN INTEGER, p_block IN INTEGER)
    IS
        v_start TIMESTAMP;
        v_end   TIMESTAMP;
        v_min   INTERVAL DAY TO SECOND := NUMTODSINTERVAL(99, 'DAY');
        v_array t_uint32_array;
        v_rand  t_uint32;

        FUNCTION interval2ms(p_interval IN INTERVAL DAY TO SECOND)
            RETURN INTEGER
        IS
        BEGIN
            RETURN 1000
                   * (  EXTRACT(DAY FROM p_interval) * 86400
                      + EXTRACT(HOUR FROM p_interval) * 3600
                      + EXTRACT(MINUTE FROM p_interval) * 60
                      + EXTRACT(SECOND FROM p_interval));
        END;
    BEGIN
        init_gen_rand(1234);

        FOR i IN 1 .. 10
        LOOP
            v_start := SYSTIMESTAMP;

            -- Iterate 10 times and report the minimum timing
            FOR j IN 1 .. p_count
            LOOP
                fill_array32(v_array, p_block);
            END LOOP;

            v_end := SYSTIMESTAMP;

            IF v_end - v_start < v_min
            THEN
                v_min := v_end - v_start;
            END IF;
        END LOOP;

        DBMS_OUTPUT.put_line(
               '32 bit BLOCK:'
            || interval2ms(v_min)
            || 'ms for '
            || p_block * p_count
            || ' randoms generation'
        );

        init_gen_rand(1234);

        FOR i IN 1 .. 10
        LOOP
            v_start := SYSTIMESTAMP;

            -- Iterate 10 times and report the minimum timing
            FOR j IN 1 .. p_count * p_block
            LOOP
                v_rand := gen_rand32;
            END LOOP;

            v_end := SYSTIMESTAMP;

            IF v_end - v_start < v_min
            THEN
                v_min := v_end - v_start;
            END IF;
        END LOOP;

        DBMS_OUTPUT.put_line(
               '32 bit SEQUE:'
            || interval2ms(v_min)
            || 'ms for '
            || p_block * p_count
            || ' randoms generation'
        );
    END speed32;

    PROCEDURE speed64(p_count IN INTEGER, p_block IN INTEGER)
    IS
        v_start TIMESTAMP;
        v_end   TIMESTAMP;
        v_min   INTERVAL DAY TO SECOND := NUMTODSINTERVAL(99, 'DAY');
        v_array t_uint64_array;
        v_rand  t_uint64;

        FUNCTION interval2ms(p_interval IN INTERVAL DAY TO SECOND)
            RETURN INTEGER
        IS
        BEGIN
            RETURN 1000
                   * (  EXTRACT(DAY FROM p_interval) * 86400
                      + EXTRACT(HOUR FROM p_interval) * 3600
                      + EXTRACT(MINUTE FROM p_interval) * 60
                      + EXTRACT(SECOND FROM p_interval));
        END;
    BEGIN
        init_gen_rand(1234);

        FOR i IN 1 .. 10
        LOOP
            v_start := SYSTIMESTAMP;

            -- Iterate 10 times and report the minimum timing
            FOR j IN 1 .. p_count
            LOOP
                fill_array64(v_array, p_block);
            END LOOP;

            v_end := SYSTIMESTAMP;

            IF v_end - v_start < v_min
            THEN
                v_min := v_end - v_start;
            END IF;
        END LOOP;

        DBMS_OUTPUT.put_line(
               '64 bit BLOCK:'
            || interval2ms(v_min)
            || 'ms for '
            || p_block * p_count
            || ' randoms generation'
        );

        init_gen_rand(1234);

        FOR i IN 1 .. 10
        LOOP
            v_start := SYSTIMESTAMP;

            -- Iterate 10 times and report the minimum timing
            FOR j IN 1 .. p_count * p_block
            LOOP
                v_rand := gen_rand64;
            END LOOP;

            v_end := SYSTIMESTAMP;

            IF v_end - v_start < v_min
            THEN
                v_min := v_end - v_start;
            END IF;
        END LOOP;

        DBMS_OUTPUT.put_line(
               '64 bit SEQUE:'
            || interval2ms(v_min)
            || 'ms for '
            || p_block * p_count
            || ' randoms generation'
        );
    END speed64;
BEGIN
    -- Initialize with default Mersenne Exponent parameters
    set_mersenne_exponent;
END;
/