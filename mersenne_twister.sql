CREATE OR REPLACE PACKAGE mersenne_twister
IS
    -- PL/SQL Implementation of the Mersenne Twister pseudo-random number generator
    --  of Takuji Nishimura and Makoto Matsumoto.
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

    --   A C-program for MT19937, with initialization improved 2002/1/26.
    --   Coded by Takuji Nishimura and Makoto Matsumoto.
    --
    --   Before using, initialize the state by using init_genrand(seed)
    --   or init_by_array(init_key, key_length).
    --
    --   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
    --   All rights reserved.
    --
    --   Redistribution and use in source and binary forms, with or without
    --   modification, are permitted provided that the following conditions
    --   are met:
    --
    --     1. Redistributions of source code must retain the above copyright
    --        notice, this list of conditions and the following disclaimer.
    --
    --     2. Redistributions in binary form must reproduce the above copyright
    --        notice, this list of conditions and the following disclaimer in the
    --        documentation and/or other materials provided with the distribution.
    --
    --     3. The names of its contributors may not be used to endorse or promote
    --        products derived from this software without specific prior written
    --        permission.
    --
    --   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    --   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    --   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    --   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
    --   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    --   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    --   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    --   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    --   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    --   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    --   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    --
    --
    --   Any feedback is very welcome.
    --   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
    --   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)

    -- Mersenne Twister state vector type
    TYPE t_mt_array IS TABLE OF INTEGER
                           INDEX BY BINARY_INTEGER;

    PROCEDURE init_genrand(p_seed INTEGER);

    PROCEDURE init_by_array(p_init_key t_mt_array, p_key_length INTEGER);

    FUNCTION genrand_int32
        RETURN INTEGER;

    FUNCTION genrand_int31
        RETURN INTEGER;

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

    -- run test of 1000 integers and 1000 real numbers to verify against
    -- standard mt19337ar.out
    PROCEDURE test_procedure;
END mersenne_twister;

CREATE OR REPLACE PACKAGE BODY mersenne_twister
IS
    -- PL/SQL Implementation of the Mersenne Twister pseudo-random number generator
    --  of Takuji Nishimura and Makoto Matsumoto.
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

    --   A C-program for MT19937, with initialization improved 2002/1/26.
    --   Coded by Takuji Nishimura and Makoto Matsumoto.
    --
    --   Before using, initialize the state by using init_genrand(seed)
    --   or init_by_array(init_key, key_length).
    --
    --   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
    --   All rights reserved.
    --
    --   Redistribution and use in source and binary forms, with or without
    --   modification, are permitted provided that the following conditions
    --   are met:
    --
    --     1. Redistributions of source code must retain the above copyright
    --        notice, this list of conditions and the following disclaimer.
    --
    --     2. Redistributions in binary form must reproduce the above copyright
    --        notice, this list of conditions and the following disclaimer in the
    --        documentation and/or other materials provided with the distribution.
    --
    --     3. The names of its contributors may not be used to endorse or promote
    --        products derived from this software without specific prior written
    --        permission.
    --
    --   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    --   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    --   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    --   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
    --   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    --   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    --   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    --   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    --   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    --   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    --   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    --
    --
    --   Any feedback is very welcome.
    --   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
    --   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)

    c_8hexdigits CONSTANT VARCHAR2(8) := 'xxxxxxxx';
    c_32bit_mask CONSTANT INTEGER := TO_NUMBER('ffffffff', c_8hexdigits);

    -- Period parameters
    c_n          CONSTANT INTEGER := 624;                   -- Mersenne Twister N, the iterator size
    c_m          CONSTANT INTEGER := 397;
    c_matrix_a   CONSTANT INTEGER := TO_NUMBER('9908b0df', c_8hexdigits);       -- constant vector a
    c_upper_mask CONSTANT INTEGER := TO_NUMBER('80000000', c_8hexdigits); -- most significant w-r bits
    c_lower_mask CONSTANT INTEGER := TO_NUMBER('7fffffff', c_8hexdigits); -- least significant r bits
    g_mt         t_mt_array;
    g_mti        INTEGER := c_n + 1;                      -- mti==N+1 means mt[N] is not initialized

    -- Return an integer that is a result of OR-ing each bit of a and b
    FUNCTION bitor(p_a IN INTEGER, p_b IN INTEGER)
        RETURN INTEGER
        DETERMINISTIC
    IS
    BEGIN
        RETURN (p_a + p_b) - BITAND(p_a, p_b);
    END bitor;

    -- Return an integer that is a result of XOR-ing each bit of a and b
    FUNCTION bitxor(p_a IN INTEGER, p_b IN INTEGER)
        RETURN INTEGER
        DETERMINISTIC
    IS
    BEGIN
        RETURN (p_a + p_b) - BITAND(p_a, p_b) * 2;
    END bitxor;

    -- Shift bits right
    FUNCTION bitshr(p_bits IN INTEGER, p_shift IN INTEGER)
        RETURN INTEGER
        DETERMINISTIC
    IS
    BEGIN
        RETURN FLOOR(p_bits / (2 ** p_shift));
    END bitshr;

    -- Shift bits left
    FUNCTION bitshl(p_bits IN INTEGER, p_shift IN INTEGER)
        RETURN INTEGER
        DETERMINISTIC
    IS
    BEGIN
        RETURN p_bits * (2 ** p_shift);
    END bitshl;

    -- initializes mt[N] with a seed
    PROCEDURE init_genrand(p_seed INTEGER)
    IS
    BEGIN
        g_mt(0) := BITAND(p_seed, c_32bit_mask);

        g_mti := 1;

        WHILE g_mti < c_n
        LOOP
            g_mt(g_mti) :=
                (1812433253 * bitxor(g_mt(g_mti - 1), bitshr(g_mt(g_mti - 1), 30)) + g_mti);

            g_mt(g_mti) := BITAND(g_mt(g_mti), c_32bit_mask);

            g_mti := g_mti + 1;
        END LOOP;
    END init_genrand;

    --
    -- initialize by an array with array-length
    -- init_key is the array for initializing keys
    -- key_length is its length
    PROCEDURE init_by_array(p_init_key t_mt_array, p_key_length INTEGER)
    IS
        i INTEGER;
        j INTEGER;
        k INTEGER;
    BEGIN
        init_genrand(19650218);
        i := 1;
        j := 0;
        k := CASE WHEN c_n > p_key_length THEN c_n ELSE p_key_length END;

        WHILE k != 0
        LOOP
            g_mt(i) :=
                  bitxor(g_mt(i), (bitxor(g_mt(i - 1), bitshr(g_mt(i - 1), 30)) * 1664525))
                + p_init_key(j)
                + j;                                                                   -- non linear
            g_mt(i) := BITAND(g_mt(i), c_32bit_mask);
            i := i + 1;
            j := j + 1;

            IF (i >= c_n)
            THEN
                g_mt(0) := g_mt(c_n - 1);
                i := 1;
            END IF;

            IF (j >= p_key_length)
            THEN
                j := 0;
            END IF;

            k := k - 1;
        END LOOP;

        k := c_n - 1;

        WHILE k != 0
        LOOP
            g_mt(i) :=
                bitxor(g_mt(i), bitxor(g_mt(i - 1), bitshr(g_mt(i - 1), 30)) * 1566083941) - i; /* non linear */
            g_mt(i) := BITAND(g_mt(i), c_32bit_mask);
            i := i + 1;

            IF (i >= c_n)
            THEN
                g_mt(0) := g_mt(c_n - 1);
                i := 1;
            END IF;

            k := k - 1;
        END LOOP;

        g_mt(0) := TO_NUMBER('80000000', c_8hexdigits); -- MSB is 1; assuring non-zero initial array
    END init_by_array;

    -- generates a random number on [0,0xffffffff]-interval
    FUNCTION genrand_int32
        RETURN INTEGER
    IS
        y  INTEGER;
        kk INTEGER;

        -- mag01(x) = x * MATRIX_A  for x=0,1
        FUNCTION mag01(x IN INTEGER)
            RETURN INTEGER
        IS
        BEGIN
            RETURN CASE
                       WHEN x = 0 THEN 0
                       WHEN x = 1 THEN c_matrix_a
                   END;
        END;
    BEGIN
        IF (g_mti >= c_n)
        THEN
            -- generate N words at one time
            IF (g_mti = c_n + 1)
            THEN
                --if init_genrand() has not been called, a default initial seed is used
                init_genrand(5489);
            END IF;

            kk := 0;

            WHILE kk < c_n - c_m
            LOOP
                y := bitor(BITAND(g_mt(kk), c_upper_mask), BITAND(g_mt(kk + 1), c_lower_mask));
                g_mt(kk) := bitxor(bitxor(g_mt(kk + c_m), bitshr(y, 1)), mag01(BITAND(y, 1)));
                kk := kk + 1;
            END LOOP;

            WHILE kk < c_n - 1
            LOOP
                y := bitor(BITAND(g_mt(kk), c_upper_mask), BITAND(g_mt(kk + 1), c_lower_mask));
                g_mt(kk) :=
                    bitxor(bitxor(g_mt(kk + (c_m - c_n)), bitshr(y, 1)), mag01(BITAND(y, 1)));
                kk := kk + 1;
            END LOOP;

            y := bitor(BITAND(g_mt(c_n - 1), c_upper_mask), BITAND(g_mt(0), c_lower_mask));
            g_mt(c_n - 1) := bitxor(bitxor(g_mt(c_m - 1), bitshr(y, 1)), mag01(BITAND(y, 1)));

            g_mti := 0;
        END IF;

        y := g_mt(g_mti);
        g_mti := g_mti + 1;

        -- Tempering
        y := bitxor(y, bitshr(y, 11));
        y := bitxor(y, BITAND(bitshl(y, 7), TO_NUMBER('9d2c5680', c_8hexdigits)));
        y := bitxor(y, BITAND(bitshl(y, 15), TO_NUMBER('efc60000', c_8hexdigits)));
        y := bitxor(y, bitshr(y, 18));

        RETURN y;
    END genrand_int32;

    -- generates a random number on [0,0x7fffffff]-interval
    FUNCTION genrand_int31
        RETURN INTEGER
    IS
    BEGIN
        RETURN bitshr(genrand_int32, 1);
    END;

    -- generates a random number on [0,1]-real-interval
    FUNCTION genrand_real1
        RETURN NUMBER
    IS
    BEGIN
        RETURN genrand_int32 / 4294967295;
    -- divided by 2^32-1
    END;

    -- generates a random number on [0,1)-real-interval
    FUNCTION genrand_real2
        RETURN NUMBER
    IS
    BEGIN
        RETURN genrand_int32 / 4294967296;
    -- divided by 2^32
    END;

    -- generates a random number on (0,1)-real-interval
    FUNCTION genrand_real3
        RETURN NUMBER
    IS
    BEGIN
        RETURN (genrand_int32 + 0.5) / 4294967296;
    -- divided by 2^32
    END;

    -- generates a random number on [0,1) with 53-bit resolution
    FUNCTION genrand_res53
        RETURN NUMBER
    IS
        a INTEGER;
        b INTEGER;
    BEGIN
        a := bitshr(genrand_int32, 5);
        b := bitshr(genrand_int32, 6);
        RETURN (a * 67108864.0 + b) / 9007199254740992;
    END;

    -- run test of 1000 integers and 1000 real numbers to verify against
    -- standard mt19937ar.out
    PROCEDURE test_procedure
    IS
        v_init mersenne_twister.t_mt_array;
        v_out  INTEGER := 0;
    BEGIN
        v_init(0) := TO_NUMBER('123', 'xxx');
        v_init(1) := TO_NUMBER('234', 'xxx');
        v_init(2) := TO_NUMBER('345', 'xxx');
        v_init(3) := TO_NUMBER('456', 'xxx');
        mersenne_twister.init_by_array(v_init, 4);

        DBMS_OUTPUT.put_line('1000 outputs of genrand_int32()');

        FOR n IN (SELECT mersenne_twister.genrand_int32 x
                  FROM DUAL
                  CONNECT BY LEVEL <= 1000)
        LOOP
            DBMS_OUTPUT.put(LPAD(n.x, 10) || ' ');
            v_out := v_out + 1;

            IF v_out = 5
            THEN
                DBMS_OUTPUT.put_line(NULL);
                v_out := 0;
            END IF;
        END LOOP;

        DBMS_OUTPUT.put_line(' ');
        DBMS_OUTPUT.put_line('1000 outputs of genrand_real2()');

        FOR n IN (SELECT mersenne_twister.genrand_real2 x
                  FROM DUAL
                  CONNECT BY LEVEL <= 1000)
        LOOP
            DBMS_OUTPUT.put(TO_CHAR(n.x, 'fm0.99999990') || ' ');
            v_out := v_out + 1;

            IF v_out = 5
            THEN
                DBMS_OUTPUT.put_line(NULL);
                v_out := 0;
            END IF;
        END LOOP;
    END test_procedure;
END mersenne_twister;