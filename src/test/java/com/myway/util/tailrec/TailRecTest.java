package com.myway.util.tailrec;

import org.junit.Test;

import java.math.BigInteger;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Fail.fail;

public class TailRecTest {
    @Test
    public void testFoldWithSum() {
        int n = 35000;
        Long expected = n * (n + 1L) / 2;
        assertThat(sum(n, 0L).eval()).isEqualTo(expected);
    }

    @Test
    public void testFoldWithSumZero() {
        assertThat(sum(0, 0L).eval()).isEqualTo(0L);
    }

    static TailRec<Long> sum(Integer n, Long out) {
        if (n <= 0)
            return TailRec.done(out);
        else
            return TailRec.more(() -> sum(n - 1, out + n));
    }

    @Test
    public void testFoldWithSumUsingMap() {
        int n = 35000;
        Long expected = n * (n + 1L) / 2;
        assertThat(sum(n).eval()).isEqualTo(expected);
    }

    static TailRec<Long> sum(Integer n) {
        if (n <= 0)
            return TailRec.done(0L);
        else
            return () -> sum(n - 1).map(y -> y + n);
    }


    @Test(expected = RuntimeException.class)
    public void testForbiddenActions() {
        try {
            TailRec.done("").resume();
            fail("should have raised exception");
        } catch (Exception e) {
            assertThat(e.getMessage()).isEqualTo("not supported operation");
            throw e;
        }
    }


    @Test
    public void testFibonacci() {
        // arrange
        int n = 10;
        // act
        Long out = fib(n);
        // assert
        assertThat(out).isEqualTo(55L);
    }

    static Long fib(Integer i) {
        return fibRec(i).eval();
    }

    static TailRec<Long> fibRec(Integer i) {
        if (i <= 1)
            return TailRec.done((long) i);
        else
            return () -> fibRec(i - 2).flatMap(
                    x -> fibRec(i - 1).map(y -> x + y)
            );
    }

    @Test
    public void testFibonacci2() {
        // arrange
        int n = 35000;
        Function<Integer, BigInteger> fib2 = i -> fibRec2(i).eval().u;
        // act
        BigInteger out = fib2.apply(n);
        // assert
        assertThat(out.toString().substring(0, 10)).isEqualTo("1651671774");
    }

    record Pair(BigInteger u, BigInteger v) {
        Pair step() {
            return new Pair(v, u.add(v));
        }
    }

    static TailRec<Pair> fibRec2(Integer i) {
        if (i <= 0)
            return TailRec.done(new Pair(BigInteger.ZERO, BigInteger.ONE));
        else
            return () -> fibRec2(i - 1).map(Pair::step);
    }
}
