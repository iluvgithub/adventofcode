package com.myway.util.tailrec;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

public class BouncerTest {

    @Test
    public void testSum() {
        // given
        int n = 18000;
        // when
        Long actual = sum(n);
        // then
        assertThat(actual).isEqualTo(n * (n + 1) / 2);
    }

    private Long sum(Integer n) {
        return sum(n, 0L).eval();
    }

    private Bouncer<Long> sum(Integer n, Long out) {
        if (n == 0) {
            return Bouncer.done(out);

        } else {
            return () -> sum(n - 1, out + n);
        }

    }

}