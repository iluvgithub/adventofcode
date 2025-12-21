package com.myway.util.tailrec;

import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;
@FunctionalInterface
public interface TailRec<A> {

    TailRec<A> resume();

    default A eval() {
        return Stream.iterate(this, TailRec::resume).
                dropWhile(TailRec::isSuspend).
                findFirst().
                map(TailRec::eval).
                orElse(null);
    }

    default boolean isSuspend() {
        return true;
    }

    static <X> TailRec<X> done(X x) {
        return new Done<>(x);
    }

    static <X> TailRec<X> more(Supplier<TailRec<X>> supplier) {
        return new More<>(supplier);
    }

    default <B> TailRec<B> flatMap(Function<A, TailRec<B>> fn) {
        return new FlatMap<>(this, fn);
    }

    default <B> TailRec<B> map(Function<A, B> fv) {
        return new FlatMap<>(this, x -> done(fv.apply(x)));
    }

    default boolean isFlatMap() {
        return false;
    }

    default <Z> TailRec<Z> shiftFlatMap(Function<A, TailRec<Z>> fun) {
        throw new RuntimeException("not implemented by default");
    }
}
record Done<X>(X x) implements TailRec<X> {

    @Override
    public TailRec<X> resume() {
        throw new RuntimeException("not supported operation");
    }

    @Override
    public X eval() {
        return x;
    }

    @Override
    public boolean isSuspend() {
        return false;
    }


}
record More<X>(Supplier<TailRec<X>> supplier) implements TailRec<X> {

    @Override
    public TailRec<X> resume() {
        return supplier.get();
    }


}

record FlatMap<T, A>(TailRec<T> tail, Function<T, TailRec<A>> fn) implements TailRec<A> {

    @Override
    public TailRec<A> resume() {
        if (!tail.isSuspend()) {
            return fn.apply(tail.eval());
        } else if (tail.isFlatMap()) {
            return tail.shiftFlatMap(fn);
        } else
            return () -> tail.resume().flatMap(fn);
    }

    @Override
    public boolean isFlatMap() {
        return true;
    }

    @Override
    public <Z> TailRec<Z> shiftFlatMap(Function<A, TailRec<Z>> fun) {
        return this.tail.flatMap(tail2 -> this.fn.apply(tail2).flatMap(fun));
    }
}

