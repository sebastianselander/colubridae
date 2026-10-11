def apply<A,B>(f: fn(A) -> B, x: A) -> B {
    f(x)
}

def id<A>(x: A) -> A {
    x
}

type List {
    Cons(int, List),
    Nil,
}

def foldl(f: fn(int, int) -> int, xs: List, accumulator: int) -> int {
    match xs {
        Nil => accumulator,
        Cons(x,xs) => foldl(f, xs, f(x,accumulator)),
    }
}

def head(xs: List) -> int {
    match xs {
        Cons(x,_tail) => x,
    }
}

def main() {
    let xs = Cons(1,Cons(2,Cons(3,Nil)));
    std::printInt(foldl(\a b -> a + b, xs, 0));
    std::printChar('\n');
    std::printInt(apply(head, xs));
    std::printChar('\n');
}
