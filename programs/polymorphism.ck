type List<A> {
    Nil,
    Cons(A, List<A>),
}

def reverse<A>(xs: List<A>) -> List<A> {
    let ys = Nil;
    loop {
        match xs {
            Cons(z,zs) => {
                ys = Cons(z,ys);
                xs = zs;
            },
            Nil => break,
        }
    };
    ys
}

def printList(xs: List<string>) {
    loop {
        match xs {
            Nil => break,
            Cons(y,ys) => {
                std::printString(y);
                xs = ys;
            }
        }
    };
    std::printString("\n");
}

def main() {
    let xs = Cons("a", Cons("b", Cons("c", Nil)));
    printList(xs);
    printList(reverse(xs));
}
