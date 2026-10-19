type List<A> {
    Nil,
    Cons(A, List<A>),
}

def main() {
    let xs = Cons(&1, Cons(&2, Nil));
    match xs {
        Cons(x,xs) => {
            std::printInt(*x);
            std::printChar('\n');
            match xs {
                Cons(y,ys) => {
                    std::printInt(*y);
                    std::printChar('\n');
                },
            }
        },
    };
    match xs {
        Cons(x,xs) => {
            *x = 1337;
            match xs {
                Cons(y,ys) => {
                    *y = 420;
                },
            };
        },
    };
    match xs {
        Cons(x,xs) => {
            std::printInt(*x);
            std::printChar('\n');
            match xs {
                Cons(y,ys) => {
                    std::printInt(*y);
                    std::printChar('\n');
                },
            }
        },
    }
}
