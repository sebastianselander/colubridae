type Ref<A> {
    Ref(&A)
}

def get<A>(box: Ref<A>) -> A {
    match box {
        Ref(ref) => *ref,
    }
}


def set<A>(box: Ref<A>, x: A) {
    match box {
        Ref(ref) => *ref = x,
    }
}

def main() {
    let y = 123;
    let v = Ref(&y);
    std::printInt(get(v));
    std::printChar('\n');
    set(v, 420);
    std::printInt(get(v));
    std::printChar('\n');
}
