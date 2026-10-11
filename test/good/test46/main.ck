import id;

def id(x: int) -> int {
    x
}

def main() {
    let x = 123;
    std::printInt(id::id(x));
    std::printInt(id(x));
    std::printChar('\n');
}
