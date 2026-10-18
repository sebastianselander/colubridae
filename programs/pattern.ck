type Id {
    Id(int)
}

def main() {
    let i = Id(123);
    match i {
        Id(n) => std::printInt(n),
    }
}
