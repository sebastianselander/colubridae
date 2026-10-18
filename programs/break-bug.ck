type Foo {
    Foo,
}
def main() {
    let x: int = loop {
        match Foo {
            Foo => break 123,
            Foo => 123,
        }
    };
    std::printInt(x)
}
