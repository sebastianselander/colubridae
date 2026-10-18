import foo::a;
import foo::b;
import bar::a;
import bar::b;

def main() {
    foo::a::say();
    foo::b::say();
    bar::a::say();
    bar::b::say();
}
