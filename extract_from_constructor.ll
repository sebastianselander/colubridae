
target triple = "x86_64-pc-linux-gnu"

declare i32 @printf(ptr, ...)
declare ptr @malloc(i64)

@internal_global_unit = internal constant i1 1

@snl = internal constant [4 x i8] c"%s\0A\00"
define i1 @printString(ptr %env, i8* %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @snl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8* %x)
	ret i1 1
}

@dnl = internal constant [4 x i8] c"%d\0A\00"
define i1 @printInt(ptr %env, i64 %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i64 %x)
	ret i1 1
}

%Foo = type { i64, [1 x i64] }

define %Foo @mkQux() {
    unreachable
}

define %Foo @mkBaz(i64 %_0) {
    %alloc = alloca %Foo
    %tag = getelementptr %Foo, %Foo* %alloc, i32 0, i32 0
    store i64 4, i64* %tag
    %value = getelementptr %Foo, %Foo* %alloc, i32 0, i32 1, i32 0
    store i64 %_0, i64* %value
    %struct = load %Foo, %Foo* %alloc
    ret %Foo %struct
}

define %Foo @Bar() {
    %_1 = alloca %Foo
    store { i64 } {i64 2}, %Foo* %_1
    %_2 = load %Foo, %Foo* %_1
    ret %Foo %_2
}

define %Foo(ptr)* @Qux() {
    ret %Foo(ptr)* @mkQux
}

define %Foo(ptr, i64)* @Baz() {
    ret %Foo(ptr, i64)* @mkBaz
}

define void @main() {
    %foo = call %Foo @mkBaz(i64 420)
    %val = extractvalue %Foo %foo, 1, 0
    call i1 @printInt(ptr null, i64 %val)
    ret void
}
