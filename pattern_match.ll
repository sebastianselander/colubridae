
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

@static_string_0 = constant [7 x i8] c "hejsan\00"

%Foo = type { i64, [2 x ptr] }

define %Foo @mkFoo(ptr %env, i8* %_2, i64 %_3) {
    %_4 = alloca %Foo
    %_5 = getelementptr %Foo, %Foo* %_4, i32 0, i32 0
    store i64 1, ptr %_5
    %_6 = getelementptr %Foo, %Foo* %_4, i32 0, i32 1, i32 0
    store i8* %_2, ptr %_6
    %_7 = getelementptr %Foo, %Foo* %_4, i32 0, i32 1, i32 1
    store i64 %_3, ptr %_7
    %_8 = load %Foo, %Foo* %_4
    ret %Foo %_8
}

define %Foo @Bar() {
    %_0 = alloca %Foo
    store { i64 } {i64 0}, %Foo* %_0
    %_1 = load %Foo, %Foo* %_0
    ret %Foo %_1
}

define %Foo(ptr, i8*, i64)* @Foo() {
    ret %Foo(ptr, i8*, i64)* @mkFoo
}

define void @main() {
    %named_0 = alloca i8*
    store i8* @static_string_0, ptr %named_0
    %named_1 = alloca i64
    store i64 42069, i64* %named_1
    %named_2 = alloca %Foo
    %_9 = call %Foo(ptr, i8*, i64)* @Foo()
    %_10 = alloca { %Foo(ptr, i8*, i64)*, ptr }
    %_11 = getelementptr { %Foo(ptr, i8*, i64)*, ptr }, { %Foo(ptr, i8*, i64)*, ptr }* %_10, i32 0, i32 0
    %_12 = getelementptr { %Foo(ptr, i8*, i64)*, ptr }, { %Foo(ptr, i8*, i64)*, ptr }* %_10, i32 0, i32 1
    store %Foo(ptr, i8*, i64)* %_9, %Foo(ptr, i8*, i64)** %_11
    store ptr null, ptr %_12
    %_13 = load { %Foo(ptr, i8*, i64)*, ptr }, { %Foo(ptr, i8*, i64)*, ptr }* %_10
    %_14 = extractvalue { %Foo(ptr, i8*, i64)*, ptr } %_13, 0
    %_15 = call %Foo(ptr, i8*, i64)* @Foo()
    %_16 = alloca { %Foo(ptr, i8*, i64)*, ptr }
    %_17 = getelementptr { %Foo(ptr, i8*, i64)*, ptr }, { %Foo(ptr, i8*, i64)*, ptr }* %_16, i32 0, i32 0
    %_18 = getelementptr { %Foo(ptr, i8*, i64)*, ptr }, { %Foo(ptr, i8*, i64)*, ptr }* %_16, i32 0, i32 1
    store %Foo(ptr, i8*, i64)* %_15, %Foo(ptr, i8*, i64)** %_17
    store ptr null, ptr %_18
    %_19 = load { %Foo(ptr, i8*, i64)*, ptr }, { %Foo(ptr, i8*, i64)*, ptr }* %_16
    %_20 = extractvalue { %Foo(ptr, i8*, i64)*, ptr } %_19, 1
    %_21 = load i8*, ptr %named_0
    %_22 = load i64, i64* %named_1
    %_23 = call %Foo %_14(ptr %_20, i8* %_21, i64 %_22)
    store %Foo %_23, %Foo* %named_2
    %res$1 = alloca %Foo
    %_24 = load %Foo, %Foo* %named_2
    store %Foo %_24, %Foo* %res$1
    ; extracting the tag from the struct
    %tagPtr = getelementptr i64, ptr %res$1, i32 0
    %tag = getelementptr i64, ptr %tagPtr, i32 0
    %tagV = load i64, ptr %tag
    ; extracting the values from the struct
    %a = getelementptr i64, ptr %res$1, i32 0
    %b = getelementptr i64, ptr %a, i32 1
    %c = getelementptr i64, ptr %b, i32 0
    %d = getelementptr i64, ptr %b, i32 1
    %int = load i64, ptr %d
    %str = load ptr, ptr %c
    call i1 @printInt(ptr null, i64 %tagV)
    call i1 @printString(ptr null, ptr %str)
    call i1 @printInt(ptr null, i64 %int)
    ret void
}
