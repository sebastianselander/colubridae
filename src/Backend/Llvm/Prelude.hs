{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.Llvm.Prelude where

import Data.String.Interpolate (i)
import Data.Text (pack)
import Relude hiding (exitFailure, exitSuccess)

import Prettyprinter qualified as Pretty

import Backend.Llvm.Lower (llvmOut)
import Backend.Llvm.Types (Decl (..), Ellipsis (Ellipsis, NoEllipsis))
import Backend.Types (Type (I, OpaquePointer, PointerType, Void))
import Names (ExplicitName (..), Ident (Ident), Namespace (Namespace))

prelude :: ([Decl], Text)
prelude =
    second (prologue <>)
        $ foldl'
            (\(decls, acc) (decl, body) -> (decl : decls, acc <> "\n" <> addTxt decl body))
            ([], "")
        $ fmap
            (\(_, b, c) -> (b, c))
            [ printString
            , printChar
            , exitSuccess
            , exitFailure
            , printInt
            ]
  where
    addTxt decl Nothing = llvmOut decl
    addTxt _ (Just body) = body

prologue :: Text
prologue =
    [i|
target triple = "x86_64-pc-linux-gnu"

@#{globalUnit} = internal constant i1 0

declare void @exit(i64)

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)
|]

globalUnit :: Text
globalUnit = "$internal_global_unit"

printString :: (ExplicitName, Decl, Maybe Text)
printString =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
     in ( ename
        , Declare (I 1) ename [OpaquePointer, PointerType (I 8)] NoEllipsis
        , Just
            [i|
@snl = internal constant [3 x i8] c"%s\\00"
define i1 @#{Pretty.pretty ename}(i8* %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @snl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8* %x)
	ret i1 1
}
|]
        )
  where
    name = "printString"

printChar :: (ExplicitName, Decl, Maybe Text)
printChar =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
     in ( ename
        , Declare (I 1) ename [OpaquePointer, PointerType (I 8)] NoEllipsis
        , Just
            [i|
@cnl = internal constant [3 x i8] c"%c\\00"
define i1 @#{Pretty.pretty ename}(i8 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @cnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8 %x)
	ret i1 1
}
|]
        )
  where
    name = "printChar"

exitSuccess :: (ExplicitName, Decl, Maybe Text)
exitSuccess =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
     in ( ename
        , Declare (I 1) ename [] NoEllipsis
        , Just
            [i|
define i1 @#{Pretty.pretty ename}() {
    call void @exit(i64 0)
    ret i1 1
}
|]
        )
  where
    name = "exit_success"

exitFailure :: (ExplicitName, Decl, Maybe Text)
exitFailure =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
     in ( ename
        , Declare (I 1) ename [] NoEllipsis
        , Just
            [i|
define i1 @#{Pretty.pretty ename}() {
    call void @exit(i64 1)
    ret i1 1
}
|]
        )
  where
    name = "exit_failure"

printInt :: (ExplicitName, Decl, Maybe Text)
printInt =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
     in ( ename
        , Declare (I 1) ename [OpaquePointer, I 8] NoEllipsis
        , Just
            [i|
@dnl = internal constant [3 x i8] c"%d\\00"
define i1 @#{Pretty.pretty ename}(i64 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @dnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i64 %x)
	ret i1 1
}
|]
        )
  where
    name = pack "printInt"
