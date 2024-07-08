{-# LANGUAGE QuasiQuotes #-}

module Backend.Llvm.Prelude where

import Data.String.Interpolate (i)
import Relude

prelude :: Text
prelude = [i|
target triple = "x86_64-pc-linux-gnu"

declare i32 @printf(ptr, ...)
declare ptr @malloc(i64)

@#{globalUnit} = internal constant i1 1

@dnl = internal constant [4 x i8] c"%d\\0A\\00"
define i1 @#{printInt}(ptr %env, i64 %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i64 %x)
	ret i1 1
}

|]

printInt :: String
printInt = "printInt"

globalUnit :: String
globalUnit = "internal_global_unit"
