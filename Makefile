all:
	cabal build

run:
	cabal run

clean:
	cabal clean 

test:
	cabal new-test --test-show-details=streaming

compile:
	clang -fno-rtti -w -gfull -stdlib=libstdc++ -O0 -Iheaders -x ir -o out out.ll
