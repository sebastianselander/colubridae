all:
	cabal build

run:
	cabal run

clean:
	cabal clean 

test:
	cabal new-test --test-show-details=streaming
