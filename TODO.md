# Bugs

- [x] Constructor variables are counted as free variables in lambdas. Implement a correct free variable finder algorithm
- [x] Nested match case are deleted by desugaring algorithm (it was code generation that didn't produce stuff unless the value was used)

# Todo

## Immediate todos

- [x] Implement pattern matching in the backend.
- [x] Rewrite the way values are stored in adts. 
      Store each value on the heap instead. 
      Might encounter stack overflows for deep recursive data types otherwise.

## Later

- [ ] Refactor and comment the code a bit.
