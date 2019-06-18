# Comments for Exercise 7

## Exercise 7.1 (3 of 4 points)
Be careful how you write down such proofs.
You have to show that `A ==> B` holds.
You startet with `B` and made a chain of implications (`==>`) that arrived at `A`.
So you have shown `B ==> A`.
You should replace all `==> {argumentation}` lines by `<== {argumentation`, then it is correct.


## Exercise 7.2 (0 of 4 points)
Missing


## Exercise 7.3
### a) (+)
The solution with a recursive `where` definition is better than the solution with a recursive function call.
The latter one allocates new memory space for each lazily evaluated recursive function call.
The former shares the same memory location for each lazy evaluation.
And you are right, the `Stream` type is a perfect example for a functor.

### b) (+)
You could use the functions from part (a) to simplify your solution, e.g. `(+) = zip (+)`.
Note that the first `+` (the one we are defining) is a function on type `Stream elem`, the second one (the one we are using) a function on type `elem`.

### c) (+)
### d) (+)
You can use the fact that `Stream elem` is an instance of `Num` to simplify your solution and also get rid of recursive function calls.
