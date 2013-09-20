# Monads

## No seriously, what are Monads?

A monad is just a bunch of rules. There are many, many analogies for
monads out there. Each of these analogies are useful, but can be obscuring on
their own, they are just one view point. To effectively wield monads
we must use many different view points together, each one revealing
a little piece of the underlying structure of monads. 
Each view point becomes another tool in our mental toolbox.

So there is no one magic-bullet analogy for monads, only many complementary
ones.

Haskell uses monads to represent side-effects. The simplist and most
practical analogy is the "tainted value" analogy. In haskell the function
that reads a file looks like this:

```haskell
readFile :: FilePath -> IO String
```

This function can't return a normal string, because the return
value has been tainted by side effects. So the IO monad is acting as
a tag saying that the returned string must be treated specially.

But an `IO String` is not very useful to us, because we want
to actually do things with it. So Haskell, in its normal paternalistic
style, allows us access the `String` inside an `IO String` only in
a very careful way. We use an operation call bind `(>>=)` to access the
string.

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Here specialized for IO and String

(>>=) :: IO String -> (String -> IO b) -> IO b
```

This says the only way we can "look inside" an `IO String` is
by providing a function that takes a `String` and returns some
other new value that has also been tainted by the outside world.
In other words we can only look at a value "inside" the `IO` monad
if we promise to make sure our result will also be tainted.

This means that if some code uses a value from the outside world, even
deep inside, it cannot be hidden, it must be made explicit in the type.
Tainting is a one way street, once you are tainted you can't be untainted.
There is no function `untaint :: IO a -> a`. One can't get an
untainted value from a tainted one.

In fact, in haskell, the very way we construct a useful program is by
ultimately creating value of type `IO ()` that we assign to special
variable called `main`.

Exercise: "Why can't one write untaint, if you could what problems would this
cause?"

One thing that can be a little strange is the type of `getLine`
in imperative languages, functions of zero arguments make some sense.
They can be thought of recipies or to-do lists. In haskell a total function
of type `() -> Foo` is isomorphic to the constant `Foo`. Because the
function only has one input value and therefore only one possible output value.

So let us return to `getLine`. In an imperative language it would
look like this `getLine :: () -> String`. Our first problem is that
the return value of this function is tainted by the outside world,
so we need to use the `IO` monad, `getLine :: () -> IO String`. Now
because of the isomorphism between unit domian functions and constants
we can just write `getLine :: IO String`.

This will seem strange at first, because getLine isn't really a function --
it's just a constant value! But that's okay because while the `IO String`
is a constant (i.e. there is only one distinct IO action that reads a line
from the console) the value _inside_ the monad is not constant. It can
be different each time we look inside.

```haskell
Prelude> getLine
hello
"hello"

Prelude> getLine
monad
"monad"

Prelude> getLine >>= (\name -> putStrLn ("Hello " ++ name))
andy
Hello andy
```

## One thing leads to another

Often when doing IO one wants to do one thing before another,
We can use `(>>=)` and just ignore the unwrapped value:

```haskell
putStr "Hello " >>= (\_ -> putStrLn "World")

putStrLn "One" >>= (\_ -> putStrLn "Two") >>= (\_ -> putStrLn "Three")
```

This pattern can be easily abstracted, it has been standardized as
`(>>) :: IO a -> IO b -> IO b`. This can be read as "and then".

```haskell
putStr "Hello " >> putStrLn "World"

putStrLn "One" >> putStrLn "Two" >> putStrLn "Three"
```

## Monad Wars III: Return of the Value

We mentioned before that there is no way to untaint a value,
once it has been tainted, we can make new tainted values from
it, but never untainted ones. But that begs the question,
can we choose to taint a value? Indeed we can, in fact,
this is a fundamental operation of a Monad. In haskell
it is confusingly called `return :: a -> IO a`.

Execise: use return to write `liftIO :: (a -> b) -> IO a -> IO b`
