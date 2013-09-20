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

~~~{data-language=haskell .nocheck}
readFile :: FilePath -> IO String
~~~

This function can't return a normal string, because the return
value has been tainted by side effects. So the IO monad is acting as
a tag saying that the returned string must be treated specially.

But an `IO String` is not very useful to us, because we want
to actually do things with it. So Haskell, in its normal paternalistic
style, allows us access the `String` inside an `IO String` only in
a very careful way. We use an operation call bind `(>>=)` to access the
string.

~~~{data-language=haskell .nocheck}
(>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Here specialized for IO and String

(>>=) :: IO String -> (String -> IO b) -> IO b
~~~

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

```instruction
Why can't one write untaint?
If you could what problems would this cause?
```

One thing that can be a little strange is the type of `getLine`.
In imperative languages, functions of zero arguments make some sense.
They can be thought of recipies or to-do lists. In haskell a total function
of type `() -> Foo` is isomorphic to a constant `Foo`. Because the
function only has one input value and therefore only one possible output value.

So let us return to `getLine`. In an imperative language it would
look like `getLine :: () -> String`. Our first problem is that
the return value of this function is tainted by the outside world,
so we need to use the `IO` monad, `getLine :: () -> IO String`. Now
because of the isomorphism between unit domian functions and constants
we can just write `getLine :: IO String`. We call a constant of type `IO a`
an "IO action". Because it stands for a bunch of side effects that will
be performed together.

This will seem strange at first, because getLine isn't really a function --
it's just a constant value! But that's okay because while the `IO String`
is a constant (i.e. there is only one distinct IO action that reads a line
from the console) the value _inside_ the monad is not constant. It can
be different each time we look inside.

~~~{data-language=haskell .nocheck}
> getLine
hello
"hello"

> getLine
monad
"monad"

> getLine >>= (\name -> putStrLn ("Hello " ++ name))
andy
Hello andy
~~~

## One thing leads to another

Often when doing IO one wants to make sure one thing happens after another,
We can use `(>>=)` and just ignore the unwrapped value:

~~~{data-language=haskell .nocheck}
putStr "Hello " >>= (\_ -> putStrLn "World")

putStrLn "One" >>= (\_ -> putStrLn "Two") >>= (\_ -> putStrLn "Three")
~~~

This pattern can be easily abstracted, it has been standardized as
`(>>) :: IO a -> IO b -> IO b`. This can be read as "and then".

~~~{data-language=haskell .nocheck}
putStr "Hello " >> putStrLn "World"

putStrLn "One" >> putStrLn "Two" >> putStrLn "Three"
~~~

```instruction
Write a program that prints something stupid, funny or rude.
Make sure to use (>>) somewhere.
```

## Monad Wars III: Return of the Value

We mentioned before that there is no way to untaint a value,
once it has been tainted, we can make new tainted values from
it, but never untainted ones. But that begs the question,
can we choose to taint a value? Indeed we can, in fact,
this is a fundamental operation of a Monad. In haskell
it is confusingly called `return :: a -> IO a`.

A common pattern is to "open up" an `IO` with bind `(>>=)`,
mess around with the contents then wrap it back up again with
`return`. We have a function to help us with this called `lift`.
Specialized for `IO` it has type `lift :: (a -> b) -> (IO a -> IO b)`.
We can use this to take a vanilla function and "lift" it into the
IO monad. It works by unwrapping the IO monad calling our function
and then wrapping the result back up again.

```instruction
use return to write 'myLiftIO :: (a -> b) -> IO a -> IO b'
```

## Do it, do it good.

Sometimes when you are doing a lot of ad-hoc interleaved IO, using
bind and return all over the place can become a pain in the fingers.
So haskell provides special syntax for using monads, called "do notation".

To use do notation, we start a "do block" with the keyword `do`.
Inside a do block, statements can be written line by line and they are
automatically joined using "and then" `(>>)`. This causes them to
be run one after the other like in an imperative programming language.
One can also easily unwrap monad values and bind them to a variable
with using a left arrow "<-" syntax.

~~~{data-language=haskell .nocheck}
  main = do
    putStrLn "Hello World"
    putStrLn "Enter your name: "
    name <- getLine
    putStrLn ("Hello: " ++ name)
~~~

The do-notation is like a DSL, under the hood it is just expanded
to a chain of bind `(>>=)` calls. Here is what the above code
would look like without do-notation:

~~~{data-language=haskell .nocheck}
  main  =
    putStrLn "Hello World" >>
    putStrLn "Enter your name: " >>
    getLine >>= (\ name ->
      putStrLn ("Hello: " ++ name))
~~~

```instruction
Write a program that asks for someone's first and second name,
then complements them (or makes fun of them).

For extra points ask for their age, and customize the complement
(insult) depending on how old they are.

Do it with do-notation first, then "desugar" it into raw bind (>>=) calls
```

```instruction
My father once told me about an "amazing AI" that his
magician/programmer friend build, which could answer any yes/no
question as well as a human.

Of course it only worked if his friend was the one typing
the question! The trick being that it just counted the
number of spaces in the question. If there was an even number
it would output true, if there was an odd number, false.
You just fiddled with the wording, or snuck in an extra space,
to get the answer that you wanted...

Looks like it's time to write your super-dooper
human-level AI and see if your friends can figure
out how it works.
```

## Stay Functional, San Diego

Even when we are programming with side effects, we still
want our programs to follow functional principals. To stop our code
ending up like C written in do-notation, there are some
principals we can try to follow:

  1. Try to do most of the work inside pure functions.

  1. Don't create an IO action until the last possible moment.

  1. Be declarative, think of program as declaring a pipeline
     or specifying an interaction, instead of being a TODO list.

  1. Avoid mixing control, state and calculation. Instead abstract
     them into small composable pieces. For inspiration see the monad
     combinators in Control.Monad (e.g. `sequence`, `forever`, `mapM`).

These principals are not specific to monads. They are applicable to
all side-effect heavy programing problems. These principles can
also be applied to imperative programs for great justice.

A lot of patterns like reactive programming, dataflow programming and CQRS
are consequences of following principals such as these.

The pithy take-away is don't "update, execute, update". Instead "represent,
transform, compose".

```open
An open-ended question:

Why is it a good idea to make side effects explicit?
```
