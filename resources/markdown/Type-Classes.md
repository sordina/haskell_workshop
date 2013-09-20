# Type Classes { #typeclasses }

A big part of writing clean reusable code is controlled
polymorphism. We are like a carny at the roller-coaster, anybody can
ride the roller coaster, but you must be at least this tall.

In the object oriented world we use the inheritance heirachy, we know
that if something is a subclass, it has at least all of the features
of its parent. Of course this is all intertwined with the sharing
of data and state, and that's bad.

In the functional world we get type classes, which is just
controlled polymorphism without the baggage. They basically
say, that I don't need to know exactly what type you'll call me with
but you need to provide a bunch of functions for me to use.

A function tells you what type classes it needs with a "context",
The context is the bit to the left of the double arrow "=>"

~~~{data-language=haskell .nocheck}
(+) :: Num a => a -> a -> a

show :: Show a => a -> String
~~~

In the above example, we have `(+)`. It can work
for any type that is a number. There are built
in types for numbers and you can also define your
own.

`show` can turn any "Showable" type into a string.
this is analogous to the `toString()` method in Java.

```instruction
Define a function that adds one to everything in a list.
What is the type of this function?
```

~~~{data-language=haskell .nocheck}
read :: Read a => String -> a

incrementAndshow :: (Num a, Show a) => a -> String
~~~

Unlike most other languages, with some kind of type driven
function dispatch (e.g. methods, overloading). Haskell can also
use the _return_ type of the function to choose functionality,
this can take a little getting used to, but it is powerful.

`read` turns a string into a value of another type,
although we don't know what this is yet. It depends,
for example on the type of the function that you are
passing the result of read into.

`incrementAndShow` demonstrates a function that uses two
type classes in its context.

```instruction
In ghci, convert a string to an integer using read, then covert
a string into a list of integers using read.

(Hint: use (::) to add a type to an expression)

If you just type 'read "1"' in ghci you get an
error, why is this?
```

```instruction
Define `incrementAndShow` which adds one to a number and
coverts it to a string.
What is they type of this function?
How did haskell infer your context?
```

# Defining Your Own Type Classes

Let's define a type class of things that can be
rated, as in 1 to 5 stars or awesome to lame.

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}

data Rating = 
  SoAwesomeICried |
  PrettyCool      |
  Meh             |
  ForTheLoveOfGodMakeItEnd
  deriving Show

class Rateable r where
  rating :: r -> Rating


data Beer = Coopers | Fosters | CarltonDraught
  deriving Show

instance Rateable Beer where
  rating Coopers = PrettyCool
  rating Fosters = ForTheLoveOfGodMakeItEnd
  rating CarltonDraught = Meh


data Movie = Movie String
  deriving Show

instance Rateable Movie where
  rating (Movie "Bladerunner") = SoAwesomeICried
  rating (Movie "Tron") = PrettyCool
  rating _ = Meh

~~~

When we define a type class we write out the name and the types of the
functions that we want associated with this type class. We also put a
type variable in our definition which refers to whatever type may
instance this type class later. We can use this type variable in the
definitions below. At this point we can also define default
definitions for the functions.

We then define two new data types, `Beer` and `Movie`
for which we add an "instance declaration" this is where
we write the definitions of the type class functions specialized
for beers of movies.

We now know how to manually add type class definitions and
we could add custom `Show` instances for each of our
new datatypes. However this would be a bunch of boilerplate
nonsense, so we can use the handy deriving directive to
automatically add a `Show` instance for us.

```instruction
Create your own Rateable type class and wacky Rating datatype.
Then create datatypes and instances for something you have a
strong opinion about, like cars or a political party.
```

```instruction
Add a review function to your type class that returns
a short textual review.
```

```instruction
Add a Rateable instance for a native type, like String.
```


There are a few other cool things you can do with typeclasses
that are not covered here. So if you want to know more, check
out some of these other articles:

<http://www.haskell.org/tutorial/classes.html>
