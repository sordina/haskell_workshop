
# Making a Web-Site with Scotty { #website }

The Haskell library [Scotty](http://hackage.haskell.org/package/scotty) is
similar to the ruby web-library [Sinatra](http://www.sinatrarb.com/).

<div class="center">

![Beam me Up](resources/images/scotty.png)

</div>

The Scotty can be installed by using the following Cabal command:

```shell
> cabal install scotty
```

Scotty's behaviour is based around [REST](http://en.wikipedia.org/wiki/Representational_state_transfer)
verbs and routes.

For example - A simple Hello-World website:

~~~{ data-language=haskell .nocheck }
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
~~~

If we inspect the type of `get` in GHCi we see this:

```ghci
> import Web.Scotty
> :info get
get :: Action action => RoutePattern -> action -> ScottyM ()
  	-- Defined in `scotty-0.4.6:Web.Scotty.Route'
```

The ActionM Action type-class instance allows us to perform any IO action we desire, such as
printing to the console, reading files, etc - through the use of the liftIO
function.

~~~{ data-language=haskell .nocheck }
myRoute = get "/hello" $ do
  liftIO $ putStrLn "What is your name?"
  myName <- liftIO readLn
  html $ mconcat ["Your name is ", myName, "... Thanks!"]
~~~

```instruction
Make a simple website using Scotty that shows the current time.
```

```open
An open question:

What features do the more advanced Haskell web-frameworks include
that Scotty lacks?
```
