# Conway's Game of Life

This is a graphical Haskell implementation of Conway's game of life.

## Why

I wrote this to get more practice writing, refactoring and cleaning up
Haskell.

## Running it

Install [stack](https://www.haskellstack.org), and then use `stack
run`. File an issue if this doesn't work---I seem to run into a
problem every time I try to use a new Haskell tool (I'm on a Mac), but
by now I've forgotten what I did to make my current setup work. I'd
love to help, and if I can't then it's still good to document these
issues.

If you get it to run, the controls (as of writing) are:

- Click or hold down the left mouse button to fill cells.
- Hold shift to erase cells.
- Drag with the right mouse button (or Ctrl + LMB on Mac) to pan.
- Press space to start or pause the simulation.

## Observations / what I learned

### `Main.hs`

Most of the code is in `Main.hs`. I usually try to separate stuff into
cleanly-separated small files, but recently I realized that trying to
figure out how to do this sends me down a rabbit hole that I sometimes
don't come out of. Because of this, I decided to just put everything
into `Main.hs` and separate it "later", which did not happen because
it turns out having everything in one file is good enough.

### Grids

I almost got stuck trying to come up with a perfect definition of a
grid, but fortunately I stopped myself (otherwise I get serious
analysis paralysis). I wrote `RectGrid` first and then played around
with QuickCheck briefly before writing the main code. Then I wrote
`InfiniteGrid` and refactored `Main.hs` to use that---it was
pleasantly easy.

`InfiniteGrid` and `RectGrid` aren't great grid implementations, but
they're just good enough for my purposes. My code does not use
`RectGrid` anymore, but I'm keeping it around for reference.

### Readability of `lens`

#### Using prisms vs `case of`

When I was first learning Haskell a bit over a year ago, I became a
big fan of `case of`. It made me wish every language had a switch
expression alongside the switch statement.

```haskell
data OpenGLError = None
                 | YouMisunderstoodTheDocs
                 | ItJustDoesntWork

openglErrorString err = case err of
  None                    -> "No error"
  YouMisunderstoodTheDocs -> "Re-read the documentation, but more closely"
  ItJustDoesntWork        -> "Something about a 'context'"
```

I believe C# might have this now, but I don't know how to get Unity to
work with the most recent version.

While writing `Main.hs`, I noticed I had a lot of code like this:

```haskell
case evt of
  EventKey (MouseButton RightButton) Down _ _ -> do
    world <- get
    panState %=% ...

  _ -> return ()
```

The last case is an underscore, which is a pattern that matches
anything. It's required---the `case of` expression has to evaluate to
a value (in this case a monadic action), and there's no `null` in
Haskell, so if no pattern matches this code throws. Unfortunately,
it's ugly.

The real intention behind that code is to do something like this:

```
-- pseudo-Haskell
--
-- 'when' is a real function in Haskell that evaluates to 'return ()'
-- if the condition does not match
when (evt is like (EventKey (MouseButton RightButton) Down _ _))
  world <- get
  panState %=% ...
```

I think that the reason that `case of` doesn't work nicely here is
because `case of` is better suited (maybe even "truly intended") for
exhaustive pattern matching. Fortunately, Haskell is a wonderfully
flexible language, and we can get code like the above by using a
special abstraction known as a `Prism`:

```haskell
when (evt & has (_EventKey.aside1 _MouseRightButton.aside2 _Down)) $ do
  world <- get
  panState %=% ...
```

`_EventKey`, `_MouseRightButton`, and `_Down` are prisms automatically
generated by using the Template Haskell `$( makePrisms ... )`
directives defined in the `lens` package. Simply put, a prism from `s`
to `a` is like a pair of functions `a -> s` and `s -> Maybe a`. The
first function says that given an `a` (like the tuple of parameters to
the `EventKey` constructor), I can construct a value of type `s` (like
an `Event`). The second function says that given an `s`, I can
sometimes get an `a`; for example, some `Event` instances are made
using the `EventKey` constructor and therefore have `EventKey` data (a
key, a key state (up or down), modifiers, and the current mouse
position); other `Event` instances could be `EventMotion` or
`EventResize`.

`&` just reverses function application: it takes the function on the
right and passes the value on the left to it as an argument.

`has` is a combinator from the `lens` package that checks whether a
"traversal" has any targets when applied to a specified value. A prism
is a special type of "traversal", and using `has` with it is
equivalent to checking whether a given value matches the prism (so
`evt & has _EventKey` is `True` iff `evt` is made with the `EventKey`
constructor). `aside1` and `aside2` are combinators that I defined
which allow viewing the first or second element of a tuple,
respectively, through a prism. Their names come from the similarity to
`aside` in the `lens` package, which is like `aside2` specialized to
2-tuples.

#### Readability example

I'm most proud of my usage of `lens` to write code that reads
surprisingly well (if you can read Haskell). For example, here's some
code that looks almost like an imperative program:

```haskell
updateMouseState evt = do
  whenJust (evt ^? _EventMotion) $ \mp ->
    mouse %=% do
      position .= mp
      clicked  .= False

  whenJust (evt ^? _EventKey . aside1 _MouseButton) $ \(b, ks, mods, mp) -> do
    mouse %=% do
      position  .= mp
      modifiers .= mods
    if b & isn't _LeftButton
      then mouse %= set clicked False
      else mouse %=% do
             wasDown <- use isDown
             clicked .= (wasDown && ks == Up)
             isDown  .= (ks == Down)
```

This reads: "When `evt` is an `EventMotion`, set the mouse position to
the event's position `mp`, and set the mouse's `clicked` state to
`False`. When `evt` is an `EventKey` with a `MouseButton` key, update
the mouse's position and modifiers. If the mouse button wasn't a left
button, then just set the mouse's `clicked` state to `False`;
otherwise, update it to `True` if the mouse was down and is now up,
and also update the mouse's `isDown` state."

In this case, `mouse`, `position`, `clicked`, `isDown` and `modifiers`
are all "lenses"---they allow getting or setting some data (e.g. a
`MouseState`) on a value (e.g. a `World`). The function
`updateMouseState` takes an `Event` and returns a stateful computation
that takes a `World` and returns an updated `World`. Note that the
function never mentions a `World` and that there is no type
signature. This shows two things I love about Haskell:

1. Type inference, even for function signatures. While one should
   generally document the types for public APIs, this is helpful for
   private functions where the types would add noise. Other languages
   can usually infer the types of "lambda functions", but otherwise
   the type is built into function definition syntax.
2. Concise syntax. The `World` is implicit (and inferred!) in the
   above; one can view it like a `this` or `self` argument. That's not
   too uncommon: the only anomaly is that most languages only allow
   one to use `this` within the definition of a class, whereas
   `updateMouseState` is a free function. The cool bit is that when I
   write,
   
   ```haskell
   mouse %=% do
     wasDown <- use isDown
     clicked .= (wasDown && ks == Up)
     isDown  .= (ks == Down)
   ```
   
   the `MouseState` is implicit within that inner `do` block! This is
   much more powerful than even Dart's cascade operator: `wasDown` is
   a temporary variable, which you can't do in a cascade. Any kind of
   code that can go outside of that `do` block can go inside, and one
   can recursively modify subfields in the same fashion. It's like
   defining little, anonymous, one-use methods.


#### The magic of `%=%`

The code above uses a special combinator `%=%` that's not provided
with the `lens` package but was easy to write. Before I wrote it, I
tried several ways of performing multiple updates to the target of a
lens. I wanted something that was as concise as Dart's `..`
("cascade") operator which allows one to write

```dart
mouse
  ..clicked = true
  ..isDown = false;
```

I tried

```haskell
applyAll :: [a -> a] -> a -> a
mouse %= applyAll
  [ set clicked True
  , set isDown False ]
```

which looks neat but repeats the word `set` and has some extra bracket
and comma noise. It also doesn't allow querying the mouse state; in
general this isn't a convenient way to represent a stateful
computation. This led me to the `execState` approach:

```haskell
mouse %= execState (do
  clicked .= True
  isDown  .= False )
```

I loved this because it allowed me to write exactly what I wanted: a
stateful update. To clean this up, I initially defined

```haskell
s %=% m = s %= execState m
```

which allowed me to get rid of `execState`. One feature this approach lacks
is the ability to use the outer monad inside the stateful computation. For example,
if I'm running in `StateT .. IO ..`, I'd like to be able to `liftIO $ print ..`:

```haskell
mouse %=% do
  wasDown <- use isDown
  liftIO $ print $ if wasDown
                     then "Was down"
                     else "Wasn't down"
  isDown .= True
```

The following definition worked:

```haskell
-- You need RankNTypes, and you need to specify the signature
-- explicitly (because 'l' is used polymorphically, for both
-- getting and setting; 'l' is a Lens).
l %=% m = do
  a  <- use l
  a' <- execStateT m a
  l .= a'
```

Finally, I realized that this could be rewritten to even work with
traversals! This means I can run the stateful computation on an entire
list of targets while using the same syntax, which is something you
can't say for many languages:

```haskell
-- For the sake of example.
printAndIncrementFst :: StateT (Int, a) IO ()
printAndIncrementFst = do
  val <- use _1
  liftIO $ print $ "Original value: " ++ show val
  _1 += 1

-- Applies printAndIncrementFst to the first element of the tuple.
printAndIncrementFstFst :: StateT ((Int, a), b) IO ()
printAndIncrementFstFst = _1 %=% printAndIncrementFst

-- Applies printAndIncrementFst to each tuple.
printAndIncrementFstAll :: StateT [(Int, a)] IO ()
printAndIncrementFstAll = each %=% printAndIncrementFst
```

And it composes very nicely:

```haskell
mouse %=% do
  position %=% do
    -- There's probably a way to write
    --   (x, y) <- use (<something> _1 _2)
    -- but I'm not sure what <something> should be.
    x <- use _1
    y <- use _2
    liftIO $ print $ "x was " ++ show _1
    liftIO $ print $ "y was " ++ show _2
    each %= negate
  isDown %=% do
    wasDown <- get
    liftIO $ print $ "isDown was " ++ show wasDown

    -- sets 'isDown' to its negation
    modify not

    -- Instead of 'get' and 'modify', this can use 'id' like 'this'
    -- from other languages and combine with lens style.
    --
    -- get           becomes    use id
    -- modify not    becomes    id %= not
```

You can't do that in Dart!

### Record syntax

This is the first time I've used Haskell's record syntax. I used to
avoid it because I was hoping it was unnecessary given the flexibility
of Haskell, but it turned out quite handy. Here's how I initialize the
`World`:

```haskell
initialWorld = World { _cells = emptyGrid // [ ((r, c), False)
                                             | r <- [-20 .. 20]
                                             , c <- [-20 .. 20] ]
                     , _viewPort = ViewPort
                         { viewPortTranslate = (0, 0)
                         , viewPortRotate = 0
                         , viewPortScale = 0 }
                     , _mouse = MouseState
                         { _mouseStateIsDown    = False
                         , _mouseStatePosition  = (0, 0)
                         , _mouseStateClicked   = False
                         , _mouseStateModifiers = Modifiers Up Up Up }
                     , _panState = initialPanState
                     , _isRunning = False }
```

Observations:

* It looks a lot like C#'s object initializer syntax. The convention
  in Haskell to put the comma at the start of a line makes this even
  neater, in my opinion. Additionally, `World` is an immutable data
  type, but C#'s object initializer syntax requires mutation.
* `lens`'s `makeFields` and `makeLenses` Template Haskell meta
  programs require naming conventions that cause some noise: extra
  underscores for `World` fields, and the entire type name is repeated
  for `MouseState` because I use `makeFields` instead of `makeLenses`.
* I'm writing this in emacs with haskell-ide-engine and lsp-haskell
  (which were a pain to set up), so whenever I added a new field to a
  type, it warned me that I needed to initialize it here. In C#,
  programmers tend to use default values instead.

This could probably be made perfect with a Template Haskell program
that allowed you to use field names directly to get rid of the
repetition caused by naming conventions:

```haskell
initialWorld = World { cells = ...
                     , viewPort = ...
                     , mouse = MouseState
                       { isDown    = False
                       , position  = (0, 0)
                       , clicked   = False
                       , modifiers = Modifiers Up Up Up }
                     ... }
```
