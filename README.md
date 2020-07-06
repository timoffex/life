# Conway's Game of Life

This is a graphical Haskell implementation of Conway's game of life.

## Why

I wrote this to get more practice writing, refactoring and cleaning up
Haskell.

## Summary & history

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

I'm most proud of my usage of `lens` to write code that reads
surprisingly well (if you can read Haskell). For example:

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

#### The magic of `%=%`

The code uses a special combinator `%=%` that's not provided with the
`lens` package but was easy to write. Before I wrote it, I tried
several ways of performing multiple updates to the target of a lens. I
wanted something that was as concise as Dart's `..` ("cascade")
operator which allows one to write

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
printAndIncrementFstFst = fst %=% printAndIncrementFst

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
    wasDown <- use isDown
    liftIO $ print $ "isDown was " ++ show wasDown
    
    -- sets 'isDown' to its negation
    isDown %= not
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
