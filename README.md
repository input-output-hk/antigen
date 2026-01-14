# AntiGen

AntiGen lets you write `QuickCheck` generators that can also be negated to generate negative examples. It can be used as a drop-in replacement for `Gen`.

## Example

```haskell
-- Returns an integer `n` (such that 0 <= n <= 5) and a string of length `n` consisting only of characters 'a'
antiGenLengthString :: AntiGen (Int, String)
antiGenLengthString = do
  -- Use `sometimes` to provide both a positive and a negative generator
  l <- choose (0, 5) `sometimes` choose (6, 10)
  s <-
    pure (replicate l 'a') `sometimes` do
      NonNegative l' <- suchThat arbitrary $ \(NonNegative x) -> x /= l
      pure $ replicate l' 'b'
  pure (l, s)
```

To generate a positive example, use `runAntiGen`
```
ghci> generate (runAntiGen antiGenLengthString)
(1, "a")
```

To generate a negative example, use `zapAntiGen`
```
ghci> generate (zapAntiGen 1 antiGenLengthString)
(6, "aaaaaa") -- length is too long
ghci> generate (zapAntiGen 1 antiGenLengthString)
(2, "bbbb") -- length of the string does not match up with the integer
```

Notice that there is exactly one mistake in the example above. 
The first argument of `zapAntiGen` can be used to specify how many negations the generator should introduce.
```
ghci> generate $ zapAntiGen 2 antiGenLengthString
(10,"b") -- both values are wrong
```
