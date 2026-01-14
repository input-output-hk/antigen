# AntiGen

AntiGen lets you write `QuickCheck` generators that can also be negated to generate neagtive examples. It can be used as a drop-in replacement for `Gen`.

## Example

```haskell
antiGenLengthString :: AntiGen (Int, String)
antiGenLengthString = do
  l <- choose (0, 5) `sometimes` choose (6, 10)
  s <-
    -- Use `sometimes` to provide both a positive and a negative generator
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

Notice that there is exactly one mistake in every negative test example. 
The first argument of `zapAntiGen` can be used to specify how many negations the generator should introduce.
```
ghci> generate $ zapAntiGen 2 antiGenLengthString
(10,"b") -- both values are wrong
```
