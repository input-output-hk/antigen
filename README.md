# AntiGen

AntiGen lets you write `QuickCheck` generators that can also be negated to generate neagtive examples. It can be used as a drop-in replacement for `Gen`.

## Example

```haskell
antiGenLengthString :: AntiGen (Int, String)
antiGenLengthString = do
  l <- antiGenSmall
  s <-
    pure (replicate l 'a') `sometimes` do
      NonNegative l' <- suchThat arbitrary $ \(NonNegative x) -> x /= l
      pure $ replicate l' 'b'
  pure (l, s)

-- To generate a positive example, use `runAntiGen`
-- > generate (runAntiGen antiGenLengthString)
-- >> (1, "a")

-- To generate a negative example, use `zapAntiGen`
-- > generate (zapAntiGen 1 antiGenLengthString)
-- >> (6, "aaaaaa") -- length is too long
-- > generate (zapAntiGen 1 antiGenLengthString)
-- >> (2, "bbbb") -- length of the string does not match up with the integer
```
