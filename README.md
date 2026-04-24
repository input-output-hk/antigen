# AntiGen

AntiGen lets you write `QuickCheck` generators that can also produce *negative* examples — subtly malformed variants of the valid data. It's a drop-in replacement for `Gen`.

## Why?

Property-based tests usually check that valid inputs are accepted. But validators and parsers also need to reject malformed inputs, and writing a second generator for the invalid case duplicates logic and tends to drift out of sync with the positive one.

With AntiGen you describe the valid shape once, marking each constrained choice with what an out-of-bounds variant would look like. The same generator can then produce either a valid example or a negative example with `n` of those choices flipped — and any later decisions that depend on an earlier choice see the flipped value, so the rest of the structure cascades accordingly.

## Example

Consider validating a length-prefixed message: a declared length field followed by a payload that must have exactly that many characters, each of which must be an `'a'`:

```haskell
data Message = Message { declaredLen :: Int, payload :: String } deriving Show

isValid :: Message -> Bool
isValid (Message n s) =
  0 <= n && n <= 10 && length s == n && all (== 'a') s

messageGen :: AntiGen Message
messageGen = do
  n <- choose (0, 10) |! choose (11, 20) -- declared length
  nElems <- pure n |! choose (n + 1, n + 10) -- actual number of elements
  s <- replicateM nElems (pure 'a' |! elements ['b' .. 'z'])
  pure (Message n s)
```

Use `runAntiGen` to draw a valid example:

```
ghci> generate (runAntiGen messageGen)
Message {declaredLen = 5, payload = "aaaaa"}
```

Use `zapAntiGen k` to draw a negative example by flipping at most `k` of the decision points:

```
ghci> generate (zapAntiGen 1 messageGen)
Message {declaredLen = 4, payload = "aaha"}
-- one character was corrupted; the length field and payload length still agree

ghci> generate (zapAntiGen 1 messageGen)
Message {declaredLen = 17, payload = "aaaaaaaaaaaaaaaaa"}
-- the length field was flipped out of range; the payload cascaded to still
-- match it — and with 17 characters, there are now 17 character-level
-- decision points rather than the original few

ghci> generate (zapAntiGen 3 messageGen)
Message {declaredLen = 15, payload = "aaaaaaaaaadaaaaaa"}
-- three mistakes:
-- length out of range, one character corrupted, declared length does not match actual length (15 vs 17)
```

The same generator now drives both directions of the validator's property:

```haskell
prop_accepts = forAll (runAntiGen   messageGen) isValid
prop_rejects = forAll (zapAntiGen 1 messageGen) (not . isValid)
```
