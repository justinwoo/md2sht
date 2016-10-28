 Test

[Haskell](http://haskell.org/) as awesome

```haskell
-- Automatic derivation of "any class" requires this language pragma:
{-# LANGUAGE DeriveAnyClass #-}
-- Automatic derivation of Generic requires this language pragma:
{-# LANGUAGE DeriveGeneric #-}

newtype URL = URL String
  deriving (Generic, Show, FromJSON)

data Config = Config
  { targets :: [URL]
  } deriving (Generic, Show, FromJSON)
```

```js
function a(b) {
  return Promise.resolve(b.x + 123 + window.x);
}
```

Unicode: `id : ∀ a. a → a`
And in the codeblock:

```agda
data _≅′_ {k : Set} (a : k) {k′ : Set} (b : k′) : Set₁ where
    subst' : (keq : (kc : Set → Set) → kc k → kc k′)
           → (c : k′ → Set) → (c (keq (λ t → t) a) → c b)
           → a ≅′ b
```

Testing the hard new lines. You know, some people like to press enter to align
their text. And line breaks might be in weird places.
