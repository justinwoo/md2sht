# Test

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
