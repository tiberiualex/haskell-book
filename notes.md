Semigroup: a typeclass with a binary associative operation.

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

Monoid: a typeclass with a binary associative operation that has an identity. Semigroup is its superclass.

```haskell
class Monoid where
  mempty :: m
  mappend :: m -> m -> m -- legacy, use <>
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

Monoid laws:

```haskell
-- left identity
mappend mempty x = x

-- right identity
mappend x mempty = x

-- associativity
mappend x (mappend y z) =
  mappend (mappend x y) z

mconcat = foldr mappend mempty
```

Functor:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Functor laws:

```haskell
-- identity
fmap id == id

-- composition
fmap (f . g) == fmap f . fmap g
```
