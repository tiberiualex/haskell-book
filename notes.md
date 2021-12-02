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

Applicative:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

It provides:

```haskell
liftA :: Applicative f =>
  (a -> b)
  -> f a
  -> f b

liftA2 :: Applicative f =>
(a -> b -> c)
  -> f a
  -> f b
  -> f c

liftA3 :: Applicative f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
```

Applicative laws:

```haskell
--identity
pure id <*> v = v

-- composition
pure (.) <*> u <*> v <*> w =
  u <*> (v <*> w)

-- homomorphims
pure f <*> pure x = pure (f x)

-- interchange
u <*> pure y = pure ($ y) <*> u
```

```haskell
($)   ::   (a -> b) -> a   ->   b
(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

Monad:

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

Monad laws:

```haskell
-- right identity
m >>= return = m

-- left identity
return x >>= f = fx

-- associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

Foldable:

```haskell
class Foldable t where
  {-# MINIMAL foldMap | foldr #-}
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
```

Traversable:

```haskell
class (Functor t, Foldable t) => Traversable t where
  {-# MINIMAL traverse | sequenceA #-}
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
```

Traversable laws:

```haskell
-- Naturality
t . traverse f = traverse (t . f)

-- Identity
traverse Identity = Identity

-- Composition
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
```
