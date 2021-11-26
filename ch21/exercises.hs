-- class (Functor t, Foldable t) => Traversable t where
--     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--     traverse f = sequenceA . fmap f
-- So it takes a function that takes x and returns a result inside an applicative
-- it takes a traversable/foldable/functor that contains values of type x
-- then it returns an applicative with the traversable structure containing the result of the first function

-- This is simpler at least, it flips 2 structures/contexts around
-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id
