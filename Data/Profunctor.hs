{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Profunctor where

import Prelude hiding ((.), id)

import Control.Arrow (Kleisli (..), (|||))
import Control.Category
import Control.Comonad
import Control.Monad
import Control.Monad.Fix
import Data.Cotraversable

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap g = dimap id g

infixr 1 ^>>, >>^, <<^, ^<<

(^>>) :: Profunctor p => (a -> b) -> p b c -> p a c
(^>>) = lmap

(>>^) :: Profunctor p => p a b -> (b -> c) -> p a c
(>>^) = flip rmap

(<<^) :: Profunctor p => p b c -> (a -> b) -> p a c
(<<^) = flip lmap

(^<<) :: Profunctor p => (b -> c) -> p a b -> p a c
(^<<) = rmap

instance Profunctor (->) where
    dimap f g a = g . a . f

instance Functor f => Profunctor (Kleisli f) where
    dimap f g (Kleisli a) = Kleisli (fmap g . a . f)

instance Functor f => Profunctor (Cokleisli f) where
    dimap f g (Cokleisli a) = Cokleisli (g . a . fmap f)

class Profunctor p => Related f p where
    relate :: p a b -> p (f a) (f b)

instance Functor f => Related f (->) where relate = fmap

instance (Applicative p, Traversable f) => Related f (Kleisli p) where
    relate = Kleisli . traverse . runKleisli

instance (Cotraversable f, Functor ɯ) => Related f (Cokleisli ɯ) where
    relate = Cokleisli . cotraverse . runCokleisli

instance {-# OVERLAPPING #-} Comonad ɯ => Related (Either a) (Cokleisli ɯ) where
    relate (Cokleisli f) =
        (\ a -> Left . copure . (a <$)) |||
        (\ a -> Right . f     . (a <$)) ^>> Cokleisli (copure <*> void)


class Profunctor p => Corelated f p where
    corelate :: p (f a) (f b) -> p a b

instance Corelated ((,) a) (->) where
    corelate f a = let (c, b) = f (c, a) in b

instance MonadFix m => Corelated ((,) a) (Kleisli m) where
    corelate (Kleisli f) = Kleisli $ \ a -> snd <$> mfix (f . flip (,) a . fst)

instance Corelated (Either a) (->) where
    corelate f = let go = either (go . f . Left) id in go . f . Right

instance Monad m => Corelated (Either a) (Kleisli m) where
    corelate (Kleisli f) = let go = either (go <=< f . Left) pure in Kleisli (go <=< f . Right)

instance Functor f => Corelated (Either a) (Cokleisli f) where
    corelate (Cokleisli f) = Cokleisli (go . fmap Right)
      where go ɯ = case f ɯ of Right b -> b
                               Left  c -> go (Left c <$ ɯ)
