-- MonadPlus.hs
-- Maximiliaan Leyman
-- GHCi vond MonadPlus elders precies niet, dus heb ik het hier zelf ingezet om hem te kunnen gebruiken.

module MonadPlus
( MonadPlus
, mzero
, mplus
, guard
) where

class Monad m => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: MonadPlus m => Bool -> m ()
guard False = mzero
guard True  = return ()
