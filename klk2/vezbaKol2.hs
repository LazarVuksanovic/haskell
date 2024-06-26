import qualified Control.Monad.Fail as Fail

data Logger a = Logger { getLog :: (a, [String]) }

instance Functor Logger where
    fmap f (Logger (a, log)) = Logger (f a, log)

instance Applicative Logger where
    pure x = Logger (x, [])
    (Logger (f, log1)) <*> (Logger (x, log2)) = Logger (f x, log1 `mappend` log2)

instance Semigroup a => Semigroup Logger where
    (Logger (x, log1)) <> (Logger (y, log2)) = Logger (x <> y, log1 <> log2)

instance Monoid a => Monoid (Logger a) where
    mempty = Logger (mempty, [])

instance Monad Logger where
    return = pure
    Logger (x, log1) >>= f = Logger (r, log1 `mappend` log2)
                             where (Logger (r, log2)) = f x

instance Fail.MonadFail Logger where
    fail msg = error msg
































instance Monoid a => Monoid (Logger a) where
    mempty = Logger (mempty, [])

instance Monad Logger where
    return x = Logger (x, [])
    (Logger (x, log)) >>= f = Logger (res, log `mappend` newLog)
                                where (Logger (res, newLog)) = f x