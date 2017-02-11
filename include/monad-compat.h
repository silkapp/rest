#if __GLASGOW_HASKELL__ >= 800
#define MONAD_ Monad m =>
#else
#define MONAD_ (Applicative m, Functor m, Monad m) =>
#endif
