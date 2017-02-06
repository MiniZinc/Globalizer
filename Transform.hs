module Transform where

import Control.Monad.State

-- Turn a maybe-transformation into a flag-setter.
monadify :: (a -> Maybe a) -> a -> State Bool a
monadify f x =
    case f x of
      Just y -> setFlag >> return y
      Nothing -> return x

setFlag = put True

-- Turn a flag-setter into a maybe-transformation.
monadToMaybe :: State Bool a -> Maybe a
monadToMaybe m = case runState m False of
                   (_, False) -> Nothing
                   (x', True) -> Just x'
