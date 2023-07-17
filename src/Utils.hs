module Utils where

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right
