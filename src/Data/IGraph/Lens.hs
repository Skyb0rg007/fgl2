
module Data.IGraph.Lens
    ( ctxPrevL
    , ctxSuccL
    , ctxNodeL
    , ctxLinksL
    ) where

import Data.IGraph
import Data.IntMap   (IntMap)
import Data.Sequence (Seq)

-- | ctxPrevL :: Lens' (Context a b) (IntMap (Seq b))
ctxPrevL :: Functor f
         => (IntMap (Seq b) -> f (IntMap (Seq b)))
         -> Context a b
         -> f (Context a b)
ctxPrevL f (Context p n s) = fmap (\p' -> Context p' n s) (f p)

-- | ctxSuccL :: Lens' (Context a b) (IntMap (Seq b))
ctxSuccL :: Functor f
         => (IntMap (Seq b) -> f (IntMap (Seq b)))
         -> Context a b
         -> f (Context a b)
ctxSuccL f (Context p n s) = fmap (\s' -> Context p n s') (f s)

-- | ctxNodeL :: Lens (Context a b) (Context a' b) (Node a) (Node a')
ctxNodeL :: Functor f
         => (Node a -> f (Node a'))
         -> Context a b
         -> f (Context a' b)
ctxNodeL f (Context p n s) = fmap (\n' -> Context p n' s) (f n)

-- | ctxLinksL :: Traversal (Context a b) (Context a b')
--                          (IntMap (Seq b)) (IntMap (Seq b'))
ctxLinksL :: Applicative f
          => (IntMap (Seq b) -> f (IntMap (Seq b')))
          -> Context a b
          -> f (Context a b')
ctxLinksL f (Context p n s) = Context <$> f p <*> pure n <*> f s
