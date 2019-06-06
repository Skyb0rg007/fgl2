module Data.IGraph.Example (
    -- * Example graphs
      a
    , b
    , c
    , d

    -- * Generating nodes
    , genUNodes
    , genLNodes
    ) where

import           Data.IGraph (IGraph, Node (..), NodeId (..), Context (..))
import qualified Data.IGraph as G

genUNodes :: Int -> [Node ()]
genUNodes n = zipWith Node [NodeId 1..NodeId n] (repeat ())

genLNodes :: Enum a => a -> Int -> [Node a]
genLNodes q i = take i $ zipWith Node [NodeId 1..] [q..]

makeNode :: Int -> a -> Node a
makeNode ident val = Node (NodeId ident) val

a :: IGraph Char ()
a = G.merge (Context [] (makeNode 1 'a') []) G.empty

b :: IGraph Char ()
b = G.mkGraph [makeNode 1 'a', makeNode 2 'b'] []

c :: IGraph Char ()
c = G.mkGraph [makeNode 1 'a', makeNode 2 'b', makeNode 3 'c'] []

d :: IGraph Char ()
d = G.merge (Context [(NodeId 1, ())] (makeNode 2 'b') []) a
