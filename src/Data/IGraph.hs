{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The definition of an inductive graph type

module Data.IGraph (
    -- * Data types
      NodeId (..)
    , Node (..)
    , Edge (..)
    , Context (..)
    , IGraph

    -- * Contruction
    , empty
    , merge
    , mkGraph
    , newNodeId

    -- * Insertion
    , insNode
    , insEdge
    , insEdges

    -- * Deconstruction
    , match

    -- * Basic Queries
    , isEmpty
    , numNodes
    , numEdges
    , graphEdges
    , graphNodes
    , nodeRange
    ) where

import           Data.Foldable    (toList)
import           Data.IntMap      (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Sequence    (Seq)
import qualified Data.Sequence as Seq
import           Foreign.Storable (Storable)
import           Data.List        (foldl')
import           Data.Maybe       (fromMaybe)

-- | Identifier for a Node in an inductive graph
newtype NodeId = NodeId Int
    deriving (Eq, Ord, Enum, Storable)

instance Show NodeId where
    show (NodeId i) = "#" ++ show i

-- | A graph node - an identifier and a value
data Node a = Node !NodeId !a
    deriving (Eq, Ord, Show)

-- | A graph edge - two node identifiers and a value
data Edge b = Edge !NodeId !NodeId !b
    deriving (Show)

-- | A node context - the edges to the node, from the node, and the node itself
data Context a b
    = Context
    { ctxPrev :: ![(NodeId, b)]  -- ^ Edges to the node
    , ctxNode :: !(Node a)       -- ^ The node itself
    , ctxNext :: ![(NodeId, b)]  -- ^ Edges from the node
    } deriving (Show, Eq)

data Context' a b = Context' !(IntMap (Seq b)) !a !(IntMap (Seq b))

type GraphType a b = IntMap (Context' a b)

-- | The abstract inductive graph type
newtype IGraph a b = IGraph (GraphType a b)

instance (Show a, Show b) => Show (IGraph a b) where
    show g = "mkGraph " ++ show (graphNodes g) ++ " " ++ show (graphEdges g)

-- | The empty graph
empty :: IGraph a b
empty = IGraph IntMap.empty

-- | Add a context to the given graph
merge :: Context a b -> IGraph a b -> IGraph a b
merge (Context p (Node (NodeId ident) nodeVal) s) (IGraph g) =
    let !p' = listToNodeMap p
        !s' = listToNodeMap s
        !g1 = IntMap.insert ident (Context' p' nodeVal s') g
        !g2 = addSucc g1 ident p'
        !g3 = addPred g2 ident s'
    in IGraph g3

listToNodeMap :: [(NodeId, b)] -> IntMap (Seq b)
listToNodeMap edges = 
    let pairs = map (\(NodeId ident, b) -> (ident, Seq.singleton b)) edges
    in IntMap.fromListWith (Seq.><) pairs


addSucc :: GraphType a b -> Int -> IntMap (Seq b) -> GraphType a b
addSucc g ident = IntMap.differenceWith go g
    where
        go :: Context' a b -> Seq b -> Maybe (Context' a b)
        go (Context' p nodeVal s) newVals =
            let s' = IntMap.insertWith (Seq.><) ident newVals s
            in Just $ Context' p nodeVal s'

addPred :: GraphType a b -> Int -> IntMap (Seq b) -> GraphType a b
addPred g ident = IntMap.differenceWith go g
    where
        go :: Context' a b -> Seq b -> Maybe (Context' a b)
        go (Context' p nodeVal s) newVals =
            let p' = IntMap.insertWith (Seq.><) ident newVals p
            in Just $ Context' p' nodeVal s

-- | Insert an edge into the graph
insEdge :: Edge b -> IGraph a b -> IGraph a b
insEdge (Edge (NodeId v) (NodeId w) val) (IGraph g) =
    let addS' (Context' p l s) =
            Context' p l (IntMap.insertWith (Seq.><) w (Seq.singleton val) s)
        addP' (Context' p l s) =
            Context' (IntMap.insertWith (Seq.><) v (Seq.singleton val) p) l s
        g1 = IntMap.adjust addS' v g
        g2 = IntMap.adjust addP' w g1
    in g2 `seq` IGraph g2

-- | Insert edges into the graph
insEdges :: [Edge b] -> IGraph a b -> IGraph a b
insEdges es g = foldl' (flip insEdge) g es

-- | Insert a node into the graph
insNode :: Node a -> IGraph a b -> IGraph a b
insNode (Node (NodeId ident) nodeVal) (IGraph g) =
    let g' = IntMap.insert ident (Context' IntMap.empty nodeVal IntMap.empty) g
    in g' `seq` IGraph g'

-- | Create a graph from a list of nodes and edges
mkGraph :: [Node a] -> [Edge b] -> IGraph a b
mkGraph vs es = insEdges es
              . IGraph
              . IntMap.fromList
              . map (\(Node (NodeId k) v) -> (k, Context' IntMap.empty v IntMap.empty))
              $ vs


-- | Split a graph into the given node and the rest of the graph
match :: NodeId -> IGraph a b -> Maybe (Context a b, IGraph a b)
match (NodeId ident) (IGraph g) =
    case IntMap.lookup ident g of
        Nothing -> Nothing
        Just (Context' p nodeVal s) ->
            let !g1 = IntMap.delete ident g
                !p' = IntMap.delete ident p
                !s' = IntMap.delete ident s
                !g2 = clearPred g1 ident s'
                !g3 = clearSucc g2 ident p'
                !p'' = nodeMapToList p'
                !s'' = nodeMapToList s'
            in Just (Context p'' (Node (NodeId ident) nodeVal) s'', IGraph g3)

nodeMapToList :: IntMap (Seq b) -> [(NodeId, b)]
nodeMapToList edges =
    let eList = IntMap.toList edges
        explode (ident, vals) = map (\val -> (NodeId ident, val)) (toList vals)
    in concatMap explode eList

clearSucc :: GraphType a b -> Int -> IntMap x -> GraphType a b
clearSucc g v = IntMap.differenceWith go g
    where
        go :: Context' a b -> x -> Maybe (Context' a b)
        go (Context' p nodeVal s) _
            = Just $ Context' p nodeVal (IntMap.delete v s)

clearPred :: GraphType a b -> Int -> IntMap x -> GraphType a b
clearPred g v = IntMap.differenceWith go g
    where
        go :: Context' a b -> x -> Maybe (Context' a b)
        go (Context' p nodeVal s) _
            = Just $ Context' (IntMap.delete v p) nodeVal s

-- | Determine if a graph is empty
isEmpty :: IGraph a b -> Bool
isEmpty (IGraph g) = IntMap.null g

-- | Number of nodes
numNodes :: IGraph a b -> Int
numNodes (IGraph g) = IntMap.size g

-- | Number of edges
numEdges :: IGraph a b -> Int
numEdges = length . graphEdges

-- | Get a list of all the edges
graphEdges :: IGraph a b -> [Edge b]
graphEdges (IGraph g) = do
    (node, Context' _ _ s) <- IntMap.toList g
    (next, labels)         <- IntMap.toList s
    label                  <- toList labels
    pure $ Edge (NodeId node) (NodeId next) label

-- | Get a list of all the nodes
graphNodes :: IGraph a b -> [Node a]
graphNodes (IGraph g) = do
    (node, Context' _ nodeVal _) <- IntMap.toList g
    pure $ Node (NodeId node) nodeVal

-- | Get the min and max NodeId values
nodeRange :: IGraph a b -> Maybe (NodeId, NodeId)
nodeRange (IGraph g) =
    (,)
    <$> fmap (NodeId . fst . fst) (IntMap.minViewWithKey g)
    <*> fmap (NodeId . fst . fst) (IntMap.maxViewWithKey g)

-- | Get a unique NodeId value, that can be positively enumerated
-- Ex. let newNodeIds = [newId graph .. ]
newNodeId :: IGraph a b -> NodeId
newNodeId (IGraph g) = fromMaybe (NodeId 1) $
    NodeId . succ . fst . fst <$> IntMap.maxViewWithKey g
