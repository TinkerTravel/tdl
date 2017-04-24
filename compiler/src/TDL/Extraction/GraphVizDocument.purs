module TDL.Extraction.GraphVizDoc where

type DigraphRec =
  { nodes :: Array Node
  , edges :: Array Edge
  }
  
data Edge = Edge Node Label Node

type Label = String

-- | Named node
data Node = Node String NodeContent

data NodeContent
  = RecordNode RecordNodeRec
  | TextNode String

type RecordNodeRec  = { fields :: Array RecordFieldRec }
type RecordFieldRec = { name :: String, typ :: String }
