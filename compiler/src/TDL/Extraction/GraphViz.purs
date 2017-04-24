module TDL.Extraction.GraphViz
  ( digraph
  , digraphStr
  ) where

import Data.Array (catMaybes, fromFoldable, length) -- TODO probably want to use List instead of Array :P
import Data.Foldable (foldMap, intercalate)
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Prelude
import Partial.Unsafe (unsafeCrashWith)
import TDL.Syntax (Declaration(..), Kind(..), Module(..), PrimType(..), Type(..))
import TDL.Extraction.GraphVizDoc as GV

digraphStr :: Module -> String
digraphStr = digraphStr' <<< digraph

-- http://www.graphviz.org/doc/info/shapes.html#record
digraphStr' :: GV.DigraphRec -> String
digraphStr' { nodes, edges } =
  "digraph {\n  node [shape=plaintext];\n  " <> lines <> "\n}"
  where
    lines = intercalate ";\n  " <<< map gvNodeStr $ nodes
      where
        gvNodeStr (GV.Node name (GV.TextNode str))        = quoteNode name
        gvNodeStr (GV.Node name (GV.RecordNode {fields})) = quoteNode name <> " [label=<\n" <> tableHtml name fields  <> "\n>]"

        quoteNode str = "\"" <> str <> "\""

tableHtml :: String -> Array GV.RecordFieldRec -> String
tableHtml name fields =
  "<table>" <> head <> rows <> "</table>"
  where
    head = "<tr><td colspan=\"2\"><b>" <> name <> "</b></td></tr>"
    rows = intercalate "\n" <<< map (tr <<< f1) $ fields
    f1 {name, typ} = td name <> td typ
    tr str = "<tr>" <> str <> "</tr>"
    td str = "<td>" <> str <> "</td>"

digraph :: Module -> GV.DigraphRec
digraph (Module name doc decls) =
  { nodes: catMaybes <<< map goDeclaration $ fromFoldable decls
  , edges: []
  }

goDeclaration :: Declaration -> Maybe GV.Node
goDeclaration (TypeDeclaration n _ k t) =
  case t of
    SumType [] -> Nothing
    SumType ts -> Just $ GV.Node n (node t)
    _          -> Just $ GV.Node n (node t)
goDeclaration (ServiceDeclaration n _ f t) = Nothing


node :: Type -> GV.NodeContent
node (PrimType t)         = GV.TextNode $ primLabel t
node (NamedType n)        = GV.TextNode $ "named type: " <> n
node (AppliedType t u)    = GV.TextNode $ "(AppliedType: " <> nodeLabel t <> " *** " <> nodeLabel u <> ")"
node (ProductType ts)     = GV.RecordNode { fields: map (\(name /\ t) -> { name, typ: nodeLabel t }) ts }
node (SumType [])         = GV.TextNode "Void"
node (SumType ts)         = GV.RecordNode { fields: map (\(name /\ t) -> { name, typ: nodeLabel t }) ts }

nodeLabel :: Type -> String
nodeLabel (NamedType n)     = n
nodeLabel (AppliedType t u) = "(" <> nodeLabel t <> " " <> nodeLabel u <> ")"
nodeLabel (PrimType t)      = primLabel t
nodeLabel (ProductType ts)  = "{" <> String.joinWith ", " entries <> "}" where entries = map (\(k /\ t) -> k <> " :: " <> nodeLabel t) ts
nodeLabel (SumType [])      = "TDLSUPPORT.Void"
nodeLabel (SumType _)       = unsafeCrashWith "nodeLabel: SumType _"

primLabel :: PrimType -> String
primLabel BoolType  = "Boolean"
primLabel I32Type   = "Int"
primLabel F64Type   = "Number"
primLabel TextType  = "String"
primLabel ArrayType = "Array"
primLabel BytesType = "Bytes"
