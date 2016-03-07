{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Declaration where

import AST.V0_16
import qualified AST.Expression as Expression
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Vector ( fromList )
import Data.Scientific ( scientific )
import Data.Aeson ( (.=) )

packString :: (Show a) => a -> T.Text
packString s =  T.pack (show s)

-- DECLARATIONS

data Declaration
  = Definition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments Expression.Expr
  | TypeAnnotation (Var.Ref, Comments) (Comments, Type)
  | Datatype
      (Commented (String, [(Comments, String)]))
      [Commented (String, [(Comments, Type)])]
      (Comments, (String, [(Comments, Type)]))
  | TypeAlias Comments
      (Commented (String, [(Comments, String)]))
      (Comments, Type)
  | PortAnnotation (Commented String) Comments Type
  | PortDefinition (Commented String) Comments Expression.Expr
  | Fixity Assoc Comments Int Comments Var.Ref
  deriving (Eq, Show)


instance A.Strippable Declaration where
  stripRegion d =
    case d of
      Definition a b c e ->
          Definition (A.stripRegion a) b c (A.stripRegion $ A.map A.stripRegion e)
      _ -> d


instance JSON.ToJSON Declaration where
  toJSON (TypeAnnotation (ref, _) (_, t)) =
      JSON.object ["type-annotation" .= [(packString ref) .= t]]
  toJSON x = JSON.String (packString x)


uncomment :: (Commented a) -> a 
uncomment (Commented _ x _) = x

parseRecordType :: [(Commented String, Commented Type, Bool)] -> [(String, Type, Bool)]
parseRecordType ts = 
  let
      parser = \(s, t, b) -> (uncomment s, uncomment t, b)
  in 
      map parser ts

makeArray list = (JSON.Array . fromList) list

instance JSON.ToJSON TypeConstructor where
  toJSON x = 
    let 
      toJSON' (NamedConstructor s) = [
          "type" .= (JSON.String (packString "type-constructor")),
          "value" .= (JSON.String (packString s))
        ]
      toJSON' (TupleConstructor i) = [
          "type" .= (JSON.String (packString "tuple-constructor")),
          "value" .= (JSON.Number $ scientific (toInteger i) 0)
        ]
    in 
      JSON.object $ toJSON' x

instance JSON.ToJSON Type where
  toJSON x = 
    let 
      toJSON' (UnitType c) = ["type" .= packString "unit"]

      toJSON' (TypeVariable s) = [
        "type" .= packString "type-variable", 
        "value" .= (packString s)
        ]

      toJSON' (TypeConstruction c cts) = [
        "type" .= packString "type-constuction", 
        "value" .= JSON.object [ 
          "constructor" .= c,
          "types" .= (makeArray (map (\(_, t) -> JSON.toJSON t) cts))
          ]
        ]

      toJSON' (TypeParens t) = [
        "type" .= packString "type-parens", 
        "value" .= (JSON.toJSON $ uncomment t)
        ]

      toJSON' (TupleType ts) = [
        "type" .= packString "tuple-type",
        "value" .= (makeArray $ map (\ct -> JSON.toJSON (uncomment ct)) ts)
        ]

      toJSON' (EmptyRecordType _) = ["type" .= packString "empty-record"]

      toJSON' (RecordType ts b) = 
        let
          jsonUncomment s = JSON.String $ packString (uncomment s)
          parser = \(s, t, b) -> makeArray [jsonUncomment s, JSON.toJSON $ uncomment t, JSON.Bool b]
        in 
          ["type" .= packString "record-type", 
              "value" .= JSON.object [
                "record" .= (makeArray $ map parser ts), 
                "bool" .= b
              ]  
          ]

      toJSON' (RecordExtensionType s ts b) = 
        let
          jsonUncomment s = JSON.String $ packString (uncomment s)
          parser = \(s, t, b) -> makeArray [jsonUncomment s, JSON.toJSON $ uncomment t, JSON.Bool b]
        in 
          ["type" .= packString "record-extension-type",
            "value" .= JSON.object [
              "extends" .= uncomment s, 
              "record" .= (makeArray $ map parser ts),
              "bool" .= b 
            ]
          ]

      toJSON' (FunctionType (it, _) ts (_, rt)) = 
        ["type" .= packString "function-type", "value" .=  JSON.object [
           "input" .= it,
           "inner" .= (makeArray $ map (\ct -> JSON.toJSON $ uncomment ct) ts),
           "return" .= rt
          ]
        ]

     in 
       JSON.object $ toJSON' (A.drop x)


-- INFIX STUFF

data Assoc = L | N | R
  deriving (Eq, Show)


assocToString :: Assoc -> String
assocToString assoc =
  case assoc of
    L -> "left"
    N -> "non"
    R -> "right"


-- DECLARATION PHASES


data Decl
  = DocComment String
  | BodyComment Comment
  | Decl (A.Located Declaration)
  deriving (Eq, Show)


instance A.Strippable Decl where
  stripRegion d =
    case d of
      Decl d' ->
          Decl $ A.stripRegion $ A.map A.stripRegion d'
      _ -> d

instance JSON.ToJSON Decl where 
  toJSON (DocComment s) = JSON.String (packString s)
  toJSON (BodyComment s) = JSON.String (packString s)
  toJSON (Decl d) = JSON.toJSON $ A.drop d
