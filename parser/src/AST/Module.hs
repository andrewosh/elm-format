{-# LANGUAGE OverloadedStrings #-}

module AST.Module
    ( Module(..), Header(..)
    , UserImport(..), ImportMethod(..)
    ) where

import qualified AST.Declaration as Declaration
import qualified AST.Module.Name as Name
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Aeson ( (.=) )
import AST.V0_16


-- MODULES


data Module = Module
    { initialComments :: Comments
    , header :: Header
    , body :: [Declaration.Decl]
    }
    deriving (Eq, Show)


instance A.Strippable Module where
  stripRegion m =
    Module
    { initialComments = initialComments m
    , header =
        Header
          { name = name $ header m
          , docs = A.stripRegion $ docs $ header m
          , exports = exports $ header m
          , postExportComments = postExportComments $ header m
          , imports = imports $ header m
          }
    , body = body m
    }

packString :: (Show a) => a -> T.Text
packString s =  T.pack (show s)

instance JSON.ToJSON Module where
    toJSON (Module comments header body) = 
        JSON.object ["body" .= body] 
        {-|
        JSON.object ["comments" .= packString comments,
                     "header" .= header,
                     "body" .= body
                    ]
        -}


-- HEADERS

{-| Basic info needed to identify modules and determine dependencies. -}
data Header = Header
    { name :: Commented Name.Raw
    , docs :: A.Located (Maybe String)
    , exports :: Var.Listing Var.Value
    , postExportComments :: Comments
    , imports :: [UserImport]
    }
    deriving (Eq, Show)

instance JSON.ToJSON Header where
    toJSON (Header name docs exports postExportComments imports) =
        JSON.object [ "name" .= packString name,
                      "docs" .= packString docs,
                      "exports" .= packString exports,
                      "postExportComments" .= packString postExportComments,
                      "imports" .= packString imports
                    ]


-- IMPORTs

data UserImport
    = UserImport (A.Located (PreCommented Name.Raw, ImportMethod))
    | ImportComment Comment
    deriving (Eq, Show)


data ImportMethod = ImportMethod
    { alias :: Maybe (Comments, PreCommented String)
    , exposedVars :: (Comments, PreCommented (Var.Listing Var.Value))
    }
    deriving (Eq, Show)
