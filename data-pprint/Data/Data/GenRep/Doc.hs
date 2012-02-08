-- |Conversion from 'GenericData' to 'Doc'
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Data.Data.GenRep.Doc
    ( Doc
    , toDoc
    ) where

import Data.Data.GenRep
import Data.Char (ord, showLitChar)
import Data.String

import Text.PrettyPrint.HughesPJ
import Data.List (intersperse)

----------------

-- |'IsString' instance for 'Doc'
instance IsString Doc where fromString = text

-------------------------

-- |Show a character literal. Unicode characters are not escaped.
showLitCharInChar :: Char -> String
showLitCharInChar c | ord c >= 161 = [c]
showLitCharInChar c = showLitChar c ""

-- |Show a character in a string literal. Unicode characters are not escaped.
showLitCharInString :: Char -> String
showLitCharInString '\''  = "'"
showLitCharInString '"'   = "\\\""
showLitCharInString c     = showLitCharInChar c 

----------------------------------------------

-- |Convert 'GenericData' to 'Doc'.
toDoc :: GenericData -> Doc
toDoc {-text (<+>) fsep punctuate comma quotes doubleQuotes brackets parens -} 
    = showsP 0 
  where
    showsP j x = case x of
        Hole           -> "…"       -- !!! ragadás
        ListHole       -> "……"
        Timeout _      -> "⊥"
        NestedError e  -> "⊥(" <+> toDoc e <+> ")"
        Error e        -> text e
        Detail s       -> showParen_ (j > 10) $ "……" <+> showsP 0 s <+> "……"
        Constructor (Char c) []         -> quotes $ text $ showLitCharInChar c
        Constructor Nil []              -> "[]"
        Constructor (Prefix f) []       -> text f
        Constructor (Infix i f)  [a,b]  -> showParen_ (j > i) $ showsP (i+1) a <+> text f <+> showsP (i+1) b
        Constructor (Infixr i f) [a,b]  -> showParen_ (j > i) $ showsP (i+1) a <+> text f <+> showsP i b
        Constructor (Infixl i f) [a,b]  -> showParen_ (j > i) $ showsP i a <+> text f <+> showsP (i+1) b
        Constructor (Tuple _) xs        -> showParen_ True $ list $ map (showsP 0) xs
        Constructor Cons [_,_]          -> fsep $ intersperse "++" $ elems x -- showListEnd "[]" "\"" "[" s
        Constructor (Prefix f) l        -> showParen_ (j > 10) $ text f <+> fsep (map (showsP 11) l)
        _                               -> error $ "showsP: " ++ show x

    showParen_ True  = parens
    showParen_ False = id

    list = fsep . punctuate comma

    collectChars (Constructor Cons [Constructor (Char c) [],b])
        | (cs, x) <- collectChars b
        = (c: cs, x)
    collectChars x = ([], x)

    collectElems x@(Constructor Cons [Constructor (Char _) [], _]) = ([], x)
    collectElems (Constructor Cons [a,b])
        | (cs, x) <- collectElems b
        = (a: cs, x)
    collectElems (Detail b) 
        | (cs, x) <- collectElems b
        = (ListHole: cs, x)
    collectElems Hole 
        = ([ListHole], Constructor Nil [])
    collectElems x = ([], x)

    elems x
        | (es@(_:_), y) <- collectChars x
        = doubleQuotes (text $ concatMap showLitCharInString es): elems y
        | (es@(_:_), y) <- collectElems x
        = (brackets . list . map (showsP 0) $ es): elems y
    elems (Constructor Nil []) = []
    elems (Detail x) = ["...", showsP 0 x]
    elems x = [showsP 0 x]


