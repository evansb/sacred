{-# LANGUAGE OverloadedStrings #-}

module Driver.JavaScript where

import  Language.JavaScript.Parser
import  Data.Text                       (pack)
import  Data.IntervalMap.FingerTree     (point)
import  Types

toPos :: TokenPosn -> SourcePos
toPos (TokenPn _ ln col) = (ln, col)

singletonNode :: STree -> STree
singletonNode (SLeaf s p) = undefined
singletonNode other = other

toTree :: JSNode -> STree
toTree n = case n of
      NN s -> toTree' s Nothing
      NT s p _ -> toTree' s (Just (toPos p))
      where
        toTree' :: Node -> Maybe SourcePos -> STree
        toTree' n p = case n of
            JSIdentifier s          -> SLeaf (pack s) p
            JSDecimal s             -> SLeaf (pack s) p
            JSLiteral s             -> SLeaf (pack s) p
            JSHexInteger s	        -> SLeaf (pack s) p
            JSOctal s               -> SLeaf (pack s) p
            JSStringLiteral _ s     -> SLeaf (pack s) p
            JSRegEx s               -> SLeaf (pack s) p
            JSArguments _ s _       -> SNode 0 (map toTree s)
