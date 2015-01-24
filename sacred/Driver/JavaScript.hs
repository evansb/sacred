{-# LANGUAGE OverloadedStrings #-}

module Driver.JavaScript where

import  Language.JavaScript.Parser
import  Data.Hashable                   (hash)
import  Data.Text                       (pack)
import  Data.IntervalMap.FingerTree     (point)
import  Types

toPos :: TokenPosn -> SourcePos
toPos (TokenPn _ ln col) = (ln, col)

singletonNode :: String -> Maybe SourcePos -> STree
singletonNode s p = SNode (hash s) [SLeaf (pack s) p]

toTree :: JSNode -> STree
toTree n = case n of
      NN s -> toTree' s Nothing
      NT s p _ -> toTree' s (Just (toPos p))
      where
        toTree' :: Node -> Maybe SourcePos -> STree
        toTree' n p = case n of
            JSIdentifier s -> singletonNode s p
            JSDecimal s -> singletonNode s p
            JSLiteral s -> singletonNode s p
            JSHexInteger s -> singletonNode s p
            JSOctal s -> singletonNode s p
            JSStringLiteral _ s -> singletonNode s p
            JSRegEx s -> singletonNode s p
            JSArguments _ s _ -> SNode 0 (map toTree s)
