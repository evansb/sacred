{-# LANGUAGE OverloadedStrings #-}

module Driver.JavaScript where

import  Language.JavaScript.Parser      hiding (parse)
import  Data.Hashable                   (hash, hashWithSalt)
import  Data.Unique                     (newUnique, hashUnique)
import  Data.Text                       (Text, pack, unpack)
import  System.IO.Unsafe
import  Types

instance LangDriver JSNode where
        parseSource =  readJs . unpack
        toGenTree = toTree . removeEof

type Type = JSNode

(+>) :: SourcePos -> Int -> SourcePos
(x, y) +> d = (x, y + d)

toPos :: TokenPosn -> SourcePos
toPos (TokenPn _ ln col) = (ln, col)

singletonNode :: String -> SourcePos -> STree
singletonNode s p = SNode (hash s) (p, p +> length s) [SLeaf (pack s) p]

minMaxPos :: [STree] -> (SourcePos, SourcePos)
minMaxPos s = (minPos, maxPos)
    where
        minPos = case s of
                    SLeaf _ s : _ -> s
                    SNode _ (l, _) _ : _ -> l
                    _ -> error "Wrong match"
        maxPos = case last s of
                     SLeaf _ s -> s
                     SNode _ (_, r) _ -> r
                     _ -> error "Wrong match"

withChildren :: [STree] -> STree
withChildren [] = SEmpty
withChildren children = SNode (combineHashes children)
                              (minMaxPos children) children

combineHashes :: [STree] -> Hash
combineHashes [] = error "combineHashes []"
combineHashes (SNode h _ _ : xs) =
        foldl (\acc x -> case x of
                SNode h _ _ -> hashWithSalt acc h
                SEmpty -> acc
                _ -> error "combineHashes leaf") h xs

toMultiTree :: [JSNode] -> STree
toMultiTree = withChildren . map toTree

removeEof :: JSNode -> JSNode
removeEof (NN (JSSourceElementsTop xs)) =
            NN (JSSourceElementsTop (take (length xs - 1) xs))
removeEof x = x

toTree :: JSNode -> STree
toTree n = case n of
      NN s -> toTree' s (0, 0)
      NT s p _ -> toTree' s (toPos p)
      where
        toTree' n p = case n of
            JSIdentifier s -> singletonNode s p
            JSDecimal s -> singletonNode s p
            JSLiteral s -> singletonNode s p
            JSHexInteger s -> singletonNode s p
            JSOctal s -> singletonNode s p
            JSStringLiteral _ s -> singletonNode s p
            JSRegEx s -> singletonNode s p
            JSArguments l s r ->
                withChildren [toTree l, toMultiTree s, toTree r]
            JSArrayLiteral l s r ->
                withChildren [toTree l, toMultiTree s, toTree r]
            JSBlock l s r ->
                withChildren [toMultiTree l, toMultiTree s, toMultiTree r]
            JSBreak l s r ->
                withChildren [toTree l, toMultiTree s, toTree r]
            JSCallExpression _ l s r ->
                withChildren [toMultiTree l, toMultiTree s, toMultiTree r]
            JSCase c e l s ->
                withChildren [toTree c, toTree e, toTree l, toMultiTree s]
            -- JSCatch (later, confused)
            JSContinue c l s ->
                withChildren [toTree c, toMultiTree l, toTree s]
            JSDefault d l s ->
                withChildren (map toTree s)
            JSDoWhile p e q r f s t ->
                withChildren (map toTree [p,e,q,r,f,s,t])
            JSElision _ -> SEmpty
            JSExpression s -> withChildren (map toTree s)
            JSExpressionBinary _ s o t ->
                withChildren [toMultiTree s, toTree o, toMultiTree t]
            JSExpressionParen l e r -> withChildren (map toTree [l,e,r])
            JSExpressionPostfix _ s o ->
                withChildren [toMultiTree s, toTree o]
            JSExpressionTernary c l t r f ->
                withChildren [toMultiTree c, toTree l, toMultiTree t, toTree r, toMultiTree f]
            JSFinally f b ->
                withChildren [toTree f, toTree b]
            JSFor a b e c f d g z h ->
                withChildren [toTree a, toTree b, toMultiTree e,
                              toTree c, toMultiTree f, toTree d, toMultiTree g,
                              toTree z, toTree h
                              ]
            JSForIn a b e c f d g ->
                withChildren [toTree a, toTree b, toTree c, toMultiTree e, toTree f,
                              toTree d, toTree g]
            JSForVar a b v e l f r g q h ->
                withChildren [
                  toTree a, toTree b, toTree v,
                  toMultiTree e, toTree l, toMultiTree f, toTree r,
                  toMultiTree g, toTree q, toTree h]
            JSForVarIn a b v e l f r g ->
                withChildren $ map toTree [a, b, v, e, l, f, r, g]
            JSFunction s e l f r g ->
                withChildren [toTree s, toTree e, toTree l, toMultiTree f,
                              toTree r, toTree g]
            JSFunctionExpression s e l f r g ->
                withChildren [toTree s, toMultiTree e, toTree l, toMultiTree f,
                              toTree r, toTree g]
            JSIf i l e r f g ->
                withChildren [toTree e, toMultiTree f, toMultiTree g]
            JSLabelled e l f ->
                withChildren [toTree e, toTree l, toTree f]
            JSMemberDot e d f ->
                withChildren [toMultiTree e, toTree d, toTree f]
            JSMemberSquare e l f r ->
                withChildren [toMultiTree e, toTree l, toTree f, toTree r]
            JSObjectLiteral l e r ->
                withChildren [toTree l, toMultiTree e, toTree r]
            JSOperator e -> toTree e
            JSPropertyAccessor e f l g r h ->
                withChildren [toTree e, toTree f, toTree l, toMultiTree g,
                              toTree r, toTree h]
            JSPropertyNameandValue e _ f ->
                withChildren [toTree e, toMultiTree f]
            JSReturn r f s ->
                withChildren [toTree r, toMultiTree f, toTree s]
            JSSourceElementsTop e ->
                toMultiTree e
            JSSwitch s l e r f ->
                withChildren (map toTree [s,l,r,e,f])
            JSThrow t e -> withChildren [toTree t, toTree e]
            JSTry t e f ->
                withChildren [toTree t, toTree e, toMultiTree f]
            JSUnary _ e ->
                toTree e
            JSVarDecl e f ->
                withChildren [toTree e, toMultiTree f]
            JSVariables v e s ->
                withChildren [toTree v, toMultiTree e, toTree s]
            JSWhile w l e r f ->
                withChildren [toTree e, toTree f]
            JSWith w l e r f ->
                withChildren [toTree w, toTree l, toTree e,
                              toTree r, toMultiTree f]

