{-# LANGUAGE OverloadedStrings #-}

module Driver.JavaScript where

import  Language.JavaScript.Parser      hiding (parse)
import  Data.Hashable                   (hash, hashWithSalt)
import  Data.Unique                     (newUnique, hashUnique)
import  Data.Text                       (Text, pack, unpack)
import  Data.IntervalMap.FingerTree     (point)
import  System.IO.Unsafe
import  Types

instance LangDriver JSNode where
        parseSource = readJs . unpack
        toGenTree = toTree . removeEof

type NodeType = JSNode

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
            JSArguments _ s _ -> withChildren (map toTree s)
            JSArrayLiteral _ s _ -> withChildren (map toTree s)
            JSBlock _ s _ -> withChildren (map toTree s)
            JSBreak _ s _ -> withChildren (map toTree s)
            JSCallExpression _ _ s _ -> withChildren (map toTree s)
            JSCase _ e _ s -> withChildren [toTree e, toMultiTree s]
            -- JSCatch (later, confused)
            JSContinue _ s _ -> withChildren (map toTree s)
            JSDefault _ _ s -> withChildren (map toTree s)
            JSDoWhile _ e _ _ f _ _ -> withChildren [toTree e, toTree f]
            JSElision _ -> SEmpty
            JSExpression s -> withChildren (map toTree s)
            JSExpressionBinary _ s o t ->
                withChildren [toMultiTree s,
                              toTree o, toMultiTree t]
            JSExpressionParen _ e _ -> toTree e
            JSExpressionPostfix _ s o ->
                withChildren [toMultiTree s, toTree o]
            JSExpressionTernary c _ t _ f ->
                withChildren [toMultiTree c, toMultiTree t, toMultiTree f]
            JSFinally _ b -> toTree b
            JSFor _ _ e _ f _ g _ h ->
                withChildren [toMultiTree e, toMultiTree f, toMultiTree g,
                              toTree h]
            JSForIn _ _ e _ f _ g ->
                withChildren [toMultiTree e, toTree f,
                              toTree g]
            JSForVar _ _ _ e _ f _ g _ h ->
                withChildren [toMultiTree e, toMultiTree f, toMultiTree g,
                              toTree h]
            JSForVarIn _ _ _ e _ f _ g ->
                withChildren $ map toTree [e, f, g]
            JSFunction _ e _ f _ g ->
                withChildren [toTree e, toMultiTree f, toTree g]
            JSFunctionExpression _ e _ f _ g ->
                withChildren [toMultiTree e, toMultiTree f, toTree g]
            JSIf _ _ e _ f g ->
                withChildren [toTree e, toMultiTree f, toMultiTree g]
            JSLabelled e _ f ->
                withChildren [toTree e, toTree f]
            JSMemberDot e _ f ->
                withChildren [toMultiTree e, toTree f]
            JSMemberSquare e _ f _ ->
                withChildren [toMultiTree e, toTree f]
            JSObjectLiteral _ e _ ->
                withChildren (map toTree e)
            JSOperator e -> toTree e
            JSPropertyAccessor e f _ g _ h ->
                withChildren [toTree e, toTree f, toMultiTree g, toTree h]
            JSPropertyNameandValue e _ f ->
                withChildren [toTree e, toMultiTree f]
            JSReturn _ f _ ->
                withChildren (map toTree f)
            JSSourceElementsTop e ->
                toMultiTree e
            JSSwitch _ _ e _ f ->
                withChildren [toTree e, toTree f]
            JSThrow _ e -> withChildren [toTree e]
            JSTry _ e f ->
                withChildren [toTree e, toMultiTree f]
            JSUnary _ e ->
                toTree e
            JSVarDecl e f ->
                withChildren [toTree e, toMultiTree f]
            JSVariables _ e _ ->
                toMultiTree e
            JSWhile _ _ e _ f ->
                withChildren [toTree e, toTree f]
            JSWith _ _ e _ f ->
                withChildren [toTree e, toMultiTree f]

