{-# LANGUAGE OverloadedStrings #-}

module Driver.JavaScript where

import  Language.JavaScript.Parser      hiding (parse)
import  Data.Hashable                   (hash, hashWithSalt)
import  Data.Unique                     (newUnique, hashUnique)
import  Data.Text                       (Text, pack, unpack)
import  Data.IntervalMap.FingerTree     (point)
import  System.IO.Unsafe
import  Types

sourceToTree :: Code -> STree
sourceToTree = toTree . readJs . unpack

toPos :: TokenPosn -> SourcePos
toPos (TokenPn _ ln col) = (ln, col)

singletonNode :: String -> SourcePos -> STree
singletonNode s p = SNode (hash s) [SLeaf (pack s) p]

emptyLeaf :: SourcePos -> STree
emptyLeaf p = SNode (hashUnique (unsafePerformIO newUnique)) [SLeaf "" p]

listNode :: [JSNode] -> SourcePos -> STree
listNode [] p = emptyLeaf p
listNode s  _ =
        let children = map toTree s in
        SNode (combineHashes children) children

fromChildren :: [STree] -> STree
fromChildren children = SNode (combineHashes children) children

combineHashes :: [STree] -> Hash
combineHashes [] = error "combineHashes []"
combineHashes (SNode h _ : xs) =
        foldl (\acc x -> case x of
                SNode h _ -> hashWithSalt acc h
                _ -> error "combineHashes leaf") h xs

toMultiTree :: [JSNode] -> STree
toMultiTree = fromChildren . map toTree

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
            JSArguments _ s _ -> listNode s p
            JSArrayLiteral _ s _ -> listNode s p
            JSBlock _ s _ -> listNode s p
            JSBreak _ s _ -> listNode s p
            JSCallExpression _ _ s _ -> listNode s p
            JSCase _ e _ s -> fromChildren [toTree e, toMultiTree s]
            -- JSCatch (later, confused)
            JSContinue _ s _ -> listNode s p
            JSDefault _ _ s -> listNode s p
            JSDoWhile _ e _ _ f _ _ -> fromChildren [toTree e, toTree f]
            JSElision _ -> emptyLeaf p
            JSExpression s -> listNode s p
            JSExpressionBinary _ s o t ->
                fromChildren [toMultiTree s,
                              toTree o, toMultiTree t]
            JSExpressionParen _ e _ -> toTree e
            JSExpressionPostfix _ s o ->
                fromChildren [toMultiTree s, toTree o]
            JSExpressionTernary c _ t _ f ->
                fromChildren [toMultiTree c,
                              toMultiTree t,
                              toMultiTree f]
            JSFinally _ b -> toTree b
            JSFor _ _ e _ f _ g _ h ->
                fromChildren [toMultiTree e,
                              toMultiTree f,
                              toMultiTree g,
                              toTree h]
            JSForIn _ _ e _ f _ g ->
                fromChildren [toMultiTree e,
                              toTree f,
                              toTree g]
            JSForVar _ _ _ e _ f _ g _ h ->
                fromChildren [toMultiTree e,
                              toMultiTree f,
                              toMultiTree g,
                              toTree h]
            JSForVarIn _ _ _ e _ f _ g ->
                fromChildren $ map toTree [e, f, g]
            JSFunction _ e _ f _ g ->
                fromChildren [toTree e,
                              toMultiTree f,
                              toTree g]
            JSFunctionExpression _ e _ f _ g ->
                fromChildren [toMultiTree e,
                              toMultiTree f,
                              toTree g]
            JSIf _ _ e _ f g ->
                fromChildren [toTree e,
                              toMultiTree f,
                              toMultiTree g]
            JSLabelled e _ f ->
                fromChildren [toTree e,
                              toTree f]
            JSMemberDot e _ f ->
                fromChildren [toMultiTree e,
                              toTree f]
            JSMemberSquare e _ f _ ->
                fromChildren [toMultiTree e,
                              toTree f]
            JSObjectLiteral _ e _ ->
                listNode e p
            JSOperator e -> toTree e
            JSPropertyAccessor e f _ g _ h ->
                fromChildren [toTree e,
                              toTree f,
                              toMultiTree g,
                              toTree h]
            JSPropertyNameandValue e _ f ->
                fromChildren [toTree e, toMultiTree f]
            JSReturn _ f _ ->
                listNode f p
            JSSourceElementsTop e ->
                toMultiTree e
            JSSwitch _ _ e _ f ->
                fromChildren [toTree e, toTree f]
            JSThrow _ e -> fromChildren [toTree e]
            JSTry _ e f ->
                fromChildren [toTree e, toMultiTree f]
            JSUnary _ e ->
                toTree e
            JSVarDecl e f ->
                fromChildren [toTree e, toMultiTree f]
            JSVariables _ e _ ->
                toMultiTree e
            JSWhile _ _ e _ f ->
                fromChildren [toTree e, toTree f]
            JSWith _ _ e _ f ->
                fromChildren [toTree e, toMultiTree f]

