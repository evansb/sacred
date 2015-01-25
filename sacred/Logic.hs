
module Logic where

import qualified Data.Sequence as S
import qualified Driver.JavaScript as JS

import Prelude hiding (foldl1)
import Data.Maybe
import Data.Hashable
import Data.Foldable

import Types

hashOf :: STree -> Hash
hashOf (SLeaf txt _) = hash txt
hashOf (SNode h _ _) = h

addDiffList p c = S.fromList $ map (Add p . hashOf) c

diff :: Hash -> STree -> STree -> S.Seq DiffType
diff _ SEmpty SEmpty = S.empty
diff p SEmpty (SLeaf txt2 _) = S.singleton (Add p (hash txt2))
diff p SEmpty (SNode h _ _) = S.singleton (Add p h)
diff p (SLeaf txt _) SEmpty = S.singleton (Remove p (hash txt))
diff p (SLeaf txt1 _) (SLeaf txt2 _)
    | txt1 == txt2 = S.empty
    | otherwise    = S.singleton (Change p (hash txt1) (hash txt2))
diff p (SNode h _ _) SEmpty = S.singleton (Remove p h)
diff p bef@(SNode h _ c) (SLeaf txt1 _)
    = S.fromList [Remove p h, Add p (hash txt1)]
diff p (SNode h1 _ c1) (SNode h2 _ c2)
    | h1 == h2 = S.empty
    | otherwise = diffList c1 c2
        where
            l1 = length c1
            l2 = length c2
            diffList c1 c2
                | l1 <= l2 =
                    let (c2', c2r) = splitAt l1 c2 in
                    let zipped = foldl1 (S.><) (zipWith (diff p) c1 c2') in
                    zipped S.>< addDiffList p c2r
                | otherwise = diffList c2 c1 -- Symmetry
diff p (SLeaf txt1 _) (SNode _ _ c)
    = Remove p (hash txt1) S.<| addDiffList p c

fitScore :: SourceRange -> STree -> SourcePos
fitScore _ SEmpty = (maxBound::Int, maxBound::Int)
fitScore int1@((rb, cb), (re, ce)) (SNode _ int2@((rb', cb'), (re', ce')) _) =
        if not (int2 `containedIn` int1)
        then (maxBound::Int, maxBound::Int)
        else (re - re' + rb' - rb, ce - ce' + cb' - cb)
        where
            ((rb', cb'), (re', ce')) `containedIn` ((rb, cb), (re, ce))
                | rb < rb' && rb' < re' && re' < re = True
                | rb == rb' && rb' <= re' && re' == re =
                    cb <= cb' && cb' <= ce' && ce' <= ce
                | otherwise = False

bestFit :: SourceRange -> STree -> Maybe Hash
bestFit pos tree = case fitScores pos tree of
                       [] -> Nothing
                       f -> Just (snd (Prelude.minimum f))
    where
        fitScores pos SEmpty      = [(fitScore pos SEmpty, 0)]
        fitScores _ (SLeaf _ _) = []
        fitScores pos t@(SNode h _ c) =
            (fitScore pos t, h) : Prelude.concatMap (fitScores pos) c


diffJS :: Code -> Code -> [DiffType]
diffJS old new = case (oldTree, newTree) of
        (SEmpty, SEmpty) -> []
        (SEmpty, SNode h _ _) -> [Add 0 h]
        (SNode h _ _, _) -> toList $ diff h oldTree newTree
        where oldTree = toGenTree (parseSource old :: JS.Type)
              newTree = toGenTree (parseSource new :: JS.Type)

commentJS :: Code -> CommentReq -> CommentRes
commentJS cde creq = CommentRes (fromMaybe 0 (bestFit range tree)) (_content creq)
        where
            range = _range creq
            tree = toGenTree (parseSource cde :: JS.Type)

respondAnalysis :: AnalysisReq -> [DiffType]
respondAnalysis req = diffJS (_aroldcode req) (_arnewcode req)

respondReview :: ReviewReq -> ReviewRes
respondReview req = map (commentJS code) creqs
        where
              code = _rrcode req
              creqs = _rrcomments req
