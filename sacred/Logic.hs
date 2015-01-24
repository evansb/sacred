
module Logic where

import qualified Data.Sequence as S
import Prelude hiding (foldl1)
import Data.Hashable
import Data.Foldable
import Types
import qualified Driver.JavaScript as JS

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

diffJS :: Code -> Code -> [DiffType]
diffJS old new = case (oldTree, newTree) of
            (SEmpty, SEmpty) -> []
            (SEmpty, SNode h _ _) -> [Add 0 h]
            (SNode h _ _, _) -> toList $ diff h oldTree newTree
            where oldTree = toGenTree (parseSource old :: JS.NodeType)
                  newTree = toGenTree (parseSource new :: JS.NodeType)

