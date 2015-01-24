
module Logic where

import qualified Data.Sequence as S
import Data.Hashable
import Types

hashOf :: STree -> Hash
hashOf (SLeaf txt _) = hash txt
hashOf (SNode h _) = h

getPos :: STree -> (SourcePos, SourcePos)
getPos (SLeaf _ p) = (p, p)
getPos (SNode _ ls) = (hb, te)
        where (hb, _) = getPos (head ls)
              (_, te) = getPos (last ls)

addDiffList toCode c = S.fromList $ map (\x -> (Add (toCode x), hashOf x)) c

diff :: (STree -> Code) -> STree -> STree -> S.Seq (DiffType, Hash)
diff toCode (SLeaf txt1 _) (SLeaf txt2 _)
    | txt1 == txt2 = S.singleton (NoChange, hash txt1)
    | otherwise    = S.singleton (Change txt1 txt2, hash txt1)

diff toCode (SNode h1 c1) (SNode h2 c2)
    | h1 == h2 = S.singleton (NoChange, h1)
    | otherwise = diffList c1 c2
        where
            l1 = length c1
            l2 = length c2
            diffList c1 c2
                | l1 <= l2 =
                    let (c2', c2r) = splitAt l1 c2 in
                    let zipped = foldl1 (S.><) (zipWith (diff toCode) c1 c2') in
                    zipped S.>< addDiffList toCode c2r
                | otherwise = diffList c2 c1 -- Symmetry

diff toCode (SLeaf txt1 _) (SNode _ c)
    = (Remove txt1, hash txt1) S.<| addDiffList toCode c
diff toCode bef@(SNode h c) (SLeaf txt1 _)
    = S.fromList [(Remove (toCode bef), h), (Add txt1, hash txt1)]
