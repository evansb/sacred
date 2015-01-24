{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import  Data.Aeson                      (FromJSON, ToJSON)
import  Data.IntervalMap.FingerTree     (Interval)
import  Data.Text                       (Text)
import  GHC.Generics                    (Generic)

type Hash = Int
type ID = String

type SourcePos = (Int, Int)
type Code = Text

data STree = SLeaf { _leafContent :: Code, _sourcePos :: SourcePos }
           | SNode { _hash :: Hash, _children :: [STree] }
           deriving (Show, Eq, Generic)

instance ToJSON STree
instance FromJSON STree

data CommentReq = CommentReq {
    _range   :: (SourcePos, SourcePos),
    _content :: Text
} deriving (Show, Eq, Generic)

instance ToJSON CommentReq
instance FromJSON CommentReq

data ReviewReq = ReviewReq {
    _rrcode :: Code,
    _rrcomments :: [CommentReq]
} deriving (Show, Eq, Generic)

instance ToJSON ReviewReq
instance FromJSON ReviewReq

data AnalysisReq = AnalysisReq {
    _aroldcode :: Code,
    _arnewcode :: Code
} deriving (Show, Eq, Generic)

instance ToJSON AnalysisReq
instance FromJSON AnalysisReq

data DiffType = Add Code | Change Code Code | Remove Code | NoChange
    deriving (Show, Eq, Generic)

instance ToJSON DiffType
instance FromJSON DiffType

class LangDriver a where
    parseSource :: Code -> a
    toGenTree :: a -> STree
