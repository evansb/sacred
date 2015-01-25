{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import  Data.Aeson                      (FromJSON, ToJSON)
import  Data.Text                       (Text)
import  GHC.Generics                    (Generic)

type Hash = Int
type ID = String

type SourcePos = (Int, Int)
type SourceRange = (SourcePos, SourcePos)
type Code = Text

data STree = SEmpty
           | SLeaf Code SourcePos
           | SNode Hash SourceRange [STree]
           deriving (Show, Eq, Generic)

instance ToJSON STree
instance FromJSON STree

data CommentReq = CommentReq {
    _range   :: SourceRange,
    _content :: Text
} deriving (Show, Eq, Generic)

instance ToJSON CommentReq
instance FromJSON CommentReq

data CommentRes = CommentRes {
    _chash :: Hash,
    _creqcontent :: Text
} deriving (Show, Eq, Generic)

instance ToJSON CommentRes
instance FromJSON CommentRes

data ReviewReq = ReviewReq {
    _rrcode :: Code,
    _rrcomments :: [CommentReq]
} deriving (Show, Eq, Generic)

instance ToJSON ReviewReq
instance FromJSON ReviewReq

data AnalysisReq = AnalysisReq {
    _aroldcode :: Code,
    _arnewcode :: Code,
    _arcomment :: [CommentRes]
} deriving (Show, Eq, Generic)

instance ToJSON AnalysisReq
instance FromJSON AnalysisReq

data DiffType = Add Hash Hash
              | Change Hash Hash Hash
              | Remove Hash Hash
              deriving (Show, Eq, Generic)

instance ToJSON DiffType
instance FromJSON DiffType

class LangDriver a where
    parseSource :: Code -> a
    toGenTree :: a -> STree
