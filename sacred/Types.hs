{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import  Data.Aeson                      (FromJSON, ToJSON)
import  Data.IntervalMap.FingerTree     (Interval)
import  Data.Text                       (Text)
import  GHC.Generics                    (Generic)

type Hash = Int
type ID = String

type SourcePos = (Int, Int)

data STree = SLeaf { _leafContent :: Text, _sourcePos :: SourcePos }
           | SNode { _hash :: Hash, _children :: [STree] }
           deriving (Show, Eq, Generic)

instance ToJSON STree
instance FromJSON STree

type Code = Text

data Comment = Comment { _id :: ID, _parent :: ID, _attached :: [Hash] }
             deriving (Show, Eq, Generic)

instance ToJSON Comment
instance FromJSON Comment
