{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import  Data.Aeson                      (FromJSON, ToJSON)
import  Data.IntervalMap.FingerTree     (Interval)
import  Data.Text                       (Text)
import  GHC.Generics                    (Generic)

type Hash = Integer
type ID = String

type SourcePos = (Int, Int)

data STree = SLeaf { _leafContent :: Text, _sourcePos :: (Maybe SourcePos) }
           | SNode { _hash :: Hash, _children :: [STree] }
           deriving (Show, Eq, Generic)

instance ToJSON STree
instance FromJSON STree

data Code = Code { _raw :: Text, lang :: String }
           deriving (Show, Eq, Generic)

instance ToJSON Code
instance FromJSON Code

data Comment = Comment { _id :: ID, _parent :: ID, _attached :: [Hash] }
             deriving (Show, Eq, Generic)

instance ToJSON Comment
instance FromJSON Comment
