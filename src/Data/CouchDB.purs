module Data.CouchDB where

import Prelude

-- TODO: Make imports more specific
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Undefined
import Data.Maybe
import Test.QuickCheck.Arbitrary

newtype Change = Change { rev :: String }

newtype Result = Result { seq :: Int, id :: String, deleted :: Maybe Boolean, changes :: Array Change }

newtype Notification = Notification { last_seq :: Int, results :: Array Result }

exampleResult = Result  { seq: 1
                        , id: "5c98f21de6f21c6bfb8bfc55cb00023e"
                        , deleted: Just true
                        , changes: [ Change { rev: "2-967a00dff5e02add41819138abb3284d" } ] }

exampleNotification = Notification { last_seq: 1, results: [ exampleResult ] }

instance eqChange :: Eq Change where
  eq (Change o1) (Change o2) = (o1.rev `eq` o2.rev)

instance eqResult :: Eq Result where
  eq (Result o1) (Result o2) = (o1.seq `eq` o2.seq) `conj` (o1.id `eq` o2.id)
                                                    `conj` (o1.deleted `eq` o2.deleted)
                                                    `conj` (o1.changes `eq` o2.changes)

instance eqNotification :: Eq Notification where
  eq (Notification o1) (Notification o2) = (o1.last_seq `eq` o2.last_seq) `conj` (o1.results `eq` o2.results)

instance showChange :: Show Change where
  show (Change o) = "Change " ++ "{ rev: " ++ o.rev ++ " }"

instance showResult :: Show Result where
  show (Result o) = "Result " ++ "{ seq: " ++ show o.seq ++ ", id: " ++ show o.id ++ ", deleted: " ++ show o.deleted ++ ", changes: " ++ show o.changes ++ " }"

instance showNotification :: Show Notification where
  show (Notification o) = "Notification " ++ "{ last_seq: " ++ show o.last_seq ++ ", results: " ++ show o.results ++ " }"

instance arbChange :: Arbitrary Change where
  arbitrary = Change <$> ({ rev: _ } <$> arbitrary)

instance arbResult :: Arbitrary Result where
  arbitrary = Result <$> ({ seq: _, id: _, deleted: _, changes: _ } <$> arbitrary
                                                                    <*> arbitrary
                                                                    <*> arbitrary
                                                                    <*> arbitrary)

instance arbNotification :: Arbitrary Notification where
  arbitrary = Notification <$> ({ last_seq: _, results: _ } <$> arbitrary
                                                            <*> arbitrary)

changesUrl :: String -> String -> Int -> String
changesUrl couchDBURL dBName sinceSeq = couchDBURL ++ "/" ++ dBName ++ query
  where
  query = "/_changes?feed=longpoll&since=" ++ show sinceSeq
