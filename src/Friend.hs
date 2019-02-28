{-# LANGUAGE DeriveGeneric #-}
module Friend
  ( Friend
  , findFriend
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text
                                                , isPrefixOf
                                                )

data Friend = Friend { id :: Int, name  :: Text, nickName :: Text }  deriving (Generic, Show, Eq)
instance FromJSON Friend
instance ToJSON Friend

findFriend :: Text -> [Friend] -> [Friend]
findFriend q = filter filterFriend
 where
  filterFriend :: Friend -> Bool
  filterFriend (Friend _ n n') = (q `isPrefixOf` n) || (q `isPrefixOf` n')
