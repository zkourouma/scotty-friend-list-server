{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( eitherDecode )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Either                    ( fromRight )
import           Web.Scotty                     ( ActionM
                                                , scotty
                                                , param
                                                , get
                                                , json
                                                , rescue
                                                , next
                                                )

import           Friend                         ( Friend
                                                , findFriend
                                                )
import           Paths_scotty_friend_list_server

file :: IO FilePath
file = getDataFileName "data/friends.json"

friends :: IO [Friend]
friends = do
  contents <- LBS.readFile =<< file
  let fs = eitherDecode contents
  return $ fromRight [] fs

main :: IO ()
main = scotty 8000 $ do
  get "/" queryHandler
  get "/" topHandler

queryHandler :: ActionM ()
queryHandler = do
  q  <- param "q" `rescue` const next
  fs <- liftIO friends
  json $ findFriend q fs

topHandler :: ActionM ()
topHandler = do
  fs <- liftIO friends
  json fs
