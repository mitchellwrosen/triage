{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import IO (print)
import Network.HTTP.Simple
import Text (encodeUtf8)
import Vector (Vector)

import qualified ByteString
import qualified ByteString.Partial as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified ListT as ListT
import qualified Set as Set

main :: IO ()
main = do
  repos :: Vector Value <-
    githubGetHaskellRepos

  let fullNames :: [Text]
      fullNames =
        take 1 (repos ^.. traverse . key "full_name" . _String)

  for_ fullNames $ \name -> do
    githubGetIssues name
      & ListT.foldM
          (\() issue -> do
            let labels :: Set Text
                labels =
                  issue ^. key "labels"
                         . values
                         . key "name"
                         . _String
                         . to Set.singleton
            print labels)
          (pure ())
          pure

beginnerLabels :: Set Text
beginnerLabels =
  Set.fromList
    [ "" ]

githubGetHaskellRepos :: IO (Vector Value)
githubGetHaskellRepos = do
  request :: Request <-
    parseRequest "https://api.github.com/search/repositories?q=language:haskell"

  response :: Response ByteString <-
    httpBS (request & addRequestHeader "User-Agent" "Haskell")

  pure (getResponseBody response ^?! key "items" . _Array)

githubGetIssues :: Text -> ListT IO Value
githubGetIssues name = do
  request :: Request <-
    liftIO
      (parseRequest
        (Char8.unpack
          ("https://api.github.com/repos/"
            <> encodeUtf8 name
            <> "/issues?per_page=100")))

  response0 :: Response Value <-
    liftIO (httpJSON (request & addRequestHeader "User-Agent" "Haskell"))

  let loop :: Response Value -> ListT IO Value
      loop response =
        headPage <|> tailPages
       where
        headPage :: ListT IO Value
        headPage =
          ListT.select (getResponseBody response0 ^?! _Array)

        tailPages :: ListT IO Value
        tailPages = do
          Just links <-
            pure (lookup "Link" (getResponseHeaders response))

          let f :: [ByteString] -> Maybe ByteString
              f = \case
                [link, " rel=\"next\""] ->
                  link
                    & Char8.dropWhile (/= '<')
                    & ByteString.drop 1
                    & ByteString.init
                    & Just
                _ ->
                  Nothing

          next :: ByteString <-
            Char8.split ',' links
              & mapMaybe (f . Char8.split ';')
              & ListT.select

          liftIO (print next)

          request :: Request <-
            liftIO (parseRequest (Char8.unpack next))

          liftIO (httpJSON (request & addRequestHeader "User-Agent" "Haskell"))
            >>= loop

  loop response0
