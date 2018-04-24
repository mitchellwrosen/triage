{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import IO (putStrLn)
import Network.HTTP.Simple
import Text (encodeUtf8)

import qualified ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified ListT as ListT
import qualified Set as Set
import qualified Text

main :: IO ()
main = do
  beginnerLabels :: Set Text <-
    bytes :: Text <-
      Text.readFile "etc/beginner-labels.txt"
    pure (Set.fromList (Text.lines bytes))

  notBeginnerLabels :: Set Text <-
    bytes :: Text <-
      Text.readFile "etc/not-beginner-labels.txt"
    pure (Set.fromList (Text.lines bytes))

  assert (null (Set.intersection beginnerLabels notBeginnerLabels)) (pure ())

  ListT.foldM

    -- Handle each issue by:
    --   - Printing title/url if it contains at least one beginner label
    --   - Accumulating unknown labels (to be categorized later)
    (\acc issue -> do
      let labels :: Set Text
          labels =
            issue ^. key "labels"
                   . values
                   . key "name"
                   . _String
                   . to Set.singleton

      unless (null (Set.intersection labels beginnerLabels)) $ do
        let title :: Text
            title =
              issue ^?! key "title" . _String

        let url :: Text
            url =
              issue ^?! key "url" . _String

        Text.putStrLn (title <> " " <> url)

      let unknownLabels :: Set Text
          unknownLabels =
            labels
              Set.\\ beginnerLabels
              Set.\\ notBeginnerLabels

      pure (acc <> unknownLabels))

    -- Start with the empty set of unknown label
    (pure mempty)

    -- Conclude by printing each unknown label encountered
    (\unknown ->
      putStrLn ("[Unknown labels] " <> show (Set.toList unknown)))

    -- For each repo, for each issue, go
    (do
      repo :: Value <-
        ListT.take 1 githubGetHaskellRepos
      githubGetIssues (repo ^?! key "full_name" . _String))

githubGetHaskellRepos :: ListT IO Value
githubGetHaskellRepos = do
  request0 :: Request <-
    liftIO
      (parseRequest
        "https://api.github.com/search/repositories?q=language:haskell")

  response0 :: Response ByteString <-
    httpBS (request0 & addRequestHeader "User-Agent" "Haskell")

  let loop :: Response ByteString -> ListT IO Value
      loop response =
        headPage <|> tailPages
       where
        headPage :: ListT IO Value
        headPage =
          ListT.select (getResponseBody response ^?! key "items" . _Array)

        tailPages :: ListT IO Value
        tailPages =
          empty

  loop response0

githubGetIssues :: Text -> ListT IO Value
githubGetIssues name = do
  request0 :: Request <-
    liftIO
      (parseRequest
        (Char8.unpack
          ("https://api.github.com/repos/"
            <> encodeUtf8 name
            <> "/issues?per_page=100")))

  response0 :: Response Value <-
    liftIO (httpJSON (request0 & addRequestHeader "User-Agent" "Haskell"))

  let loop :: Response Value -> ListT IO Value
      loop response =
        headPage <|> tailPages
       where
        headPage :: ListT IO Value
        headPage =
          ListT.select (getResponseBody response0 ^?! _Array)

        tailPages :: ListT IO Value
        tailPages = do
          request :: Request <-
            maybe empty pure (getResponseNext response)
          liftIO (httpJSON (request & addRequestHeader "User-Agent" "Haskell"))
            >>= loop

  loop response0

getResponseNext :: Response a -> Maybe Request
getResponseNext response = do
  bytes :: ByteString <-
    lookup "Link" (getResponseHeaders response)

  let links :: [ByteString]
      links =
        Char8.split ',' bytes

  url:_ :: [ByteString] <-
    pure (mapMaybe stripNext links)

  parseRequest (Char8.unpack url)

 where
  stripNext :: ByteString -> Maybe ByteString
  stripNext bytes = do
    prefix :: ByteString <-
      ByteString.stripSuffix "rel=\"next\"" bytes

    prefix
      & Char8.dropWhile (/= '<')
      & ByteString.drop 1
      & Char8.takeWhile (/= '>')
      & Just
