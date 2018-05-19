{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}

import Control.Lens ((^.), (^?!), to)
import Data.Aeson.Lens
import File.Text (readFile)
import Json.Encode (Value)
import Network.HTTP.Simple
import Text (decodeUtf8, pack, unpack)

import qualified ListT
import qualified Set
import qualified Text

main :: IO ()
main = do
  beginnerLabels :: Set Text <- do
    bytes :: Text <-
      readFile "etc/beginner-labels.txt"
    pure (Set.fromList (Text.lines bytes))

  notBeginnerLabels :: Set Text <- do
    bytes :: Text <-
      readFile "etc/not-beginner-labels.txt"
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

        putStrLn (title <> " " <> url)

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
      putStrLn ("[Unknown labels] " <> pack (show (Set.toList unknown))))

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
        (unpack
          ("https://api.github.com/repos/" <> name <> "/issues?per_page=100")))

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
  bytes :: Text <-
    decodeUtf8 <$> lookup "Link" (getResponseHeaders response)

  let links :: [Text]
      links =
        Text.split (== ',') bytes

  url:_ :: [Text] <-
    pure (mapMaybe stripNext links)

  parseRequest (unpack url)

 where
  stripNext :: Text -> Maybe Text
  stripNext bytes = do
    prefix :: Text <-
      Text.stripSuffix "rel=\"next\"" bytes

    prefix
      & Text.dropWhile (/= '<')
      & Text.drop 1
      & Text.takeWhile (/= '>')
      & Just
