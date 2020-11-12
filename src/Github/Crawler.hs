{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Github.Crawler ( crawlTimeline
                      ) where

import           Control.Applicative ((<|>), optional)
import           Control.Concurrent (threadDelay)
import           Control.Monad (guard)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Maybe (isJust, catMaybes)
import           Data.Text (Text)
import           Debug.Trace (trace)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Text.HTML.Scalpel
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as T

type Username = String
type RepoURL = T.Text


getConfig :: IO (Config Text)
getConfig = do
  m <- newTlsManager
  return $ Config { decoder = defaultDecoder
                  , manager = Just m
                  }


extractRepo :: RepoURL -> Maybe Text
extractRepo url = case take 3 $ T.splitOn "/" url of
  ["", user, repo] -> Just $ T.concat ["/", user, "/", repo]
  _                -> Nothing

  

uniqueRepos :: [RepoURL] -> [T.Text]
uniqueRepos = nubOrd . catMaybes . map extractRepo 

crawlTimeline :: Username -> IO (Either Text [RepoURL])
crawlTimeline userName = do
  cfg <- getConfig
  fmap uniqueRepos <$> crawlTimelineURL "https://github.com/nh2?tab=overview&from=2019-07-01&to=2019-07-31" cfg
--crawlTimelineURL ("https://github.com/" ++ userName)


crawlTimelineURL :: URL -> Config Text -> IO (Either Text [RepoURL])
crawlTimelineURL url cfg = do
  next <- scrapeURLWithConfig cfg url scrapeTimeLineContent
  case next of
    Just (timelineContent, Nothing) -> return $ Right timelineContent
    Just (timelineContent, Just nextURL) -> do
      print (timelineContent, nextURL)
      threadDelay 50000 --- 1 second because of rate limit
      eOlderContents <- crawlTimelineURL nextURL cfg
      return $ case eOlderContents of
        Left err -> Left err
        Right olderContents -> Right $  timelineContent ++ olderContents
    Nothing -> return $ Left $ T.pack $ "Could not scrape at URL " ++ url


scrapeRepositoryURLs :: Scraper Text [RepoURL]
scrapeRepositoryURLs = attrs "href" $
    "a" @:
    [ 
    ]


scrapeTimeLineContent :: Scraper Text ([RepoURL], Maybe URL)
scrapeTimeLineContent = do
  let doesButtonExist = do
        mbText <- optional $ text $ "button" @: [hasClass "ajax-pagination-btn"]
        return $ isJust mbText
  let getAction = do
        b <- doesButtonExist
        if b then
          (Just . T.unpack) <$> (attr "action" ("form"  @: [hasClass "ajax-pagination-form"]))
        else
          return Nothing
  contents <- concat <$> chroots ("div" @: [hasClass "TimelineItem"]) scrapeRepositoryURLs
  url <- fmap ("https://github.com" ++) <$> getAction
  return (contents, url)











