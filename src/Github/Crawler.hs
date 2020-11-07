{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Github.Crawler ( crawlTimeline
                      ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Text.HTML.Scalpel
import           Control.Applicative ((<|>))
import           Control.Concurrent (threadDelay)
import           Control.Monad (guard)
import           Debug.Trace (trace)
import           Network.HTTP.Client.TLS (newTlsManager)
type Username = String

type RepoURL = T.Text


getConfig :: IO (Config Text)
getConfig = do
  m <- newTlsManager
  return $ Config { decoder = defaultDecoder
                  , manager = Just m
                  }

crawlTimeline :: Username -> IO (Either Text [RepoURL])
crawlTimeline userName = do
  cfg <- getConfig
  crawlTimelineURL "https://github.com/nh2?tab=overview&from=2019-07-01&to=2019-07-31" cfg
--crawlTimelineURL ("https://github.com/" ++ userName)


crawlTimelineURL :: URL -> Config Text -> IO (Either Text [RepoURL])
crawlTimelineURL url cfg = do
  next <- scrapeURLWithConfig cfg url scrapeTimeLineContent
  case next of
    Just (timelineContent, Nothing) -> return $ Right timelineContent
    Just (timelineContent, Just nextURL) -> do
      print (timelineContent, nextURL)
      threadDelay 500000 -- 1 second because of rate limit
      eOlderContents <- crawlTimelineURL nextURL cfg
      return $ case eOlderContents of
        Left err -> Left err
        Right olderContents -> Right $  timelineContent ++ olderContents
    Nothing -> return $ Left $ T.pack $ "Could not scrape at URL " ++ url


scrapeRepositoryURLs :: Scraper Text [RepoURL]
scrapeRepositoryURLs = attrs "href" $
    "a" @:
    [ hasClass "link-gray-dark"
    , "data-hovercard-type" @= "repository"
    ]


scrapeTimeLineContent :: Scraper Text ([RepoURL], Maybe URL)
scrapeTimeLineContent = do
  let doesButtonExist = do
        t <- text $ "button" @: [hasClass "ajax-pagination-btn"]
        return $ not $ T.null t
  let getAction = do
        b <- doesButtonExist
        if b then
          (Just . T.unpack) <$> (attr "action" ("form"  @: [hasClass "ajax-pagination-form"]))
        else
          return Nothing
  url <- fmap ("https://github.com" ++) <$> getAction
  contents <- concat <$> chroots ("div" @: [hasClass "TimelineItem"]) scrapeRepositoryURLs
  return (contents, url)











