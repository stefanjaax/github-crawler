{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Github.Crawler ( TimelineContent(..)
                      , crawlTimeline
                      ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Text.HTML.Scalpel
import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Debug.Trace (trace)
import           Network.HTTP.Client.TLS (newTlsManager)
type Username = String

data TimelineContent =
  TimelineContent
    { link :: Text
    , action :: Text
    } deriving (Eq, Ord, Show)


getConfig :: IO (Config Text)
getConfig = do
  m <- newTlsManager
  return $ Config { decoder = defaultDecoder
                  , manager = Just m
                  }

crawlTimeline :: Username -> IO (Either Text [TimelineContent])
crawlTimeline userName = do
  cfg <- getConfig
  crawlTimelineURL "https://github.com/nh2?tab=overview&from=2019-07-01&to=2019-07-31" cfg
--crawlTimelineURL ("https://github.com/" ++ userName)


crawlTimelineURL :: URL -> Config Text -> IO (Either Text [TimelineContent])
crawlTimelineURL url cfg = do
  next <- scrapeURLWithConfig cfg url scrapeTimeLineContent
  print next
  case next of
    Just (timelineContent, Nothing) -> return $ Right timelineContent
    Just (timelineContent, Just nextURL) -> do
      eOlderContents <- crawlTimelineURL nextURL cfg
      return $ case eOlderContents of
        Left err -> Left err
        Right olderContents -> Right $  timelineContent ++ olderContents
    Nothing -> return $ Left $ T.pack $ "Could not scrape at URL " ++ url


scrapeTimeLineContent :: Scraper Text ([TimelineContent], Maybe URL)
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
  let getContentEntry = do
        link <- attr "data-hovercard-type" $ "span" @: [hasClass "d-inline-block"]
        action <- attr "data-hovercard-type" $ "span" @: [hasClass "d-inline-block"]
        return $ TimelineContent { link = link
                                 , action = action
                                 }
  url <- fmap ("https://github.com" ++) <$> getAction
  contents <- chroots ("div" @: [hasClass "profile-rollup-summarized"]) getContentEntry
  return (contents, url)











