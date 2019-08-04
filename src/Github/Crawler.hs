{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

data TimelinePullRequestContent =
  TimelinePullRequestContent
    { repository :: Text
    } deriving (Eq, Ord, Show)

data TimelineIssueContent =
  TimelineIssueContent
    { repository :: Text
    } deriving (Eq, Ord, Show)

data TimelineContent
  = TimelineContentIssue TimelineIssueContent
  | TimelineContentPullRequest TimelinePullRequestContent
  deriving (Eq, Ord, Show)

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


scrapeRepository :: Scraper Text Text
scrapeRepository = attr "data-hovercard-url" $
    "span" @:
    [ hasClass "css-truncate"
    , "data-hovercard-type" @= "repository"
    ]


scrapeTimelinePullRequestContent :: Scraper Text TimelinePullRequestContent
scrapeTimelinePullRequestContent = do
  repository <- scrapeRepository
  return $ TimelinePullRequestContent
    { repository = repository
    }


scrapeTimelineIssueContent :: Scraper Text TimelineIssueContent
scrapeTimelineIssueContent = do
  repository <- scrapeRepository
  return TimelineIssueContent
    { repository = repository
    }


scrapePullRequestRollupWrapper :: Scraper Text [TimelineContent]
scrapePullRequestRollupWrapper = do
  matches $ "svg" @: [hasClass "octicon-git-pull-request"]
  chroots ("div" @: [hasClass "profile-rollup-summarized"]) $
    TimelineContentPullRequest <$> scrapeTimelinePullRequestContent


scrapeTimelineWrapper :: Scraper Text [TimelineContent]
scrapeTimelineWrapper = scrapePullRequestRollupWrapper


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
  url <- fmap ("https://github.com" ++) <$> getAction
  contents <- concat <$> chroots ("div" @: [hasClass "profile-rollup-wrapper"]) scrapeTimelineWrapper
  return (contents, url)











