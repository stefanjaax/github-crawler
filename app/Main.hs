module Main where

import Github.Crawler
import qualified Data.Text.IO as T

main :: IO ()
main = do
    timelineContent <- crawlTimeline "nh2"
    case timelineContent of
        Right content -> print content
        Left _ -> do
            print timelineContent
            print $ length timelineContent

