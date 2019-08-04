module Main where

import Github.Crawler
import qualified Data.Text.IO as T

main :: IO ()
main = do
    timelineContent <- crawlTimeline "nh2"
    print timelineContent

