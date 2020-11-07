module Main where

import Github.Crawler
import qualified Data.Text.IO as T
import Data.Containers.ListUtils (nubOrd)

main :: IO ()
main = do
    timelineContent <- crawlTimeline "nh2"
    case timelineContent of
    	Right content -> print $ nubOrd content
    	Left _ -> print timelineContent

