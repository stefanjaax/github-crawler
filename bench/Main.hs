import           Criterion
import           Criterion.Main
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup
import qualified Data.Text.IO as T

main :: IO ()
main =
  defaultMain
  [ env (T.readFile "bench/example.html") $ \html ->
      bgroup "Parsing"
        [ bench "example.html"  $ nf (show . TagSoup.parseTagsOptions TagSoup.parseOptions) html
        , bench "example.html - fast"  $ nf (show . TagSoup.parseTagsOptions TagSoup.parseOptionsFast) html
        ]
  ]