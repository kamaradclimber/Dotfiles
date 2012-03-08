module Main where

-- {{{ Imports
import Imm.Core
import Imm.Types
import Imm.Config
import System.FilePath
-- }}}

main :: IO ()
main = imm myFeeds myParameters

myParameters :: Parameters
myParameters = defaultGlobalSettings 


myFeeds :: [FeedGroup]
myFeeds = [ (fiveStars , [
    "http://www.archlinux.org/feeds/news/",
    "http://www.cyrille-borne.com/index.php?feed/atom"])  ]

fiveStars :: FeedSettings
fiveStars = FeedSettings {
    mMailDirectory = \refDirs -> (mHome refDirs) </> "Mail/Gmail/rss"}
