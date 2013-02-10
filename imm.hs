module Main where

-- {{{ Imports
import Imm.Boot
import Imm.Config
import Imm.Types
import Imm.Util 

import System.Directory
import System.Environment.XDG.BaseDir
-- }}}

main :: IO ()
main = imm myConf

mySettings :: CustomSettings
mySettings s = s {
    mMaildir = getHomeDirectory >/> "Mail" >/> "Gmail" >/> "rss"
}

myConf :: FeedList
myConf = zip (repeat mySettings) feeds


 
feeds =  ["http://planet.haskell.org/rss20.xml",
  "http://familleseux.net/public/static/example.rss",
  "http://www.roc14.org/component/ninjarsssyndicator/?feed_id=2",
  "http://about-gnulinux.info/dotclear/index.php?feed/atom",
  "http://abstrusegoose.com/feed",
  "http://alan.petitepomme.net/cwn/cwn.rss", -- cannot parse date
  "http://alaska-kamtchatka.blogspot.com/feeds/posts/default?alt=rss",
  "http://algorythmes.blogspot.com/feeds/posts/default?alt=rss",
  "http://blog.admin-linux.org/category/logiciels-libres/feed", --404
  "http://blog.admin-linux.org/feed",
  "http://blog.andrew.net.au/index.rss",
  "http://blog.chromium.org/feeds/posts/default?alt=rss",
  "http://blog.crimenumerique.fr/feed/",
  "http://blog.lefigaro.fr/flux.php?blog=17", --404?
  "http://blog.lefigaro.fr/flux.php?blog=19", --404?
  "http://blog.lefigaro.fr/flux.php?blog=32",--404?
  "http://blog.nicolargo.com/feed", --404?
  "http://blog.notdot.net/feeds/atom.xml",
  "http://blog.theclimber.be/?feed/category/Informatique/atom",
  "http://blogs.valvesoftware.com/feed/?cat=7",
  "http://blog.xkcd.com/feed/",
  "http://caml.inria.fr/news.fr.rss", --Cannot decode byte '\x20': Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream
  "http://carlchenet.wordpress.com/category/debian-fr/feed/",
  "http://cherryonthe.popnews.com/feed/",
  "http://chromeos-france.fr/category/chrome-os/feed/",
  "http://codeascraft.etsy.com/feed/",
  "http://codingly.com/feed/",
  "http://doneed.net/b/feed/",
  "http://dutherenverseauborddelatable.wordpress.com/feed/",
  "http://engineering.foursquare.com/feed/",
  "http://experiencelinuxienne.blogspot.com/feeds/posts/default?alt=rss",
  "http://feedproxy.google.com/2803",
  "http://feeds.delicious.com/v2/rss/palpitt?count=15",
  "http://feeds.feedburner.com/CartesianClosedComic?format=xml",
  "http://feeds.feedburner.com/GoogleOperatingSystem",
  "http://feeds.feedburner.com/HighScalability",
  "http://feeds.feedburner.com/bouletcorp",
  "http://feeds.feedburner.com/codinghorror/",
  "http://feeds.feedburner.com/debuggable",
  "http://feeds.feedburner.com/gtutor",
  "http://feeds.feedburner.com/labandepasdessinee",
  "http://feeds.feedburner.com/mongotips",
  "http://feeds.feedburner.com/nosql",
  "http://feeds.feedburner.com/siat-mongodb",
  "http://feeds.feedburner.com/xg_blognotes",
  "http://feeds.labnol.org/labnol",
  "http://feeds.wordaligned.org/wordaligned", --cannot parse date
  "http://francoisgeff.com/feed/", --fausse 404, le site existe
  "http://frandigou.blogspot.com/feeds/posts/default?alt=rss",
  "https://github.com/blog.atom",
  "http://gmailblog.blogspot.com/feeds/posts/default?alt=rss",
  "http://google-opensource.blogspot.com/feeds/posts/default?alt=rss",
  "http://googledocs.blogspot.com/feeds/posts/default?alt=rss",
  "http://highlyscalable.wordpress.com/",
  "http://internetactu.blog.lemonde.fr/feed/",
  "http://interstices.info/feed/Rss2.jsp?id=c_13634",
  "http://julien.danjou.info/blog/index.xml",
  "http://kindlewallpapers.tumblr.com/rss",
  "http://lemire.me/blog/feed/",
  "http://linux-attitude.fr/feed",
  "http://linux-attitude.fr/feed/atom",
  "http://linux.leunen.com/?feed=rss2&cat=15",
  "http://linux.leunen.com/?feed=rss2&cat=5",
  "http://linuxfr.org/journaux.atom",
  "http://linuxfr.org/news.atom",
  "http://linuxmanua.blogspot.com/feeds/posts/default",
  "http://makeapowerfulpoint.com/feed/",
  "http://marie-issy-munich.blogspot.com/feeds/posts/default?alt=rss",
  "http://mauvaisgout.net/feed",
  "http://michel-eudes.net/blog/rss.php", --Cannot decode byte '\x6c': Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream
  "http://montmartre-altitude.blogspot.com/feeds/posts/default?alt=rss",
  "http://mrpouit.free.fr/?feed=rss2", -- 404?
  "http://notch.tumblr.com/rss",
  "http://obfuscurity.com/rss2.xml",
  "http://owni.fr/categorie/une/feed/",
  "http://passeurdesciences.blog.lemonde.fr/feed/",
  "http://philippe.scoffoni.net/feed/", --Cannot decode byte '\x8b': Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream
  "http://planet-fr.debian.net/rss20.xml",
  "http://planet.archlinux.org/atom.xml",
  "http://planet.auto-hebergement.fr/feed.php?type=rss",
  "http://planet.mongodb.org/recent.atom",
  "http://playtime.blog.lemonde.fr/feed/",
  "http://preshing.com/feed",
  "http://qntm.org/rss.php",
  "http://rats-bleus.blogspot.com/feeds/posts/default?alt=rss",
  "http://reyt.net/feed/", -- 404?
  "http://rss.allocine.fr/ac/cine/prochainement",
  "http://sebsauvage.net/rhaa/rss_fulltext.php",
  "http://securiteoff.blogspot.com/feeds/posts/default?alt=rss",
  "http://spikedmath.com/atom.xml",
  "http://stevehanov.ca/blog/?atom", -- 404?
  "http://t37.net/",
  "http://tanguy.ortolo.eu/blog/feed/atom",
  "http://tech.just-imho.net/atom.xml",
  "http://techblog.appnexus.com/feed/",
  "http://techblog.netflix.com/feeds/posts/default?alt=rss",
  "http://theclimber.fritalk.com/feed/atom",
  "http://timbroder.com/feed", --404?
  "http://ubunteros.tuxfamily.org/spip.php?page=backend",
  "http://vidberg.blog.lemonde.fr/feed/",
  "http://what-if.xkcd.com/feed.atom",
  "http://www.antoinebenkemoun.fr/feed/", --404?
  "http://www.archlinux.org/feeds/news/",
  "http://www.arkandis.com/feed/", --404?
  "http://www.christian-faure.net/feed/", --404?
  "http://www.croc-informatique.fr/feed/",
  "http://www.cyrille-borne.com/index.php?feed/atom",
  "http://www.diaryofaninja.com/rss/main",
  "http://www.direnepasdire.org/?format=feed&type=rss",
  "http://www.framablog.org/index.php/feed/atom",
  "http://www.generation-libre.com/feed/",
  "http://www.generation-linux.fr/index.php?feed/rss2",
  "http://www.goopilation.com/feed", --404?
  "http://www.labo-microsoft.com/tips/rss/",
  "http://www.larsen-b.com/feed",
  "http://www.linux-mag.com/feed/",
  "http://www.linuxjournal.com/node/feed", --404?
  "http://www.mangetamain.fr/feed",
  "http://www.math-linux.com/spip.php?page=backend&id_rubrique=17",
  "http://www.math-linux.com/spip.php?page=backend&id_rubrique=19",
  "http://www.oezratty.net/wordpress/feed/",
  "http://www.planet-libre.org/feed.php?type=rss",
  "http://www.rabbitmq.com/blog/feed/",
  "http://www.reddit.com/r/archlinux/.rss",
  "http://www.reddit.com/r/mongodb/.rss",
  "http://www.regardscitoyens.org/feed/",
  "http://www.road2mayotte.org/blog/?feed=rss2",
  "http://www.rockstarprogrammer.org/rss/full/",
  "http://www.royans.net/arch/feed/", --404?
  "http://www.sanctuaire.fr.eu.org/rss.php",
  "http://www.simple-it.fr/blog/index.php?feed/atom", --404?
  "http://www.smbc-comics.com/rss.php",
  "http://www.synergeek.fr/feed/",
  "http://www.thedoghousediaries.com/?feed=rss2",
  "http://www.think-underground.com/feed/atom",
  "http://www.unpeud.info/component/content/frontpage/frontpage?format=feed&type=rss",
  "http://www.webmonkey.com/feed/",
  "http://xkcd.com/rss.xml",
  "http://yeknan.free.fr/dc2/index.php?feed/category/Ubuntu/atom",
  "http://zythom.blogspot.com/feeds/posts/default"
  ]
