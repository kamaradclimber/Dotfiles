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
  "http://blog.opensyd.fr/feed/", -- user error (openTCPConnection: host lookup failure for "blog.opensyd.fr")
  "http://blog.theclimber.be/?feed/category/Alpinisme-et-escalade/atom",
  "http://blog.theclimber.be/?feed/category/Informatique/atom",
  "http://blogs.valvesoftware.com/feed/?cat=7",
  "http://blog.xkcd.com/feed/",
  "http://blogues.ebsi.umontreal.ca/jms/index.php/feed/rss2",
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
  "http://feeds.feedburner.com/FredericDeVillamilcom",
  "http://feeds.feedburner.com/GoogleOperatingSystem",
  "http://feeds.feedburner.com/HighScalability",
  "http://feeds.feedburner.com/OxydeDeChromeOs",
  "http://feeds.feedburner.com/ThisIsWhyImBroke",
  "http://feeds.feedburner.com/bleebot", --connot parse date
  "http://feeds.feedburner.com/bouletcorp",
  "http://feeds.feedburner.com/codinghorror/",
  "http://feeds.feedburner.com/debuggable",
  "http://feeds.feedburner.com/gtutor",
  "http://feeds.feedburner.com/labandepasdessinee",
  "http://feeds.feedburner.com/littlebigdetails",
  "http://feeds.feedburner.com/mongotips",
  "http://feeds.feedburner.com/nosql",
  "http://feeds.feedburner.com/siat-mongodb",
  "http://feeds.feedburner.com/xg_blognotes",
  "http://feeds.labnol.org/labnol",
  "http://feeds.wordaligned.org/wordaligned", --cannot parse date
  "http://francoisgeff.com/feed/", --fausse 404, le site existe
  "http://frandigou.blogspot.com/feeds/posts/default?alt=rss",
  "http://frederic.bezies.free.fr/blog/?feed=rss2",
  "http://gege2061.homecomputing.fr/feed/",
  "http://gmailblog.blogspot.com/feeds/posts/default?alt=rss",
  "http://google-opensource.blogspot.com/feeds/posts/default?alt=rss",
  "http://googledocs.blogspot.com/feeds/posts/default?alt=rss",
  "http://images.math.cnrs.fr/spip.php?page=backend",
  "http://internetactu.blog.lemonde.fr/feed/",
  "http://interstices.info/feed/Rss2.jsp?id=c_13634",
  "http://julien.danjou.info/blog/index.xml",
  "http://kindlewallpapers.tumblr.com/rss",
  "http://libre-ouvert.toile-libre.org/feed.php?rss",
  "http://linux-attitude.fr/feed",
  "http://linux-attitude.fr/feed/atom",
  "http://linux.leunen.com/?feed=rss2&cat=15",
  "http://linux.leunen.com/?feed=rss2&cat=5",
  "http://linuxfr.org/backend/journaux/rss20.rss",
  "http://linuxfr.org/backend/news/rss20.rss",
  "http://linuxfr.org/journaux.atom",
  "http://linuxfr.org/news.atom",
  "http://linuxmanua.blogspot.com/feeds/posts/default",
  "http://marie-issy-munich.blogspot.com/feeds/posts/default?alt=rss",
  "http://mauvaisgout.net/feed",
  "http://medspx.homelinux.org/blog/index.rss20",
  "http://michel-eudes.net/blog/rss.php", --Cannot decode byte '\x6c': Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream
  "http://montmartre-altitude.blogspot.com/feeds/posts/default?alt=rss",
  "http://mrpouit.free.fr/?feed=rss2", -- 404?
  "http://nicolas.kruchten.com/content/feed/",
  "http://notch.tumblr.com/rss",
  "http://nuts-and-bolts-of-cakephp.com/feed/",
  "http://ocaml.janestreet.com/?q=rss.xml",
  "http://obfuscurity.com/rss2.xml",
  "http://owni.fr/categorie/une/feed/",
  "http://passeurdesciences.blog.lemonde.fr/feed/",
  "http://philippe.scoffoni.net/feed/", --Cannot decode byte '\x8b': Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream
  "http://planet-fr.debian.net/rss20.xml",
  "http://planet.archlinux.org/atom.xml",
  "http://planet.auto-hebergement.fr/feed.php?type=rss",
  "http://playtime.blog.lemonde.fr/feed/",
  "http://rats-bleus.blogspot.com/feeds/posts/default?alt=rss",
  "http://reyt.net/feed/", -- 404?
  "http://rss.wikio.fr/search/Dominique+Seux.rss", --user error (openTCPConnection: host lookup failure for "rss.wikio.fr")
  "http://sebsauvage.net/rhaa/rss_fulltext.php",
  "http://securiteoff.blogspot.com/feeds/posts/default?alt=rss",
  "http://spikedmath.com/atom.xml",
  "http://stevehanov.ca/blog/?atom", -- 404?
  "http://syndication.thedailywtf.com/TheDailyWtf",
  "http://t37.net/",
  "http://tanguy.ortolo.eu/blog/feed/atom",
  "http://tech.just-imho.net/atom.xml",
  "http://techblog.appnexus.com/feed/",
  "http://techblog.netflix.com/feeds/posts/default?alt=rss",
  "http://theclimber.fritalk.com/feed/atom",
  "http://timbroder.com/feed", --404?
  "http://ubunteros.tuxfamily.org/spip.php?page=backend",
  "http://univers-libre.net/index.php/feed/", --hangs, should timeout
  "http://vidberg.blog.lemonde.fr/feed/",
  "http://www.42experiment.org/feed/",
  "http://www.antoinebenkemoun.fr/feed/", --404?
  "http://www.archlinux.org/feeds/news/",
  "http://www.arkandis.com/feed/", --404?
  "http://www.bomahy.nl/hylke/blog/feed/",--404?
  "http://www.christian-faure.net/feed/", --404?
  "http://www.clapico.com/feed/",
  "http://www.croc-informatique.fr/feed/",
  "http://www.csquad.org/feed/",
  "http://www.cyrille-borne.com/index.php?feed/atom",
  "http://www.diaryofaninja.com/rss/main",
  "http://www.direnepasdire.org/?format=feed&type=rss",
  "http://www.framablog.org/index.php/feed/atom",
  "http://www.geeek.org/feed/atom", --404?
  "http://www.generation-libre.com/feed/",
  "http://www.generation-linux.fr/index.php?feed/rss2",
  "http://www.goopilation.com/feed", --404?
  "http://www.labo-microsoft.com/tips/rss/",
  "http://www.larsen-b.com/feed",
  "http://www.laviemoderne.net/clapotis/feed/rss.html",
  "http://www.laviemoderne.net/lames-de-fond/feed/rss.html",
  "http://www.linux-mag.com/feed/",
  "http://www.linuxjournal.com/node/feed", --404?
  "http://www.mangetamain.fr/feed",
  "http://www.math-linux.com/spip.php?page=backend&id_rubrique=17",
  "http://www.math-linux.com/spip.php?page=backend&id_rubrique=19",
  "http://www.mybusinesseducation.fr/taxonomy/term/44/0/feed",
  "http://www.oezratty.net/wordpress/feed/",
  "http://www.planet-libre.org/feed.php?type=rss",
  "http://www.prospective-numerique.gouv.fr/rss.xml",
  "http://www.rabbitmq.com/blog/feed/",
  "http://www.reddit.com/r/archlinux/.rss",
  "http://www.reddit.com/r/mongodb/.rss",
  "http://www.regardscitoyens.org/feed/",
  "http://www.road2mayotte.org/blog/?feed=rss2",
  "http://www.rockstarprogrammer.org/rss/full/",
  "http://www.royans.net/arch/feed/", --404?
  "http://www.rue89.com/category/typepublication/pause-cafe/feed",
  "http://www.sanctuaire.fr.eu.org/rss.php",
  "http://www.simple-it.fr/blog/index.php?feed/atom", --404?
  "http://www.smbc-comics.com/rss.php",
  "http://www.sparkleshare.org/news.xml", --404?
  "http://www.synergeek.fr/feed/",
  "http://www.thedoghousediaries.com/?feed=rss2",
  "http://www.think-underground.com/feed/atom",
  "http://www.unpeud.info/component/content/frontpage/frontpage?format=feed&type=rss",
  "http://www.webmonkey.com/feed/",
  "http://xaviergorce.com/?feed=rss2", --404?
  "http://xhtml.net/rss.php",
  "http://xkcd.com/rss.xml",
  "http://yeknan.free.fr/dc2/index.php?feed/category/Ubuntu/atom",
  "http://zythom.blogspot.com/feeds/posts/default"
  ]
