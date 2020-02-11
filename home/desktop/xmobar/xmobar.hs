------------------------------------------------------------------------------
-- |
-- Copyright: (c) 2018, 2019 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 21:03
--
--
-- An example of a Haskell-based xmobar. Compile it with
--   ghc --make -- xmobar.hs
-- with the xmobar library installed or simply call:
--   xmobar /path/to/xmobar.hs
-- and xmobar will compile and launch it for you and
------------------------------------------------------------------------------

import           Xmobar

-- Configuration, using predefined monitors as well as our HelloWorld
-- plugin:

config :: Config
config = defaultConfig
  { font             = "xft:Sans Mono-6"
  , additionalFonts  = []
  , borderColor      = "black"
  , border           = TopB
  , bgColor          = "black"
  , fgColor          = "grey"
  , alpha            = 255
  , position         = Top
  , textOffset       = -1
  , iconOffset       = -1
  , lowerOnStart     = True
  , pickBroadest     = False
  , persistent       = False
  , hideOnStart      = False
  , iconRoot         = "."
  , allDesktops      = True
  , overrideRedirect = True
  , commands         =
    [ Run $ Network
      "eth0"
      ["-L", "0", "-H", "32", "--normal", "green", "--high", "red"]
      10
    , Run $ Network
      "eth1"
      ["-L", "0", "-H", "32", "--normal", "green", "--high", "red"]
      10
    , Run $ Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10
    , Run $ Memory ["-t", "Mem: <usedratio>%"] 10
    , Run $ Com "uname" ["-s", "-r"] "" 36000
    , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
    ]
  , sepChar          = "%"
  , alignSep         = "}{"
  , template = "%cpu% | %memory% | %cpu% }\
               \ { <fc=#ee9a00>%date%</fc>"
  }

main :: IO ()
main = xmobar config
