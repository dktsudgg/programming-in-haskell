{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{- cabal:
build-depends: base, haskell-gi-base, gi-gtk == 3.0.*
-}
module Main where

-- import Lib
-- main :: IO ()
-- main = someFunc

import qualified GI.Gtk as Gtk
import Data.GI.Base
import GI.Gtk (widgetWidthRequest, widgetHeightRequest)

main :: IO ()
main = do
  Gtk.init Nothing

  win <- new Gtk.Window [ #title := "Hi there"
                        , #widthRequest   := 400
                        , #heightRequest  := 300
                        ]

  on win #destroy Gtk.mainQuit

  button <- new Gtk.Button [ #label := "Click me" ]

  on button #clicked (set button [ #sensitive := False,
                                   #label := "Thanks for clicking me" ])

  #add win button

  #showAll win

  Gtk.main
