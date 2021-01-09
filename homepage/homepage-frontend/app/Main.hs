{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Monad       ( void )
import           Data.FileEmbed      ( embedFile, embedStringFile )
import qualified Data.Map           as Map
import           Data.Text           ( Text )
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import qualified Data.Text.Encoding as Text
import           Reflex.Dom

headers :: [(Int, Text, Text)]
headers      = read $(embedStringFile "headers.txt")
homepageCss  =      $(embedFile       "homepage.css" )
markdownHtml =      $(embedStringFile "markdown.html")

main :: IO ()
main = do
  -- HTML <head> tag.
  headEl <- renderStatic $ el "head" $ do
    el "title"    $ text "Telescope"
    el "style"    $ text $ Text.decodeUtf8 homepageCss
    elAttr "meta" (Map.fromList
      [("name", "viewport"), ("content", "width=device-width, initial-scale=1")])
      $ pure ()
  -- HTML <body> tag.
  bodyEl <- renderStatic $ void $ el "body" $ do
    elClass "div" "sidebar" $ do
      elClass "div" "headers" $ flip mapM_ headers $ \(n, t, s) ->
        elAttr "a" ("href" =: ("#" <> s)) $
          el ("h" <> Text.pack (show $ n + 2)) $ text t
      elClass "div" "github" $
        elAttr "a" ("href" =: "https://github.com/jerbaroo/telescope") $
          elAttr "img" ("src" =: "GitHub_Logo.png") $ pure ()
    elClass "div" "content" $ el "div" $ elClass "div" "replace" $ pure ()
  -- Write to file.
  let headText = Text.decodeUtf8 $ snd headEl
      bodyText = Text.replace "<div class=\"replace\"></div>" markdownHtml $
        Text.decodeUtf8 $ snd bodyEl
  Text.writeFile "index.html" $ headText <> bodyText
