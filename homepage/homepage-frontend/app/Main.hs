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

headers :: [(Int, Text)]
headers      = read $(embedStringFile "headers.txt")
highlightCss =      $(embedFile       "highlight.css")
homepageCss  =      $(embedFile       "homepage.css" )
markdownHtml =      $(embedStringFile "markdown.html")
css = Text.decodeUtf8 $ highlightCss <> homepageCss

main :: IO ()
main = do
  -- HTML <head> tag.
  headEl <- renderStatic $ el "head" $ do
    el "title"    $ text "Telescope"
    el "style"    $ text css
    elAttr "meta" (Map.fromList
      [("name", "viewport"), ("content", "width=device-width, initial-scale=1")])
      $ pure ()
  -- HTML <body> tag.
  bodyEl <- renderStatic $ void $ el "body" $ do
    elClass "div" "sidebar" $ do
      elClass "div" "headers" $ flip mapM_ headers $ \(n, t) ->
        elAttr "a" ("href" =: ("#" <> t)) $
          el ("h" <> Text.pack (show $ n + 2)) $ text t
      elClass "div" "github" $
        elAttr "a" ("href" =: "https://github.com/jerbaroo/telescope") $
          elAttr "img" ("src" =: "diagram/GitHub_Logo.png") $ pure ()
    elClass "div" "content" $ el "div" $ elClass "div" "replace" $ pure ()
  -- Write to file.
  let headText = Text.decodeUtf8 $ snd headEl
      bodyText = Text.replace "<div class=\"replace\"></div>" markdownHtml $
        Text.decodeUtf8 $ snd bodyEl
  Text.writeFile "index.html" $ headText <> bodyText
