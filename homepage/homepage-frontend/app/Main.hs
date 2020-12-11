{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Monad       ( void )
import           Data.FileEmbed      ( embedFile, embedStringFile )
import qualified Data.Map           as Map
import           Data.Text           ( Text )
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Reflex.Dom

headers :: [(Int, Text)]
headers      = read $(embedStringFile "headers.txt")
highlightCss =      $(embedFile       "highlight.css")
homepageCss  =      $(embedFile       "homepage.css" )
markdownHtml =      $(embedStringFile "markdown.html")
css = Text.decodeUtf8 $ highlightCss <> homepageCss

main :: IO ()
main = mainWidgetWithHead headElement $ void $ do
  elClass "div" "sidebar" $ do
    elClass "div" "headers" $ flip mapM_ headers $ \(n, t) ->
      elAttr "a" ("href" =: ("#" <> t)) $
        el ("h" <> Text.pack (show $ n + 2)) $ text t
    elClass "div" "github" $
      elAttr "a" ("href" =: "https://github.com/jerbaroo/telescope") $
        elAttr "img" ("src" =: "diagram/GitHub_Logo.png") $ pure ()
  elClass "div" "content" $ elDynHtml' "div" $ pure markdownHtml

headElement :: MonadWidget t m => m ()
headElement = do
  el "title"    $ text "Telescope"
  el "style"    $ text css
  elAttr "meta" (Map.fromList
    [("name", "viewport"), ("content", "width=device-width, initial-scale=1")])
    $ pure ()
