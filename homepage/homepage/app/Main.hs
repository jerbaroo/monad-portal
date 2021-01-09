{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Cheapskate                     ( Block(..), CodeAttr(..) )
import qualified Cheapskate                    as Cheap
import qualified Control.Monad.State           as State
import           Data.Either.Combinators        ( fromRight' )
import           Data.FileEmbed                 ( embedDir, embedFile )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import qualified Data.Sequence                 as Seq
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Lazy                as Text.Lazy
import           Data.Tuple.Extra               ( first, second )
import qualified Skylighting                   as Sky
import qualified Text.Blaze.Html               as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import           System.FilePath                ( takeFileName )

-- | The README text.
readme :: Text
readme = Text.decodeUtf8 $(embedFile "README.md")

-- | The text for all markdown files in the "docs" folder.
docs :: Map FilePath Text
docs = Map.fromList
  $ map (first takeFileName . second Text.decodeUtf8) $(embedDir "docs")

-- | Paths of markdown files in concatenation order.
filepaths :: [FilePath]
filepaths = ["docs/ABOUT.md", "docs/TUTORIAL.md", "docs/FAQ.md"]

-- | The text for all markdown files concatenated together.
markdownText :: Text
markdownText = Text.intercalate "\n" $ [readme] ++ map f filepaths
  where f filepath = fromJust $ Map.lookup (takeFileName filepath) docs

-- | Add highlighting to any markdown code block.
addHighlighting :: Block -> Block
addHighlighting (CodeBlock (CodeAttr lang _) codeText) =
  HtmlBlock
    $ Text.concat $ Text.Lazy.toChunks
    $ Blaze.renderHtml $ Blaze.toHtml
    $ Sky.formatHtmlBlock Sky.defaultFormatOpts
    -- Exception thrown if code in markdown doesn't parse.
    $ fromRight' $ Sky.tokenize config syntax codeText
  where config = Sky.TokenizerConfig Sky.defaultSyntaxMap False
        syntax = fromJust $ Sky.syntaxByName Sky.defaultSyntaxMap lang
addHighlighting x = x

-- | Markdown converted to HTML with code highlighting.
markdownWithHighlighting :: Cheap.Doc -> Text
markdownWithHighlighting =
  Text.Lazy.toStrict . Blaze.renderHtml . Blaze.toHtml . Cheap.walk addHighlighting

-- | Make any header into an anchor link, and extract the header information.
anchorHeader :: State.MonadState [(Int, Text, Text)] m => Block -> m Block
anchorHeader (Header n inlines) = do
  let safeHeaderText = Text.replace "'" "" headerText
  State.modify (++ [(n, headerText, safeHeaderText)])
  pure $ HtmlBlock $ mconcat
    [ "<h", Text.pack $ show n, ">"
    , "<a name='", safeHeaderText, "'>", headerText, "</a>"
    , "</h", Text.pack $ show n, ">"
    ]
  where headerText = headerText' $ Seq.viewl inlines
        headerText' Seq.EmptyL                = ""
        headerText' ((Cheap.Str t) Seq.:< xs) = t   <> headerText' (Seq.viewl xs)
        headerText' ((Cheap.Space) Seq.:< xs) = " " <> headerText' (Seq.viewl xs)
        headerText' (_             Seq.:< xs) = ""  <> headerText' (Seq.viewl xs)
anchorHeader x = pure x

main :: IO ()
main = do
  let (markdown, headers) = State.runState
        (Cheap.walkM anchorHeader $ Cheap.markdown Cheap.def markdownText) []
  writeFile "headers.txt"   $ show headers
  writeFile "markdown.html" $ Text.unpack $ markdownWithHighlighting markdown
