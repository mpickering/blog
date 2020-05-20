#!/usr/bin/env cabal -wghc-8.8.3 run
{- cabal:
build-depends: base >= 4
             , hakyll
             , random-shuffle
             , containers
             , pandoc
             , split
-}

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Applicative ((<$>))
import           Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Extensions
import qualified Data.Set as S
import Hakyll.Core.Compiler.Internal
import Debug.Trace
import System.Random.Shuffle
import Data.List.Split

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "chicago-author-date.csl" $ compile cslCompiler
    match "library.bib" $ compile biblioCompiler
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "ide/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "papers/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "plugins.md", "publications.md", "bristol2020.md", "bristol2020-register.html", "hurstwood.md", "hurstwood-start.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls



    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          csl <- load (fromFilePath "chicago-author-date.csl")
          bib <- load (fromFilePath "library.bib")
          fmap (writePandocWith mathOpts) (getResourceBody
            >>= readPandocBiblio defaultHakyllReaderOptions csl bib)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "rss"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let archiveCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- take 5 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
            let indexCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["ide/contributors.html"] $ do
        route idRoute
        compile $ do
            let contribCtx = (field "name" $ \item -> return (name (itemBody item))) `mappend`
                             (field "gh" $ \item -> return (gh  (itemBody item)))
            let archiveCtx =
                    listField "person" contribCtx loadContribs `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "ide/templates/contributors.html" archiveCtx
                >>= loadAndApplyTemplate "ide/templates/ide.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "ide/posts/*" $ do
        route $ setExtension "html"
        compile $ do
          csl <- load (fromFilePath "chicago-author-date.csl")
          bib <- load (fromFilePath "library.bib")
          fmap (writePandocWith mathOpts) (getResourceBody
            >>= readPandocBiblio defaultHakyllReaderOptions csl bib)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "ide/templates/post.html"postCtx
            >>= saveSnapshot "rss"
            >>= loadAndApplyTemplate "ide/templates/ide.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    match (fromList ["ide/about.md"])
                    --, "ide/funding.md"])
                    $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "ide/templates/ide.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "ide/index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "ide/posts/*" "content"
            let post = take 1 posts
            let indexCtx =
                    listField "posts-show" teaserCtx (return post) `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "ide/templates/ide.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
    match "ide/templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend`
                        bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "rss"
        renderAtom feedConfig feedCtx posts

    create ["ide/atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend`
                        bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "ide/posts/*" "rss"
        renderAtom ideFeedConfig feedCtx posts


data Contrib = Contrib { name :: String, gh :: String } deriving Show

loadContribs :: Compiler ([Item Contrib])
loadContribs = do
  csv_item <- compilerUnsafeIO (readFile  "ide/contributors.csv")
  items <- traverse (makeItem . parseRow) (lines csv_item)
  compilerUnsafeIO $ shuffleM items

  where
    parseRow s = Contrib name gh
      where
        [name, gh] = splitOn "," s
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle = "mpickering.github.io"
  , feedDescription = ""
  , feedAuthorName = "Matthew Pickering"
  , feedAuthorEmail = "matthewtpickering@gmail.com"
  , feedRoot = "http://mpickering.github.io"
  }

ideFeedConfig :: FeedConfiguration
ideFeedConfig = FeedConfiguration
  { feedTitle = "IDE 2020 Updates"
  , feedDescription = "Updates on Haskell IDE work taking place summer 2020"
  , feedAuthorName = "Matthew Pickering"
  , feedAuthorEmail = "matthewtpickering@gmail.com"
  , feedRoot = "http://mpickering.github.io/"
  }

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

mathOpts =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr enableExtension defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in writerOptions
