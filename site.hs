--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Applicative ((<$>))
import           Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Extensions
import qualified Data.Set as S


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "chicago-author-date.csl" $ compile cslCompiler
    match "library.bib" $ compile biblioCompiler
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "papers/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "plugins.md", "publications.md"]) $ do
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

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend`
                        bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "rss"
        renderAtom feedConfig feedCtx posts


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
