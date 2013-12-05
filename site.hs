--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
ncplugFeedConf :: FeedConfiguration
ncplugFeedConf = FeedConfiguration { feedTitle = "NCPLUG"
                                   , feedDescription = "Non-conforming Programming Languages User Group posts"
                                   , feedAuthorName = "NCPLUG"
                                   , feedAuthorEmail = "ncplug@fosslabs.ru"
                                   , feedRoot = "http://ncplug.fosslabs.ru" }
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "fonts/*" $ do
      route   idRoute
      compile copyFileCompiler

    match (fromList ["about.rst"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= saveSnapshot "rsscontent"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["feed.rss"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
                 loadAllSnapshots "posts/*" "content"
        renderRss ncplugFeedConf feedCtx posts

    match "contact.html" $ do
      route idRoute
      compile $ do
        getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Главная"             `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "<span>%e</span>%b" `mappend`
    teaserField "teaser" "content"          `mappend`
    defaultContext
