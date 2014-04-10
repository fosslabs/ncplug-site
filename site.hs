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
conf :: Configuration
conf = defaultConfiguration { deployCommand = "cp -Rv _site/* /site" }
--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith conf $ do
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
        let feedCtx = postCtx `mappend` teaserField "description" "content"
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

    -- match "fp-course/*" $ do
    --   route $ setExtension "html"
    --   compile $ pandocCompiler
    --       >>= saveSnapshot "fp-content"
    --       >>= loadAndApplyTemplate "templates/post.html"    postCtx
    --       >>= saveSnapshot "fp-rss-content"
    --       >>= loadAndApplyTemplate "templates/default.html" postCtx
    --       >>= relativizeUrls

    -- create ["fp-course.pdf"] $ do
    --   route idRoute
    --   compile $ pandocCompiler

    -- create ["fp-course.html"] $ do
    --   route idRoute
    --   compile $ do
    --     lessons <- recentFirst =<< loadAll "fp-course/*"
    --     let courseCtx =
    --           listField "lessons" postCtx (return lessons) `mappend`
    --           constField "title" "FP Course"               `mappend`
    --           defaultContext
    --     makeItem ""
    --       >>= loadAndApplyTemplate "templates/fp-course.html" courseCtx
    --       >>= loadAndApplyTemplate "templates/default.html"   courseCtx
    --       >>= relativizeUrls

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
