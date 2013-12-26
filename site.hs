{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.List (isSuffixOf)
import System.FilePath.Posix (takeBaseName,takeDirectory,(</>))

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> postList

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler


niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </> takeBaseName p
                                 </> "index.html"
                           where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
  where clean = (`removeTrailing` "index.html")

removeTrailing :: Eq a => [a] -> [a] -> [a]
removeTrailing xs suf | suf `isSuffixOf` xs = take (length xs - length suf) xs
                      | otherwise = xs

postList :: Compiler String
postList = do
    posts <- loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl defaultContext posts
