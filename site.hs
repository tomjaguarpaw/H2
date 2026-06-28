{-# LANGUAGE OverloadedStrings #-}

import Data.List (isSuffixOf)
import Data.String (fromString)
import Hakyll
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import Text.Pandoc.Options

main :: IO ()
main = hakyll $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route directoryRoute
    compile $
      pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions {writerHTMLMathMethod = MathJax ""}
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      let indexCtx =
            foldMap (\(name, list) -> field name (\_ -> list)) topPostList
              <> field "posts" (\_ -> postList)

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate
          "templates/default.html"
          ((field "title" $ \_ -> return "The H2 Wiki") <> defaultContext)
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "templates/*" $ compile templateCompiler

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

indexRoute :: Identifier -> FilePath
indexRoute ident =
  takeDirectory p
    </> takeBaseName p
    </> "index.html"
  where
    p = toFilePath ident

directoryRoute :: Routes
directoryRoute = customRoute indexRoute

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
  where
    clean = (`removeTrailing` "index.html")

removeTrailing :: (Eq a) => [a] -> [a] -> [a]
removeTrailing xs suf
  | suf `isSuffixOf` xs = take (length xs - length suf) xs
  | otherwise = xs

postList :: Compiler String
postList = do
  posts <- loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl defaultContext posts

topPostList :: [(String, Compiler String)]
topPostList =
  [ ( "bluefinPosts",
      foo
        [ "bluefin-all",
          "bluefin-plucking-constraints",
          "bluefin-prevents-handles-leaking",
          "bluefin-streams-finalize-promptly",
          "bluefin-versus-oop"
        ]
    ),
    ( "mathsAndPhysicsPosts",
      foo
        [ "bells-theorem-made-simpler",
          "symbolic-expressions-can-be-automatically-differentiated",
          "why-is-naive-symbolic-differentiation-slow",
          "automatic-differentiation-worked-examples",
          "automatic-differentiation-worked-examples-maths"
        ]
    ),
    ( "haskellPosts",
      foo
        [ "haskells-missing-mutable-ref",
          "ioscopedref-reference-implementation",
          "fork-fragile-reader-like-operations",
          "nested-strict-data",
          "data-map-strict-map-not-strict-map",
          "impure-lazy-language",
          "make-invalid-laziness-unrepresentable",
          "foldl-traverses-state-foldr-traverses-anything",
          "scrap-your-iteration-combinators",
          "ghc-8.10-9.6-experience-report",
          "opaleyes-api-breakage-policy",
          "demystifying-dlist",
          "exitfailure-doesnt-exit",
          "good-design-and-type-safety-in-yahtzee",
          "using-brain-less-refactoring-yahtzee",
          "hascallstack-domain-errors",
          "improving-the-typed-process-documentation",
          "mysterious-incomposability-of-decidable"
        ]
    ),
    ( "programmingPosts",
      foo
        [ "git-rebase-conflicts",
          "git-survival-guide"
        ]
    )
  ]

foo :: [String] -> Compiler String
foo postNames = do
  posts <- mapM (load . fromString . ("posts/" <>) . (<> ".md")) postNames
  tpl <- loadBody "templates/post-item.html"
  applyTemplateList tpl defaultContext posts
