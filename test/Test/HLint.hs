module Test.HLint
       ( hlintSpec
       ) where

import Test.Hspec (Spec, describe, it)

import Hintman.App (AppEnv)
import Hintman.Core.Review (Comment (..))
import Hintman.HLint (runHLint)

import Test.Assert (equals, satisfies)
import Test.Data (pr1, pr2)


hlintSpec :: AppEnv -> Spec
hlintSpec env = describe "HLint works on opened PRs" $ do
    it "works with non-code PRs" $
        env & (pr1 >>= runHLint) `equals` []
    it "creates correct eta-reduce comment for PR 2" $
        env & (pr2 >>= runHLint) `satisfies` (etaComment `elem`)
    it "creates correct part line comment for PR 2" $
        env & (pr2 >>= runHLint) `satisfies` (avoidLambdaComment `elem`)
    it "creates remove line comment for PR 2" $
        env & (pr2 >>= runHLint) `satisfies` (removePragmaComment `elem`)
    it "creates multiline comment for PR 2" $
        env & (pr2 >>= runHLint) `satisfies` (multilineComment `elem`)
  where
    mkComment :: Int -> Text -> Comment
    mkComment pos txt = Comment
        { commentPath = "Main.hs"
        , commentPosition = pos
        , commentBody = txt
        }

    etaComment :: Comment
    etaComment = mkComment 9 $ unlines
        [ "Warning: Eta reduce"
        , "```suggestion"
        , "greet = (++) \"Hello \""
        , "```"
        ]

    avoidLambdaComment :: Comment
    avoidLambdaComment = mkComment 12 $ unlines
        [ "Warning: Avoid lambda"
        , "```suggestion"
        , "foo a b = (succ) a + b"
        , "```"
        ]

    removePragmaComment :: Comment
    removePragmaComment = mkComment 0 $ unlines
        [ "Warning: Unused LANGUAGE pragma"
        , "```suggestion"
        , ""
        , "```"
        ]

    multilineComment :: Comment
    multilineComment = mkComment 17 $ unlines
        [ "Warning: Eta reduce"
        , "```"
        , "multiline = putStrLn"
        , "```"
        ]
