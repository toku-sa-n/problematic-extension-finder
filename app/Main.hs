{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

import           GHC.Data.EnumSet
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Hs
import           GHC.Hs.Dump
import           GHC.LanguageExtensions
import qualified GHC.Parser                                          as GHC
import           GHC.Parser.Lexer
import           GHC.Types.SrcLoc
import           GHC.Utils.Error
import           GHC.Utils.Outputable                                hiding
                                                                     (empty)
import qualified GHC.Utils.Outputable                                as GLP
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config
import           Text.Pretty.Simple

main :: IO ()
main = getContents >>= pPrint . filter (not . snd) . tryParse

tryParse :: String -> [(Extension, Bool)]
tryParse code = fmap (\x -> (x, parseOk x code)) [minBound ..]

printOutputable :: Outputable a => a -> IO ()
printOutputable = putStrLn . showOutputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

parseOk :: Extension -> String -> Bool
parseOk extension code =
  case runParser (parserOpts extension) code GHC.parseModule of
    POk {}     -> True
    PFailed {} -> False

runParser :: ParserOpts -> String -> P a -> ParseResult a
runParser opts str parser = unP parser parseState
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    b = stringToStringBuffer str
    parseState = initParserState opts b location

parserOpts :: Extension -> ParserOpts
#if MIN_VERSION_ghc_lib_parser(9,4,1)
parserOpts ext =
  mkParserOpts
    (fromList [ext])
    diagOpts
    [] -- There are no supported languages and extensions (this list is used only in error messages)
    False -- Safe imports are off.
    False -- Haddock comments are treated as normal comments.
    True -- Comments are kept in an AST.
    False -- Do not update the internal position of a comment.
  where
    diagOpts =
      DiagOpts
        { diag_warning_flags = empty
        , diag_fatal_warning_flags = empty
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
        }
#else
parserOpts opts =
  mkParserOpts
    ES.empty -- No compiler warnings are enabled.
    ES.empty
    False -- Safe imports are off.
    False -- Haddock comments are treated as normal comments.
    True -- Comments are kept in an AST.
    False -- Do not update the internal position of a comment.
#endif
