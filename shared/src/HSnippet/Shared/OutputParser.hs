{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module HSnippet.Shared.OutputParser where

------------------------------------------------------------------------------
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------
import           HSnippet.Shared.Types.BuildMessage
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Parses a list of lines into either a non-message line or a BuildMessage.
outParser :: [Text] -> [Either Text BuildMessage]
outParser [] = []
outParser (l:ls) = do
    case parseOnly messageStart l of
      Left _ -> Left l : outParser ls
      Right tuple -> restParser tuple [l] ls

restParser
    :: (Text, Int, Int, MessageType)
    -> [Text]
    -> [Text]
    -> [Either Text BuildMessage]
restParser (nm,line,col,ty) msgLines [] =
    [Right $ BuildMessage nm line col ty msgLines]
restParser tuple@(nm,line,col,ty) msgLines (l:ls) =
    if not (T.null l) && isSpace (T.head l)
       then restParser tuple (msgLines ++ [l]) ls
       else Right (BuildMessage nm line col ty msgLines) : outParser (l:ls)

messageStart :: Parser (Text, Int, Int, MessageType)
messageStart = do
    nm <- takeWhile1 (/= ':')
    _ <- string ":"
    line <- decimal
    _ <- string ":"
    col <- decimal
    _ <- string ":"
    ty <- choice
      [ " Warning:" *> return BuildWarning
      , many' anyChar >> endOfInput >> return BuildError
      ]
    return (nm, line, col, ty)

-- The rest are unused for now

messageParser :: Parser BuildMessage
messageParser = do
    (startLine, (nm, line, col, ty)) <- match messageStart
    rest <- many' spaceLine
    return $ BuildMessage nm line col ty (startLine : rest)

spaceLine :: Parser Text
spaceLine = fst <$> match (space >> takeTill endOfLine_fast)

endOfLine_fast :: Char -> Bool
endOfLine_fast c = c == '\r' || c == '\n'
