module LogAnalysis where

import Log
import Text.ParserCombinators.Parsec
import Data.Maybe          (fromMaybe)
import Text.Read           (readMaybe)

tStampAndMessage :: Parser (TimeStamp, String)
tStampAndMessage = do tstamp <- many1 digit
                      spaces
                      msg <- manyTill anyChar eol
                      return (read tstamp::Int, msg)

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

info :: Parser LogMessage
info = do char 'I'
          spaces
          (tstamp, msg) <- tStampAndMessage
          return $ LogMessage Info tstamp msg

warn :: Parser LogMessage
warn = do char 'W'
          spaces
          (tstamp, msg) <- tStampAndMessage
          return $ LogMessage Warning tstamp msg

errr :: Parser LogMessage
errr = do char 'E'
          spaces
          sev <- many1 digit
          spaces
          (tstamp, msg) <- tStampAndMessage
          return $ LogMessage (Error (read sev::Int)) tstamp msg

unkn :: Parser LogMessage
unkn = do s <- manyTill anyChar eol
          return $ Unknown s

anyMessage :: Parser LogMessage
anyMessage = try info <|> warn <|> errr <|> unkn

logFile :: Parser [LogMessage]
logFile = manyTill anyMessage eof

parseLog :: String -> [LogMessage]
parseLog s = case parse logFile "testLog" s of
  Right ms -> ms
  Left e -> error $ show e

parseMessage' :: String -> LogMessage
parseMessage' line = fromMaybe (Unknown line) (pm . words $ line)
  where
    pm :: [String] -> Maybe LogMessage
    pm ("I":ts:msg)    = LogMessage Info                          <$> readMaybe ts <*> pure (unwords msg)
    pm ("W":ts:msg)    = LogMessage Warning                       <$> readMaybe ts <*> pure (unwords msg)
    pm ("E":le:ts:msg) = LogMessage <$> fmap Error (readMaybe le) <*> readMaybe ts <*> pure (unwords msg)
    pm _               = Nothing

simpleParse :: String -> [LogMessage]
simpleParse = map parseMessage' . lines

insert :: LogMessage -> MessageTree LogMessage -> MessageTree LogMessage
insert lmnew Leaf = Node Leaf lmnew Leaf
insert lmnew (Node mtl lm mtr)
  | time lmnew <= time lm  = Node (insert lmnew mtl) lm mtr
  | time lmnew > time lm = Node mtl lm (insert lmnew mtr)
  where time (LogMessage _ ts _) = ts
        time (Unknown _)         = 0
insert _ mt = mt

build :: [LogMessage] -> MessageTree LogMessage
build = foldr insert Leaf

inOrder :: MessageTree LogMessage -> [LogMessage]
inOrder Leaf = []
inOrder (Node mtl lm mtr) = inOrder mtl ++ [lm] ++ inOrder mtr

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter important . inOrder . build
  where important (LogMessage (Error sev) _ _) = sev >= 50
        important _ = False
        getMsg (LogMessage _ _ msg') = msg'
        getMsg (Unknown msg') = msg'

main :: IO ()
main = testWhatWentWrong parseLog whatWentWrong "src/error.log"
       >>= mapM_ putStrLn
