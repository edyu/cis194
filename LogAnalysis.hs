{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators: none
Notes:
-}

module LogAnalysis where

import Log
import Data.List (sortBy, isInfixOf)
import Data.Char (toLower)

-- Exercise 1
-- Parse an individual line from the log file
tryParseMsg :: MessageType -> [String] -> String -> MaybeLogMessage
tryParseMsg t (w:ws) line = let ts = readInt w
                            in case ts of
                               InvalidInt -> InvalidLM line
                               ValidInt i -> ValidLM (LogMessage t i (unwords ws))
tryParseMsg _ [] line     = InvalidLM line

tryParseError :: [String] -> String -> MaybeLogMessage
tryParseError (w:ws) line = let sev = readInt w
                            in case sev of
                               InvalidInt -> InvalidLM line
                               ValidInt s -> if null ws
                                             then InvalidLM line
                                             else case readInt (head ws) of
                                                  InvalidInt  -> InvalidLM line
                                                  ValidInt ts -> ValidLM (LogMessage (Error s) ts (unwords (tail ws)))
tryParseError [] line     = InvalidLM line

tryParse :: [String] -> String -> MaybeLogMessage
tryParse ("E":ws) line = tryParseError ws line
tryParse ("I":ws) line = tryParseMsg Info ws line
tryParse ("W":ws) line = tryParseMsg Warning ws line
tryParse _ line        = InvalidLM line

parseMessage :: String -> MaybeLogMessage
parseMessage line = tryParse (words line) line

-- Exercise 2
-- Throw out the invalid messages
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly []     = []
validMessagesOnly (m:ms) = case m of
                           InvalidLM _ -> validMessagesOnly ms
                           ValidLM lm  -> lm : validMessagesOnly ms

-- Exercise 3
-- Parse te entire log file at once
parse :: String -> [LogMessage]
parse allLines = validMessagesOnly (map parseMessage (lines allLines))

-- Exercise 4
-- Compare two LogMessages based on their timestamps
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 `compare` ts2

-- Exercise 5
-- Sort the list of messages
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages ms = sortBy compareMsgs ms

-- Exercise 6
-- Return a list of LogMessages corresponding to any
-- errors with severity of 50 or greater, sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map (\(LogMessage _ _ m) -> m) (filter filterError lms)
  where filterError (LogMessage mt _ _) = case mt of
                                          Error sev | sev >= 50 -> True
                                          _                     -> False

-- Exercise 7
-- Include only those messages that contain the string provided
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout word lms =
  filter (\(LogMessage _ _ msg) -> isInfixOf lcword (lowerCase msg))
    lms
  where lowerCase w = map toLower w
        lcword      = lowerCase word
