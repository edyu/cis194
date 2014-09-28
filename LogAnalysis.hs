{-# OPTIONS_GHC -Wall #-}
{-
Name: Ed Yu
Collaborators: none
Notes:
-}

module LogAnalysis where

import Log

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
