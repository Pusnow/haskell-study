module LogAnalysis where
import Log


-- EX1


parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I":ts:msg )= LogMessage Info (read ts) (unwords msg)
parseMessageWords ("E":level:ts:msg)= LogMessage (Error (read level)) (read ts) (unwords msg)
parseMessageWords ("W":ts:msg) = LogMessage Warning (read ts) (unwords msg)
parseMessageWords x = Unknown (unwords x)

parseMessage :: String -> LogMessage
parseMessage x = parseMessageWords (words x)



-- MAIN

main = do
    print("ex1-1", parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help")
    print("ex1-2", parseMessage "I 29 la la la" == LogMessage Info 29 "la la la")
    print("ex1-3", parseMessage "This is not in the right format" == Unknown "This is not in the right format")