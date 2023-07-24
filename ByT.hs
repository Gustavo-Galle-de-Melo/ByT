import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import System.Directory (removeFile)
import System.Environment (getArgs, getExecutablePath)
import System.Process (createProcess, shell, waitForProcess)
import qualified Data.ByteString as B (readFile, writeFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["help"] -> help
        ["?"] -> help

        [file] -> do
            
            code <- readFile (file ++ ".byt")

            -- If the file is empty
            if null code then do

                -- Copy the compiler's .exe
                compiler <- getExecutablePath
                executable <- B.readFile compiler
                B.writeFile (file ++ ".exe") executable

            else do

                -- Compile from ByT to Haskell
                let compiledCode = compile code
                writeFile (file ++ ".hs") compiledCode

                -- Compile from Haskell to .exe
                (_, _, _, processHandle) <- createProcess $ shell $ "ghc " ++ file ++ ".hs"
                waitForProcess processHandle

                -- Delete the auxiliary files
                mapM_ (removeFile . (file ++)) [".hs", ".hi", ".o"] -- Delete ".hs" from this list to see the compiled code

        _ -> help

help :: IO ()
help = putStrLn "Use: ./ByT <file name without '.byt'>"


compile :: String -> String
compile bytCode = header ++ convertedCode
    where
        convertedCode = unlines $ zipWith (\n -> compileLine n . removeComments . words) [1..] $ lines bytCode

removeComments :: [String] -> [String]
removeComments = takeWhile (not . isPrefixOf "//")

compileLine :: Int -> [String] -> String
compileLine _ [] = []
compileLine _ (name : "=" : stack@(_:_)) = convertName name ++ " = " ++ joinWithSeparator (map convertName stack)
compileLine num line = error $ "Line " ++ show num ++ " does not match '<name> = <stack>': " ++ unwords line

joinWithSeparator :: [String] -> String
joinWithSeparator = drop 4 . concatMap (" $$ " ++)

-- Example: "123abc@$" -> "var_123abc'64''36'"
convertName :: String -> String
convertName = ("var_" ++) . concatMap removeSpecialChars

removeSpecialChars :: Char -> String
removeSpecialChars c
    | c <= '\128' && isAlphaNum c   = [c]
    | otherwise                     = "\'" ++ show (fromEnum c) ++ "\'"


-- Beginning of the compiled code
header :: String
header = unlines [
        "import Data.Bits (testBit)",
        "import Data.Bool (bool)",
        "import Data.Maybe (fromJust)",
        "",
        "type Bits = [Bool]",
        "",
        "data ByT",
        "    = End",
        "    | ByT {",
        "        bits :: Bits,",
        "        code :: Maybe (ByT -> ByT)",
        "    }",
        "",
        "($$) :: ByT -> ByT -> ByT",
        "infixr 1 $$",
        "x $$ f = ByT bs (code r)",
        "    where",
        "        r = fromJust (code f) x",
        "        bs = case code r of",
        "            Nothing -> bits r",
        "            _ -> bits f ++ bits x",
        "",
        "var_0 :: ByT",
        "var_0 = ByT [False] $ Just $ \\x -> case x of",
        "    End -> ByT [] Nothing",
        "    _ -> ByT undefined $ Just $ \\y -> case y of",
        "        End -> ByT [] Nothing",
        "        _ -> ByT undefined $ Just $ \\z -> case z of",
        "            End -> ByT (bits y) Nothing",
        "            _ -> (z $$ y) $$ x",
        "",
        "var_1 :: ByT",
        "var_1 = ByT [True] $ Just $ \\x -> case x of",
        "    End -> ByT [] Nothing",
        "    _ -> ByT undefined $ Just $ \\y -> case y of",
        "        End -> ByT [] Nothing",
        "        _ -> x $$ y",
        "",
        "fromChar :: Char -> Bits",
        "fromChar char = map (testBit (fromEnum char)) [7, 6 .. 0]",
        "",
        "fromString :: String -> Bits",
        "fromString = concatMap fromChar",
        "",
        "toChar :: Bits -> Char",
        "toChar = toEnum . foldl (\\acc bit -> 2 * acc + bool 0 1 bit) 0 . take 8 . (++ repeat False)",
        "",
        "toString :: Bits -> String",
        "toString [] = \"\"",
        "toString bits = toChar (take 8 bits) : toString (drop 8 bits)",
        "",
        "mainStack :: ByT",
        "mainStack = var_main",
        "",
        "evalMainStack :: Bits -> Bits",
        "evalMainStack = bits . (End $$) . foldl (flip ($$)) mainStack . map (bool var_0 var_1)",
        "",
        "main :: IO ()",
        "main = interact $ takeWhile (/= '\\0') . toString . evalMainStack . fromString . (++ \"\\0\") . takeWhile (/= '\\0')",
        "",
        "",
        "{- USER CODE -}"
    ]
