import Data.List
import Data.List.Split
import System.Environment
import System.IO
import System.IO.Error
import Tag

tagElemAssoc = [ (TagKindClass,      "Structure")
               , (TagKindDefine,     "Define")
               , (TagKindEnumerator, "Constant")
               , (TagKindFunction,   "Function")
               , (TagKindEnumName,   "Structure")
               , (TagKindPrototype,  "Function")
               , (TagKindTypedef,    "Type")
               , (TagKindUnionName,  "Structure")
               ]

getHighlightClass t m = fmap snd $ (find (\x -> (fst x) == t) m)

getFilename = do
    argList <- getArgs
    if (length argList /= 1)
       then ioError $ userError "Usage: tagdeco <tagfile>"
       else return (head argList)

bucketKinds tags = 
    let kinds = map fst tagElemAssoc in
    filter (not.null) $ [filter ((==k) . getTagKind) tags | k <- kinds]

genKeywordList [] = ""
genKeywordList ts = let keywords  = unwords $ map tagRecordName ts
                        groupName = kindString (head ts)
                    in
                    "syn keyword " ++ groupName ++ " " ++ keywords
                    where
                    kindString = humanReadableField . getTagKindField
                    
tagStringsToKeywordList tagStrs =
    let tags    = map parseTag tagStrs
        buckets = bucketKinds tags
    in
    map genKeywordList buckets

genHiLine (tagKind, storSpec) =
    let sc = humanReadableKind tagKind in
    "hi def link " ++ sc ++ " " ++ storSpec

main = do
    tagStrs  <- fmap (dropComments . lines) $ getFilename >>= readFile
    let keywords = tagStringsToKeywordList tagStrs
    mapM_ putStrLn keywords
    mapM_ (putStrLn . genHiLine) tagElemAssoc
    where
    dropComments = dropWhile (\x -> (head x) == '!')
