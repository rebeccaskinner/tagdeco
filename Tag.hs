module Tag where
import Data.List
import Data.List.Split

data TagField = FieldArity  String  |
                FieldClass  String  |
                FieldEnum   String  |
                FieldFile   String  |
                FieldKind   TagKind |
                FieldStruct String  |
                FieldUnion  String  |
                FieldUnknown deriving (Eq)

data TagKind = TagKindClass      |
               TagKindDefine     |
               TagKindEnumerator |
               TagKindFunction   |
               TagKindFilename   |
               TagKindEnumName   |
               TagKindMember     |
               TagKindPrototype  |
               TagKindStructName |
               TagKindTypedef    |
               TagKindUnionName  |
               TagKindVariable   |
               TagKindUnknown deriving (Eq)

data TagRecord = TagRecord { tagRecordName    :: String
                           , tagRecordFile    :: String
                           , tagRecordAddress :: String
                           , tagRecordFields  :: [TagField]
                           }

instance Show TagField where
    show (FieldArity a)  = "arity:"++a
    show (FieldClass s)  = "class:"++s
    show (FieldEnum  e)  = "enum:"++e
    show (FieldFile  f)  = "file:"++f
    show (FieldKind  k)  = show k
    show (FieldStruct s) = "struct:"++s
    show (FieldUnion u)  = "union:"++u
    show FieldUnknown    = ""

instance Show TagKind where
    show TagKindClass      = "c"
    show TagKindDefine     = "d"
    show TagKindEnumerator = "e"
    show TagKindFunction   = "f"
    show TagKindFilename   = "F"
    show TagKindEnumName   = "g"
    show TagKindMember     = "m"
    show TagKindPrototype  = "p"
    show TagKindStructName = "s"
    show TagKindTypedef    = "t"
    show TagKindUnionName  = "u"
    show TagKindVariable   = "v"
    show TagKindUnknown    = ""

instance Show TagRecord where
    show (TagRecord name file address fields) =
        name ++ "\t" ++ file ++ "\t" ++ address ++ (makeFieldsStr fields)
        where
        makeFieldsStr [] = ""
        makeFieldsStr f  = "; \"\t" ++ (concat . intersperse "\t" . map show) f

humanReadableKind k = 
    case k of
         TagKindClass      -> "tagDecoClass"
         TagKindDefine     -> "tagDecoDefine"
         TagKindEnumerator -> "tagDecoEnumerator"
         TagKindFunction   -> "tagDecoFunction"
         TagKindFilename   -> "tagDecoFilename"
         TagKindEnumName   -> "tagDecoEnumName"
         TagKindMember     -> "tagDecoMember"
         TagKindPrototype  -> "tagDecoPrototype"
         TagKindStructName -> "tagDecoStructName"
         TagKindTypedef    -> "tagDecoTypedef"
         TagKindUnionName  -> "tagDecoUnionName"
         TagKindVariable   -> "tagDecoVariable"
         TagKindUnknown    -> "Unknown"

humanReadableField (FieldKind k) = humanReadableKind k
humanReadableField f = takeWhile (/= ':') $ show f

parseTagField f =
    case findIndex (==':') f of
         Nothing  -> FieldKind $ parseTagKind f
         Just idx -> let (field,value) = splitAt idx f in
                         parseFieldType field (tail value)
    where
    parseTagKind a =
        case a of
             "c" -> TagKindClass
             "d" -> TagKindDefine
             "e" -> TagKindEnumerator
             "f" -> TagKindFunction
             "F" -> TagKindFilename
             "g" -> TagKindEnumName
             "m" -> TagKindMember
             "p" -> TagKindPrototype
             "s" -> TagKindStructName
             "t" -> TagKindTypedef
             "u" -> TagKindUnionName
             "v" -> TagKindVariable
             _   -> TagKindUnknown
    parseFieldType f v =
        case f of
             "arity"  -> FieldArity v
             "class"  -> FieldClass v
             "enum"   -> FieldEnum v
             "file"   -> FieldFile v
             "kind"   -> FieldKind $ parseTagKind v
             "struct" -> FieldStruct v
             "union"  -> FieldUnion v
             _        -> FieldUnknown

parseTag s = 
    let (fore:aft) = splitOn ";\"" s
        (tagName : tagFile : tagAddr : _) = splitOn "\t" fore
        tagFields = wordsBy (=='\t') (head aft)

    in
    TagRecord tagName tagFile tagAddr (map parseTagField tagFields)

getTagKindField t =
    let fields = tagRecordFields t
        ks     = kinds fields in
    if ks == []
       then FieldKind TagKindUnknown
       else head (kinds fields)
    where
    kinds = filter isKind
    isKind (FieldKind _) = True
    isKind _ = False

getTagKind t = let (FieldKind k) = getTagKindField t in k

showTag t =
    (tagRecordName t) ++ " - " ++ (humanReadableField $ getTagKindField t)
