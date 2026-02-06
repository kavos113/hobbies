{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

type Author = T.Text

type Title = T.Text

data Book = Book
  { author :: Author,
    title :: Title
  }
  deriving (Show)

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book =
  mconcat
    [ "<div>\n",
      titleTag,
      authorTag,
      "</div>\n"
    ]
  where
    titleTag = T.concat ["  <h1>", title book, "</h1>\n"]
    authorTag = T.concat ["  <p>by ", author book, "</p>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<!DOCTYPE html>\n",
      "<html>\n",
      "  <head>\n",
      "    <meta charset=\"UTF-8\">\n",
      "    <title>Book List</title>\n",
      "  </head>\n",
      "  <body>\n",
      booksHtml,
      "  </body>\n",
      "</html>\n"
    ]
  where
    booksHtml = (mconcat . map bookToHtml) books

book1 :: Book
book1 = Book {author = "George Orwell", title = "1984"}

book2 :: Book
book2 = Book {author = "だれか", title = "未知の物語"}

book3 :: Book
book3 = Book {author = "J.K. Rowling", title = "Harry Potter and the Philosopher's Stone"}

books :: [Book]
books = [book1, book2, book3]

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

pickRecord :: B.ByteString -> (MarcRecordRaw, B.ByteString)
pickRecord bs = B.splitAt recordLength bs
  where
    recordLength = getRecordLength (getLeader bs)

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords bs =
  if bs == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = pickRecord bs

type MarcDirectoryRaw = B.ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 (B.drop 12 leader))

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - leaderLength - 1

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take dirLength afterLeader
  where
    dirLength = getDirectoryLength (getLeader record)
    afterLeader = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory dirRaw =
  if dirRaw == B.empty
    then []
    else next : splitDirectory rest
  where
    (next, rest) = B.splitAt dirEntryLength dirRaw

data FieldMetaData = FieldMetaData
  { tag :: T.Text,
    fieldLength :: Int,
    fieldStart :: Int
  }
  deriving (Show)

makeFieldMetaData :: MarcDirectoryEntryRaw -> FieldMetaData
makeFieldMetaData entry = FieldMetaData tag len start
  where
    (rawTag, rest) = B.splitAt 3 entry
    (rawLen, rawStart) = B.splitAt 4 rest
    tag = E.decodeUtf8 rawTag
    len = rawToInt rawLen
    start = rawToInt rawStart

getFieldMetaData :: [MarcDirectoryEntryRaw] -> [FieldMetaData]
getFieldMetaData = map makeFieldMetaData

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetaData -> FieldText
getTextField record meta = E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength (getLeader record)
    baseAddress = getBaseAddress (getLeader record)
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart meta) baseRecord
    byteStringValue = B.take (fieldLength meta) baseAtEntry

fieldDelimiter :: Char
fieldDelimiter = '\x1E'

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "700"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetaData :: T.Text -> MarcRecordRaw -> Maybe FieldMetaData
lookupFieldMetaData aTag record =
  if null results
    then Nothing
    else Just (head results)
  where
    metadata = (getFieldMetaData . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

lookupSubField :: Maybe FieldMetaData -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubField Nothing _ _ = Nothing
lookupSubField (Just meta) subfield record =
  if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record meta
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) (filter (not . T.null) subfields)

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubField meta subfield record
  where
    meta = lookupFieldMetaData aTag record

lookupTitle :: MarcRecordRaw -> Maybe T.Text
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe T.Text
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs content = zip titles authors
  where
    records = allRecords content
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
  map
    ( \(title, author) ->
        Book
          { title = fromJust title,
            author = fromJust author
          }
    )
    justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

main :: IO ()
main = do
  args <- getArgs
  let inputFile =
        if not (null args)
          then head args
          else "marc/sample.dat"
  content <- B.readFile inputFile
  let processed = take 10 (marcToPairs content)
  print processed