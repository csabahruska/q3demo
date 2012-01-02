module Q3Demo.Loader.Zip where

import System.Time ( toUTCTime, addToClockTime, CalendarTime (..), ClockTime (..), TimeDiff (..) )
import Data.Bits ( shiftL, shiftR, (.&.) )
import Data.Binary
import Data.Binary.Get
import Data.List ( nub, find, filter, isPrefixOf )
import System.FilePath
import Control.Monad ( when, unless, zipWithM, liftM )
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map as M

-- from bytestring
import qualified Data.ByteString.Lazy as B

-- from utf8-string
import Data.ByteString.Lazy.UTF8 ( toString, fromString )

-- from zlib
import qualified Codec.Compression.Zlib.Raw as Zlib


data Archive = Archive
                { zEntries                :: [Entry]              -- ^ Files in zip archive
                , zSignature              :: Maybe B.ByteString   -- ^ Digital signature
                , zComment                :: B.ByteString         -- ^ Comment for whole zip archive
                } deriving (Read, Show)

data Entry = Entry
               { eRelativePath            :: FilePath            -- ^ Relative path, using '/' as separator
               , eCompressionMethod       :: CompressionMethod   -- ^ Compression method
               , eLastModified            :: Integer             -- ^ Modification time (seconds since unix epoch)
               , eCRC32                   :: Word32              -- ^ CRC32 checksum
               , eCompressedSize          :: Word32              -- ^ Compressed size in bytes
               , eUncompressedSize        :: Word32              -- ^ Uncompressed size in bytes
               , eExtraField              :: B.ByteString        -- ^ Extra field - unused by this library
               , eFileComment             :: B.ByteString        -- ^ File comment - unused by this library
               , eInternalFileAttributes  :: Word16              -- ^ Internal file attributes - unused by this library
               , eExternalFileAttributes  :: Word32              -- ^ External file attributes (system-dependent)
               , eCompressedData          :: B.ByteString        -- ^ Compressed contents of file
               } deriving (Read, Show, Eq)

data CompressionMethod = Deflate
                       | NoCompression
                       deriving (Read, Show, Eq)

data ZipOption = OptRecursive               -- ^ Recurse into directories when adding files
               | OptVerbose                 -- ^ Print information to stderr
               deriving (Read, Show, Eq)

toArchive :: B.ByteString -> Archive
toArchive = runGet getArchive

filesInArchive :: Archive -> [FilePath]
filesInArchive = (map eRelativePath) . zEntries

findEntriesByPath :: FilePath -> Archive -> [Entry]
findEntriesByPath path archive = filter (\e -> isPrefixOf path (eRelativePath e)) (zEntries archive)

findEntryByPath :: FilePath -> Archive -> Maybe Entry
findEntryByPath path archive = find (\e -> path == eRelativePath e) (zEntries archive)

fromEntry :: Entry -> B.ByteString
fromEntry entry =
  let uncompressedData = decompressData (eCompressionMethod entry) (eCompressedData entry)
  in  if eCRC32 entry == CRC32.crc32 uncompressedData
         then uncompressedData
         else error "CRC32 mismatch"

--------------------------------------------------------------------------------
-- Internal functions for reading and writing zip binary format.

-- | Perform a sequence of actions until one returns Nothing;
-- return list of results.
many :: Monad m => m (Maybe a) -> m [a]
many p = do
  r <- p
  case r of
       Just x  ->  many p >>= return . (x:)
       Nothing -> return []

-- Note that even on Windows, zip files use "/" internally as path separator.
zipifyFilePath :: FilePath -> String
zipifyFilePath path =
  let dir = takeDirectory path
      fn  = takeFileName path
      (_drive, dir') = splitDrive dir
      -- note: some versions of filepath return ["."] if no dir
      dirParts = dropWhile (==".") $ splitDirectories dir'
  in  (concat (map (++ "/") dirParts)) ++ fn

-- | Compress a lazy bytestring.
decompressData :: CompressionMethod -> B.ByteString -> B.ByteString
decompressData Deflate       = Zlib.decompress
decompressData NoCompression = id

-- | MSDOS datetime: a pair of Word16s (date, time) with the following structure:
--
-- > DATE bit     0 - 4           5 - 8           9 - 15
-- >      value   day (1 - 31)    month (1 - 12)  years from 1980
-- > TIME bit     0 - 4           5 - 10          11 - 15
-- >      value   seconds*        minute          hour
-- >              *stored in two-second increments
--
data MSDOSDateTime = MSDOSDateTime { msDOSDate :: Word16
                                   , msDOSTime :: Word16
                                   } deriving (Read, Show, Eq)

-- | Epoch time corresponding to the minimum DOS DateTime (Jan 1 1980 00:00:00).
minMSDOSDateTime :: Integer
minMSDOSDateTime = 315532800

-- | Convert a clock time to a MSDOS datetime.  The MSDOS time will be relative to UTC.
epochTimeToMSDOSDateTime :: Integer -> MSDOSDateTime
epochTimeToMSDOSDateTime epochtime | epochtime < minMSDOSDateTime =
  epochTimeToMSDOSDateTime minMSDOSDateTime
  -- if time is earlier than minimum DOS datetime, return minimum
epochTimeToMSDOSDateTime epochtime =
  let ut = toUTCTime (TOD epochtime 0)
      dosTime = toEnum $ (ctSec ut `div` 2) + shiftL (ctMin ut) 5 + shiftL (ctHour ut) 11
      dosDate = toEnum $ ctDay ut + shiftL (fromEnum (ctMonth ut) + 1) 5 + shiftL (ctYear ut - 1980) 9
  in  MSDOSDateTime { msDOSDate = dosDate, msDOSTime = dosTime }

-- | Convert a MSDOS datetime to a 'ClockTime'.
msDOSDateTimeToEpochTime :: MSDOSDateTime -> Integer
msDOSDateTimeToEpochTime (MSDOSDateTime {msDOSDate = dosDate, msDOSTime = dosTime}) =
  let seconds = fromIntegral $ 2 * (dosTime .&. 0O37)
      minutes = fromIntegral $ (shiftR dosTime 5) .&. 0O77
      hour    = fromIntegral $ shiftR dosTime 11
      day     = fromIntegral $ dosDate .&. 0O37
      month   = fromIntegral $ ((shiftR dosDate 5) .&. 0O17) - 1
      year    = fromIntegral $ shiftR dosDate 9
      timeSinceEpoch = TimeDiff
               { tdYear = year + 10, -- dos times since 1980, unix epoch starts 1970
                 tdMonth = month,
                 tdDay = day - 1,  -- dos days start from 1
                 tdHour = hour,
                 tdMin = minutes,
                 tdSec = seconds,
                 tdPicosec = 0 }
      (TOD epochsecs _) = addToClockTime timeSinceEpoch (TOD 0 0)
  in  epochsecs

-- A zip file has the following format (*'d items are not supported in this implementation):
--
-- >   [local file header 1]
-- >   [file data 1]
-- >   [data descriptor 1*]
-- >   .
-- >   .
-- >   .
-- >   [local file header n]
-- >   [file data n]
-- >   [data descriptor n*]
-- >   [archive decryption header*]
-- >   [archive extra data record*]
-- >   [central directory]
-- >   [zip64 end of central directory record*]
-- >   [zip64 end of central directory locator*]
-- >   [end of central directory record]
--
-- Files stored in arbitrary order.  All values are stored in
-- little-endian byte order unless otherwise specified.
--
--  Central directory structure:
--
-- >   [file header 1]
-- >   .
-- >   .
-- >   .
-- >   [file header n]
-- >   [digital signature]
--
--  End of central directory record:
--
-- >   end of central dir signature    4 bytes  (0x06054b50)
-- >   number of this disk             2 bytes
-- >   number of the disk with the
-- >   start of the central directory  2 bytes
-- >   total number of entries in the
-- >   central directory on this disk  2 bytes
-- >   total number of entries in
-- >   the central directory           2 bytes
-- >   size of the central directory   4 bytes
-- >   offset of start of central
-- >   directory with respect to
-- >   the starting disk number        4 bytes
-- >   .ZIP file comment length        2 bytes
-- >   .ZIP file comment       (variable size)

getArchive :: Get Archive
getArchive = do
  locals <- many getLocalFile
  files <- many (getFileHeader (M.fromList locals))
  digSig <- lookAheadM getDigitalSignature
  endSig <- getWord32le
  unless (endSig == 0x06054b50) $ fail "Did not find end of central directory signature"
  skip 2 -- disk number
  skip 2 -- disk number of central directory
  skip 2 -- num entries on this disk
  skip 2 -- num entries in central directory
  skip 4 -- central directory size
  skip 4 -- offset of central directory
  commentLength <- getWord16le
  zipComment <- getLazyByteString (toEnum $ fromEnum commentLength)
  return $ Archive
           { zEntries                = files
           , zSignature              = digSig
           , zComment                = zipComment
           }

-- Local file header:
--
-- >    local file header signature     4 bytes  (0x04034b50)
-- >    version needed to extract       2 bytes
-- >    general purpose bit flag        2 bytes
-- >    compression method              2 bytes
-- >    last mod file time              2 bytes
-- >    last mod file date              2 bytes
-- >    crc-32                          4 bytes
-- >    compressed size                 4 bytes
-- >    uncompressed size               4 bytes
-- >    file name length                2 bytes
-- >    extra field length              2 bytes
--
-- >    file name (variable size)
-- >    extra field (variable size)
--
-- Note that if bit 3 of the general purpose bit flag is set, then the
-- compressed size will be 0 and the size will be stored instead in a
-- data descriptor record AFTER the file contents. The record normally
-- begins with the signature 0x08074b50, then 4 bytes crc-32, 4 bytes
-- compressed size, 4 bytes uncompressed size.

getLocalFile :: Get (Maybe (Word32, B.ByteString))
getLocalFile = do
  sig <- lookAhead getWord32le
  if sig /= 0x04034b50
    then return Nothing
    else do
      offset <- bytesRead
      skip 4  -- signature
      skip 2  -- version
      bitflag <- getWord16le
      skip 2  -- compressionMethod
      skip 2  -- last mod file time
      skip 2  -- last mod file date
      skip 4  -- crc32
      compressedSize <- getWord32le
      when (compressedSize == 0xFFFFFFFF) $
        fail "Can't read ZIP64 archive."
      skip 4  -- uncompressedsize
      fileNameLength <- getWord16le
      extraFieldLength <- getWord16le
      skip (fromIntegral fileNameLength)  -- filename
      skip (fromIntegral extraFieldLength) -- extra field
      compressedData <- if bitflag .&. 0O10 == 0
          then getLazyByteString (fromIntegral compressedSize)
          else -- If bit 3 of general purpose bit flag is set,
               -- then we need to read until we get to the
               -- data descriptor record.  We assume that the
               -- record has signature 0x08074b50; this is not required
               -- by the specification but is common.
               do raw <- many $ do
                           s <- lookAhead getWord32le
                           if s == 0x08074b50
                              then return Nothing
                              else liftM Just getWord8
                  skip 4 -- signature
                  skip 4 -- crc32
                  cs <- getWord32le  -- compressed size
                  skip 4 -- uncompressed size
                  if fromIntegral cs == length raw
                     then return $ B.pack raw
                     else fail "Content size mismatch in data descriptor record" 
      return $ Just (fromIntegral offset, compressedData)

-- File header structure:
--
-- >    central file header signature   4 bytes  (0x02014b50)
-- >    version made by                 2 bytes
-- >    version needed to extract       2 bytes
-- >    general purpose bit flag        2 bytes
-- >    compression method              2 bytes
-- >    last mod file time              2 bytes
-- >    last mod file date              2 bytes
-- >    crc-32                          4 bytes
-- >    compressed size                 4 bytes
-- >    uncompressed size               4 bytes
-- >    file name length                2 bytes
-- >    extra field length              2 bytes
-- >    file comment length             2 bytes
-- >    disk number start               2 bytes
-- >    internal file attributes        2 bytes
-- >    external file attributes        4 bytes
-- >    relative offset of local header 4 bytes
--
-- >    file name (variable size)
-- >    extra field (variable size)
-- >    file comment (variable size)

getFileHeader :: M.Map Word32 B.ByteString -- ^ map of (offset, content) pairs returned by getLocalFile
              -> Get (Maybe Entry)
getFileHeader locals = do
  sig <- lookAhead getWord32le
  if sig /= 0x02014b50
     then return Nothing
     else do
       skip 4 -- skip past signature
       skip 2 -- version made by
       versionNeededToExtract <- getWord16le
       unless (versionNeededToExtract <= 20) $
         fail "This archive requires zip >= 2.0 to extract."
       skip 2 -- general purpose bit flag
       rawCompressionMethod <- getWord16le
       compressionMethod <- case rawCompressionMethod of
                             0 -> return NoCompression
                             8 -> return Deflate
                             _ -> fail $ "Unknown compression method " ++ show rawCompressionMethod
       lastModFileTime <- getWord16le
       lastModFileDate <- getWord16le
       crc32 <- getWord32le
       compressedSize <- getWord32le
       uncompressedSize <- getWord32le
       fileNameLength <- getWord16le
       extraFieldLength <- getWord16le
       fileCommentLength <- getWord16le
       skip 2 -- disk number start
       internalFileAttributes <- getWord16le
       externalFileAttributes <- getWord32le
       relativeOffset <- getWord32le
       fileName <- getLazyByteString (toEnum $ fromEnum fileNameLength)
       extraField <- getLazyByteString (toEnum $ fromEnum extraFieldLength)
       fileComment <- getLazyByteString (toEnum $ fromEnum fileCommentLength)
       compressedData <- case (M.lookup relativeOffset locals) of
                         Just x  -> return x
                         Nothing -> fail $ "Unable to find data at offset " ++ show relativeOffset
       return $ Just $ Entry
                 { eRelativePath            = toString fileName
                 , eCompressionMethod       = compressionMethod
                 , eLastModified            = msDOSDateTimeToEpochTime $
                                              MSDOSDateTime { msDOSDate = lastModFileDate,
                                                              msDOSTime = lastModFileTime }
                 , eCRC32                   = crc32
                 , eCompressedSize          = compressedSize
                 , eUncompressedSize        = uncompressedSize
                 , eExtraField              = extraField
                 , eFileComment             = fileComment
                 , eInternalFileAttributes  = internalFileAttributes
                 , eExternalFileAttributes  = externalFileAttributes
                 , eCompressedData          = compressedData
                 }

--  Digital signature:
--
-- >     header signature                4 bytes  (0x05054b50)
-- >     size of data                    2 bytes
-- >     signature data (variable size)

getDigitalSignature :: Get (Maybe B.ByteString)
getDigitalSignature = do
  hdrSig <- getWord32le
  if hdrSig /= 0x08064b50
     then return Nothing
     else do
        sigSize <- getWord16le
        getLazyByteString (toEnum $ fromEnum sigSize) >>= return . Just
