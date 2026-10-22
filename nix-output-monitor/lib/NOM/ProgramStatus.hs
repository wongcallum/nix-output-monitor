{- | Reporting nom’s progress to the terminal via the Program Status Protocol (OSC 7501).

https://www.superlogical.com/rex/docs/build/program-status
-}
module NOM.ProgramStatus (Report (..), Status (..), stateToReport, encodeReport) where

import Data.Bits ((.&.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.Char (isControl)
import Data.Text qualified as Text
import NOM.State (DependencySummary (..), NOMState (..), ProgressState (..))
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import Relude

data Status = Idle | Working | Done | Error
  deriving stock (Show, Eq)

data Report = MkReport
  { status :: Status
  , progress :: Maybe Int
  -- ^ Percentage from 0 to 100, Nothing when there is nothing to count yet.
  , message :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | The report for nom’s root record: working while nix runs, done or error once nom has finished.
stateToReport :: NOMState -> Report
stateToReport nom_state
  | isFinished =
      MkReport
        { status = if numFailedBuilds > 0 || numNixErrors > 0 then Error else Done
        , progress = Nothing
        , message
        }
  | otherwise =
      MkReport
        { status = Working
        , progress = guard (total > 0) $> numFinished * 100 `div` total
        , message
        }
 where
  MkDependencySummary{..} = nom_state.fullSummary
  isFinished = nom_state.progressState == Finished
  numFailedBuilds = CMap.size failedBuilds
  numNixErrors = length nom_state.nixErrors
  numFinishedBuilds = CMap.size completedBuilds + numFailedBuilds
  numBuilds = CSet.size plannedBuilds + CMap.size runningBuilds + numFinishedBuilds
  numDownloads = CSet.size plannedDownloads + CMap.size runningDownloads + CMap.size completedDownloads
  numUploads = CMap.size runningUploads + CMap.size completedUploads
  numFinished = numFinishedBuilds + CMap.size completedDownloads
  total = numBuilds + numDownloads
  message = guard (not (null counts)) $> Text.intercalate ", " counts
  counts =
    showCount (numBuilds > 0) (show (CMap.size completedBuilds) <> "/" <> show numBuilds <> " built")
      -- Builds nix gave up on stay running after nom has finished.
      <> showCount (not isFinished && CMap.size runningBuilds > 0) (show (CMap.size runningBuilds) <> " running")
      <> showCount (numDownloads > 0) (show (CMap.size completedDownloads) <> "/" <> show numDownloads <> " downloaded")
      -- We don’t know how many uploads to expect, so we can only count them.
      <> showCount (numUploads > 0) (show (CMap.size completedUploads) <> " uploaded")
      <> showCount (numFailedBuilds > 0) (show numFailedBuilds <> " failed")
      <> showCount (numNixErrors > 0) (show numNixErrors <> " nix error" <> memptyIfFalse (numNixErrors /= 1) "s")
  showCount :: Bool -> Text -> [Text]
  showCount cond = memptyIfFalse cond . one

-- | The escape sequence for a report.
encodeReport :: Report -> ByteString
encodeReport report =
  "\ESC]7501;"
    <> ByteString.intercalate
      ":"
      ( ["state=" <> state_name, "app=nom"]
          <> maybeToList (("progress=" <>) . show <$> report.progress)
          <> maybeToList (("msg=" <>) . encodeText <$> report.message)
      )
    <> "\ESC\\"
 where
  state_name = case report.status of
    Idle -> "idle"
    Working -> "working"
    Done -> "done"
    Error -> "error"
  -- Terminals discard the whole report if the decoded text contains control characters or is longer than 2048 bytes.
  encodeText = Base64.encode . truncateUtf8 2048 . encodeUtf8 . Text.map (\c -> if isControl c then ' ' else c)

-- | Cuts a UTF-8 string to at most the given number of bytes without splitting a character.
truncateUtf8 :: Int -> ByteString -> ByteString
truncateUtf8 limit bytes
  | ByteString.length bytes <= limit = bytes
  | otherwise = ByteString.take boundary bytes
 where
  boundary = fromMaybe 0 $ find startsCharacter [limit, limit - 1 .. 0]
  startsCharacter = maybe True ((/= 0x80) . (.&. 0xC0)) . ByteString.indexMaybe bytes
