{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}

module Pandora.PandoraLikes (PandoraRequest, Track(..),
		StationId, SortOrder(..), SortKey(..),
		getLikedTracks, requestByUser, requestByUserBookmarks, requestByStation, simpleRequestByStation) where

-- import Network.HTTP.Enumerator (simpleHttp)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Text.HTML.TagSoup
import Data.List.Split (splitOneOf)
import Data.Maybe (listToMaybe)
import Data.List (intercalate, isInfixOf)
import Data.Typeable
import Data.Data
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SB
import Control.Monad.State.Strict

data Track = Track {name :: String, artist :: String, date :: Maybe String}
	deriving (Show, Eq, Typeable, Data)

type TrackHTMLFragment = [Tag String]
type FeedbackIndex = Int
type Request = FeedbackIndex -> String

data PandoraRequest = PandoraRequest {trackSplitter :: [Tag String] -> [TrackHTMLFragment],
						trackMaker :: TrackHTMLFragment -> Track,
						request :: Request}

type StationId = String

data SortOrder = Ascending | Descending
	deriving (Show, Eq)

data SortKey = Artist | Date
	deriving (Show, Eq)


openURL :: String -> IO String
-- openURL x = simpleHttp x >>= return . SB.unpack . SB.concat . LB.toChunks
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

matchTagAndClass :: String -> String -> Tag String -> Bool
matchTagAndClass t c x = isTagOpenName t x && c `isInfixOf` fromAttrib "class" x

tracksByStationSplitter :: [Tag String] -> [TrackHTMLFragment]
tracksByStationSplitter tags = partitions p tags
	where	p (TagOpen tag _) 	= tag == "li"
		p (TagText "Show More")	= True
		p _			= False

tracksByUserSplitter :: [Tag String] -> [TrackHTMLFragment]
tracksByUserSplitter = partitions $ matchTagAndClass "div" "infobox-body"


{-
 - (partitions (\x -> isTagOpenName "div" x && "infobox-body" `isInfixOf` fromAttrib "class" x))
 - take 2 $ wordsBy (flip elem "\n\t")  $ innerText
 -}

makeByStationTrack :: TrackHTMLFragment -> Track
makeByStationTrack thf = Track (rawInfo !! 0) (removeBy $ rawInfo !! 1) (Just $ rawInfo !! 2)
		where 	f = filter (/= "") . splitOneOf "\n\t\160" . innerText
			removeBy = drop 3
			rawInfo = f thf

makeByUserTrack thf = (makeByStationTrack thf) {date = Nothing}


getFeedbackIndex :: [Tag String] -> Maybe FeedbackIndex
getFeedbackIndex tags = maybe Nothing (Just . read . fromAttrib "data-nextStartIndex") showMore
			where 	showMore = listToMaybe (filter (\x -> isTagOpenName "div" x && fromAttrib "class" x == "show_more") tags)

makeRequestByStationString :: StationId -> SortOrder -> SortKey -> FeedbackIndex -> String
makeRequestByStationString sid so sk fbi = base ++ intercalate "&" [stationIdFragment, feedbackIndexFragment, sortOrderFragment, sortKeyFragment]
			where 	base =  "http://www.pandora.com/content/station_track_thumbs?"
				stationIdFragment = "stationId=" ++ sid
				feedbackIndexFragment = "posFeedbackStartIndex=" ++ (show fbi)
				sortOrderFragment = "posSortAsc=" ++ case so of
						Ascending -> "true"
						Descending -> "false"
				sortKeyFragment = "posSortBy=" ++ case sk of
						Date -> "date"
						Artist -> "artist"

mrus :: String -> String -> FeedbackIndex -> String
mrus base user fbi = base ++ intercalate "&" [userFragment, feedbackIndexFragment] where
							userFragment = "webname=" ++ user
							feedbackIndexFragment = "trackStartIndex=" ++ show fbi

makeRequestByUserString = mrus "http://www.pandora.com/content/tracklikes?"
makeRequestByUserBookmarkString = mrus "http://www.pandora.com/content/bookmarked_tracks?"

requestByStation :: StationId -> SortOrder -> SortKey -> PandoraRequest
requestByStation sid so sk = PandoraRequest {
								request = makeRequestByStationString sid so sk,
								trackSplitter = tracksByStationSplitter,
								trackMaker = makeByStationTrack }

requestByUser :: String -> PandoraRequest
requestByUser u = PandoraRequest {
					request = makeRequestByUserString u,
					trackSplitter = tracksByUserSplitter,
					trackMaker = makeByUserTrack}

requestByUserBookmarks u = (requestByUser u) {request = makeRequestByUserBookmarkString u}

simpleRequestByStation:: StationId -> PandoraRequest
simpleRequestByStation sid = requestByStation sid Descending Date




getNext' :: PandoraRequest -> (Maybe FeedbackIndex) -> IO ([Track], Maybe FeedbackIndex)
getNext' preq (Just fbi) = do
								tags <- fmap parseTags $ openURL (request preq fbi)
								let tracks = map (trackMaker preq) $ trackSplitter preq tags
								let fbi' = getFeedbackIndex tags
								return (tracks, fbi')
getNext' _ Nothing = return ([], Nothing)

getNext :: PandoraRequest -> StateT (Maybe FeedbackIndex) IO [Track]
getNext preq = StateT $ getNext' preq


runUntil :: Monad m => (a -> Bool) -> StateT a m b -> a -> m [(b, a)]
runUntil p st i = if p i then return [] else do
		k@(val, i') <- runStateT st i
		liftM (return k ++) $ runUntil p st i'

evalUntil p st i = liftM (map fst) $ runUntil p st i
execUntil p st i = liftM (map snd) $ runUntil p st i

runWhile = runUntil . (not .)
evalWhile p st i = liftM (map fst) $ runWhile p st i
execWhile p st i = liftM (map snd) $ runWhile p st i

		-- if p fbi' then return [tracks'] else liftM ([tracks'] ++) runUntil p $ return fbi'

getLikedTracks :: PandoraRequest -> IO [Track]
getLikedTracks preq = liftM concat $ evalUntil (== Nothing) (getNext preq) (Just 0)
