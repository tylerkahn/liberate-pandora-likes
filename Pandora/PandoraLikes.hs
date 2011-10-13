{-# LANGUAGE DeriveDataTypeable #-}

module Pandora.PandoraLikes (Track(..),
		StationId, SortOrder(..), SortKey(..),
		getLikedTracks, requestByUser, requestByStation, simpleRequestByStation) where

import Network.HTTP (getResponseBody, getRequest, simpleHTTP)
import Text.HTML.TagSoup
import Data.List.Split (splitOneOf)
import Data.Maybe (listToMaybe)
import Data.List (intercalate, isInfixOf)
import Data.Typeable
import Data.Data


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

atoi :: String -> Int
atoi s = read s :: Int

openURL :: String -> IO String
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
getFeedbackIndex tags = maybe Nothing (Just . atoi . fromAttrib "data-nextStartIndex") showMore
			where 	showMore = listToMaybe $ filter (matchTagAndClass "div" "show_more") tags

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

makeRequestByUserString :: String -> FeedbackIndex -> String
makeRequestByUserString user fbi = base ++ intercalate "&" [userFragment, feedbackIndexFragment] where
							base = "http://www.pandora.com/content/tracklikes?"
							userFragment = "webname=" ++ user
							feedbackIndexFragment = "trackStartIndex=" ++ show fbi

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

simpleRequestByStation:: StationId -> PandoraRequest
simpleRequestByStation sid = requestByStation sid Descending Date

getLikedTracks' :: PandoraRequest -> Maybe FeedbackIndex -> IO [Track] -> IO [Track]
getLikedTracks' _   Nothing  tracks = tracks
getLikedTracks' preq (Just fbi) tracks = do
					tags <- fmap parseTags $ openURL (request preq fbi)
					let tracks' = map (trackMaker preq) $ trackSplitter preq tags
					let fbi' = getFeedbackIndex tags
					getLikedTracks' preq fbi' (fmap (++ tracks') tracks)


getLikedTracks :: PandoraRequest -> IO [Track]
getLikedTracks preq = getLikedTracks' preq (Just 0) (return [])
