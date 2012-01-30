{-# LANGUAGE DeriveDataTypeable #-}

module Pandora.PandoraLikes (Track(..),
		StationId, SortOrder(..), SortKey(..), Request,
		makeRequest, getLikedTracks, defaultRequest) where

import Network.HTTP (getResponseBody, getRequest, simpleHTTP)
import Text.HTML.TagSoup
import Data.List.Split (splitOneOf)
import Data.Maybe (listToMaybe)
import Data.List (intercalate)
import Data.Typeable
import Data.Data


data Track = Track {name :: String, artist :: String, date :: String}
	deriving (Show, Eq, Typeable, Data)

type TrackHTMLFragment = [Tag String]
type FeedbackIndex = Int

type StationId = String

type Request = FeedbackIndex -> String

data SortOrder = Ascending | Descending
	deriving (Show, Eq)

data SortKey = Artist | Date
	deriving (Show, Eq)


openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

splitIntoTrackSections :: [Tag String] -> [TrackHTMLFragment]
splitIntoTrackSections tags = partitions p tags
	where	p (TagOpen tag _) 	= tag == "li"
		p (TagText "Show More")	= True
		p _			= False

makeTrack :: TrackHTMLFragment -> Track
makeTrack thf = Track (rawInfo !! 0) (removeBy $ rawInfo !! 1) (rawInfo !! 2)
		where 	f = filter (/= "") . splitOneOf "\n\t\160" . innerText
			removeBy = drop 3
			rawInfo = f thf


getFeedbackIndex :: [Tag String] -> Maybe FeedbackIndex
getFeedbackIndex tags = maybe Nothing (Just . read . fromAttrib "data-nextStartIndex") showMore
			where 	showMore = listToMaybe (filter (\x -> isTagOpenName "div" x && fromAttrib "class" x == "show_more") tags)

makeRequest :: StationId -> SortOrder -> SortKey -> Request
makeRequest sid so sk fbi = base ++ intercalate "&" [stationIdFragment, feedbackIndexFragment, sortOrderFragment, sortKeyFragment]
			where 	base =  "http://www.pandora.com/content/station_track_thumbs?"
				stationIdFragment = "stationId=" ++ sid
				feedbackIndexFragment = "posFeedbackStartIndex=" ++ (show fbi)
				sortOrderFragment = "posSortAsc=" ++ case so of
						Ascending -> "true"
						Descending -> "false"
				sortKeyFragment = "posSortBy=" ++ case sk of
						Date -> "date"
						Artist -> "artist"

defaultRequest :: StationId -> Request
defaultRequest sid = makeRequest sid Descending Date

getLikedTracks' :: Request -> Maybe FeedbackIndex -> IO [Track] -> IO [Track]
getLikedTracks' _   Nothing  tracks = tracks
getLikedTracks' req (Just fbi) tracks = do
					tags <- fmap parseTags $ openURL (req fbi)
					let tracks' = map makeTrack $ splitIntoTrackSections tags
					let fbi' = getFeedbackIndex tags
					getLikedTracks' req fbi' (fmap (++ tracks') tracks)


getLikedTracks :: Request -> IO [Track]
getLikedTracks req = getLikedTracks' req (Just 0) (return [])
