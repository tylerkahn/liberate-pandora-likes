{-# LANGUAGE DeriveDataTypeable #-}

module Pandora.PandoraLikes (Track(..),
		StationId, SortOrder(..), SortKey(..),
		makeRequestString, getLikedTracks) where

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

data SortOrder = Ascending | Descending
	deriving (Show, Eq)

data SortKey = Artist | Date
	deriving (Show, Eq)


atoi :: String -> Int
atoi s = read s :: Int

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
getFeedbackIndex tags = maybe Nothing (Just . atoi . fromAttrib "data-nextStartIndex") showMore
			where 	showMore = listToMaybe (filter (\x -> isTagOpenName "div" x && fromAttrib "class" x == "show_more") tags)

makeRequestString :: StationId -> SortOrder -> SortKey -> FeedbackIndex -> String
makeRequestString sid so sk fbi = base ++ intercalate "&" [stationIdFragment, feedbackIndexFragment, sortOrderFragment, sortKeyFragment]
			where 	base =  "http://www.pandora.com/content/station_track_thumbs?"
				stationIdFragment = "stationId=" ++ sid
				feedbackIndexFragment = "posFeedbackStartIndex=" ++ (show fbi)
				sortOrderFragment = "posSortAsc=" ++ case so of
						Ascending -> "true"
						Descending -> "false"
				sortKeyFragment = "posSortBy=" ++ case sk of
						Date -> "date"
						Artist -> "artist"


getLikedTracks' :: (FeedbackIndex -> String) -> Maybe FeedbackIndex -> IO [Track] -> IO [Track]
getLikedTracks' _   Nothing  tracks = tracks
getLikedTracks' req (Just fbi) tracks = do
					tags <- fmap parseTags $ openURL (req fbi)
					let tracks' = map makeTrack $ splitIntoTrackSections tags
					let fbi' = getFeedbackIndex tags
					getLikedTracks' req fbi' (fmap (++ tracks') tracks)


getLikedTracks :: (FeedbackIndex -> String) -> IO [Track]
getLikedTracks req = getLikedTracks' req (Just 0) (return [])
