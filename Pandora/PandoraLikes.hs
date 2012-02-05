{-# LANGUAGE DeriveDataTypeable, Arrows #-}

module Pandora.PandoraLikes (Track(..), Station(..),
		StationId, SortOrder(..), SortKey(..), TrackRequest,
		makeTrackRequest, getLikedTracks, defaultTrackRequest) where

import Network.HTTP (getResponseBody, getRequest, simpleHTTP)
import Text.XML.HXT.Core
import Data.List.Split (splitEvery, splitOneOf)
import Data.Maybe (listToMaybe)
import Data.List (intercalate)
import Data.Typeable
import Data.Data
import Text.Regex.PCRE
import Text.Regex.PCRE.String

data Track = Track {trackTitle :: String, trackArtist :: String, trackDate :: String}
	deriving (Show, Eq, Typeable, Data)

data Station = Station {stationName :: String, stationId :: StationId}
	deriving (Show, Eq, Typeable, Data)

type FeedbackIndex = Int

type StationId = String

type TrackRequest = FeedbackIndex -> String

data SortOrder = Ascending | Descending
	deriving (Show, Eq)

data SortKey = Artist | Date
	deriving (Show, Eq)


openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

parseHTML = readString [withValidate no,
						withParseHTML yes,
						withWarnings no]

pandoraDate :: String -> Bool
pandoraDate s = s =~ "\\d{2}-\\d{2}-\\d{4}"

extractTracks :: String -> IO [Track]
extractTracks html = do
				nameArtists <- runX $ parseHTML html >>> deep (isElem >>> hasName "a" >>> getChildren >>> getText)
				dates <- runX $ parseHTML html >>> deep ((isText >>> getText >>> isA pandoraDate))
				return [Track n a d | [d, n, a] <- zipWith (:) dates (splitEvery 2 nameArtists)]

extractStations :: String -> IO [Station]
extractStations html = do
				links <- runX $ parseHTML html >>> deep (isElem >>> hasName "h3" >>> getChildren >>> getChildren >>> isElem >>> hasName "a" >>> getAttrValue "href")
				names <- runX $ parseHTML html >>> deep (isElem >>> hasName "h3" >>> getChildren >>> getChildren >>> isElem >>> hasName "a" >>> getChildren >>> getText)
				let sids = map (last . splitOneOf "/") links
				return $ zipWith Station names sids

getFeedbackIndex :: String -> IO (Maybe FeedbackIndex)
getFeedbackIndex html = do
					s <- runX $ parseHTML html >>> deep (isElem >>> hasAttr "data-nextstartindex" >>> getAttrValue "data-nextstartindex")
					return $ listToMaybe s >>= return . read

makeTrackRequest :: StationId -> SortOrder -> SortKey -> TrackRequest
makeTrackRequest sid so sk fbi = base ++ intercalate "&" [stationIdFragment, feedbackIndexFragment, sortOrderFragment, sortKeyFragment]
			where 	base =  "http://www.pandora.com/content/station_track_thumbs?"
				stationIdFragment = "stationId=" ++ sid
				feedbackIndexFragment = "posFeedbackStartIndex=" ++ (show fbi)
				sortOrderFragment = "posSortAsc=" ++ case so of
						Ascending -> "true"
						Descending -> "false"
				sortKeyFragment = "posSortBy=" ++ case sk of
						Date -> "date"
						Artist -> "artist"

makeStationsRequest :: String -> String
makeStationsRequest = (++) "http://www.pandora.com/content/stations?webname="

defaultTrackRequest :: StationId -> TrackRequest
defaultTrackRequest sid = makeTrackRequest sid Descending Date

getLikedTracks' :: TrackRequest -> Maybe FeedbackIndex -> IO [Track] -> IO [Track]
getLikedTracks' _   Nothing  tracks = tracks
getLikedTracks' req (Just fbi) tracks = do
					html <- openURL $ req fbi
					tracks' <- extractTracks html
					fbi' <- getFeedbackIndex html
					getLikedTracks' req fbi' (fmap (++ tracks') tracks)


getLikedTracks :: TrackRequest -> IO [Track]
getLikedTracks req = getLikedTracks' req (Just 0) (return [])


getStations :: String -> IO [Station]
getStations username = do
				html <- openURL $ makeStationsRequest username
				extractStations html
