import System.Environment
import qualified Text.JSON.Generic as JSON (toJSON)
import qualified Text.JSON as JSON (encode)

import PandoraLikes

usage :: IO ()
usage = do
	putStrLn "Usage:"
	putStrLn "PandoraLikes <station_id>"

main = do
	args <- getArgs
	case (length args) of
		1 -> do
			let stationId = (args !! 0)
			tracks <- getLikedTracks (makeRequestString stationId Descending Date)
			putStrLn $ JSON.encode $ JSON.toJSON tracks
		otherwise -> usage

