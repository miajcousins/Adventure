module SaveLoad where

import World

import System.IO (hFlush, hSetBuffering, stdin, stdout, BufferMode(BlockBuffering, NoBuffering))
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Data.Aeson (eitherDecode, (.:), withObject, FromJSON(parseJSON), Value)
import qualified Data.Aeson.Types as A (Parser)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson.Key (fromString)

{-Function to load a previously saved game state. Prompts user for filename of file to load. -}

loadFile :: GameData -> IO (GameData, String)
loadFile gd = do
    putStr "Name of file to load: "
    hFlush stdout                                   -- Flush the standard output buffer + Disable buffering for standard input
    hSetBuffering stdin NoBuffering
    filename <- getLine
    hSetBuffering stdin $ BlockBuffering Nothing    -- Revert standard input buffering to its original state

    let filePath = "./savedGames/" ++ filename
    exists <- doesFileExist filePath
    if exists then
        do fileContents <- readFile filePath
           case eitherDecode (C.pack fileContents) :: Either String GameData of         -- Try parsing JSON from file
                Left err -> return (gd, "Invalid file")
                Right gameData -> return (gameData, "Successfully loaded game state from " ++ filename)
    else
        return (gd, "File does not exist")


evalBool :: String -> Bool
evalBool "True" = True
evalBool "False" = False

{- Parser to parse GameData record from JSON string -}

instance FromJSON GameData where
    parseJSON = withObject "GameData" $ \v -> do
        locationId <- v .: fromString "locationId"                          -- parses string value from 'locationId'
        world <- v .: fromString "world" >>= traverse parseWorldTuple       -- parses list of strings from 'world'/'inventory'
        inventory <- v .: fromString "inventory" >>= traverse parseJSON     -- then passes them to corresponding parsers 
        poured <- evalBool <$> v .: fromString "poured"                     -- parses string from 'poured'...'gameDark'
        caffeinated <- evalBool <$> v .: fromString "caffeinated"           -- then uses evalBool to determine boolean value
        lightsOn <- evalBool <$> v .: fromString "lightsOn"
        dressed <- evalBool <$> v .: fromString "dressed"
        finished <- evalBool <$> v .: fromString "finished"
        gotKeys <- evalBool <$> v .: fromString "gotKeys"
        brushed <- evalBool <$> v .: fromString "brushed"
        gameDark <- evalBool <$> v .: fromString "gameDark"
        return $ GameData {                                                 -- return GameData record
            locationId = locationId,
            world = world,
            inventory = inventory,
            poured = poured,
            caffeinated = caffeinated,
            lightsOn = lightsOn,
            dressed = dressed,
            finished = finished,
            gotKeys = gotKeys,
            brushed = brushed,
            gameDark = gameDark
        }

{- Parser to parse Room record from JSON string -}

instance FromJSON Room where
  parseJSON = withObject "Room" $ \v -> do                  
    roomDesc <- v .: fromString "roomDesc"                  -- parses string value from 'roomDesc'
    exits <- v .: fromString "exits" >>= parseJSON          -- parses list of string exit and object records and 
    objects <- v .: fromString "objects" >>= parseJSON      -- passes them to corresponding parsers
    return Room {                                           -- returns Room record
        roomDesc = roomDesc,
        exits = exits,
        objects = objects
    }

{- Parser to parse Exit record from JSON string -}

instance FromJSON Exit where
  parseJSON = withObject "Exit" $ \v -> do
    exitDir <- v .: fromString "exitDir"        -- parses string values from 'exitDir', 'exitDesc' and 'room'
    exitDesc <- v .: fromString "exitDesc"
    room <- v .: fromString "room"
    return Exit {                               -- returns Exit record
        exitDir = exitDir,
        exitDesc = exitDesc,
        room = room 
    }

{- Parser to parse Object record from JSON string -}

instance FromJSON World.Object where
  parseJSON = withObject "Object" $ \v -> do
    objName <- v .: fromString "objName"                -- parses string values from 'objName', 'objLongname', and 'objDesc'
    objLongname <- v .: fromString "objLongname"
    objDesc <- v .: fromString "objDesc"
    return Obj {                                        -- returns Obj record
        objName = objName,
        objLongname = objLongname,
        objDesc = objDesc 
    }

{- Parser to parse tuple of (GameData,String) from JSON string -}

parseWorldTuple :: Value -> A.Parser (String, Room)
parseWorldTuple = withObject "WorldTuple" $ \v -> do
    room_name <- v .: fromString "roomId"
    room <- v .: fromString "room"
    return (room_name, room)            -- returns tuple (GameData,String)


saveToFile :: GameData -> IO String
saveToFile gd = do 
    putStr "Save under filename: "
    hFlush stdout                                       -- Flush the standard output buffer
    hSetBuffering stdin NoBuffering                     -- Disable buffering for standard input
    filename <- getLine
    hSetBuffering stdin $ BlockBuffering Nothing        -- Revert standard input buffering to its original state

    createDirectoryIfMissing True "savedGames"          -- find / create directory to save file into
    let filePath = "./savedGames/" ++ filename
    let content = stringifyGameData gd

    writeFile filePath content
    return ("Successfully wrote to " ++ filename)

{- function to convert GameData record to string -}

stringifyGameData :: GameData -> String
stringifyGameData gd =
    "{\"locationId\":\"" ++ locationId gd ++ "\",\"world\":[" ++ worldString ++ "]," ++
    "\"inventory\":[" ++ inventoryString ++ "],\"poured\":\"" ++ show (poured gd) ++ "\",\"caffeinated\":\"" ++ show (caffeinated gd) ++
    "\",\"lightsOn\":\"" ++ show (lightsOn gd) ++ "\",\"dressed\":\"" ++ show (dressed gd) ++ "\",\"finished\":\"" ++ show (finished gd) ++
    "\",\"gotKeys\":\"" ++ show (gotKeys gd) ++ "\",\"brushed\":\"" ++ show (brushed gd) ++ "\",\"gameDark\":\"" ++ show (gameDark gd) ++ "\"}"
    where
        worldString = foldr (\(n,r) a -> concatenateStrings ("{\"roomId\":\"" ++ n ++ "\",\"room\":" ++ stringifyRoom r ++ "}") a) "" (world gd)
        inventoryString = foldr (\o a -> concatenateStrings (stringifyObject o) a) "" (inventory gd)

{- function to convert Object record to string -}

stringifyObject :: World.Object -> String
stringifyObject o =
    "{\"objName\":\"" ++ objName o ++ "\",\"objLongname\":\"" ++ objLongname o ++ "\",\"objDesc\":\"" ++ objDesc o ++ "\"}"

{- function to convert Room record to string -}

stringifyRoom :: Room -> String
stringifyRoom room =
    "{\"roomDesc\":\"" ++ roomDesc room ++ "\",\"exits\":[" ++ exitsString ++ "],\"objects\":[" ++ objectsString ++ "]}"
    where
        exitsString = foldr (\r a -> concatenateStrings (stringifyExit r) a) "" (exits room)
        objectsString = foldr (\r a -> concatenateStrings (stringifyObject r) a) "" (objects room)

{- function to convert Exit record to string -}

stringifyExit :: Exit -> String
stringifyExit e =
    "{\"exitDir\":\"" ++ exitDir e ++ "\",\"exitDesc\":\"" ++ exitDesc e ++ "\",\"room\":\"" ++ room e ++ "\"}"

{- function to append String to stringified list -}

concatenateStrings :: String -> String -> String
concatenateStrings r a | a == "" = r
                        | otherwise = r ++ "," ++ a