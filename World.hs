module World where

import Data.List (find)

-- Define the Object data type
data Object = Obj { objName :: String,
                    objLongname :: String,
                    objDesc :: String }
   deriving Eq

-- Make Object an instance of Show to customize its display
instance Show Object where
   show = objLongname

-- Define the Exit data type
data Exit = Exit { exitDir :: String,
                   exitDesc :: String,
                   room :: String }
   deriving Eq

-- Define the Room data type
data Room = Room { roomDesc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

-- Define the Direction data type
data Direction = North | South | East | West | Out | Up | Down
   deriving (Eq, Enum, Show)

-- Define the main game state data type
data GameData = GameData { locationId :: String,  -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object],   -- objects player has
                           poured :: Bool,           -- coffee is poured
                           caffeinated :: Bool,      -- coffee is drunk
                           lightsOn :: Bool,         -- lights are switched on
                           dressed :: Bool,          -- player is dressed
                           finished :: Bool,         -- set to True at the end
                           gotKeys :: Bool,          -- set to True when keys collected
                           brushed :: Bool,          -- teeth have been brushed
                           gameDark :: Bool          -- lights are turned on
                         }
   deriving Eq

-- Check if the player has won
won :: GameData -> Bool
won gd = locationId gd == "street"

-- Make Room an instance of Show to customize its display
instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exitDesc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs

-- Return a string representation of the room without displaying objects when lights are off
hideInv :: Room -> String
hideInv (Room desc exits objs) = desc ++ "\n" ++ concatMap exitDesc exits ++ "\n\nLights are off, you cannot see anything."

-- Make GameData an instance of Show to customize its display
instance Show GameData where
   show gd = if lightsOn gd then show (getCurrentRoom gd) else hideInv (getCurrentRoom gd)

-- Define types for game actions
type Action  = Object -> GameData -> (GameData, String)
type Move = Direction -> GameData -> (GameData, String)
type Command = GameData -> (GameData, String)
type IOCommand = GameData -> IO (GameData, String)

-- Define various game objects
mug, fullmug, coffeepot, keys, laptop, toothbrush, jeans, trainers, hoodie :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
keys = Obj "keys" "front door keys" "A set of keys to open the front door"
laptop = Obj "laptop" "a work laptop" "A laptop for making lecture notes"
toothbrush = Obj "toothbrush" "a toothbrush" "A toothbrush for getting rid of smelly breath"
jeans = Obj "jeans" "a pair of jeans" "A pair of distressed levi jeans"
trainers = Obj "trainers" "a pair of trainers" "A pair of Adidas sambas"
hoodie = Obj "hoodie" "a hoodie" "A vintage nike sweatshirt"

-- Define various game rooms
bedroom, kitchen, hall, street, livingroom, wardrobe, bathroom :: Room

-- Initialize rooms with descriptions, exits, and objects
bedroom = Room "You are in the bedroom. "
               [Exit "north" "To the north is a bathroom. " "bathroom",
                Exit "east" "To the east is the wardrobe. " "wardrobe",
                Exit "down" "Down the stairs is the hallway. " "hall"]
               [mug, laptop, jeans]

kitchen = Room "You are in the kitchen. "
               [Exit "south" "To the south is the living room. " "living room",
                Exit "west" "To the west is a hallway. " "hall"]
               [coffeepot]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit "east" "To the east is the kitchen. " "kitchen",
             Exit "up" "Up the stairs is the bedroom. " "bedroom"]
            [trainers]

livingroom = Room "You are in the living room. "
               [Exit "north" "To the north is the kitchen. " "kitchen"]
               [keys, hoodie]

wardrobe = Room "You are in the wardrobe. "
               [Exit "west" "To the west is the bedroom. " "bedroom"]
               []

bathroom = Room "You are in the bathroom. "
               [Exit "south" "To the south is the bedroom. " "bedroom"]
               [toothbrush]

-- Define new data for the hall when the door is opened
openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit "east" "To the east is a kitchen. " "kitchen",
               Exit "out" "You can go outside. " "street"]

-- Define the street room
street = Room "You have made it out of the house."
              []
              []

-- Define the game world as a list of room tuples
gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street),
             ("living room", livingroom),
             ("bathroom", bathroom),
             ("wardrobe", wardrobe)]

-- Define the initial game state
initState :: GameData
initState = GameData "bedroom" gameworld [] False False False False False False False False

-- Return the room the player is currently in
getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (locationId gd) (world gd))

-- Return the current room using find and pattern matching
getCurrentRoom :: GameData -> Room
getCurrentRoom gd = case find (\(x,_) -> x == (locationId gd)) (world gd) of
                           Just (_,room) -> room
                           Nothing -> bedroom
