-- Imports
import Time (..)
import Signal (..)
import List (..)
import List
import Array (toList, fromList, push)
import Random (..)
import Signal
import Color (..)
import Window
import Graphics.Element (..)
import Graphics.Collage (..)
import Keyboard (..)
import Debug

{-- **** Inputs **** --}

type alias Input = { keys:List KeyCode }

delta : Signal Time
delta = inSeconds <~ fps 60

input : Signal Input
input = sampleOn delta <| Input <~ keysDown

{-- **** Model **** --}

(gameWidth, gameHeight) = (640, 350)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)

(minBuildingWidth, maxBuildingWidth) = (30, 120)
(minBuildingHeight, maxBuildingHeight) = (75, 200)

gap = 2

type State = Intro 
            | Pause
            | Turn Player
            | Win Player

type alias Object a = { a | x:Float, y:Float }

type alias Player = Object { score: Int, name:String }

type alias Banana = Object { vx:Float, vy:Float, displayed: Bool, rotation:Float }

type alias Building = Object { height:Float, width:Float }

type alias Skyline = List Building 

type alias Sun = Object { smiling:Bool }

type Direction = Left | Right

type alias Wind = { direction:Direction, strength:Float }

type Slope = Upward | Downward | V | InvertedV

type alias Game = 
    { state   : State
    , player1 : Player
    , player2 : Player
    , skyline : Skyline
    , wind    : Wind
    , banana  : Banana
    , gravity : Float
    , playTo  : Int
    , sun     : Sun 
    , seed    : Seed }

wind : Wind
wind = { direction=Left, strength=0.5 }

-- Create a list of building widths that packs buildings
-- between the walls of the game. 
-- minBuildingWidth < width < maxBuilidingWidth
-- Pass this a seed and an empty list
buildingWidths : List Float -> Seed -> (List Float, Seed)
buildingWidths widths seed = 
    let 
        total = foldr (+) 0 widths
        (next, seed') = generate (float minBuildingWidth maxBuildingWidth) seed
    in
        if total + next > gameWidth then (,) (gameWidth-total :: widths) seed' else buildingWidths (next :: widths) seed'

-- Create a list of building heights
-- Pass this a seed, the number of buildings, and an empty list
buildingHeights : Int -> List Float -> Seed -> (List Float, Seed)
buildingHeights numBuildings heights seed = 
    let
        (height, seed') = generate (float minBuildingHeight maxBuildingHeight) seed
    in
        if numBuildings == 0 then (heights, seed') else buildingHeights (numBuildings - 1) (height :: heights) seed'


-- Calculate x positions of buildings in the skyline
xPositions : List { a | width:Float } -> List Float -> List Float
xPositions skyline positions =
    let 
        total = gameWidth - foldr (\building next -> building.width + next) 0 skyline
    in
        if length skyline == 0 then positions else
            -- Have to convert skyline to an Array to push the new position to the end of the list
            -- and then convert back to a List to return
            xPositions (tail skyline)
            (toList 
                <| push ((halfWidth - (.width (head skyline))/2) - total) 
                (fromList positions))
            
yPositions : List { a | height:Float } -> List Float
yPositions skyline =
    List.map (\n -> (n.height/2) - halfHeight + 20) skyline

generateSkyline : Seed -> (Skyline, Seed)
generateSkyline seed = 
    let (widths, seed') = buildingWidths [] seed
        (heights, seed'') = buildingHeights (List.length widths) [] seed'
        buildings = List.map2 (\w h -> {height=h, width=w}) widths heights
        xPos = List.map2 (\x building -> {building | x=x}) (xPositions buildings []) buildings
    in
       (,) (List.map2 (\y building -> {building | y=y}) (yPositions buildings) xPos) seed''

player : Float -> Float -> String -> Player
player x y name = { x=x, y=y, score=0, name=name }

--positionPlayerLeft : Skyline -> Seed -> (Seed, Int, Int)

defaultGame : Game
defaultGame = 
    let seed = initialSeed 42
        (skyline, seed') = generateSkyline seed
    in
    { state   = Pause
    , player1 = player -60 0 "Player 1"
    , player2 = player 60 0 "Player 2"
    , skyline = List.map (\building -> {building | width <- building.width - gap}) skyline
    , wind    = wind
    , banana  = { x=0, y=0, vx=0, vy=0, displayed=False, rotation=0 }
    , gravity = 9.3
    , playTo  = 3
    , sun     = { x=0, y=halfHeight-40, smiling=True }
    , seed    = seed
    }




{-- **** Update **** --}

stepGame : Input -> Game -> Game
stepGame input game =
    Debug.watch "Game" game

gameState : Signal Game
gameState = foldp stepGame defaultGame input




{-- **** View **** --}



-- Render a skyline
-- the bottoms of the buildings will be aligned at y=0
renderSkyline : Skyline -> Form
renderSkyline skyline =
    group 
        <| List.map (\building -> move (building.x, building.y) (outlined (solid green) (rect building.width building.height))) skyline
        
displaySun : Sun -> Form
displaySun sun = 
    let 
        img = if sun.smiling then "images/sun-smile.png" else "images/sun-oface.png"
    in 
        move (sun.x, sun.y) <| toForm (image 43 33 img)

displayPlayer : Player -> Form
displayPlayer player =
   move (player.x, player.y) <| toForm (image 28 29 "images/gorilla.png")

gameCollage : (Int, Int) -> Game -> Element
gameCollage (w,h) game =
    let elements = 
        if | game.state == Intro -> 
            [ filled (rgb 0 0 0) (rect gameWidth gameHeight) ]
           | otherwise ->         
        [ filled (rgb 0 0 173) (rect gameWidth gameHeight)
        , renderSkyline game.skyline
        , displaySun game.sun
        , displayPlayer game.player1
        , displayPlayer game.player2
        ]
    in
        collage gameWidth gameHeight elements

display : (Int, Int) -> Game -> Element
display (w,h) game = 
    container w h middle <| gameCollage (w,h) game
main = Signal.map2 display Window.dimensions gameState