-- Imports
import Time (..)
import Signal (..)
import List(..)
import List
import Array (get, fromList)
import Random (..)
import Signal
import Color (..)
import Window
import Graphics.Element (..)
import Graphics.Collage (..)
import Keyboard (..)
import Debug
import Maybe

{-- **** Inputs **** --}

type alias Input = { space:Bool, arrows: Int }

delta : Signal Time
delta = inSeconds <~ fps 60

input : Signal Input
input = sampleOn delta <| Input
                       <~ space
                        ~ (Signal.map .x arrows)

{-- **** Model **** --}

(gameWidth, gameHeight) = (640, 350)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(playerWidth, playerHeight) = (28, 29)

(minBuildingWidth, maxBuildingWidth) = (30, 120)
(minBuildingHeight, maxBuildingHeight) = (75, 200)

type State = Intro 
            | Pause
            | Turn Player
            | Win Player

type alias Object a = { a | x:Float, y:Float }

type alias Player = Object { height:Float, width:Float, score: Int, name:String }

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
    , seed    : Int
    , keyDown : Int }

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
        (this, seed') = generate (float minBuildingWidth maxBuildingWidth) seed
    in
        if total + this > gameWidth 
            then (,) widths seed' 
            else buildingWidths (append widths (this :: [])) seed'

-- Create a list of building heights
-- Pass this a seed, the number of buildings, and an empty list
buildingHeights : Int -> List Float -> Seed -> (List Float, Seed)
buildingHeights numBuildings heights seed = 
    let
        (height, seed') = generate (float minBuildingHeight maxBuildingHeight) seed
    in
        if numBuildings == 0 
            then (,) heights seed'
            else buildingHeights (numBuildings - 1) (append heights (height :: [])) seed'

-- Calculate x positions of buildings in the skyline
xPositions : List { a | width:Float } -> List Float
xPositions skyline =
    let 
        gap = (gameWidth - foldr (\cur prev -> prev + cur.width) 0 skyline) / (toFloat ((length skyline) + 1))
        widths = List.map .width skyline
        leftEdges = List.scanl (\a b -> a + b + gap) (-halfWidth + gap) widths
    in
      List.map2 (+) leftEdges (List.map (\n -> n/2) widths)

            
yPositions : List { a | height:Float } -> List Float
yPositions skyline =
    List.map (\n -> (n.height/2) - halfHeight + 20) skyline

topOf : Int -> Skyline -> Maybe (Float, Float)
topOf i skyline =
    let building = get i (fromList skyline)
    in
        case building of 
            Nothing -> Nothing
            Just building -> Just (building.x, building.y + building.height/2)

    

generateSkyline : Seed -> (Skyline, Seed)
generateSkyline seed = 
    let (widths, seed') = buildingWidths [] seed
        (heights, seed'') = buildingHeights (List.length widths) [] seed'
        buildings = List.map2 (\w h -> {height=h, width=w}) widths heights
        xPos = List.map2 (\x building -> {building | x=x}) (xPositions buildings) buildings
    in
       (,) (List.map2 (\y building -> {building | y=y}) (yPositions buildings) xPos) seed''

player : Float -> Float -> String -> Player
player x y name = { width=playerWidth, height=playerHeight, x=x, y=y, score=0, name=name }


defaultGame : Int -> Game
defaultGame seedInt = 
    let 
        seed = initialSeed seedInt
        (skyline, seed1) = generateSkyline seed
        (random12, seed2) = generate (int 1 2) seed1
        (random23, seed3) = generate (int 2 3) seed2

        (p1x, p1y) = Maybe.withDefault (0,0) (topOf random12 skyline)
        (p2x, p2y) = Maybe.withDefault (0,0) (topOf ((length skyline) - random23) skyline)

    in
    { state   = Pause
    , player1 = player p1x (p1y + 1 + playerHeight/2) "Player 1"
    , player2 = player p2x (p2y + 1 + playerHeight/2) "Player 2"
    , skyline = skyline
    , wind    = wind
    , banana  = { x=0, y=0, vx=0, vy=0, displayed=False, rotation=0 }
    , gravity = 9.3
    , playTo  = 3
    , sun     = { x=0, y=halfHeight-40, smiling=True }
    , seed    = seedInt
    , keyDown = 0
    }




{-- **** Update **** --}

stepGame : Input -> Game -> Game
stepGame {space, arrows} game =
    let
        s = Debug.watch "seed" game.seed
        next = defaultGame (game.seed + 1)
        prev = defaultGame (game.seed - 1)
        a = Debug.watch "arrows" arrows
    in
        if  | arrows ==  1 && not (game.keyDown ==  1) -> { next | keyDown <-  1 }
            | arrows == -1 && not (game.keyDown == -1) -> { prev | keyDown <- -1 }
            | arrows ==  0 && not (game.keyDown ==  0) -> { game | keyDown <-  0 }
            | otherwise                                ->   game
        

gameState : Signal Game
gameState = foldp stepGame (defaultGame 42) input


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
   move (player.x, player.y) <| toForm (image playerWidth playerHeight "images/gorilla.png")

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