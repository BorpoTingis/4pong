import Html exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Text
import Char
import Time exposing (..)
import Window
import Set exposing (Set)
import Task
import AnimationFrame


main = program {init = (initialGame, initialSizeCmd)
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

-- KeyDown/KeyUp/keysDown technique taken from this answer : 
--     http://stackoverflow.com/a/39127092/509928

type Msg = KeyDown KeyCode
         | KeyUp KeyCode
         | WindowResize (Int,Int)
         | Tick Float
         | NoOp


-- default game state, 4 players and one ball
-- Need to figure out how to set (x,y) coords for the paddles, currently only takes (x)Htm
defaultGame : Game
defaultGame =
  { state = Pause
  , ball = Ball 0 0 200 200
  , player1 = player (20-halfWidth)
  , player2 = player (halfWidth-20)
  , player3 = player (20 - halfWidth)
  , player4 = player (halfWidth -20)
  }

-- inputs, none for paddles 3 & 4 since they will be AI controlled? can add later if needed.
--  keysDown "activates" the event when the key is pressed down. I'm not sure if this is ideal since someone with faster switches
--  On their keyboard would have an advantage. Not super important
inputs : Game -> Float -> Input
inputs game delta =
{ space = Set.member(Char.toCode ' ') (game.keysDown) --set space to spacebar key
, reset = Set.member (Char.toCode 'R') (game.keysDown) --set reset to R key
, pause = Set.member (Char.toCode 'P') (game.keysDown) --set pause to P key
, direction = if Set.member 38 (game.keysDown) then 1 -- sets down to down arrow
              else if Set.member 40 (game.keysDown) then 1 --sets up to up arrow
              else 0
, delta = inSeconds delta              
}

update msg game =
  case msg of
    KeyDown key ->
      ({ game | keysDown = Set.insert key game.keysDown }, Cmd.none) -- starts the event when the key is pressed down
    KeyUp key ->
      ({ game | keysDown = Set.remove key game.keysDown }, Cmd.none) -- ends the event when the key is released
    tick delta ->
      let input = getInput game delta
      in (updateGame input game, Cmd.none)
    WindowResize dim ->
      ({ game | windowDim = dim}, Cmd.none)   
    NoOp ->
      (game, Cmd.none) 

-- Subscriptons! Allows us to listen for inputs.
subscriptions _=
  Sub.batch
    [ Keyboard.downs KeyDown
    , keyboard.ups KeyUp
    , Window.resizes sizeToMsg
    , AnimationFrame.diffs Tick
    ]

-- initialSizeCmd/sizeToMsg technique taken from this answer : 
--  https://www.reddit.com/r/elm/comments/4jfo32/getting_the_initial_window_dimensions/d369kw1/

initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform sizeToMsg (Window.size)

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  WindowResize (size.width, size.height)



-- MODEL STUFF

 -- The game screen
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)


-- Play/pause button
type State = Play | Pause


type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }


type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , score : Int
  }


type alias Game =
  { keyDown : Set KeyCode
  , windowDim : (Int, Int) 
  , state : State
  , ball : Ball
  , player1 : Player
  , player2 : Player
  , player3 : Player
  , player4 : Player
  }


delta : Signal Time
delta = 
    Signal.map inSeconds (fps 35) --might want to change to 60 if possible because 35fps is totally lame
player : Float -> Player
player x =
  Player x 0 0 0 0

input : Signal Input
input =
  Signal.sampleOn delta <|
      Signal.map4  




  -- VIEW
  view : (Int,Int) -> Game -> Element
  view (w,h) game =
    let
      scores =
        txt (Text.height 50) (toString game.player1.score ++ "  " ++ toString game.player2.score ++ " " ++ toString game.player3.score ++ " " ++ toString game.player4.score)
    in
      container w h middle <|
      collage gameWidth gameHeight
        [ rect gameWidth gameHeight
            |> filled pongGreen
        , oval 15 15
            |> make game.ball
        , rect 10 40
            |> make game.player1
        , rect 10 40
            |> make game.player2
        , rect 40 10 -- 40 10 makes the paddle horizontal
            |> make game.player3
        , rect 40 10
            |> make game.player4
        , toForm scores
            |> move (0, gameHeight/2 - 40)
        ]


-- default colors, black background with a white ball
  pongBlack =
    rgb rgb 0 0 0

  textWhite =
    rgb 255 255 255
