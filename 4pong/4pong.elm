import Html exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window


 -- The game screen, or MODEL
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
  { state : State
  , ball : Ball
  , player1 : Player
  , player2 : Player
  ,
  ,
  }


player : Float -> Player
player x =
  Player x 0 0 0 0


-- Default game state, paused, with two players and one ball
defaultGame : Game
defaultGame =
  { state = Pause
  , ball = Ball 0 0 200 200
  , player1 = player (20-halfWidth)
  , player2 = player (halfWidth-20)
  }


  -- VIEW
  view : (Int,Int) -> Game -> Element
  view (w,h) game =
    let
      scores =
        txt (Text.height 50) (toString game.player1.score ++ "  " ++ toString game.player2.score)
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
        , toForm scores
            |> move (0, gameHeight/2 - 40)
        , toForm (if game.state == Play then spacer 1 1 else txt identity msg)
            |> move (0, 40 - gameHeight/2)
        ]


  pongBlack =
    rgb rgb 0 0 0


  textWhite =
    rgb 255 255 255
