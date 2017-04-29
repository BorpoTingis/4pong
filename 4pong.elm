import Html exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard exposing (..)
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


-- inputs, none for paddles 3 & 4 since they will be AI controlled? can add later if needed.
--  keysDown "activates" the event when the key is pressed down. I'm not sure if this is ideal since someone with faster switches
--  On their keyboard would have an advantage. Not super important

getInputs : Game -> Float -> Input
getInputs game delta
         = { space = Set.member (Char.toCode ' ') (game.keysDown)
           , reset = Set.member (Char.toCode 'R') (game.keysDown)
           , pause = Set.member (Char.toCode 'P') (game.keysDown)
           , dir1 = if Set.member 38 (game.keysDown) then 1 -- down arrow
                   else if Set.member 40 (game.keysDown) then -1 -- up arrow
                     else 0
            , dir3 = if Set.member 37 (game.keysDown) then -1 -- left arrow
                   else if Set.member 39 (game.keysDown) then 1 -- right arrow
                     else 0
            , dir4 = if Set.member 65 (game.keysDown) then -1 -- a key
                   else if Set.member 68 (game.keysDown) then 1 -- d key
                   else 0
            , dir2 = if Set.member 87 (game.keysDown) then 1 -- w key
                    else if Set.member 83 (game.keysDown) then -1 --s key
                    else 0
           , delta = inSeconds delta
           }
update msg game =
  case msg of
    KeyDown key ->
      ({ game | keysDown = Set.insert key game.keysDown }, Cmd.none) -- starts the event when the key is pressed down
    KeyUp key ->
      ({ game | keysDown = Set.remove key game.keysDown }, Cmd.none) -- ends the event when the key is released
    Tick delta ->
      let input = getInputs game delta
      in (updateGame input game, Cmd.none)
    WindowResize dim ->
      ({ game | windowDim = dim}, Cmd.none)
    NoOp ->
      (game, Cmd.none)

-- Subscriptons! Allows us to listen for inputs.
subscriptions _=
  Sub.batch
    [ Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
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
  { keysDown : Set KeyCode
  , windowDim : (Int, Int)
  , state : State
  , ball1 : Ball
  , ball2 : Ball
  , player1 : Player
  , player2 : Player
  , player3 : Player
  , player4 : Player
  }

type alias Input =
  { space : Bool
  , reset : Bool
  , pause : Bool
  , dir1 : Int
  , dir2 : Int
  , dir3 : Int
  , dir4 : Int
  , delta : Time
  }

player : Float -> Player
player initialX =
  { x = initialX
  , y = 0
  , vx = 0
  , vy = 0
  , score = 0
  }

initialBall1 = { x = 0, y = 0, vx = 200, vy = 200 }

initialBall2 = { x = 0, y = 0, vx = 200, vy = 200}

initialPlayer1 =  { x = 20 - halfWidth, y = 0, vx = 0, vy = 0, score = 0 }

initialPlayer2 = { x = halfWidth - 20, y = 0, vx = 0, vy = 0, score = 0 }

initialPlayer3 = { x = 0, y = 20 - halfHeight, vx = 0, vy = 0, score = 0 }

initialPlayer4 = { x = 0, y = halfHeight - 20, vx = 0, vy = 0, score = 0 }


-- default game state, 4 players and one ball
-- Need to figure out how to set (x,y) coords for the paddles, currently only takes (x)
initialGame =
  { keysDown = Set.empty
  , windowDim = (0,0)
  , state   = Pause
  , ball1    = initialBall1
  , ball2 = initialBall2
  , player1 = initialPlayer1
  , player2 = initialPlayer2
  , player3 = initialPlayer3
  , player4 = initialPlayer4
  }

-- UPDATE
updateGame : Input -> Game -> Game
updateGame {space, reset, pause, dir1, dir2, dir3, dir4, delta} ({state, ball1, ball2, player1, player2, player3, player4} as game) =
  let score1 = if ball1.x >  halfWidth then 1 else if ball2.x > halfWidth then 1 else 0
      score2 = if ball1.x < -halfWidth then 1 else if ball2.x < -halfWidth then 1 else 0
      score3 = if ball1.y > halfHeight then 1 else if ball2.y > halfHeight then 1 else 0
      score4 = if ball1.y < -halfHeight then 1 else if ball2.y < -halfHeight then 1 else 0



      newState =
        if  space then Play
        else if (pause) then Pause
        else if (score1 /= score2) then Pause
        else state

      newBall1 =
        if state == Pause
            then ball1
            else updateBall delta ball1 player1 player2 player3 player4

      newBall2 =
        if state == Pause
          then ball2
          else updateBall delta ball2 player1 player2 player3 player4
 in
      if reset
         then { game | state   = Pause
                     , ball1    = initialBall1
                     , ball2 = initialBall2
                     , player1 = initialPlayer1
                     , player2 = initialPlayer2
                     , player3 = initialPlayer3
                     , player4 = initialPlayer4
              }
      else { game | state   = newState
                     , ball1    = newBall1
                     , ball2 = newBall2
                     , player1 = updatePlayerY delta dir1 score1 player1
                     , player2 = updatePlayerY delta dir2 score2 player2
                     , player3 = updatePlayerX delta dir3 score3 player3
                     , player4 = updatePlayerX delta dir4 score4 player4

              }

updateBall : Time -> Ball -> Player -> Player -> Player -> Player -> Ball
updateBall t ({x, y, vx, vy} as ball) p1 p2 p3 p4 =
  if not (ball.x |> near 0 halfWidth)
    then { ball | x = 0, y = 0 }
    else physicsUpdate t
            { ball |
                vx = stepV vx (within ball p1) (within ball p2),
                vy = stepV vy (y < 7-halfHeight) (y > halfHeight-7)
            }

updatePlayerY : Time -> Int -> Int -> Player -> Player
updatePlayerY t dir points player =
  let player1 = physicsUpdate  t { player | vy = toFloat dir * 200 }

  in
      { player1 |
          y = clamp (22 - halfHeight) (halfHeight - 22) player1.y,
          score = player.score + points
      }
updatePlayerX : Time -> Int -> Int -> Player -> Player
updatePlayerX t dir points player =
  let player1 = physicsUpdate  t { player | vx = toFloat dir * 200 }

  in
      { player1 |
          x = clamp (22 - halfHeight) (halfHeight - 22) player1.x,
          score = player.score + points
      }


-- updateComputer : Ball -> Int -> Player -> Player
-- updateComputer ball points player =
--     { player |
--         y = clamp (22 - halfHeight) (halfHeight - 22) ball.y,
--         score = player.score + points
--     }

physicsUpdate t ({x, y, vx, vy} as obj) =
  { obj |
      x = x + vx * t,
      y = y + vy * t
  }

near : Float -> Float -> Float -> Bool
near k c n =
    n >= k-c && n <= k+c

within ball paddle =
    near paddle.x 8 ball.x && near paddle.y 20 ball.y


stepV v lowerCollision upperCollision =
  if lowerCollision then abs v
  else if upperCollision then 0 - abs v
  else v

-- VIEW, this is where we would add a menu screen I think
view : Game -> Html Msg
view {windowDim, state, ball1, ball2, player1, player2, player3, player4} =
  let scores : Element
      scores = txt (Text.height 50) (toString player1.score ++ "  " ++ toString player2.score ++ "  " ++ toString player3.score ++ "  " ++ toString player4.score)
      (w,h) = windowDim
  in
      toHtml <|
      container w h middle <|
      collage gameWidth gameHeight
        [ rect gameWidth gameHeight
            |> filled pongBlack
        , verticalLine gameHeight
            |> traced (dashed white)
        , oval 15 15
            |> make ball1
        , oval 15 15
            |> make ball2
        , rect 10 40
            |> make player1
        , rect 10 40
            |> make player2
        , rect 40 10
            |> make player3
        , rect 40 10
            |> make player4
        , toForm scores
            |> move (0, gameHeight/2 - 40)
        , toForm (playOrPause state)
            |> move (0, 40 - gameHeight/2)
        ]

playOrPause state =
    case state of
        Play    -> txt identity ""
        Pause   -> txt identity pauseMessage

verticalLine height = path [(0, height), (0, -height)]


-- default colors, black background with a white ball
pongBlack = rgb 0 0 0

textWhite = rgb 255 255 255

txt f = Text.fromString >> Text.color textWhite >> Text.monospace >> f >> leftAligned
pauseMessage = "SPACE to start, P to pause, R to reset \nplayer1: &uarr; &darr;, player2: W S, player3: &larr; &rarr;, player4: A D"

make obj shape =
    shape
      |> filled white
      |> move (obj.x,obj.y)
