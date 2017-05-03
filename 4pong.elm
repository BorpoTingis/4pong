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
           , dir1 = if Set.member 89 (game.keysDown) then 1 -- y arrow
                   else if Set.member 72 (game.keysDown) then -1 -- h arrow
                     else 0
            , dir4 = if Set.member 67 (game.keysDown) then -1 -- c arrow
                   else if Set.member 66 (game.keysDown) then 1 -- b arrow
                     else 0
            , dir3 = if Set.member 49 (game.keysDown) then -1 -- 1 key
                   else if Set.member 51 (game.keysDown) then 1 -- 3 key
                   else 0
            , dir2 = if Set.member 79 (game.keysDown) then 1 -- o key
                    else if Set.member 76 (game.keysDown) then -1 --l key
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
  , ball3 : Ball --Powerup
  , ball4 : Ball --Powerup
  , player1 : Player
  , player2 : Player
  , player3 : Player
  , player4 : Player
  , ai1 : Player --PowerupTest
  , ai2 : Player
  , ai3 : Player
  , ai4 : Player
  , flagY : Ball -- Powerup

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

initialBall1 = { x = 0, y = 0, vx = 150, vy = 150 }

initialBall2 = { x = 0, y = 0, vx = -150, vy = -150}

initialBall3 = {x = 0, y = 0, vx = 100, vy = -100} --Powerup

initialBall4 = {x = 0, y = 0, vx = -100, vy = 100} --Powerup

initialPlayer1 =  { x = 20 - halfWidth, y = 0, vx = 0, vy = 0, score = 0 }

initialPlayer2 = { x = halfWidth - 20, y = 0, vx = 0, vy = 0, score = 0 }

initialPlayer3 = { x = 0, y = 10 - halfHeight, vx = 0, vy = 0, score = 0 }

initialPlayer4 = { x = 0, y = halfHeight - 10, vx = 0, vy = 0, score = 0 }

initialAi1 = { x = 20 - halfWidth, y = 0, vx = 0, vy = 0, score = 0} --Powerup

initialAi2 = { x = halfWidth - 20, y = 0, vx = 0, vy = 0, score = 0 }

initialAi3 = { x = 0, y = 10 - halfHeight, vx = 0, vy = 0, score = 0 }

initialAi4 = { x = 0, y = halfHeight - 10, vx = 0, vy = 0, score = 0 }

initialFlagY = { x = 0, y = 0, vx = 200, vy = 200} --Powerup
-- default game state, 4 players and one ball
-- Need to figure out how to set (x,y) coords for the paddles, currently only takes (x)
initialGame =
  { keysDown = Set.empty
  , windowDim = (0,0)
  , state   = Pause
  , ball1    = initialBall1
  , ball2 = initialBall2
  , ball3 = initialBall3 --Powerup
  , ball4 = initialBall4 --Powerup
  , player1 = initialPlayer1
  , player2 = initialPlayer2
  , player3 = initialPlayer3
  , player4 = initialPlayer4
  , ai1 = initialAi1 --Powerup
  , ai2 = initialAi2
  , ai3 = initialAi3
  , ai4 = initialAi4
  , flagY = initialFlagY
  }

-- UPDATE
updateGame : Input -> Game -> Game
updateGame {space, reset, pause, dir1, dir2, dir3, dir4, delta} ({state, ball1, ball2, ball3, ball4, player1, player2, player3, player4, ai1, ai2, ai3, ai4, flagY} as game) = --powerup test
  let
      score2 = if ball1.x <  -halfWidth then 1
        else if ball2.x < -halfWidth then 1
          else if ball1.y > halfHeight then 1
            else if ball2.y > halfHeight then 1
              else if ball4.x < -halfWidth then 1 --Powerup
                else if ball4.y > halfHeight then 1
              else 0

      score1 = if ball1.x > halfWidth then 1
        else if ball2.x > halfWidth then 1
          else if ball1.y < -halfHeight then 1
            else if ball2.y < -halfHeight then 1
              else if ball3.x > halfWidth then 1 --Powerup
                else if ball3.y < -halfHeight then 1
                  else 0


      newState =
        if  space then Play
        else if (pause) then Pause
        else if (player1.score >= 15) then Pause
        else if (player2.score >= 15) then Pause--save this for capping max score
        else state

      newBall1 =
        if state == Pause
            then ball1
            else updateBall delta ball1 player1 player2 player3 player4 ai1 ai2 ai3 ai4

      newBall2 =
        if state == Pause
          then ball2
          else updateBall delta ball2 player1 player2 player3 player4 ai1 ai2 ai3 ai4

      newBall3 = --powerup
        if (state == Pause)
          then ball3
          else updateBall delta ball3 player1 player2 player3 player4 ai1 ai2 ai3 ai4
      newBall4 = --powerup
        if (state == Pause)
          then ball4
          else updateBall delta ball4 player1 player2 player3 player4 ai1 ai2 ai3 ai4

      newFlagY = --powerup
        if (state == Pause)
          then flagY
          else updateFlag delta flagY

 in

      if reset
         then { game | state   = Pause
                     , ball1    = initialBall1
                     , ball2 = initialBall2
                     , ball3 = initialBall3 --Powerup
                     , ball4 = initialBall4 --Powerup
                     , player1 = initialPlayer1
                     , player2 = initialPlayer2
                     , player3 = initialPlayer3
                     , player4 = initialPlayer4
                     , ai1 = initialAi1 --Powerup
                     , ai2 = initialAi2
                     , ai3 = initialAi3
                     , ai4 = initialAi4
                     , flagY = initialFlagY --Powerup
              }
        else if (player1.score >= 5 && player1.score < 7 && player2.score < 5) --team1 has 1st powerup
          then { game | state   = newState
                         , ball1 = newBall1
                         , ball2 = newBall2
                         , ball3 = newBall3--Powerup
                         , player1 = updatePlayerY delta dir1 score1 player1
                         , player2 = updatePlayerY delta dir2 score2 player2
                         , player3 = updatePlayerX delta dir3 score2 player3
                         , player4 = updatePlayerX delta dir4 score1 player4
                         , flagY = newFlagY

                  }
        else if (player2.score >= 5 && player2.score < 7 && player1.score < 5) --team 2 has 1st
          then { game | state   = newState
                         , ball1 = newBall1
                         , ball2 = newBall2
                         , ball4 = newBall4--Powerup
                         , player1 = updatePlayerY delta dir1 score1 player1
                         , player2 = updatePlayerY delta dir2 score2 player2
                         , player3 = updatePlayerX delta dir3 score2 player3
                         , player4 = updatePlayerX delta dir4 score1 player4
                         , flagY = newFlagY
                  }
        else if (player1.score >= 7 && player2.score < 5) --team1 has 2nd powerup
          then { game | state   = newState
                         , ball1 = newBall1
                         , ball2 = newBall2
                         , ball3 = newBall3--Powerup
                         , player1 = updatePlayerY delta dir1 score1 player1
                         , player2 = updatePlayerY delta dir2 score2 player2
                         , player3 = updatePlayerX delta dir3 score2 player3
                         , player4 = updatePlayerX delta dir4 score1 player4
                         , ai1 = updateAiY flagY score1 player1 --powerup
                         , ai4 = updateAiX flagY score1 player4
                         , flagY = newFlagY --Powerup
                  }
        else if (player2.score >= 7 && player1.score < 5) --team2 has 2nd powerup
          then { game | state   = newState
                         , ball1 = newBall1
                         , ball2 = newBall2
                         , ball3 = newBall3--Powerup
                         , player1 = updatePlayerY delta dir1 score1 player1
                         , player2 = updatePlayerY delta dir2 score2 player2
                         , player3 = updatePlayerX delta dir3 score2 player3
                         , player4 = updatePlayerX delta dir4 score1 player4
                         , ai2 = updateAiY flagY score2 player2 --powerup
                         , ai3 = updateAiX flagY score2 player3
                         , flagY = newFlagY --Powerup
                  }

        else if (player1.score >= 7 && player2.score >= 5 && player2.score < 7) --team1 has 2nd, team2 has 1st
          then { game | state   = newState
                         , ball1 = newBall1
                         , ball2 = newBall2
                         , ball3 = newBall3--Powerup
                         , ball4 = newBall4
                         , player1 = updatePlayerY delta dir1 score1 player1
                         , player2 = updatePlayerY delta dir2 score2 player2
                         , player3 = updatePlayerX delta dir3 score2 player3
                         , player4 = updatePlayerX delta dir4 score1 player4
                         , ai1 = updateAiY flagY score1 player1 --powerup
                         , ai4 = updateAiX flagY score1 player4
                         , flagY = newFlagY --Powerup
                  }

        else if (player2.score >= 7 && player1.score >= 5 && player1.score < 7) --team2 has 2nd, team1 has 1st
          then { game | state   = newState
                         , ball1 = newBall1
                         , ball2 = newBall2
                         , ball3 = newBall3--Powerup
                         , ball4 = newBall4
                         , player1 = updatePlayerY delta dir1 score1 player1
                         , player2 = updatePlayerY delta dir2 score2 player2
                         , player3 = updatePlayerX delta dir3 score2 player3
                         , player4 = updatePlayerX delta dir4 score1 player4
                         , ai2 = updateAiY flagY score2 player2 --powerup
                         , ai3 = updateAiX flagY score2 player3
                         , flagY = newFlagY --Powerup
                  }
        else if (player2.score >= 5 && player1.score >= 5 && player2.score < 7 && player1.score < 7) --both have 1st
          then { game | state   = newState
                         , ball1 = newBall1
                         , ball2 = newBall2
                         , ball3 = newBall3
                         , ball4 = newBall4--Powerup
                         , player1 = updatePlayerY delta dir1 score1 player1
                         , player2 = updatePlayerY delta dir2 score2 player2
                         , player3 = updatePlayerX delta dir3 score2 player3
                         , player4 = updatePlayerX delta dir4 score1 player4
                         , flagY = newFlagY
                  }
      else if (player2.score >= 7 && player1.score >= 7) --both have 2nd
        then { game | state   = newState
                       , ball1 = newBall1
                       , ball2 = newBall2
                       , ball3 = newBall3--Powerup
                       , ball4 = newBall4--Powerup
                       , player1 = updatePlayerY delta dir1 score1 player1
                       , player2 = updatePlayerY delta dir2 score2 player2
                       , player3 = updatePlayerX delta dir3 score2 player3
                       , player4 = updatePlayerX delta dir4 score1 player4
                       , ai1 = updateAiY flagY score1 player1 --powerup
                       , ai4 = updateAiX flagY score1 player4
                       , ai2 = updateAiY flagY score2 player2 --powerup
                       , ai3 = updateAiX flagY score2 player3
                       , flagY = newFlagY --Powerup
                }

      else { game | state   = newState
                     , ball1 = newBall1
                     , ball2 = newBall2

                     , player1 = updatePlayerY delta dir1 score1 player1
                     , player2 = updatePlayerY delta dir2 score2 player2
                     , player3 = updatePlayerX delta dir3 score2 player3
                     , player4 = updatePlayerX delta dir4 score1 player4
                     , flagY = newFlagY
              }

updateBall : Time -> Ball -> Player -> Player -> Player -> Player -> Player -> Player -> Player -> Player -> Ball
updateBall t ({x, y, vx, vy} as ball) p1 p2 p3 p4 ai1 ai2 ai3 ai4 =
  if not (ball.x |> near 0 halfWidth)
    then { ball | x = 0, y = 0 }
  else if not (ball.y |> near 0 halfHeight)
    then { ball | x = 0, y = 0 }
    else if (p1.score < 3 && p2.score < 3) then physicsUpdate t --powerup --both has none
            { ball |
                vx = stepV vx (withinY ball p1) (withinY ball p1) (withinY ball p2) (withinY ball p2),
                vy = stepV vy (withinX ball p3) (withinX ball p3) (withinX ball p4) (withinX ball p4)
            }
    else  if (p1.score >= 3 && p2.score < 3) then physicsUpdate t --team1 has 1st
            { ball |
                vx = stepV vx (powerupWithinY ball p1) (powerupWithinY ball p1) (withinY ball p2) (withinY ball p2),
                vy = stepV vy (withinX ball p3) (withinX ball p3) (powerupWithinX ball p4) (powerupWithinX ball p4)
            }
    else  if (p1.score < 3 && p2.score >= 3) then physicsUpdate t --team2 has 1st
            { ball |
                vx = stepV vx (withinY ball p1) (withinY ball p1) (powerupWithinY ball p2) (powerupWithinY ball p2),
                vy = stepV vy (powerupWithinX ball p3) (powerupWithinX ball p3) (withinX ball p4) (withinX ball p4)
            }
    else  if (p1.score >= 3 && p2.score >= 3 && p1.score < 7 && p2.score < 7) then physicsUpdate t --both have 1st
            { ball |
                vx = stepV vx (powerupWithinY ball p1) (powerupWithinY ball p1) (powerupWithinY ball p2) (powerupWithinY ball p2),
                vy = stepV vy (powerupWithinX ball p3) (powerupWithinX ball p3) (powerupWithinX ball p4) (powerupWithinX ball p4)
            }
    else  if (p1.score >= 7 && p2.score < 3) then physicsUpdate t --team1 has 2nd
            { ball |
                vx = stepV vx (powerupWithinY ball p1) (withinY ball ai1) (withinY ball p2) (withinY ball p2), --(withinY ball ai1),
                vy = stepV vy (withinX ball p3) (withinX ball p3) (powerupWithinX ball p4) (withinX ball ai4) --(withinX ball ai4)
            }
    else  if (p1.score < 3 && p2.score >= 7) then physicsUpdate t --team2 has 2nd
            { ball |
                vx = stepV vx (withinY ball p1) (withinY ball p1) (powerupWithinY ball p2) (withinY ball ai2),-- (withinY ball ai2),
                vy = stepV vy (powerupWithinX ball p3) (withinX ball ai3) (withinX ball p4) (withinX ball p4) --(withinX ball ai3)
            }
    else  if (p1.score >= 7 && p2.score >= 3 && p2.score < 7) then physicsUpdate t --team1 has 2nd, team2 has 1st
            { ball |
                vx = stepV vx (powerupWithinY ball p1) (withinY ball ai1) (powerupWithinY ball p2) (powerupWithinY ball p2), --(withinY ball ai1),
                vy = stepV vy (powerupWithinX ball p3) (powerupWithinX ball p3) (powerupWithinX ball p4) (withinX ball ai4) --(withinX ball ai4)
            }
    else  if (p2.score >= 7 && p1.score >= 3 && p1.score < 7) then physicsUpdate t --team1 has 1st, team2 has 2nd
            { ball |
                vx = stepV vx (powerupWithinY ball p1) (powerupWithinY ball p1) (powerupWithinY ball p2) (withinY ball ai2), -- (withinY ball ai2),
                vy = stepV vy (powerupWithinX ball p3) (withinX ball ai3) (powerupWithinX ball p4) (powerupWithinX ball p4)-- (withinX ball ai3)
            }
    else physicsUpdate t --both have 2nd
            { ball |
                vx = stepV vx (powerupWithinY ball p1) (withinY ball ai1) (powerupWithinY ball p2) (withinY ball ai2), -- (withinY ball ai1) (withinY ball ai2),
                vy = stepV vy (powerupWithinX ball p3) (withinX ball ai3) (powerupWithinX ball p4) (withinX ball ai4) --(withinY ball ai3) (withinY ball ai4)
            }
updateFlag : Time -> Ball -> Ball --powerup
updateFlag t ({x, y, vx, vy} as ball) = physicsUpdate t
        { ball |
            vx = stepV vx (x < 7 - halfWidth) (x < 7 - halfWidth) (x > halfWidth - 7) (x > halfWidth - 7),
            vy = stepV vy (y < 7 - halfHeight) (y < 7 - halfHeight) (y > halfHeight - 7) (y > halfHeight - 7)
        }

updateAiY : Ball -> Int -> Player -> Player
updateAiY flag points player =
  { player |
      y = clamp (22 - halfHeight) (halfHeight - 22) flag.y,
      score = player.score + points
    }

updateAiX : Ball -> Int -> Player -> Player
updateAiX flag points player =
  { player |
      x = clamp (22 - halfWidth) (halfWidth - 22) flag.x,
      score = player.score + points
    }


updatePlayerY : Time -> Int -> Int -> Player -> Player
updatePlayerY t dir points player =
  let player1 = physicsUpdate  t { player | vy = toFloat dir * 200}

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
          x = clamp (22 - halfWidth) (halfWidth - 22) player1.x,
          score = player.score + points
      }

physicsUpdate t ({x, y, vx, vy} as obj) =
  { obj |
      x = x + vx * t,
      y = y + vy * t
  }

near : Float -> Float -> Float -> Bool
near k c n =
    n >= k-c && n <= k+c

withinY ball paddle =
    near paddle.x 8 ball.x && near paddle.y 20 ball.y

powerupWithinY ball paddle =
  near paddle.x 8 ball.x && near paddle.y 30 ball.y

withinX ball paddle =
    near paddle.x 40 ball.x && near paddle.y 8 ball.y

powerupWithinX ball paddle =
  near paddle.x 50 ball.x && near paddle.y 8 ball.y

stepV v lowerCollision lower1 upperCollision upper1  =
  if lowerCollision then abs v
  else if upperCollision then 0 - abs v
  else if lower1 then abs v
  else if upper1 then 0 - abs v
  else v

-- VIEW, this is where we would add a menu screen I think
view : Game -> Html Msg
view {windowDim, state, ball1, ball2, ball3, ball4, player1, player2, player3, player4, ai1, ai2, ai3, ai4, flagY} =
  let scores : Element
      scores = txt (Text.height 25) ("Team&larr;&uarr; : " ++ toString player1.score ++ "  Team&rarr;&darr; : " ++ toString player2.score)
      (w,h) = windowDim
  in

      toHtml <|
      container w h middle <|
      collage gameWidth gameHeight
        [ rect gameWidth gameHeight
            |> filled gray
        , redLine gameHeight
            |> traced (dashed team1)
        , blueLine gameHeight
            |> traced (dashed team2)
        , oval 15 15
            |> makeBall ball1
        , oval 15 15
            |> makeBall ball2
        , oval 15 15
            |> makePowerBall1 ball3 --Powerup
        , oval 15 15
            |> makePowerBall2 ball4 --Powerup
        , if (player1.score < 7) then
          rect 10 40
            |> makeAi ai1
          else rect 10 40
            |> makeBall ai1
        , if (player2.score < 7) then
          rect 10 40
            |> makeAi ai2
          else rect 10 40
            |> makeBall ai2
        , if (player2.score < 7) then
          rect 70 10
            |> makeAi ai3
          else rect 70 10
            |> makeBall ai3
        , if (player1.score < 7) then
          rect 70 10
            |> makeAi ai4
          else rect 70 10
            |> makeBall ai4


        , if (player1.score < 3) then
          rect 10 40
            |> makeTeam1 player1
          else rect 10 70
            |> makeTeam1 player1

        , if (player2.score < 3) then
          rect 10 40
            |> makeTeam2 player2
          else rect 10 70
            |> makeTeam2 player2
        , if (player2.score < 3) then
          rect 70 10
            |> makeTeam2 player3
          else rect 90 10
            |> makeTeam2 player3
        , if (player1.score < 3) then
          rect 70 10
            |> makeTeam1 player4
          else rect 90 10
            |> makeTeam1 player4
        , toForm scores
            |> move (0, gameHeight/2 - 40)
        , toForm (playOrPause state)
            |> move (0, 80 - gameHeight/2)
        , toForm (powerupString)
            |> move (0, gameHeight/2 - 120)
        ]

playOrPause state =
    case state of
        Play    -> txt identity ""
        Pause   -> txt identity pauseMessage

powerupString = txt2 identity powerupMessage
blueLine height = path [(gameWidth - 10, gameHeight), (-gameWidth, -gameHeight + 10)]
redLine height = path [(gameWidth + 10, gameHeight), (-gameWidth, -gameHeight - 10)]

-- default colors
team1 = rgb 0 0 0

team2 = rgb 255 255 255

pongBlack = rgb 0 0 0

gray = rgb 125 125 125

textWhite = rgb 255 255 255

gold = rgb 218 165 32

txt f = Text.fromString >> Text.color textWhite >> Text.monospace >> f >> leftAligned
pauseMessage = "SPACE to start, P to pause, R to reset \nplayer&larr;: Y H, player&uarr;: C B, player&darr;: 1 3, player&rarr;: O L"

txt2 g = Text.fromString >> Text.color black >> Text.monospace >> g >> leftAligned
powerupMessage = "1st power-up: extended paddles\nrequried score: 3\n\n2nd power-up: an extra team-ball\nrequired score: 5\n\n3rd power-up: team-AI\nrequired score: 7"

makeBall obj shape =
    shape
      |> filled gold
      |> move (obj.x,obj.y)

makeFlag obj shape =
    shape
      |> filled red --testing color
      |> move (obj.x,obj.y)

makePowerBall1 obj shape =
  shape
    |> filled team1
    |> move (obj.x, obj.y)

makePowerBall2 obj shape =
  shape
    |> filled team2
    |> move (obj.x, obj.y)

makeTeam1 obj shape =
    shape
      |> filled team1
      |> move (obj.x,obj.y)

makeTeam2 obj shape =
    shape
      |> filled team2
      |> move (obj.x,obj.y)

makeAi obj shape =
    shape
      |> filled gray
      |> move (obj.x,obj.y)
