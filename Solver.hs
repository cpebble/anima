module Solver where

import PParser

data Direction = Up | Down | Left | Right

type State = (Puzzle, Direction)

move :: Puzzle -> Puzzle
move puz =
  let newAct =
        map
          ( \(Actor c x y) ->
              -- Check if movable
              case tiles puz !! y !! x of
                Block -> Actor {color=c, _x=x, _y=y}
                _ -> -- Check for deadlocks
                    

          )
          $ actors puz
   in newAct

solve :: Puzzle
solve = undefined
