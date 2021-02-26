module Infrastructure.Recording where

import qualified Infrastructure.Action as An
import qualified Infrastructure.TestT as Test
import Control.Monad.State (modify)

addAction :: An.Action -> Test.TestT ()
addAction a = modify (a:)

getResult :: (a, [An.Action]) -> a
getResult = fst

getActions :: (a, [An.Action]) -> [An.Action]
getActions = reverse . snd 
