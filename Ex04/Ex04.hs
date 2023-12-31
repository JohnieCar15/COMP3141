module Ex04 where

import Data.Semigroup ( Max(Max, getMax) )
import Data.Monoid
import Control.Monad.State (State, get, put, evalState)
import Test.QuickCheck
import Priority
import Size
import Control.Concurrent (Chan)

-- DEFINITIONS AND HELPER FUNCTIONS --

type NodeInfo = (Size, Priority)
data QueueTree a
  = Null
  | Leaf NodeInfo a
  | Node NodeInfo (QueueTree a) (QueueTree a)
  deriving (Show)

nodeInfo :: QueueTree a -> NodeInfo
nodeInfo Null = mempty
nodeInfo (Leaf i _) = i
nodeInfo (Node i _ _) = i

sizeOf :: QueueTree a -> Size
sizeOf = fst . nodeInfo

maxPrio :: QueueTree a -> Priority
maxPrio = snd . nodeInfo

-- checks whether the tree structure
-- is balanced (i.e. that the left subtree and the right
-- subtree don't ever differ too much in size)
balanced :: QueueTree a -> Bool
balanced (Node i l r) =
  let sl = unSize (sizeOf l) in
  let sr = unSize (sizeOf r) in
  abs (sl - sr) <= 1 && balanced l && balanced r
balanced _ = True

wfTree :: QueueTree Char
wfTree = Node (size 3,priority 7)
  ( Node (size 2,priority 7)
      (Leaf (size 1,priority 7) 'b')
      (Leaf (size 1,priority 5) 'c')
  )
  (Leaf (size 1,priority 3) 'a')

nwfTree :: QueueTree Char
nwfTree = Node (size 2,priority 7)
  ( Node (size 2,priority 5)
      (Leaf (size 1,priority 7) 'b')
      (Leaf (size 1,priority 5) 'c')
  )
  (Leaf (size 1,priority 3) 'a')



-- EXERCISE STARTS HERE --

-- Task 1a. Write a well-formedness predicate
--          wf for the `QueueTree` data type.

-- Hint: Both `Priority` and `Size` are semigroups/monoids.
-- This means that the type `NodeInfo` is also automatically
-- a monoid.

wf :: QueueTree a -> Bool
wf Null = True
wf (Leaf (s, p) x) = s == size 1 && unPriority p >= 1
wf (Node (s, p) l r) = 
  let currp = unPriority p in
  let currs = unSize s in
  let pl = unPriority (maxPrio l) in
  let pr = unPriority (maxPrio r) in
  let sl = unSize (sizeOf l) in
  let sr = unSize (sizeOf r) in
  currp >= pl && currp >= pr && sl + sr == currs && wf l && wf r

-- Task 1b. Write smart constructors `leaf` and `node`
--          for the `QueueTree` data type which maintain
--          the well-formedness invariant. I.e. given
--          well-formed inputs, the smart constructors
--          should give well-formed outputs.
--          You should /not/ tweak the structure of the ~QueueTree~
--          beyond updating the ~NodeInfo~; in particular don't do
--          ~node Null Null = Null~.

leaf :: Priority -> a -> QueueTree a
leaf prio = Leaf (size 1, prio)

node :: QueueTree a -> QueueTree a -> QueueTree a
node l r = 
  let sl = unSize (sizeOf l) in
  let sr = unSize (sizeOf r) in
  let pl = unPriority (maxPrio l) in
  let pr = unPriority (maxPrio r) in
  Node (size (sl + sr), priority (max pl pr)) l r

-- Task 2a. Implement the usual priority queue functions
--          for the type `QueueTree`. These are
--          pop - Remove the element from the queue that has the
--               highest priority. Return the modified queue,
--               along with the removed element (if any).
--          insert - add an element to the queue with the given priority.

pop :: QueueTree a -> (QueueTree a, Maybe a)
pop = error "'pop' not implemented"

insert :: Priority -> a -> QueueTree a -> QueueTree a
insert = error "'insert' not implemented"

-- Task 2b. Implement a function `fromList` that converts a
--          list of `(Priority, x)` pairs into a well-formed
--          and balanced `QueueTree x` structure.

fromList :: [(Priority, a)] -> QueueTree a
fromList = error "'fromList' not implemented"

-- Hint: you can use `fromList` to implement an `Arbitrary`
-- instance for `QueueTree`, allowing you to test your work.


-- Task 3. Implement stateful versions of the pop and insert
--         operations above using the `State` type in Haskell's
--         standard mtl library.
--         Implement a `peek` operation which just returns the
--         highest-priority element without changing the
--         state of the queue.
--         Do not use the `state` function in your final
--         implementations!

pop' :: State (QueueTree a) (Maybe a)
pop' = error "'pop'' not implemented"

insert' :: Priority -> a -> State (QueueTree a) ()
insert' = error "'insert'' not implemented"

peek' :: State (QueueTree a) (Maybe a)
peek' = error "'peek'' not implemented"



-- END OF EXERCISE --

-- You can use the following three examples to test your
-- implementations of pop' and insert', and to practice
-- reading `State`-ful functions.

-- Returns the highest priority currently in the `QueueTree`
-- without changing the state.
getMaxPrio' :: State (QueueTree a) Priority
getMaxPrio' =
  get >>= \q ->
  return (maxPrio q)

-- Removes the element with the second-highest priority
-- in the queue.
dip' :: State (QueueTree a) ()
dip' =
  getMaxPrio' >>= \p ->
  pop'        >>= \h1 ->
  pop'        >>= \h2 ->
  case h1 of
    Nothing -> return ()
    Just h1 -> insert' p h1

-- a `State`-free version of dip
dip :: QueueTree Char -> QueueTree Char
dip = evalState $
  dip' >>= \() ->
  get
