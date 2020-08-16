module Automata.Cells.Operators (
    ceq, cneq,
    cid, cnot, ccontra,
    cand, cor, cxor,
    cleft, cright
) where

import Automata.Cells.Types

-- | (==) for cells.
ceq :: Cell -> Cell -> Cell
ceq L L = L
ceq D D = L
ceq _ _ = D

-- | (/=) for cells.
cneq :: Cell -> Cell -> Cell
cneq c1 c2 = cnot $ ceq c1 c2

-- | `id` for cells.
cid :: Cell -> Cell
cid = id

-- | `not` for cells.
cnot :: Cell -> Cell
cnot D  = L
cnot L = D

-- | "Contradiction" for cells.
ccontra :: Cell -> Cell -> Cell
ccontra _ _ = D

-- | `and` for cells.
cand :: Cell -> Cell -> Cell
cand D D = D
cand D L = D
cand L D = D
cand L L = L

-- | `or` for cells.
cor :: Cell -> Cell -> Cell
cor D D = D
cor D L = L
cor L D = L
cor L L = L

-- | `xor` for cells.
cxor :: Cell -> Cell -> Cell
cxor c1 c2 = cnot $ ceq c1 c2

cleft :: Cell -> Cell -> Cell
cleft c _ = c

cright :: Cell -> Cell -> Cell
cright _ c = c
