module Automata.Cells.Rules where

import Automata.Cells.Operators
import Automata.Cells.Types

rule1 :: Cell -> Cell -> Cell -> Cell
rule1 L L L = D
rule1 _ _ _ = L

rule3 :: Cell -> Cell -> Cell -> Cell
rule3 L _ _   = D
rule3 D c1 c2 = cnot $ cleft c1 c2

rule7 :: Cell -> Cell -> Cell -> Cell
rule7 D c1 c2 = cnot $ c1 `cor` c2
rule7 L _ _   = D

rule22 :: Cell -> Cell -> Cell -> Cell
rule22 D c1 c2 = c1 `cneq` c2
rule22 L c1 c2 = cnot $ c1 `cor` c2

rule30 :: Cell -> Cell -> Cell -> Cell
rule30 D c1 c2 = c1 `cor` c2
rule30 L c1 c2 = cnot $ cor c1 c2

rule41 :: Cell -> Cell -> Cell -> Cell
rule41 L D L   = L
rule41 L _ _   = D
rule41 D c1 c2 = c1 `ceq`c2

rule45 :: Cell -> Cell -> Cell -> Cell
rule45 L D L   = L
rule45 L _ _   = D
rule45 D L D   = L
rule45 D c1 c2 = c1 `ceq` c2

rule57 :: Cell -> Cell -> Cell -> Cell
rule57 L c1 _  = cnot c1
rule57 D c1 c2 = c1 `ceq` c2

rule60 :: Cell -> Cell -> Cell -> Cell
rule60 L c1 _ = cnot c1
rule60 D c1 _ = c1

rule62 :: Cell -> Cell -> Cell -> Cell
rule62 L c1 _  = cnot c1
rule62 D c1 c2 = c1 `cor` c2

rule73 :: Cell -> Cell -> Cell -> Cell
rule73 L D L   = D
rule73 L c1 c2 = c1 `cneq` c2
rule73 D c1 c2 = c1 `ceq` c2

rule86 :: Cell -> Cell -> Cell -> Cell
rule86 L _ c2  = cnot c2
rule86 D c1 c2 = c1 `cneq` c2

rule90 :: Cell -> Cell -> Cell -> Cell
rule90 D c1 c2 = c1 `cor` c2
rule90 L c1 c2 = cnot $ cright c1 c2

rule105 :: Cell -> Cell -> Cell -> Cell
rule105 L c1 c2 = c1 `cneq` c2
rule105 D c1 c2 = c1 `ceq` c2

rule107 :: Cell -> Cell -> Cell -> Cell
rule107 L c1 c2 = c1 `cneq` c2
rule107 D L D   = D
rule107 D _ _   = L

rule110 :: Cell -> Cell -> Cell -> Cell
rule110 D c1 c2 = c1 `cor` c2
rule110 L c1 c2 = c1 `cneq` c2

rule150 :: Cell -> Cell -> Cell -> Cell
rule150 L c1 c2 = c1 `ceq` c2
rule150 D c1 c2 = c1 `cneq` c2
