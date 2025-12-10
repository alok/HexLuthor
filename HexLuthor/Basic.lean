/-
  Demo file for HexColor syntax and widget
  Open this file in VS Code with the Lean extension to see the color widgets!

  HOW TO USE:
  1. Open this file in VS Code with the Lean 4 extension
  2. Click on any hex color literal (e.g., #h"FF0000")
  3. Look at the Infoview panel - you'll see a colored square preview!

  The @[expr_presenter] also shows colors when viewing Hex expressions in goals.
-/
import HexLuthor.HexColor
import ProofWidgets.Component.Panel.SelectionPanel

open HexLuthor

-- Hex color syntax: #h"RRGGBB" (click to see color widget in infoview!)
#check #h"FF0000"  -- Red
#check #h"00FF00"  -- Green
#check #h"0000FF"  -- Blue
#check #h"FFFFFF"  -- White
#check #h"000000"  -- Black
#check #h"FF8C00"  -- Dark Orange
#check #h"9400D3"  -- Dark Violet
#check #h"00CED1"  -- Dark Turquoise
#check #h"4169E1"  -- Royal Blue

-- Use in definitions
def myFavoriteColor : Hex := #h"4169E1"  -- Royal Blue
def sunset : Hex := #h"FF6347"  -- Tomato
def ocean : Hex := #h"000000"  -- Sea blue

-- Mix with the Hex API
#eval Hex.toHexString #h"FFCC00"  -- Should print "#FFCC00"
#eval Hex.toHexString myFavoriteColor  -- "#4169E1"

-- Example showing colors in proof goals (click on the goal to see colors)
example : myFavoriteColor = #h"4169E1" := by
  -- Place cursor here and look at the goal - the @[expr_presenter] shows colors
  rfl

-- Debug test
#check (rfl : myFavoriteColor = myFavoriteColor)
