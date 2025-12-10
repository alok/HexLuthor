/-
  Demo file for HexColor syntax and widget
  Open this file in VS Code with the Lean extension to see the color widgets!

  HOW TO USE:
  1. Open this file in VS Code with the Lean 4 extension
  2. Click on any hex color literal (e.g., #xFF0000)
  3. Look at the Infoview panel - you'll see a colored square preview!

  The @[expr_presenter] also shows colors when viewing Hex expressions in goals.
-/
import HexLuthor.Widget
import ProofWidgets.Component.Panel.SelectionPanel

open HexLuthor

-- Hex color syntax: #xRRGGBB (no quotes needed!)
#check #xFF0000  -- Red
#check #x00FF00  -- Green
#check #x0000FF  -- Blue
#check #xFFFFFF  -- White
#check #x000000  -- Black
#check #xFF8C00  -- Dark Orange
#check #x30103E  -- Deep Purple
#check #x00CED1  -- Dark Turquoise
#check #x4169E1  -- Royal Blue

-- Use in definitions
def myFavoriteColor : Hex := #x56585D  -- Royal Blue
def sunset : Hex := #xFF6347  -- Tomato
def ocean : Hex := #x006994  -- Sea blue

-- Mix with the Hex API
#eval Hex.toHexString #x736118  -- "#736118"
#eval Hex.toHexString myFavoriteColor  -- "#4169E1"

-- Example showing colors in proof goals (click on the goal to see colors)
example : myFavoriteColor = #x4169E1 := by
  -- Place cursor here and look at the goal - the @[expr_presenter] shows colors
  rfl

-- Debug test
#check (rfl : myFavoriteColor = myFavoriteColor)
