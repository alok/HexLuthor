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
-- set_option doc.verso true
open HexLuthor

-- Hex color syntax: #xRRGGBB or #xRRGGBBAA (with alpha)
#check #x7E2121  -- Wine
#check #x00FF00  -- Green
#check #x0000FF  -- Blue
#check #xFFFFFF  -- White
#check #x000000  -- Black
#check #xFF8C00  -- Dark Orange
#check #x30103E  -- Deep Purple
#check #x00CED1  -- Dark Turquoise
#check #x4169E1  -- Royal Blue

-- With alpha channel (8-digit hex)
#check #xFF0000FF  -- Red, fully opaque (alpha = FF = 255)
#check #xFF000080  -- Red, 50% transparent (alpha = 80 = 128)
#check #xFF000000  -- Red, fully transparent (alpha = 00 = 0)

-- Use in definitions
def myFavoriteColor : Hex := #x4169E1  -- Royal Blue
def sunset : Hex := #xFF6347  -- Tomato
def ocean : Hex := #x006994  -- Sea blue

def testColor : Hex := #x801CAB
def testColors : Hex × Hex := (#x006994, #xCEBAD7)
def testColors2 : Hex × Hex := (testColor, ocean)
#eval testColor
#eval testColors
#eval testColors2

-- Mix with the Hex API
#eval Hex.toHexString #x801CAB  -- "#801CAB"
#eval Hex.toHexString myFavoriteColor  -- "#4169E1"

-- Alpha channel examples
#eval (#xFF000080).a  -- 128 (50% transparent)
#eval Hex.toHexString #xFF000080  -- "#FF000080" (includes alpha since != 255)
#eval Hex.rgba 255 0 0 128  -- Create with explicit alpha

-- Example showing colors in proof goals (click on the goal to see colors)
example : myFavoriteColor = #x4169E1 := by
  -- Place cursor here and look at the goal - the @[expr_presenter] shows colors
  rfl

-- Debug test
#check (rfl : myFavoriteColor = myFavoriteColor)
