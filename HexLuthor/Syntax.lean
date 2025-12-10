/-
  HexLuthor.Syntax: Custom #xRRGGBB syntax declaration

  Just declares the syntax. Import HexLuthor.Widget for the full
  elaborator with widget support.
-/
import HexLuthor.Hex

open Lean Parser

namespace HexLuthor

/-- Term syntax for hex color literals: #xRRGGBB (no quotes needed!) -/
syntax:max (name := hexColorLit) "#x" noWs hexnum : term

end HexLuthor
