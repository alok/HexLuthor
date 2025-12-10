/-
  HexColor: Custom syntax for hex colors with VS Code widget visualization
-/
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Presentation.Expr

open Lean Parser Server Widget Elab Term Meta
open scoped ProofWidgets.Jsx

namespace HexLuthor

/-- A hex color with red, green, blue components (0-255) -/
structure Hex where
  r : UInt8
  g : UInt8
  b : UInt8
  deriving Repr, DecidableEq, Inhabited

namespace Hex

def white : Hex := ⟨255, 255, 255⟩
def black : Hex := ⟨0, 0, 0⟩
def red : Hex := ⟨255, 0, 0⟩
def green : Hex := ⟨0, 255, 0⟩
def blue : Hex := ⟨0, 0, 255⟩

/-- Convert a hex color to CSS hex string like "#RRGGBB" -/
def toHexString (c : Hex) : String :=
  let hexChars := "0123456789ABCDEF".toList
  let toHex2 (n : UInt8) : String :=
    let hi := (n.toNat / 16) % 16
    let lo := n.toNat % 16
    let hiChar := hexChars[hi]!
    let loChar := hexChars[lo]!
    s!"{hiChar}{loChar}"
  s!"#{toHex2 c.r}{toHex2 c.g}{toHex2 c.b}"

/-- Parse a single hex digit character to a number 0-15 -/
def hexDigitToNat (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

/-- Parse a hex string like "RRGGBB" (without #) to a Hex color -/
def fromHexString? (s : String) : Option Hex := do
  guard (s.length == 6)
  let chars := s.toList
  let d0 ← hexDigitToNat chars[0]!
  let d1 ← hexDigitToNat chars[1]!
  let d2 ← hexDigitToNat chars[2]!
  let d3 ← hexDigitToNat chars[3]!
  let d4 ← hexDigitToNat chars[4]!
  let d5 ← hexDigitToNat chars[5]!
  let r := (d0 * 16 + d1).toUInt8
  let g := (d2 * 16 + d3).toUInt8
  let b := (d4 * 16 + d5).toUInt8
  return ⟨r, g, b⟩

end Hex

/-! ## Named Colors Database (CSS/X11 standard) -/

/-- A named color entry -/
structure NamedColor where
  name : String
  color : Hex
  deriving Repr

/-- CSS named colors - a curated subset of common colors -/
def namedColors : Array NamedColor := #[
  -- Reds
  ⟨"Red", ⟨255, 0, 0⟩⟩,
  ⟨"Crimson", ⟨220, 20, 60⟩⟩,
  ⟨"Firebrick", ⟨178, 34, 34⟩⟩,
  ⟨"DarkRed", ⟨139, 0, 0⟩⟩,
  ⟨"IndianRed", ⟨205, 92, 92⟩⟩,
  ⟨"Salmon", ⟨250, 128, 114⟩⟩,
  ⟨"Tomato", ⟨255, 99, 71⟩⟩,
  ⟨"Coral", ⟨255, 127, 80⟩⟩,
  -- Oranges
  ⟨"Orange", ⟨255, 165, 0⟩⟩,
  ⟨"DarkOrange", ⟨255, 140, 0⟩⟩,
  ⟨"OrangeRed", ⟨255, 69, 0⟩⟩,
  -- Yellows
  ⟨"Yellow", ⟨255, 255, 0⟩⟩,
  ⟨"Gold", ⟨255, 215, 0⟩⟩,
  ⟨"Khaki", ⟨240, 230, 140⟩⟩,
  ⟨"LemonChiffon", ⟨255, 250, 205⟩⟩,
  -- Greens
  ⟨"Green", ⟨0, 128, 0⟩⟩,
  ⟨"Lime", ⟨0, 255, 0⟩⟩,
  ⟨"LimeGreen", ⟨50, 205, 50⟩⟩,
  ⟨"ForestGreen", ⟨34, 139, 34⟩⟩,
  ⟨"DarkGreen", ⟨0, 100, 0⟩⟩,
  ⟨"SeaGreen", ⟨46, 139, 87⟩⟩,
  ⟨"SpringGreen", ⟨0, 255, 127⟩⟩,
  ⟨"Olive", ⟨128, 128, 0⟩⟩,
  ⟨"OliveDrab", ⟨107, 142, 35⟩⟩,
  -- Cyans
  ⟨"Cyan", ⟨0, 255, 255⟩⟩,
  ⟨"Aqua", ⟨0, 255, 255⟩⟩,
  ⟨"Teal", ⟨0, 128, 128⟩⟩,
  ⟨"DarkCyan", ⟨0, 139, 139⟩⟩,
  ⟨"Turquoise", ⟨64, 224, 208⟩⟩,
  ⟨"DarkTurquoise", ⟨0, 206, 209⟩⟩,
  -- Blues
  ⟨"Blue", ⟨0, 0, 255⟩⟩,
  ⟨"Navy", ⟨0, 0, 128⟩⟩,
  ⟨"DarkBlue", ⟨0, 0, 139⟩⟩,
  ⟨"MediumBlue", ⟨0, 0, 205⟩⟩,
  ⟨"RoyalBlue", ⟨65, 105, 225⟩⟩,
  ⟨"SteelBlue", ⟨70, 130, 180⟩⟩,
  ⟨"DodgerBlue", ⟨30, 144, 255⟩⟩,
  ⟨"DeepSkyBlue", ⟨0, 191, 255⟩⟩,
  ⟨"SkyBlue", ⟨135, 206, 235⟩⟩,
  ⟨"CornflowerBlue", ⟨100, 149, 237⟩⟩,
  -- Purples
  ⟨"Purple", ⟨128, 0, 128⟩⟩,
  ⟨"Indigo", ⟨75, 0, 130⟩⟩,
  ⟨"DarkViolet", ⟨148, 0, 211⟩⟩,
  ⟨"DarkOrchid", ⟨153, 50, 204⟩⟩,
  ⟨"MediumOrchid", ⟨186, 85, 211⟩⟩,
  ⟨"Violet", ⟨238, 130, 238⟩⟩,
  ⟨"Magenta", ⟨255, 0, 255⟩⟩,
  ⟨"Orchid", ⟨218, 112, 214⟩⟩,
  ⟨"Plum", ⟨221, 160, 221⟩⟩,
  -- Pinks
  ⟨"Pink", ⟨255, 192, 203⟩⟩,
  ⟨"HotPink", ⟨255, 105, 180⟩⟩,
  ⟨"DeepPink", ⟨255, 20, 147⟩⟩,
  ⟨"LightPink", ⟨255, 182, 193⟩⟩,
  -- Browns
  ⟨"Brown", ⟨165, 42, 42⟩⟩,
  ⟨"Maroon", ⟨128, 0, 0⟩⟩,
  ⟨"SaddleBrown", ⟨139, 69, 19⟩⟩,
  ⟨"Sienna", ⟨160, 82, 45⟩⟩,
  ⟨"Chocolate", ⟨210, 105, 30⟩⟩,
  ⟨"Peru", ⟨205, 133, 63⟩⟩,
  ⟨"Tan", ⟨210, 180, 140⟩⟩,
  ⟨"SandyBrown", ⟨244, 164, 96⟩⟩,
  -- Grays
  ⟨"White", ⟨255, 255, 255⟩⟩,
  ⟨"Snow", ⟨255, 250, 250⟩⟩,
  ⟨"Ivory", ⟨255, 255, 240⟩⟩,
  ⟨"WhiteSmoke", ⟨245, 245, 245⟩⟩,
  ⟨"Gainsboro", ⟨220, 220, 220⟩⟩,
  ⟨"Silver", ⟨192, 192, 192⟩⟩,
  ⟨"LightGray", ⟨211, 211, 211⟩⟩,
  ⟨"Gray", ⟨128, 128, 128⟩⟩,
  ⟨"DarkGray", ⟨169, 169, 169⟩⟩,
  ⟨"DimGray", ⟨105, 105, 105⟩⟩,
  ⟨"Black", ⟨0, 0, 0⟩⟩,
  -- Special
  ⟨"SlateGray", ⟨112, 128, 144⟩⟩,
  ⟨"DarkSlateGray", ⟨47, 79, 79⟩⟩,
  ⟨"LightSlateGray", ⟨119, 136, 153⟩⟩,
  ⟨"Beige", ⟨245, 245, 220⟩⟩,
  ⟨"Wheat", ⟨245, 222, 179⟩⟩,
  ⟨"Lavender", ⟨230, 230, 250⟩⟩,
  ⟨"MistyRose", ⟨255, 228, 225⟩⟩,
  ⟨"AliceBlue", ⟨240, 248, 255⟩⟩,
  ⟨"Honeydew", ⟨240, 255, 240⟩⟩,
  ⟨"MintCream", ⟨245, 255, 250⟩⟩
]

/-- Squared Euclidean distance between two colors in RGB space -/
def colorDistanceSq (c1 c2 : Hex) : Nat :=
  let dr := (c1.r.toNat : Int) - c2.r.toNat
  let dg := (c1.g.toNat : Int) - c2.g.toNat
  let db := (c1.b.toNat : Int) - c2.b.toNat
  (dr * dr + dg * dg + db * db).toNat

/-- Find the closest named color to a given hex color -/
def closestColorName (c : Hex) : String :=
  let result := namedColors.foldl (init := ("Unknown", 3 * 256 * 256)) fun (bestName, bestDist) nc =>
    let dist := colorDistanceSq c nc.color
    if dist < bestDist then (nc.name, dist) else (bestName, bestDist)
  result.1

/-- Check if a color is an exact match to a named color -/
def exactColorName? (c : Hex) : Option String :=
  namedColors.findSome? fun nc => if nc.color == c then some nc.name else none

/-- Get the color name (exact match if available, otherwise closest) -/
def colorName (c : Hex) : String :=
  match exactColorName? c with
  | some name => name
  | none => s!"≈{closestColorName c}"

/-! ## Custom Syntax: #h"RRGGBB" -/

/-- Term syntax for hex color literals: #h"RRGGBB" -/
syntax:max (name := hexColorLit) "#h" noWs str : term

/-- Create HTML for a hex color preview with name -/
def hexColorHtml (cssColor : String) (name : String) : ProofWidgets.Html :=
  <span style={json% {display: "inline-flex", alignItems: "center", gap: "8px", padding: "4px"}}>
    <span style={json% {display: "inline-block", width: "20px", height: "20px", backgroundColor: $(cssColor), border: "1px solid #666", borderRadius: "3px"}}></span>
    <code style={json% {fontSize: "1em"}}>{.text cssColor}</code>
    <span style={json% {opacity: "0.7", fontStyle: "italic"}}>{.text name}</span>
  </span>

/-- Get a unicode color square approximation for a hex color -/
def colorSquare (c : Hex) : String :=
  -- Use unicode squares for common colors, else use ◼
  let r := c.r.toNat
  let g := c.g.toNat
  let b := c.b.toNat
  -- Simple heuristic for color matching
  if r > 200 && g < 100 && b < 100 then "🟥"      -- Red
  else if r > 200 && g > 150 && b < 100 then "🟧" -- Orange
  else if r > 200 && g > 200 && b < 100 then "🟨" -- Yellow
  else if r < 100 && g > 200 && b < 100 then "🟩" -- Green
  else if r < 100 && g < 100 && b > 200 then "🟦" -- Blue
  else if r > 100 && b > 200 then "🟪"            -- Purple
  else if r > 200 && g > 200 && b > 200 then "⬜" -- White
  else if r < 50 && g < 50 && b < 50 then "⬛"    -- Black
  else if r > 150 && g > 100 && b < 100 then "🟫" -- Brown-ish
  else "◼"                                        -- Generic

/-- Core elaboration logic for hex colors -/
def elabHexColorCore (hexVal : String) (stx : Syntax) (expectedType? : Option Expr) : TermElabM Expr := do
  match Hex.fromHexString? hexVal with
  | some color =>
    -- Create the Hex value
    let r := Syntax.mkNumLit (toString color.r.toNat)
    let g := Syntax.mkNumLit (toString color.g.toNat)
    let b := Syntax.mkNumLit (toString color.b.toNat)
    let hexExpr ← elabTerm (← `(Hex.mk $r $g $b)) expectedType?

    let cssColor := color.toHexString
    let name := colorName color
    let html := hexColorHtml cssColor name

    -- Save panel widget info for the infoview (shows when clicking on term)
    Widget.savePanelWidgetInfo
      (hash ProofWidgets.HtmlDisplayPanel.javascript)
      (return json% { html: $(← rpcEncode html) })
      stx

    -- Add inlay hint with color square (shows inline in editor!)
    if let some tailPos := stx.getTailPos? then
      let square := colorSquare color
      let inlayHint : Elab.InlayHint := {
        position := tailPos
        label := .name s!" {square}"
        tooltip? := some cssColor
        paddingLeft := false
        paddingRight := false
        lctx := ← getLCtx
      }
      pushInfoLeaf <| .ofCustomInfo inlayHint.toCustomInfo

    return hexExpr
  | none =>
    throwError "Invalid hex color: \"{hexVal}\". Expected 6 hex digits (RRGGBB)"

/-- Elaborator for #h"RRGGBB" syntax -/
@[term_elab hexColorLit]
def elabHexColor : TermElab := fun stx expectedType? => do
  match stx with
  | `(#h$hexStr:str) =>
    let hexVal := hexStr.getString
    elabHexColorCore hexVal stx expectedType?
  | _ => throwUnsupportedSyntax

/-! ## Expression Presenter for Hex colors -/

/-- Try to extract Hex values from an expression -/
def extractHexFromExpr? (e : Expr) : MetaM (Option Hex) := do
  -- Try to reduce and extract the Hex struct
  let e ← whnf e
  -- Match: Hex.mk r g b
  let_expr Hex.mk r g b := e | return none
  -- Try to evaluate r, g, b as UInt8
  let some rVal ← Meta.evalNat r | return none
  let some gVal ← Meta.evalNat g | return none
  let some bVal ← Meta.evalNat b | return none
  return some ⟨rVal.toUInt8, gVal.toUInt8, bVal.toUInt8⟩

/-- Presenter for Hex expressions - shows color preview inline -/
@[expr_presenter]
def hexPresenter : ProofWidgets.ExprPresenter where
  userName := "Hex Color"
  layoutKind := .inline
  present e := do
    -- Check if type is Hex
    let ty ← Meta.inferType e
    let_expr HexLuthor.Hex := ty | return .text s!"{← Meta.ppExpr e}"
    -- Try to extract the color value
    match ← extractHexFromExpr? e with
    | some color =>
      let cssColor := color.toHexString
      let name := colorName color
      let pp ← Meta.ppExpr e
      -- Colored square + hex + name
      return <span style={json% {display: "inline-flex", alignItems: "center", gap: "4px"}}>
        <span style={json% {background: $(cssColor), width: "12px", height: "12px", border: "1px solid gray", borderRadius: "2px", display: "inline-block"}}></span>
        <code>{.text cssColor}</code>
        <span style={json% {opacity: "0.7", fontStyle: "italic"}}>{.text name}</span>
        <span style={json% {opacity: "0.4"}}>{.text s!" ({pp})"}</span>
      </span>
    | none =>
      -- Can't evaluate statically, just show the expression
      return .text s!"{← Meta.ppExpr e}"

end HexLuthor
