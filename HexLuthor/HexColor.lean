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

/-! ## Custom Syntax: #h"RRGGBB" -/

/-- Term syntax for hex color literals: #h"RRGGBB" -/
syntax:max (name := hexColorLit) "#h" noWs str : term

/-- Create HTML for a hex color preview -/
def hexColorHtml (cssColor : String) : ProofWidgets.Html :=
  <span style={json% {display: "inline-flex", alignItems: "center", gap: "8px", padding: "4px"}}>
    <span style={json% {display: "inline-block", width: "20px", height: "20px", backgroundColor: $(cssColor), border: "1px solid #666", borderRadius: "3px"}}></span>
    <code style={json% {fontSize: "1em"}}>{.text cssColor}</code>
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
    let html := hexColorHtml cssColor

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
      let pp ← Meta.ppExpr e
      -- Simple colored square + text
      return <span style={json% {display: "inline-flex", alignItems: "center", gap: "4px"}}>
        <span style={json% {background: $(cssColor), width: "12px", height: "12px", border: "1px solid gray", borderRadius: "2px", display: "inline-block"}}></span>
        <code>{.text cssColor}</code>
        <span style={json% {opacity: "0.5"}}>{.text s!" ({pp})"}</span>
      </span>
    | none =>
      -- Can't evaluate statically, just show the expression
      return .text s!"{← Meta.ppExpr e}"

end HexLuthor
