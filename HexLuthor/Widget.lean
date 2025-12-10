/-
  HexLuthor.Widget: Full elaborator with ProofWidgets integration

  This module provides:
  - The term elaborator for #xRRGGBB syntax
  - Panel widgets in the infoview when clicking on hex colors
  - Expression presenter for Hex values in goals

  Import this for the full visual experience.
-/
import HexLuthor.Syntax
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Presentation.Expr

open Lean Widget Meta Elab Elab.Term Server
open scoped ProofWidgets.Jsx

namespace HexLuthor

/-- Create HTML for a hex color preview with name -/
def hexColorHtml (cssColor : String) (name : String) : ProofWidgets.Html :=
  <span style={json% {display: "inline-flex", alignItems: "center", gap: "8px", padding: "4px"}}>
    <span style={json% {display: "inline-block", width: "20px", height: "20px", backgroundColor: $(cssColor), border: "1px solid #666", borderRadius: "3px"}}></span>
    <code style={json% {fontSize: "1em"}}>{.text cssColor}</code>
    <span style={json% {opacity: "0.7", fontStyle: "italic"}}>{.text name}</span>
  </span>

/-- Core elaboration logic for hex colors with full widget support -/
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

    return hexExpr
  | none =>
    throwError "Invalid hex color: \"{hexVal}\". Expected 6 hex digits (RRGGBB)"

/-- Elaborator for #xRRGGBB syntax -/
@[term_elab hexColorLit]
def elabHexColor : TermElab := fun stx expectedType? => do
  match stx with
  | `(#x$hexNum:hexnum) =>
    let hexVal := hexNum.raw[0].getAtomVal
    elabHexColorCore hexVal stx expectedType?
  | _ => throwUnsupportedSyntax

/-- Try to extract Hex values from an expression -/
def extractHexFromExpr? (e : Expr) : MetaM (Option Hex) := do
  let e ← whnf e
  let_expr Hex.mk r g b := e | return none
  let some rVal ← evalNat r | return none
  let some gVal ← evalNat g | return none
  let some bVal ← evalNat b | return none
  return some ⟨rVal.toUInt8, gVal.toUInt8, bVal.toUInt8⟩

/-- Presenter for Hex expressions - shows color preview inline in infoview -/
@[expr_presenter]
def hexPresenter : ProofWidgets.ExprPresenter where
  userName := "Hex Color"
  layoutKind := .inline
  present e := do
    let ty ← inferType e
    let_expr HexLuthor.Hex := ty | return .text s!"{← ppExpr e}"
    match ← extractHexFromExpr? e with
    | some color =>
      let cssColor := color.toHexString
      let name := colorName color
      let pp ← ppExpr e
      return <span style={json% {display: "inline-flex", alignItems: "center", gap: "4px"}}>
        <span style={json% {background: $(cssColor), width: "12px", height: "12px", border: "1px solid gray", borderRadius: "2px", display: "inline-block"}}></span>
        <code>{.text cssColor}</code>
        <span style={json% {opacity: "0.7", fontStyle: "italic"}}>{.text name}</span>
        <span style={json% {opacity: "0.4"}}>{.text s!" ({pp})"}</span>
      </span>
    | none =>
      return .text s!"{← ppExpr e}"

end HexLuthor
