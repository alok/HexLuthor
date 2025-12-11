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

/-- Evaluate an expression to a Hex value using the Lean runtime.
    Much lighter than `reduce` - compiles and runs instead of symbolic unfolding.

    **Risks**: Non-terminating expressions will hang. Malformed expressions could
    crash the process. Callers should pre-filter by type and catch exceptions.
    Pattern from ProofWidgets.Demos.RbTree.evalColour. -/
unsafe def evalHexUnsafe (e : Expr) : MetaM Hex :=
  Lean.Meta.evalExpr' Hex ``Hex e

@[implemented_by evalHexUnsafe]
opaque evalHex (e : Expr) : MetaM Hex

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
    -- Create the Hex value with all 4 components (including alpha)
    let r := Syntax.mkNumLit (toString color.r.toNat)
    let g := Syntax.mkNumLit (toString color.g.toNat)
    let b := Syntax.mkNumLit (toString color.b.toNat)
    let a := Syntax.mkNumLit (toString color.a.toNat)
    let hexExpr ← elabTerm (← `(Hex.mk $r $g $b $a)) expectedType?

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
    throwError "Invalid hex color: \"{hexVal}\". Expected 6 or 8 hex digits (RRGGBB or RRGGBBAA)"

/-- Elaborator for #xRRGGBB syntax -/
@[term_elab hexColorLit]
def elabHexColor : TermElab := fun stx expectedType? => do
  match stx with
  | `(#x$hexNum:hexnum) =>
    let hexVal := hexNum.raw[0].getAtomVal
    elabHexColorCore hexVal stx expectedType?
  | _ => throwUnsupportedSyntax

/-- Try to extract Hex values from an expression.
    Uses `evalHex` to evaluate at runtime - much lighter than symbolic `reduce`. -/
def extractHexFromExpr? (e : Expr) : MetaM (Option Hex) := do
  try
    let hex ← evalHex e
    return some hex
  catch _ =>
    return none

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
