-- Core type and utilities (no ProofWidgets dependency)
import HexLuthor.Hex

-- Syntax and elaboration
import HexLuthor.Syntax

-- ProofWidgets integration for rich infoview experience
import HexLuthor.Widget

-- Demo/example file
import HexLuthor.Basic

/-!
# HexLuthor: Hex Color Literals for Lean 4

A library for hex color literals with syntax highlighting and widget support.

## Import Options

- `import HexLuthor` - Full library with widgets (recommended)
- `import HexLuthor.Hex` - Just the Hex type and utilities (no ProofWidgets)
- `import HexLuthor.Syntax` - Hex type + `#xRRGGBB` syntax
- `import HexLuthor.Widget` - Everything including infoview widgets

## Usage

```lean
import HexLuthor

def myColor : Hex := #xFF0000  -- Red
#check #x00FF00               -- Green
```
-/
