# HexLuthor Color Preview

Inline color preview for Lean 4 `#h"RRGGBB"` hex color syntax.

![HexLuthor in action](assets/screenshot.png)

## Features

- **Colored text** - Hex literals render in their actual color
- **Color picker** - Click the color swatch to open VS Code's native picker
- **2-way sync** - Drag the picker and the code updates automatically

## Installation

### From VSIX

```bash
code --install-extension hexluthor-colors-0.2.0.vsix
```

Or: `Cmd+Shift+P` → "Extensions: Install from VSIX..."

### Development

```bash
cd vscode-hexluthor
npm install
npm run compile
# Press F5 to launch Extension Development Host
```

## Usage

```lean
import HexLuthor.HexColor

#check #h"FF0000"  -- Red text!
#check #h"00FF00"  -- Green
#check #h"0000FF"  -- Blue

def myColor : Hex := #h"4169E1"  -- Royal Blue
```

## Requirements

- VS Code 1.80.0+
- Lean 4 extension
