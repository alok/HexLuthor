import HexLuthor

open HexLuthor

def main : IO Unit := do
  IO.println s!"White: {Hex.toHexString #h"FFFFFF"}"
  IO.println s!"Red: {Hex.toHexString #h"FF0000"}"
  IO.println s!"Royal Blue: {Hex.toHexString #h"4169E1"}"
