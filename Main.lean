import HexLuthor

open HexLuthor

def main : IO Unit := do
  IO.println s!"White: {Hex.toHexString #xFFFFFF}"
  IO.println s!"Red: {Hex.toHexString #xFF0000}"
  IO.println s!"Royal Blue: {Hex.toHexString #x4169E1}"
