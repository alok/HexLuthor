/-
  HexLuthor.Hex: Core hex color type and utilities

  This module provides the fundamental Hex type and color operations
  WITHOUT any ProofWidgets dependencies. Import this if you just need
  the type and parsing functionality.
-/
import Std.Internal.Parsec

namespace HexLuthor

open Std.Internal.Parsec String

/-- A hex color with red, green, blue, alpha components (0-255).
    Alpha defaults to 255 (fully opaque). -/
structure Hex where
  /-- Red component. `0` is no red, `255` is full red. -/
  r : UInt8 := 0
  /-- Green component. `0` is no green, `255` is full green. -/
  g : UInt8 := 0
  /-- Blue component. `0` is no blue, `255` is full blue. -/
  b : UInt8 := 0
  /-- Alpha component. `0` is fully transparent, `255` is fully opaque. -/
  a : UInt8 := 255
  deriving BEq, Hashable, Repr, DecidableEq, Inhabited

namespace Hex

/-- Create an opaque RGB color (alpha = 255) -/
@[inline] def rgb (r g b : UInt8) : Hex := ⟨r, g, b, 255⟩

/-- Create an RGBA color with explicit alpha -/
@[inline] def rgba (r g b a : UInt8) : Hex := ⟨r, g, b, a⟩

def white : Hex := rgb 255 255 255
def black : Hex := rgb 0 0 0
def red : Hex := rgb 255 0 0
def green : Hex := rgb 0 255 0
def blue : Hex := rgb 0 0 255

/-- Convert a hex color to CSS hex string like "#RRGGBB" -/
def toHexString (c : Hex) : String :=
  let hexChars : Vector Char 16 := #v[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']
  let toHex2 (n : UInt8) : String :=
    let hi := (n.toNat / 16) % 16
    let lo := n.toNat % 16
    let hiChar := hexChars[hi]
    let loChar := hexChars[lo]
    s!"{hiChar}{loChar}"
  s!"#{toHex2 c.r}{toHex2 c.g}{toHex2 c.b}"

/-! ### Parsec-based hex parsing -/

/-- Convert a hex digit char to its numeric value (0-15) -/
def hexCharToNat (c : Char) : Nat :=
  if '0' ≤ c && c ≤ '9' then c.toNat - '0'.toNat
  else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10
  else c.toNat - 'A'.toNat + 10  -- 'A'..'F'

/-- Parse two hex digits as a UInt8 (e.g., "FF" → 255) -/
def hexPair : Std.Internal.Parsec.String.Parser UInt8 := do
  let hi ← hexDigit
  let lo ← hexDigit
  return (hexCharToNat hi * 16 + hexCharToNat lo).toUInt8

/-- Parse a 6 or 8 digit hex color string "RRGGBB" or "RRGGBBAA" into a Hex -/
def hexColorParser : Std.Internal.Parsec.String.Parser Hex := do
  let r ← hexPair
  let g ← hexPair
  let b ← hexPair
  -- Optional alpha channel
  let a ← (hexPair <* eof) <|> (eof *> pure 255)
  return { r, g, b, a }

/-- Parse a hex string like "RRGGBB" (without #) to a Hex color -/
def fromHexString? (s : String) : Option Hex :=
  match hexColorParser.run s with
  | .ok hex => some hex
  | .error _ => none

end Hex

/-! ## Named Colors Database (CSS/X11 standard) -/

/-- A named color entry -/
structure NamedColor where
  name : String
  color : Hex
  deriving Repr

/-- CSS named colors - comprehensive coverage including dark/light variants -/
def namedColors : Array NamedColor := #[
  -- Reds (light to dark)
  ⟨"LightCoral", { r := 240, g := 128, b := 128 }⟩,
  ⟨"Salmon", { r := 250, g := 128, b := 114 }⟩,
  ⟨"DarkSalmon", { r := 233, g := 150, b := 122 }⟩,
  ⟨"LightSalmon", { r := 255, g := 160, b := 122 }⟩,
  ⟨"Coral", { r := 255, g := 127, b := 80 }⟩,
  ⟨"Tomato", { r := 255, g := 99, b := 71 }⟩,
  ⟨"Red", { r := 255, g := 0, b := 0 }⟩,
  ⟨"Crimson", { r := 220, g := 20, b := 60 }⟩,
  ⟨"IndianRed", { r := 205, g := 92, b := 92 }⟩,
  ⟨"Firebrick", { r := 178, g := 34, b := 34 }⟩,
  ⟨"DarkRed", { r := 139, g := 0, b := 0 }⟩,
  ⟨"Maroon", { r := 128, g := 0, b := 0 }⟩,

  -- Oranges
  ⟨"LightOrange", { r := 255, g := 200, b := 128 }⟩,
  ⟨"PeachPuff", { r := 255, g := 218, b := 185 }⟩,
  ⟨"Moccasin", { r := 255, g := 228, b := 181 }⟩,
  ⟨"PapayaWhip", { r := 255, g := 239, b := 213 }⟩,
  ⟨"Orange", { r := 255, g := 165, b := 0 }⟩,
  ⟨"DarkOrange", { r := 255, g := 140, b := 0 }⟩,
  ⟨"OrangeRed", { r := 255, g := 69, b := 0 }⟩,
  ⟨"BurntOrange", { r := 204, g := 85, b := 0 }⟩,

  -- Yellows
  ⟨"LightYellow", { r := 255, g := 255, b := 224 }⟩,
  ⟨"LemonChiffon", { r := 255, g := 250, b := 205 }⟩,
  ⟨"LightGoldenrodYellow", { r := 250, g := 250, b := 210 }⟩,
  ⟨"PaleGoldenrod", { r := 238, g := 232, b := 170 }⟩,
  ⟨"Yellow", { r := 255, g := 255, b := 0 }⟩,
  ⟨"Gold", { r := 255, g := 215, b := 0 }⟩,
  ⟨"Goldenrod", { r := 218, g := 165, b := 32 }⟩,
  ⟨"DarkGoldenrod", { r := 184, g := 134, b := 11 }⟩,
  ⟨"Khaki", { r := 240, g := 230, b := 140 }⟩,
  ⟨"DarkKhaki", { r := 189, g := 183, b := 107 }⟩,

  -- Greens (light to dark)
  ⟨"GreenYellow", { r := 173, g := 255, b := 47 }⟩,
  ⟨"Chartreuse", { r := 127, g := 255, b := 0 }⟩,
  ⟨"LawnGreen", { r := 124, g := 252, b := 0 }⟩,
  ⟨"Lime", { r := 0, g := 255, b := 0 }⟩,
  ⟨"LimeGreen", { r := 50, g := 205, b := 50 }⟩,
  ⟨"PaleGreen", { r := 152, g := 251, b := 152 }⟩,
  ⟨"LightGreen", { r := 144, g := 238, b := 144 }⟩,
  ⟨"MediumSpringGreen", { r := 0, g := 250, b := 154 }⟩,
  ⟨"SpringGreen", { r := 0, g := 255, b := 127 }⟩,
  ⟨"MediumSeaGreen", { r := 60, g := 179, b := 113 }⟩,
  ⟨"SeaGreen", { r := 46, g := 139, b := 87 }⟩,
  ⟨"Green", { r := 0, g := 128, b := 0 }⟩,
  ⟨"ForestGreen", { r := 34, g := 139, b := 34 }⟩,
  ⟨"DarkGreen", { r := 0, g := 100, b := 0 }⟩,
  ⟨"DarkOliveGreen", { r := 85, g := 107, b := 47 }⟩,
  ⟨"Olive", { r := 128, g := 128, b := 0 }⟩,
  ⟨"OliveDrab", { r := 107, g := 142, b := 35 }⟩,
  ⟨"YellowGreen", { r := 154, g := 205, b := 50 }⟩,

  -- Cyans / Aquas
  ⟨"LightCyan", { r := 224, g := 255, b := 255 }⟩,
  ⟨"PaleTurquoise", { r := 175, g := 238, b := 238 }⟩,
  ⟨"Aquamarine", { r := 127, g := 255, b := 212 }⟩,
  ⟨"MediumAquamarine", { r := 102, g := 205, b := 170 }⟩,
  ⟨"Turquoise", { r := 64, g := 224, b := 208 }⟩,
  ⟨"MediumTurquoise", { r := 72, g := 209, b := 204 }⟩,
  ⟨"DarkTurquoise", { r := 0, g := 206, b := 209 }⟩,
  ⟨"Cyan", { r := 0, g := 255, b := 255 }⟩,
  ⟨"Aqua", { r := 0, g := 255, b := 255 }⟩,
  ⟨"LightSeaGreen", { r := 32, g := 178, b := 170 }⟩,
  ⟨"CadetBlue", { r := 95, g := 158, b := 160 }⟩,
  ⟨"DarkCyan", { r := 0, g := 139, b := 139 }⟩,
  ⟨"Teal", { r := 0, g := 128, b := 128 }⟩,

  -- Blues (light to dark)
  ⟨"LightBlue", { r := 173, g := 216, b := 230 }⟩,
  ⟨"PowderBlue", { r := 176, g := 224, b := 230 }⟩,
  ⟨"SkyBlue", { r := 135, g := 206, b := 235 }⟩,
  ⟨"LightSkyBlue", { r := 135, g := 206, b := 250 }⟩,
  ⟨"DeepSkyBlue", { r := 0, g := 191, b := 255 }⟩,
  ⟨"DodgerBlue", { r := 30, g := 144, b := 255 }⟩,
  ⟨"CornflowerBlue", { r := 100, g := 149, b := 237 }⟩,
  ⟨"SteelBlue", { r := 70, g := 130, b := 180 }⟩,
  ⟨"RoyalBlue", { r := 65, g := 105, b := 225 }⟩,
  ⟨"Blue", { r := 0, g := 0, b := 255 }⟩,
  ⟨"MediumBlue", { r := 0, g := 0, b := 205 }⟩,
  ⟨"DarkBlue", { r := 0, g := 0, b := 139 }⟩,
  ⟨"Navy", { r := 0, g := 0, b := 128 }⟩,
  ⟨"MidnightBlue", { r := 25, g := 25, b := 112 }⟩,

  -- Purples / Violets (light to very dark)
  ⟨"Lavender", { r := 230, g := 230, b := 250 }⟩,
  ⟨"Thistle", { r := 216, g := 191, b := 216 }⟩,
  ⟨"Plum", { r := 221, g := 160, b := 221 }⟩,
  ⟨"Violet", { r := 238, g := 130, b := 238 }⟩,
  ⟨"Orchid", { r := 218, g := 112, b := 214 }⟩,
  ⟨"Fuchsia", { r := 255, g := 0, b := 255 }⟩,
  ⟨"Magenta", { r := 255, g := 0, b := 255 }⟩,
  ⟨"MediumOrchid", { r := 186, g := 85, b := 211 }⟩,
  ⟨"MediumPurple", { r := 147, g := 112, b := 219 }⟩,
  ⟨"BlueViolet", { r := 138, g := 43, b := 226 }⟩,
  ⟨"DarkViolet", { r := 148, g := 0, b := 211 }⟩,
  ⟨"DarkOrchid", { r := 153, g := 50, b := 204 }⟩,
  ⟨"DarkMagenta", { r := 139, g := 0, b := 139 }⟩,
  ⟨"Purple", { r := 128, g := 0, b := 128 }⟩,
  ⟨"RebeccaPurple", { r := 102, g := 51, b := 153 }⟩,
  ⟨"MediumSlateBlue", { r := 123, g := 104, b := 238 }⟩,
  ⟨"SlateBlue", { r := 106, g := 90, b := 205 }⟩,
  ⟨"DarkSlateBlue", { r := 72, g := 61, b := 139 }⟩,
  ⟨"Indigo", { r := 75, g := 0, b := 130 }⟩,
  -- Very dark purples (for colors like #30103E)
  ⟨"DeepPurple", { r := 48, g := 16, b := 62 }⟩,
  ⟨"MidnightPurple", { r := 40, g := 20, b := 60 }⟩,
  ⟨"DarkIndigo", { r := 50, g := 0, b := 80 }⟩,
  ⟨"BlackPurple", { r := 30, g := 10, b := 40 }⟩,
  ⟨"DarkPlum", { r := 60, g := 20, b := 60 }⟩,

  -- Pinks
  ⟨"LavenderBlush", { r := 255, g := 240, b := 245 }⟩,
  ⟨"MistyRose", { r := 255, g := 228, b := 225 }⟩,
  ⟨"Pink", { r := 255, g := 192, b := 203 }⟩,
  ⟨"LightPink", { r := 255, g := 182, b := 193 }⟩,
  ⟨"HotPink", { r := 255, g := 105, b := 180 }⟩,
  ⟨"DeepPink", { r := 255, g := 20, b := 147 }⟩,
  ⟨"PaleVioletRed", { r := 219, g := 112, b := 147 }⟩,
  ⟨"MediumVioletRed", { r := 199, g := 21, b := 133 }⟩,

  -- Browns (light to dark)
  ⟨"Cornsilk", { r := 255, g := 248, b := 220 }⟩,
  ⟨"BlanchedAlmond", { r := 255, g := 235, b := 205 }⟩,
  ⟨"Bisque", { r := 255, g := 228, b := 196 }⟩,
  ⟨"NavajoWhite", { r := 255, g := 222, b := 173 }⟩,
  ⟨"Wheat", { r := 245, g := 222, b := 179 }⟩,
  ⟨"BurlyWood", { r := 222, g := 184, b := 135 }⟩,
  ⟨"Tan", { r := 210, g := 180, b := 140 }⟩,
  ⟨"RosyBrown", { r := 188, g := 143, b := 143 }⟩,
  ⟨"SandyBrown", { r := 244, g := 164, b := 96 }⟩,
  ⟨"Peru", { r := 205, g := 133, b := 63 }⟩,
  ⟨"Chocolate", { r := 210, g := 105, b := 30 }⟩,
  ⟨"Sienna", { r := 160, g := 82, b := 45 }⟩,
  ⟨"Brown", { r := 165, g := 42, b := 42 }⟩,
  ⟨"SaddleBrown", { r := 139, g := 69, b := 19 }⟩,
  -- Very dark browns
  ⟨"DarkBrown", { r := 92, g := 64, b := 51 }⟩,
  ⟨"Espresso", { r := 59, g := 36, b := 27 }⟩,
  ⟨"CoffeeBrown", { r := 75, g := 54, b := 33 }⟩,

  -- Grays (white to black, fine gradation)
  ⟨"White", { r := 255, g := 255, b := 255 }⟩,
  ⟨"Snow", { r := 255, g := 250, b := 250 }⟩,
  ⟨"Ivory", { r := 255, g := 255, b := 240 }⟩,
  ⟨"FloralWhite", { r := 255, g := 250, b := 240 }⟩,
  ⟨"GhostWhite", { r := 248, g := 248, b := 255 }⟩,
  ⟨"WhiteSmoke", { r := 245, g := 245, b := 245 }⟩,
  ⟨"Seashell", { r := 255, g := 245, b := 238 }⟩,
  ⟨"AntiqueWhite", { r := 250, g := 235, b := 215 }⟩,
  ⟨"Linen", { r := 250, g := 240, b := 230 }⟩,
  ⟨"OldLace", { r := 253, g := 245, b := 230 }⟩,
  ⟨"Beige", { r := 245, g := 245, b := 220 }⟩,
  ⟨"Gainsboro", { r := 220, g := 220, b := 220 }⟩,
  ⟨"LightGray", { r := 211, g := 211, b := 211 }⟩,
  ⟨"Silver", { r := 192, g := 192, b := 192 }⟩,
  ⟨"DarkGray", { r := 169, g := 169, b := 169 }⟩,
  ⟨"Gray", { r := 128, g := 128, b := 128 }⟩,
  ⟨"DimGray", { r := 105, g := 105, b := 105 }⟩,
  ⟨"LightSlateGray", { r := 119, g := 136, b := 153 }⟩,
  ⟨"SlateGray", { r := 112, g := 128, b := 144 }⟩,
  ⟨"DarkSlateGray", { r := 47, g := 79, b := 79 }⟩,
  -- Very dark grays
  ⟨"Charcoal", { r := 54, g := 69, b := 79 }⟩,
  ⟨"Jet", { r := 52, g := 52, b := 52 }⟩,
  ⟨"Onyx", { r := 53, g := 56, b := 57 }⟩,
  ⟨"EerieBlack", { r := 27, g := 27, b := 27 }⟩,
  ⟨"Black", { r := 0, g := 0, b := 0 }⟩,

  -- Additional special colors
  ⟨"AliceBlue", { r := 240, g := 248, b := 255 }⟩,
  ⟨"Azure", { r := 240, g := 255, b := 255 }⟩,
  ⟨"Honeydew", { r := 240, g := 255, b := 240 }⟩,
  ⟨"MintCream", { r := 245, g := 255, b := 250 }⟩,

  -- Metallic approximations
  ⟨"Copper", { r := 184, g := 115, b := 51 }⟩,
  ⟨"Bronze", { r := 205, g := 127, b := 50 }⟩,
  ⟨"BrassYellow", { r := 181, g := 166, b := 66 }⟩,

  -- Neon/Electric colors
  ⟨"ElectricBlue", { r := 125, g := 249, b := 255 }⟩,
  ⟨"ElectricPurple", { r := 191, g := 0, b := 255 }⟩,
  ⟨"NeonGreen", { r := 57, g := 255, b := 20 }⟩,
  ⟨"NeonPink", { r := 255, g := 16, b := 240 }⟩,

  -- Nature-inspired
  ⟨"ForestMoss", { r := 56, g := 93, b := 56 }⟩,
  ⟨"Sage", { r := 176, g := 208, b := 176 }⟩,
  ⟨"Seafoam", { r := 159, g := 226, b := 191 }⟩,
  ⟨"Ocean", { r := 0, g := 105, b := 148 }⟩,
  ⟨"DeepOcean", { r := 0, g := 51, b := 102 }⟩,
  ⟨"Sunset", { r := 250, g := 214, b := 165 }⟩,
  ⟨"Dusk", { r := 78, g := 81, b := 128 }⟩,
  ⟨"Wine", { r := 114, g := 47, b := 55 }⟩,
  ⟨"Burgundy", { r := 128, g := 0, b := 32 }⟩,
  ⟨"Mauve", { r := 224, g := 176, b := 255 }⟩,
  ⟨"Lilac", { r := 200, g := 162, b := 200 }⟩,
  ⟨"Periwinkle", { r := 204, g := 204, b := 255 }⟩,
  ⟨"Wisteria", { r := 201, g := 160, b := 220 }⟩
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

/-- Get a unicode color square approximation for a hex color -/
def colorSquare (c : Hex) : String :=
  let r := c.r.toNat
  let g := c.g.toNat
  let b := c.b.toNat
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

end HexLuthor
