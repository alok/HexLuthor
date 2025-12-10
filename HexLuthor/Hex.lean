/-
  HexLuthor.Hex: Core hex color type and utilities

  This module provides the fundamental Hex type and color operations
  WITHOUT any ProofWidgets dependencies. Import this if you just need
  the type and parsing functionality.
-/
import Std.Internal.Parsec

namespace HexLuthor

open Std.Internal.Parsec String

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

/-- Parse a 6-digit hex color string "RRGGBB" into a Hex -/
def hexColorParser : Std.Internal.Parsec.String.Parser Hex := do
  let r ← hexPair
  let g ← hexPair
  let b ← hexPair
  eof
  return ⟨r, g, b⟩

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
  ⟨"LightCoral", ⟨240, 128, 128⟩⟩,
  ⟨"Salmon", ⟨250, 128, 114⟩⟩,
  ⟨"DarkSalmon", ⟨233, 150, 122⟩⟩,
  ⟨"LightSalmon", ⟨255, 160, 122⟩⟩,
  ⟨"Coral", ⟨255, 127, 80⟩⟩,
  ⟨"Tomato", ⟨255, 99, 71⟩⟩,
  ⟨"Red", ⟨255, 0, 0⟩⟩,
  ⟨"Crimson", ⟨220, 20, 60⟩⟩,
  ⟨"IndianRed", ⟨205, 92, 92⟩⟩,
  ⟨"Firebrick", ⟨178, 34, 34⟩⟩,
  ⟨"DarkRed", ⟨139, 0, 0⟩⟩,
  ⟨"Maroon", ⟨128, 0, 0⟩⟩,

  -- Oranges
  ⟨"LightOrange", ⟨255, 200, 128⟩⟩,
  ⟨"PeachPuff", ⟨255, 218, 185⟩⟩,
  ⟨"Moccasin", ⟨255, 228, 181⟩⟩,
  ⟨"PapayaWhip", ⟨255, 239, 213⟩⟩,
  ⟨"Orange", ⟨255, 165, 0⟩⟩,
  ⟨"DarkOrange", ⟨255, 140, 0⟩⟩,
  ⟨"OrangeRed", ⟨255, 69, 0⟩⟩,
  ⟨"BurntOrange", ⟨204, 85, 0⟩⟩,

  -- Yellows
  ⟨"LightYellow", ⟨255, 255, 224⟩⟩,
  ⟨"LemonChiffon", ⟨255, 250, 205⟩⟩,
  ⟨"LightGoldenrodYellow", ⟨250, 250, 210⟩⟩,
  ⟨"PaleGoldenrod", ⟨238, 232, 170⟩⟩,
  ⟨"Yellow", ⟨255, 255, 0⟩⟩,
  ⟨"Gold", ⟨255, 215, 0⟩⟩,
  ⟨"Goldenrod", ⟨218, 165, 32⟩⟩,
  ⟨"DarkGoldenrod", ⟨184, 134, 11⟩⟩,
  ⟨"Khaki", ⟨240, 230, 140⟩⟩,
  ⟨"DarkKhaki", ⟨189, 183, 107⟩⟩,

  -- Greens (light to dark)
  ⟨"GreenYellow", ⟨173, 255, 47⟩⟩,
  ⟨"Chartreuse", ⟨127, 255, 0⟩⟩,
  ⟨"LawnGreen", ⟨124, 252, 0⟩⟩,
  ⟨"Lime", ⟨0, 255, 0⟩⟩,
  ⟨"LimeGreen", ⟨50, 205, 50⟩⟩,
  ⟨"PaleGreen", ⟨152, 251, 152⟩⟩,
  ⟨"LightGreen", ⟨144, 238, 144⟩⟩,
  ⟨"MediumSpringGreen", ⟨0, 250, 154⟩⟩,
  ⟨"SpringGreen", ⟨0, 255, 127⟩⟩,
  ⟨"MediumSeaGreen", ⟨60, 179, 113⟩⟩,
  ⟨"SeaGreen", ⟨46, 139, 87⟩⟩,
  ⟨"Green", ⟨0, 128, 0⟩⟩,
  ⟨"ForestGreen", ⟨34, 139, 34⟩⟩,
  ⟨"DarkGreen", ⟨0, 100, 0⟩⟩,
  ⟨"DarkOliveGreen", ⟨85, 107, 47⟩⟩,
  ⟨"Olive", ⟨128, 128, 0⟩⟩,
  ⟨"OliveDrab", ⟨107, 142, 35⟩⟩,
  ⟨"YellowGreen", ⟨154, 205, 50⟩⟩,

  -- Cyans / Aquas
  ⟨"LightCyan", ⟨224, 255, 255⟩⟩,
  ⟨"PaleTurquoise", ⟨175, 238, 238⟩⟩,
  ⟨"Aquamarine", ⟨127, 255, 212⟩⟩,
  ⟨"MediumAquamarine", ⟨102, 205, 170⟩⟩,
  ⟨"Turquoise", ⟨64, 224, 208⟩⟩,
  ⟨"MediumTurquoise", ⟨72, 209, 204⟩⟩,
  ⟨"DarkTurquoise", ⟨0, 206, 209⟩⟩,
  ⟨"Cyan", ⟨0, 255, 255⟩⟩,
  ⟨"Aqua", ⟨0, 255, 255⟩⟩,
  ⟨"LightSeaGreen", ⟨32, 178, 170⟩⟩,
  ⟨"CadetBlue", ⟨95, 158, 160⟩⟩,
  ⟨"DarkCyan", ⟨0, 139, 139⟩⟩,
  ⟨"Teal", ⟨0, 128, 128⟩⟩,

  -- Blues (light to dark)
  ⟨"LightBlue", ⟨173, 216, 230⟩⟩,
  ⟨"PowderBlue", ⟨176, 224, 230⟩⟩,
  ⟨"SkyBlue", ⟨135, 206, 235⟩⟩,
  ⟨"LightSkyBlue", ⟨135, 206, 250⟩⟩,
  ⟨"DeepSkyBlue", ⟨0, 191, 255⟩⟩,
  ⟨"DodgerBlue", ⟨30, 144, 255⟩⟩,
  ⟨"CornflowerBlue", ⟨100, 149, 237⟩⟩,
  ⟨"SteelBlue", ⟨70, 130, 180⟩⟩,
  ⟨"RoyalBlue", ⟨65, 105, 225⟩⟩,
  ⟨"Blue", ⟨0, 0, 255⟩⟩,
  ⟨"MediumBlue", ⟨0, 0, 205⟩⟩,
  ⟨"DarkBlue", ⟨0, 0, 139⟩⟩,
  ⟨"Navy", ⟨0, 0, 128⟩⟩,
  ⟨"MidnightBlue", ⟨25, 25, 112⟩⟩,

  -- Purples / Violets (light to very dark)
  ⟨"Lavender", ⟨230, 230, 250⟩⟩,
  ⟨"Thistle", ⟨216, 191, 216⟩⟩,
  ⟨"Plum", ⟨221, 160, 221⟩⟩,
  ⟨"Violet", ⟨238, 130, 238⟩⟩,
  ⟨"Orchid", ⟨218, 112, 214⟩⟩,
  ⟨"Fuchsia", ⟨255, 0, 255⟩⟩,
  ⟨"Magenta", ⟨255, 0, 255⟩⟩,
  ⟨"MediumOrchid", ⟨186, 85, 211⟩⟩,
  ⟨"MediumPurple", ⟨147, 112, 219⟩⟩,
  ⟨"BlueViolet", ⟨138, 43, 226⟩⟩,
  ⟨"DarkViolet", ⟨148, 0, 211⟩⟩,
  ⟨"DarkOrchid", ⟨153, 50, 204⟩⟩,
  ⟨"DarkMagenta", ⟨139, 0, 139⟩⟩,
  ⟨"Purple", ⟨128, 0, 128⟩⟩,
  ⟨"RebeccaPurple", ⟨102, 51, 153⟩⟩,
  ⟨"MediumSlateBlue", ⟨123, 104, 238⟩⟩,
  ⟨"SlateBlue", ⟨106, 90, 205⟩⟩,
  ⟨"DarkSlateBlue", ⟨72, 61, 139⟩⟩,
  ⟨"Indigo", ⟨75, 0, 130⟩⟩,
  -- Very dark purples (for colors like #30103E)
  ⟨"DeepPurple", ⟨48, 16, 62⟩⟩,
  ⟨"MidnightPurple", ⟨40, 20, 60⟩⟩,
  ⟨"DarkIndigo", ⟨50, 0, 80⟩⟩,
  ⟨"BlackPurple", ⟨30, 10, 40⟩⟩,
  ⟨"DarkPlum", ⟨60, 20, 60⟩⟩,

  -- Pinks
  ⟨"LavenderBlush", ⟨255, 240, 245⟩⟩,
  ⟨"MistyRose", ⟨255, 228, 225⟩⟩,
  ⟨"Pink", ⟨255, 192, 203⟩⟩,
  ⟨"LightPink", ⟨255, 182, 193⟩⟩,
  ⟨"HotPink", ⟨255, 105, 180⟩⟩,
  ⟨"DeepPink", ⟨255, 20, 147⟩⟩,
  ⟨"PaleVioletRed", ⟨219, 112, 147⟩⟩,
  ⟨"MediumVioletRed", ⟨199, 21, 133⟩⟩,

  -- Browns (light to dark)
  ⟨"Cornsilk", ⟨255, 248, 220⟩⟩,
  ⟨"BlanchedAlmond", ⟨255, 235, 205⟩⟩,
  ⟨"Bisque", ⟨255, 228, 196⟩⟩,
  ⟨"NavajoWhite", ⟨255, 222, 173⟩⟩,
  ⟨"Wheat", ⟨245, 222, 179⟩⟩,
  ⟨"BurlyWood", ⟨222, 184, 135⟩⟩,
  ⟨"Tan", ⟨210, 180, 140⟩⟩,
  ⟨"RosyBrown", ⟨188, 143, 143⟩⟩,
  ⟨"SandyBrown", ⟨244, 164, 96⟩⟩,
  ⟨"Peru", ⟨205, 133, 63⟩⟩,
  ⟨"Chocolate", ⟨210, 105, 30⟩⟩,
  ⟨"Sienna", ⟨160, 82, 45⟩⟩,
  ⟨"Brown", ⟨165, 42, 42⟩⟩,
  ⟨"SaddleBrown", ⟨139, 69, 19⟩⟩,
  -- Very dark browns
  ⟨"DarkBrown", ⟨92, 64, 51⟩⟩,
  ⟨"Espresso", ⟨59, 36, 27⟩⟩,
  ⟨"CoffeeBrown", ⟨75, 54, 33⟩⟩,

  -- Grays (white to black, fine gradation)
  ⟨"White", ⟨255, 255, 255⟩⟩,
  ⟨"Snow", ⟨255, 250, 250⟩⟩,
  ⟨"Ivory", ⟨255, 255, 240⟩⟩,
  ⟨"FloralWhite", ⟨255, 250, 240⟩⟩,
  ⟨"GhostWhite", ⟨248, 248, 255⟩⟩,
  ⟨"WhiteSmoke", ⟨245, 245, 245⟩⟩,
  ⟨"Seashell", ⟨255, 245, 238⟩⟩,
  ⟨"AntiqueWhite", ⟨250, 235, 215⟩⟩,
  ⟨"Linen", ⟨250, 240, 230⟩⟩,
  ⟨"OldLace", ⟨253, 245, 230⟩⟩,
  ⟨"Beige", ⟨245, 245, 220⟩⟩,
  ⟨"Gainsboro", ⟨220, 220, 220⟩⟩,
  ⟨"LightGray", ⟨211, 211, 211⟩⟩,
  ⟨"Silver", ⟨192, 192, 192⟩⟩,
  ⟨"DarkGray", ⟨169, 169, 169⟩⟩,
  ⟨"Gray", ⟨128, 128, 128⟩⟩,
  ⟨"DimGray", ⟨105, 105, 105⟩⟩,
  ⟨"LightSlateGray", ⟨119, 136, 153⟩⟩,
  ⟨"SlateGray", ⟨112, 128, 144⟩⟩,
  ⟨"DarkSlateGray", ⟨47, 79, 79⟩⟩,
  -- Very dark grays
  ⟨"Charcoal", ⟨54, 69, 79⟩⟩,
  ⟨"Jet", ⟨52, 52, 52⟩⟩,
  ⟨"Onyx", ⟨53, 56, 57⟩⟩,
  ⟨"EerieBlack", ⟨27, 27, 27⟩⟩,
  ⟨"Black", ⟨0, 0, 0⟩⟩,

  -- Additional special colors
  ⟨"AliceBlue", ⟨240, 248, 255⟩⟩,
  ⟨"Azure", ⟨240, 255, 255⟩⟩,
  ⟨"Honeydew", ⟨240, 255, 240⟩⟩,
  ⟨"MintCream", ⟨245, 255, 250⟩⟩,

  -- Metallic approximations
  ⟨"Copper", ⟨184, 115, 51⟩⟩,
  ⟨"Bronze", ⟨205, 127, 50⟩⟩,
  ⟨"BrassYellow", ⟨181, 166, 66⟩⟩,

  -- Neon/Electric colors
  ⟨"ElectricBlue", ⟨125, 249, 255⟩⟩,
  ⟨"ElectricPurple", ⟨191, 0, 255⟩⟩,
  ⟨"NeonGreen", ⟨57, 255, 20⟩⟩,
  ⟨"NeonPink", ⟨255, 16, 240⟩⟩,

  -- Nature-inspired
  ⟨"ForestMoss", ⟨56, 93, 56⟩⟩,
  ⟨"Sage", ⟨176, 208, 176⟩⟩,
  ⟨"Seafoam", ⟨159, 226, 191⟩⟩,
  ⟨"Ocean", ⟨0, 105, 148⟩⟩,
  ⟨"DeepOcean", ⟨0, 51, 102⟩⟩,
  ⟨"Sunset", ⟨250, 214, 165⟩⟩,
  ⟨"Dusk", ⟨78, 81, 128⟩⟩,
  ⟨"Wine", ⟨114, 47, 55⟩⟩,
  ⟨"Burgundy", ⟨128, 0, 32⟩⟩,
  ⟨"Mauve", ⟨224, 176, 255⟩⟩,
  ⟨"Lilac", ⟨200, 162, 200⟩⟩,
  ⟨"Periwinkle", ⟨204, 204, 255⟩⟩,
  ⟨"Wisteria", ⟨201, 160, 220⟩⟩
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
