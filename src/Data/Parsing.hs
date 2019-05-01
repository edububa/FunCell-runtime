module Data.Parsing where

-- external imports
import Text.Regex

{-| The 'rangeRegex' value defines a regex that matches ranges of
  references. For example: (A0..B0). -}
rangeRegex :: Regex
rangeRegex = mkRegex "[(][A-Z]+[0-9]+[0-9]*[:][A-Z]+[0-9]+[0-9]*[)]"

{-| The 'listRegex' value defines a regex that matches lists of
  references. For example: [A0..B2]. -}
listRegex :: Regex
listRegex = mkRegex "[[][A-Z]+[0-9]+[0-9]*[:][A-Z]+[0-9]+[0-9]*[]]"

{-| The 'referencesRegex' value defines a regex that matches references. -}
referencesRegex :: Regex
referencesRegex = mkRegex "[A-Z]+[0-9]+[0-9]*"

{-| The 'columnRegex' value defines a regex that matches the column in a
    reference. -}
columnRegex :: Regex
columnRegex = mkRegex "[A-Z]+"

{-| The 'rowRegex' value defines a regex that matches the row in a
    reference. -}
rowRegex :: Regex
rowRegex = mkRegex "[0-9]+[0-9]*"
