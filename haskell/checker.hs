import Data.List
-- Strong password has
-- at least 15 chars
-- uppercase letters
-- lowercase letters
-- numbers

-- Negative: Processes the string 4 times! Should be able to do so in one pass.

strong :: String -> Bool
strong "" = False
strong xs = hasUppercase && hasLowercase && hasNumbers && atLeast 15
          where hasUppercase = contains ['A'..'Z']
                hasLowercase = contains ['a'..'z']
                hasNumbers   = contains ['0'..'9']
                contains     = \x -> notEmpty $ intersect x xs
                atLeast      = \x -> (>=x).length $ xs
                notEmpty     = (>0).length
