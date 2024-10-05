import Data.List

-- 12TET A = 440Hz
data Note = Pitchy
    | Note {
        letter :: Letter,
        modifier:: Modifier }
    deriving (Eq, Ord)

instance Show Note where
    show Pitchy = "X" -- Brown = "\xF0\x9F\x92\xA9"
    show (Note l m) = show m ++ show l

data Letter = F | G | A | B | C | D | E
    deriving (Eq, Ord)
data Modifier = Flat | Natural | Sharp
    deriving (Eq, Ord)

instance Show Letter where
    show F = "F"
    show G = "G"
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"

instance Show Modifier where
    show Flat = "b"
    show Natural = []
    show Sharp = "#"

instance Enum Letter where
    toEnum 0 = F
    toEnum 1 = G
    toEnum 2 = A
    toEnum 3 = B
    toEnum 4 = C
    toEnum 5 = D
    toEnum 6 = E
    toEnum i = toEnum (mod i 7)
    fromEnum F = 0
    fromEnum G = 1
    fromEnum A = 2
    fromEnum B = 3
    fromEnum C = 4
    fromEnum D = 5
    fromEnum E = 6

parseScale2 ns = parseScale' ('F':) 1 False (Note F Natural)
    where
        abcL F = 'F'
        abcL G = 'G'
        abcL A = 'A'
        abcL B = 'B'
        abcL C = 'c'
        abcL D = 'd'
        abcL E = 'e'

        charL F = 'F'
        charL G = 'G'
        charL A = 'A'
        charL B = 'B'
        charL C = 'C'
        charL D = 'D'
        charL E = 'E'

        nextChar F = 'H'
        nextChar G = 'I'
        nextChar A = 'J'
        nextChar B = 'C'
        nextChar C = 'K'
        nextChar D = 'L'
        nextChar E = 'F'

        elemNs = flip elem ns
        notElemNs = flip notElem ns

        parseScale' :: (String -> String) -> Int -> Bool -> Note -> String
        parseScale' dl 7 _ _ = dl "f |\\\n"
        parseScale' dl n _ (Note l Natural)
            | nextChar l == (charL . succ) l =
                parseScale' dl n False (Note (succ l) Flat)
            | elemNs (nextChar l) && elemNs (charL . succ $ l) =
                parseScale' (dl . ('^':) . (abcL l:)) (n + 1) False (Note (succ l) Flat)
            | elemNs (nextChar l) =
                parseScale' (dl . ('_':) . ((abcL . succ) l:)) (n + 1) True (Note (succ l) Flat)
            | otherwise = parseScale' dl n False (Note l Sharp)
        parseScale' dl n _ (Note l Sharp)
            | elemNs (charL . succ $ l) =
                parseScale' (dl . ((abcL . succ) l:)) (n + 1) False (Note (succ l) Natural)
            | (nextChar . succ) l /= (charL . succ . succ) l && elemNs (nextChar . succ $ l) =
                parseScale' (dl . ('^':) . ((abcL . succ) l:)) (n + 1) False (Note (succ . succ $ l) Flat)
            | otherwise = parseScale' dl n False (Note (succ l) Sharp)
        parseScale' dl n fromFlat (Note l Flat)
            | nextChar l == (charL . succ) l && elemNs (charL l) && notElemNs (nextChar l) =
                -- we know there is no `succ l` in `ns`
                parseScale' (dl . ('_':) . ((abcL . succ) l:)) (n + 1) False (Note (succ l) Natural)
            | elemNs (charL l) = let prefix = if fromFlat then ('=':) else id in
                parseScale' (dl . prefix . (abcL l:)) (n + 1) False (Note l Natural)
            | nextChar l /= (charL . succ) l && elemNs (nextChar l) && elemNs (charL . succ $ l)
                && ((nextChar . succ) l /= (charL . succ . succ) l || elemNs (charL . succ . succ $ l)) =
                parseScale' (dl . ('^':) . (abcL l:)) (n + 1) False (Note (succ l) Flat)
            | nextChar l /= (charL . succ) l && elemNs (nextChar l) =
                parseScale' (dl . ('_':) . ((abcL . succ) l:)) (n + 1) True (Note (succ l) Flat)
            | otherwise = parseScale' dl n False (Note l Sharp)

scales f = map f
    . filter (\s -> length s == 7 && 'F' `elem` s)
    . subsequences
    $ "FCGDAEBHKILJ"

