import Data.List

data Letter = F | G | A | B | C | D | E
    deriving (Eq, Ord)
data Modifier = Flat | Natural | Sharp
    deriving (Eq, Ord)

-- parseScale' DiffList CompletedNote::Letter CN::Mod LastNote::Mod
-- Sharp for CN::Mod indicates a note was not added, otherwise it would be Flat or Natural
-- LN::Mod remembers whether a Flat or Sharp was added and reverts to Natural when a note is not added
parseScale ns = parseScale' ('F':) F Natural Natural
    where
        parseScale' dl F Flat _ = dl "f |\\\n"
        parseScale' dl F Natural from
            | elem 'H' ns && elem 'G' ns = parseScale' (dl . ('^':) . ('F':)) G Flat Sharp
            | elem 'H' ns = parseScale' (dl . ('_':) . ('G':)) G Flat Flat
            | otherwise = parseScale' dl F Sharp Natural
        parseScale' dl F Sharp from
            | elem 'G' ns = parseScale' (dl . ('G':)) G Natural Natural
            | elem 'I' ns = parseScale' (dl . ('^':) . ('G':)) A Flat Sharp
            | otherwise = parseScale' dl G Sharp Natural
        parseScale' dl G Flat from
            | elem 'G' ns = let preproc = if from == Flat then ('=':) else id in
                parseScale' (dl . preproc . ('G':)) G Natural Natural
            | elem 'I' ns && elem 'A' ns = parseScale' (dl . ('^':) . ('G':)) A Flat Sharp
            | elem 'I' ns = parseScale' (dl . ('_':) . ('A':)) A Flat Flat
            | otherwise = parseScale' dl G Sharp Natural
        parseScale' dl G Natural from
            | elem 'I' ns && elem 'A' ns = parseScale' (dl . ('^':) . ('G':)) A Flat Sharp
            | elem 'I' ns = parseScale' (dl . ('_':) . ('A':)) A Flat Flat
            | otherwise = parseScale' dl G Sharp Natural
        parseScale' dl G Sharp from
            | elem 'A' ns = parseScale' (dl . ('A':)) A Natural Natural
            | elem 'J' ns = parseScale' (dl . ('^':) . ('A':)) B Flat Sharp
            | otherwise = parseScale' dl A Sharp Natural
        parseScale' dl A Flat from
            | elem 'A' ns = let preproc = if from == Flat then ('=':) else id in
                parseScale' (dl . preproc . ('A':)) A Natural Natural
            | elem 'J' ns && elem 'B' ns && elem 'C' ns = parseScale' (dl . ('^':) . ('A':)) B Flat Sharp
            | elem 'J' ns = parseScale' (dl . ('_':) . ('B':)) B Flat Flat
            | otherwise = parseScale' dl A Sharp Natural
        parseScale' dl A Natural from
            | elem 'J' ns && elem 'B' ns = parseScale' (dl . ('^':) . ('A':)) B Flat Sharp
            | elem 'J' ns = parseScale' (dl . ('_':) . ('B':)) B Flat Flat
            | otherwise = parseScale' dl A Sharp Natural
        parseScale' dl A Sharp from
            | elem 'B' ns = parseScale' (dl . ('B':)) B Natural Natural
            | otherwise = parseScale' dl B Sharp Natural
        parseScale' dl B Flat from
            | elem 'B' ns && notElem 'C' ns = parseScale' (dl . ('_':) . ('c':)) C Flat Flat
            | elem 'B' ns = let preproc = if from == Flat then ('=':) else id in
                parseScale' (dl . preproc . ('B':)) B Natural Natural
            | otherwise = parseScale' dl B Sharp Natural
        parseScale' dl B Natural from = parseScale' dl C Flat Natural
        parseScale' dl B Sharp from
            | elem 'C' ns = parseScale' (dl . ('c':)) C Natural Natural
            | elem 'K' ns = parseScale' (dl . ('^':) . ('c':)) D Flat Sharp
            | otherwise = parseScale' dl C Sharp Natural
        parseScale' dl C Flat from
            | elem 'C' ns = let preproc = if from == Flat then ('=':) else id in
                parseScale' (dl . preproc . ('c':)) C Natural Natural
            | elem 'K' ns && elem 'D' ns = parseScale' (dl . ('^':) . ('c':)) D Flat Sharp
            | elem 'K' ns = parseScale' (dl . ('_':) . ('d':)) D Flat Flat
            | otherwise = parseScale' dl C Sharp Natural
        parseScale' dl C Natural from
            | elem 'K' ns && elem 'D' ns = parseScale' (dl . ('^':) . ('c':)) D Flat Sharp
            | elem 'K' ns = parseScale' (dl . ('_':) . ('d':)) D Flat Flat
            | otherwise = parseScale' dl C Sharp Natural
        parseScale' dl C Sharp from
            | elem 'D' ns = parseScale' (dl . ('d':)) D Natural Natural
            | elem 'L' ns = parseScale' (dl . ('^':) . ('d':)) E Flat Sharp
            | otherwise = parseScale' dl D Sharp Natural
        parseScale' dl D Flat from
            | elem 'D' ns = let preproc = if from == Flat then ('=':) else id in
                parseScale' (dl . preproc . ('d':)) D Natural Natural
            | elem 'L' ns && elem 'E' ns = parseScale' (dl . ('^':) . ('d':)) E Flat Sharp
            | elem 'L' ns = parseScale' (dl . ('_':) . ('e':)) E Flat Flat
            | otherwise = parseScale' dl D Sharp Natural
        parseScale' dl D Natural from
            | elem 'L' ns && elem 'E' ns = parseScale' (dl . ('^':) . ('d':)) E Flat Sharp
            | elem 'L' ns = parseScale' (dl . ('_':) . ('e':)) E Flat Flat
            | otherwise = parseScale' dl D Sharp Natural
        parseScale' dl D Sharp from
            | elem 'E' ns = parseScale' (dl . ('e':)) E Natural Natural
            | otherwise = parseScale' dl E Sharp Natural
        parseScale' dl E Flat from
            | elem 'E' ns = let preproc = if from == Flat then ('=':) else id in
                parseScale' (dl . preproc . ('e':)) F Flat Natural
            | otherwise = parseScale' dl E Sharp Natural
        parseScale' dl E Natural from = parseScale' dl F Flat Natural
        parseScale' dl E Sharp _ = parseScale' dl F Flat Natural

scales = map parseScale
    . filter (\s -> length s == 7 && 'F' `elem` s)
    . subsequences
    $ "FCGDAEBHKILJ"
