module PLists where

data PExpr = PChar Char | PBool Bool | PInt Int | PDouble Double | PList { list :: [PExpr] }
 deriving (Eq, Ord, Show, Read)

countAtoms :: PExpr -> Int
countAtoms (PList list) = sum $ map countAtoms list
countAtoms _ = 1

flatten :: PExpr -> PExpr
flatten (PList l) = PList $ concat $ map (list . flatten) l
flatten atom = PList [atom]

len :: PExpr -> Int
len (PList l) = sum $ map len l
len atom = 1

(+++) :: PExpr -> PExpr -> PExpr
(PList lhs) +++ (PList rhs) = PList (lhs ++ rhs)
