module PropCalculus where

type Name = String

data Prop = Const Bool
          | Var Name
	  | Not Prop
	  | And Prop Prop
	  | Or Prop Prop
	  | Implies Prop Prop

instance Show Prop where
  show (Const b)          = if b then "T" else "F"
  show (Var x)            = x
  show (Not fi)           = '¬' : show fi
  show (fi `And` psi)     = "(" ++ show fi ++ " & " ++ show psi ++ ")"
  show (fi `Or` psi)      = "(" ++ show fi ++ " V " ++ show psi ++ ")"
  show (fi `Implies` psi) = "(" ++ show fi ++ " -> " ++ show psi ++ ")"

instance Eq Prop where
  Var x == Var y = x == y
  Not fi == Not psi =  fi == psi
  fi1 `And` psi1 == fi2 `And` psi2 = fi1 == fi2 && psi1 == psi2
  fi1 `Or`  psi1 == fi2 `Or`  psi2 = fi1 == fi2 && psi1 == psi2  
  fi1 `Implies` psi1 == fi2 `Implies` psi2 = fi1 == fi2 && psi1 == psi2
  _ == _ = False

type Environment = [(Name, Bool)]

varValue :: Environment -> Name -> Bool
varValue ((name, value) : t) x 
 | x == name = value
 | otherwise = varValue t x

evaluateWith :: Environment -> Prop -> Bool
evaluateWith _   (Const x)          = x
evaluateWith env (Var x)            = varValue env x
evaluateWith env (Not fi)           = not $ evaluateWith env fi
evaluateWith env (fi `And` psi)     = evaluateWith env fi && evaluateWith env psi
evaluateWith env (fi `Or`  psi)     = evaluateWith env fi || evaluateWith env psi
evaluateWith env (fi `Implies` psi) = evaluateWith env (Not fi) || evaluateWith env psi

allVarsOf :: Prop -> [Name]
allVarsOf (Const _)          = []
allVarsOf (Var x)            = [x]
allVarsOf (Not fi)           = allVarsOf fi
allVarsOf (fi `And` psi)     = unionVars fi psi 
allVarsOf (fi `Or`  psi)     = unionVars fi psi
allVarsOf (fi `Implies` psi) = unionVars fi psi

unionVars :: Prop -> Prop -> [Name]
unionVars fi psi = makeSet $ allVarsOf fi ++ allVarsOf psi

makeSet :: (Eq a) => [a] -> [a]
makeSet [] = []
makeSet (h:t) 
 | elem h t  = rest
 | otherwise = h : rest
 where rest = makeSet t

bind :: [Name] -> [Bool] -> Environment
bind = zip

allBools :: Int -> [[Bool]]
allBools 0 = [[]]
allBools n = [ (x:xs) | x <- [True, False], xs <- allBools (n - 1) ]

allEnvs :: [Name] -> [Environment]
allEnvs vars = map (bind vars) valueLists
 where valueLists = allBools (length vars)

valuesWith :: [Environment] -> Prop -> [Bool]
valuesWith envs fi = map (\e -> evaluateWith e fi) envs

allPossibleValues :: Prop -> [Bool]
allPossibleValues fi = valuesWith envs fi 
 where envs = allEnvs $ allVarsOf fi 

isTautology :: Prop -> Bool
isTautology fi = and $ allPossibleValues fi

isSatisfiable :: Prop -> Bool
isSatisfiable fi = or $ allPossibleValues fi

isContradiction :: Prop -> Bool
isContradiction fi = isTautology $ Not fi

semanticallyImplies :: Prop -> Prop -> Bool
semanticallyImplies fi psi =
 let vars = unionVars fi psi
     envs = allEnvs vars
     valuesFi  = valuesWith envs fi
     valuesPsi = valuesWith envs psi
     pairs = zip valuesFi valuesPsi
 in all (\(x, y) -> (not x) || y) pairs

(|=) = semanticallyImplies

semanticallyEquivalent :: Prop -> Prop -> Bool
semanticallyEquivalent fi psi = fi |= psi && psi |= fi

(|=|) = semanticallyEquivalent

{-
isAxiom :: Prop -> Bool
-- THEN-1
isAxiom (fi `Implies` (psi `Implies` chi)) = fi == chi
--THEN-2
isAxiom ((fi `Implies` (chi `Implies` psi)) `Implies` ((theta `Implies` eta) `Implies` (alpha `Implies` beta))) = fi == theta && fi == alpha && chi == eta &&  psi == beta
-- AND-1 or AND-2
isAxiom ((fi `And` psi) `Implies` chi) = fi == chi || psi == chi
-- AND-3
isAxiom (fi `Implies` (chi `Implies` (psi `And` eta))) = fi == psi && chi == eta
-- OR-1 or OR-2
isAxiom (fi `Implies` (psi `Or` chi)) = fi == psi || fi == chi
-- OR-3
isAxiom ((phi `Implies` psi ) `Implies` ((chi `Implies` eta) `Implies` ((theta `Or` alpha) `Implies` beta))) = phi == theta && psi == eta && psi == beta && chi == theta
--NOT-1
isAxiom ((fi `Implies` chi) `Implies` ((psi `Implies` Not eta) `Implies` Not theta)) = fi == psi && fi == theta && chi == eta
-- NOT-2
isAxiom (fi `Implies` (Not psi `Implies` _)) = fi == psi 
isAxiom (fi `Or` Not psi) = fi == psi
-} 
