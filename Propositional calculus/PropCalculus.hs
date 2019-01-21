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

allVars :: Prop -> [Name]
allVars (Const _)          = []
allVars (Var x)            = [x]
allVars (Not fi)           = allVars fi
allVars (fi `And` psi)     = makeSet $ allVars fi ++ allVars psi
allVars (fi `Or`  psi)     = makeSet $ allVars fi ++ allVars psi
allVars (fi `Implies` psi) = makeSet $ allVars fi ++ allVars psi

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

isTautology :: Prop -> Bool
isTautology prop = 
 let vars = allVars prop
     envs = allEnvs vars
     values = map (\e -> evaluateWith e prop) envs
 in and values

