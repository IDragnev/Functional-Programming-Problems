module NFA where

type State = Int
type Transition = (State, Char, State)

data NFA = NFA { states :: [State]
               , initialStates :: [State]
	       , finalStates :: [State]
	       , delta :: [Transition]
	       }
 deriving (Show, Read)

epsilon :: NFA
epsilon  = NFA { states = [0]
               , initialStates = [0]
	       , finalStates = [0]
	       , delta = [] 
	       }

singleton :: Char -> NFA
singleton symbol = NFA { states = [0, 1]
                       , initialStates = [0]
		       , finalStates = [1]
		       , delta = [(0, symbol, 1)]
		       }

shiftList :: Int -> [Int] -> [Int]
shiftList n = map (+n)

shiftDelta :: Int -> [Transition] -> [Transition]
shiftDelta n = map (\(from, with, to) -> (from + n, with, to + n)) 

shiftNFA :: Int -> NFA -> NFA
shiftNFA n (NFA states initial final delta) = 
   NFA (shiftList n states)
       (shiftList n initial)
       (shiftList n final) 
       (shiftDelta n delta)

merge :: NFA -> NFA -> NFA
merge (NFA states1 init1 final1 delta1) 
      (NFA states2 init2 final2 delta2) = 
         NFA (states1 ++ states2)
	     (init1 ++ init2)
	     (final1 ++ final2)
	     (delta1 ++ delta2)

union :: NFA -> NFA -> NFA
union lhs rhs = merge lhs (shiftNFA n rhs)
 where n = length (states lhs)

genTransitions :: State -> Char -> [State] -> [Transition]
genTransitions from with states = map (\q -> (from, with, q)) states

mergingTransitions :: NFA -> NFA -> [Transition]
mergingTransitions (NFA _ _ finalsLhs deltaLhs) (NFA _ initialsRhs _ _) = 
  let transitionsToFinal = filter (\(_, _, q) -> elem q finalsLhs) deltaLhs
      transitionsToInitial = 
        map (\(from, with, to) -> genTransitions from with initialsRhs) transitionsToFinal 
  in concat transitionsToInitial

initialStatesOfConcatenation :: NFA -> NFA -> [State]
initialStatesOfConcatenation (NFA _ initial final _) rhs
 | any ((flip elem) final) initial = initial ++ (initialStates rhs)
 | otherwise = initial

concatenate :: NFA -> NFA -> NFA
concatenate lhs@(NFA statesLhs initialLhs finalLhs deltaLhs) rhs = 
   NFA newStates newInitials newFinals newDelta
   where
       newStates = statesLhs ++ (states shiftedRhs)
       newInitials = initialStatesOfConcatenation lhs shiftedRhs
       newFinals = finalStates shiftedRhs
       newDelta = deltaLhs ++ (delta shiftedRhs) ++ (mergingTransitions lhs shiftedRhs)  
       shiftedRhs = shiftNFA n rhs
       n = length statesLhs

plus :: NFA -> NFA
plus nfa@(NFA states initials finals delta) = NFA states initials finals newDelta
 where newDelta = delta ++ (mergingTransitions nfa nfa)

star :: NFA -> NFA
star nfa = union (plus nfa) epsilon

matches :: String -> NFA -> Bool
matches word nfa = any (\q -> canWalk word nfa q) (initialStates nfa)

canWalk :: String -> NFA -> State -> Bool
canWalk [] (NFA _ _ finals _) q = elem q finals 
canWalk (h:t) nfa@(NFA states initials finals delta) q = 
    any (\(q1, c, q2) -> q == q1 && h == c && canWalk t nfa q2) delta

--incomplete
fromRegex :: String -> NFA
fromRegex ('(':t) = fromRegex t
fromRegex (c:[]) = singleton c
fromRegex (c:"*") = star (singleton c)
fromRegex (c:"+") = plus (singleton c)
fromRegex (h:t) = buildNFA (singleton h) t
 where
   buildNFA result [] = result
   buildNFA result ")" = result
   buildNFA result (')':'+':t) = buildNFA (plus result) t
   buildNFA result (')':'*':t) = buildNFA (star result) t
   buildNFA result ('|':t) = union result (fromRegex t)
   buildNFA result (x:'*':t) = buildNFA (concatenate result (star $ singleton x)) t
   buildNFA result (x:'+':t) = buildNFA (concatenate result (plus $ singleton x)) t
   buildNFA result (h:t) =  buildNFA (concatenate result (singleton h)) t
