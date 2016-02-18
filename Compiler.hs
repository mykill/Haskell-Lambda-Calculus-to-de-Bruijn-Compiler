-- Michael June Aranas
-- CSc 153 Compiler
-- This program will only accept bounded variables.

import Parsing
type Ide = String

{----------------------------------------------------------------}
	-- Lambda Calculus
data LCExp = Var Ide
	 | App LCExp LCExp
	 | Lam Ide LCExp
	   deriving Show

{----------------------------------------------------------------}
	-- de Bruijn
data DBExp = Ind Int
	 | Apply DBExp DBExp
	 | Abs DBExp
	   deriving Show

expr :: Parser LCExp
expr = lam +++ app +++ var

lam :: Parser LCExp
lam = (do symbol "(\\"; e <-identifier; symbol "."; f <- expr; symbol ")"; return ((Lam e f))) 
 
app :: Parser LCExp
app = (do symbol "("; t <- expr; e <- expr; symbol ")"; return (App t e))  

var :: Parser LCExp
var = (do t <- identifier;  return (Var t))

parseLC :: String -> LCExp
parseLC str = case parse expr str of
                           [(ast,[])] -> ast
                           [(_,out)]  -> error ("unused input " ++ out)
                           []         -> error "invalid input"
						   
{-------------------------------------------------------------------------------------------}
	-- Lambda Calculus to de Bruijn Indices
lc2db :: [String] -> LCExp -> DBExp
lc2db st (App m n) = Apply (lc2db st m) (lc2db st n)
lc2db st (Lam x t) = Abs (lc2db (x:st) t)
lc2db st (Var x) = Ind (find 0 x st)
    where
      find index var (name:rest) = if var == name then index
                                   else find (index+1) var rest
								   
{-------------------------------------------------------------------------------------------}
	-- lifting operation in de Bruijn indices
lift :: Int -> Int -> DBExp -> DBExp 
lift k n (Ind i) 
	| i < k = Ind i
	| otherwise = Ind (n+i) 
lift k n (Abs t) = Abs (lift(k+1) n t) 
lift k n (Apply f a) = Apply (lift k n f)(lift k n a)

{-------------------------------------------------------------------------------------------}
	-- Substitution
subst :: DBExp -> (Int, DBExp) -> DBExp
subst (Ind i) (k, exp)
	| (i < k) = Ind i
	| (i == k) = lift 0 k exp
	| (i > k) = Ind (i - 1)
subst (Abs e) (k, exp) = Abs (subst e (k + 1, exp))
subst (Apply fun arg) (k, exp) = Apply (subst fun (k, exp)) (subst arg (k, exp))

{-------------------------------------------------------------------------------------------}
	-- Beta Reduction in de Bruijn
betaReduce :: DBExp -> DBExp
betaReduce (Apply m@(Abs x) n) = subst x (0, n)

{-------------------------------------------------------------------------------------------}
	-- unparsing the dB terms
unparseDB :: DBExp -> String
unparseDB (Ind i) = show i
unparseDB (Abs e) = "(\\." ++ (unparseDB e) ++ ")"
unparseDB (Apply fun arg) = "(" ++ (unparseDB fun) ++ " " ++ (unparseDB arg) ++ ")" 


{-------------------------------------------------------------------------------------------}
	-- Compiler ()
data Com = ACCESS Int
	| GRAB Char Com
	| PUSH (Com) Char Com
	deriving Show
	
{-------------------------------------------------------------------------------------------}
	-- de Bruijn to Machine code
compile :: DBExp -> Com
compile (Ind i) = ACCESS i
compile (Abs e) = GRAB (';') (compile (e))
compile (Apply fun arg) = PUSH (compile (arg)) (';') (compile (fun))

{------------------------------------------------------------------------------------------}
-- EXAMPLES

-- lc2db [] (parseLC "(\\x.(\\y.(x y)))") 		// converting from Lambda Calculus to de Bruijn abstract syntax
--		= Abs (Abs (Apply (Ind 1) (Ind 0))) 
   	
-- unparseDB (lc2db [] (parseLC "(\\x.(\\y.(x y)))"))    // unparsing the de Bruijn abstract syntax into dB notation
-- 		= "(\\.(\\.(1 0)))"	 	

-- betaReduce (lc2db [] (parseLC "((\\x.x) (\\x.x))" )) 	// beta reduction of dB notation into dB abstract syntax
-- 		= Abs (Ind 0)		 

-- unparseDB (betaReduce (lc2db [] (parseLC "((\\x.x) (\\x.x))" )))		// in terms of dB notation
--		= Abs (Ind 0)) n

-- compile (lc2db [] (parseLC "((\\x.x) (\\x.x))" )) 	 		// compiling the de Bruijn note that the expression must be bounded.
-- 		= PUSH (GRAB ';' (ACCESS 0)) ';' (GRAB ';' (ACCESS 0))

-- compile (lc2db [] (parseLC "(\\x.(\\y.(x y)))" ))
--      = GRAB ';' (GRAB ';' (PUSH (ACCESS 0) ';' (ACCESS 1)))

-- compile (betaReduce (lc2db [] (parseLC "((\\x.x) (\\x.x))" ))) 		// compiling the reduced dB term
--		= GRAB ';' (ACCESS 0)


