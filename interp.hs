import Data.Maybe

data Op = Add | Mult deriving (Show)

opTrans :: Op -> Int -> Int -> Int
opTrans Add  = (+)
opTrans Mult = (*)

data Ast = 
      Number Int
    | Bin Op Ast Ast
    | Fun String Ast
    | With (String, Ast) Ast
    | App Ast Ast
    | Set String Ast
    | Seq Ast Ast
    | Var String
    deriving (Show)

type Loc = Int
type Env = [(String, Loc)]
type Store = [(Loc, Val)]

data Val = Numb Int | Closure String Ast Env | Void
    deriving (Show)

newtype State a = S (Store -> (a, Store))

app :: State a -> Store -> (a, Store)
app (S st) str = st str

instance Functor State where
    -- fmap :: (a -> b) -> State a -> State b
    fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative State where
    -- pure :: a -> State a
    pure x = S (\s -> (x, s))
    
    -- (<*>) :: State (a -> b) -> State a -> State b
    stf <*> stx = S (\s -> 
        let (f, s0) = app stf s in 
            let (x, s1) = app stx s0 in (f x, s1))

instance Monad State where
    -- (>>=) :: State a -> (a -> State b) -> State b
    st >>= f = S (\s -> let (x, s0) = app st s in app (f x) s0)

getVar :: Loc -> State Val
getVar loc = S (\str -> (fromMaybe undefined (lookup loc str), str))

setVar :: Loc -> Val -> State Val
setVar loc nv = S (\str -> (Void, (loc, nv) : str))

newloc :: State Loc
newloc = S (\str -> (length str, str))

interp :: Ast -> Env -> State Val
interp (Number v) _ = return $ Numb v
interp (Fun p b) e = return $ Closure p b e
interp (With (var, exp) bdy) e = interp (App (Fun var bdy) exp) e
interp (Bin op x y) e = do (Numb v) <- interp x e
                           (Numb w) <- interp y e
                           return $ Numb (opTrans op v w)
interp (Seq x y) e = interp x e >> interp y e
interp (Var x) e = return (fromMaybe undefined (lookup x e)) >>= getVar
interp (App f x) e = do (Closure fp fb fe) <- interp f e
                        x' <- interp x e
                        nl <- newloc
                        setVar nl x'
                        interp fb $ (fp, nl) : fe
interp (Set x y) e = do let lx = fromMaybe undefined (lookup x e)
                        nv <- interp y e
                        setVar lx nv
                        return Void

run :: Ast -> Val
run ast = let (val, _) = app (interp ast []) [] in val

runDebug :: Ast -> (Val, Store)
runDebug ast = app (interp ast []) []

p0 = App (Fun "x" (Bin Add (Number 2) (Var "x"))) (Number 3)

p1 = App (Fun "f" 
             (App (Var "f") (Number 5))) 
         (Fun "x" 
             (Seq (Set "x" (Bin Add (Number 3) (Var "x"))) 
                  (Bin Mult (Number 10) (Var "x"))))