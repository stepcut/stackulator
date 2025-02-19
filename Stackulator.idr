module Stackulator

-- http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/

-- import Control.Monad.State
import Data.SortedMap
import Effects
import Effect.State
import Effect.Exception

InferEff : Type -> Type
-- InferEff t = { [EXCEPTION String, STATE Int] } Eff t
InferEff t = Eff t [STATE Int, EXCEPTION String]

public export
data Var : Type where
  Name    : String -> Var
  GenName : String -> Int -> Var
  Dummy   : Var

public export
implementation Eq Var where
  (Name n1) == (Name n2) = n1 == n2
  (GenName n1 i1) == (GenName n2 i2) = (n1 == n2) && (i1 == i2)
  Dummy == Dummy = True
  _ == _ = False

public export
implementation Ord Var where
  compare x y =
        if x == y then EQ else
          case (x, y) of
               (Name n1, Name n2) => compare n1 n2
               (Name n1, _)       => LT
               (GenName _ _, Name _) => GT
               (GenName n1 i1, GenName n2 i2) => compare (n1, i1) (n2, i2)
               (GenName _ _, Dummy) => LT
               (Dummy, Dummy) => EQ
               (Dummy, _)     => GT

export
ppVar : Var -> String
ppVar (Name s)      = s
ppVar (GenName s i) = s ++ show i
ppVar (Dummy)        = "_"

public export
data Literal
     = LDouble Double

ppLiteral : Literal -> String
ppLiteral (LDouble d) = show d

public export
data Primitive : Type where
  Plus   : Primitive
  Minus  : Primitive
  Times  : Primitive
  Divide : Primitive

ppPrimitive : Primitive -> String
ppPrimitive Plus   = "+"
ppPrimitive Minus  = "-"
ppPrimitive Times  = "*"
ppPrimitive Divide = "/"

public export
data Term : Type where
  TVar     : Var -> Term
  Universe : Nat -> Term
--   Lit    : Literal -> Term
  Pi       : Var -> Term -> Term -> Term
  Lambda   : Var -> Term -> Term -> Term
  App      : Term -> Term -> Term
  Lit      : Literal -> Term
  Prim     : Primitive -> Term

export
ppTerm : Term -> String
ppTerm  (TVar v)      = ppVar v
ppTerm (Universe Z)   = "Type"
ppTerm (Universe n)   = "Type " ++ show n
ppTerm (Pi v a b)     = "forall " ++ ppVar v ++ " : " ++ ppTerm a ++ ". " ++ ppTerm b
ppTerm (Lambda v x y) = "(\\" ++ ppVar v ++ " : " ++ ppTerm x ++ " => " ++ ppTerm y ++ ")"
ppTerm (App f x)      = ppTerm f ++ " " ++ ppTerm x
ppTerm (Lit lit)      = ppLiteral lit
ppTerm (Prim p)       = ppPrimitive p

public export
data Statement : Type where
     Let     : Var  -> Term -> Statement
     Assume  : Var  -> Term -> Statement
     Forget  : Var  -> Statement
     STerm   : Term -> Statement

||| pretty-print a `Statement`
ppStatement : Statement -> String
ppStatement (Let v t)    = "let "    ++ ppVar v ++ " = " ++ ppTerm t
ppStatement (Assume v t) = "assume " ++ ppVar v ++ " : " ++ ppTerm t
ppStatement (Forget v)   = "forget " ++ ppVar v
ppStatement (STerm t)    = ppTerm t

inc : Eff Int [STATE Int]
inc =
  do n <- get
     let n' = n + 1
     put n'
     pure n'

fresh : Var -> Eff Var [STATE Int]
fresh v =
  do k <- inc
     case v of
          (Name n)       => pure (GenName n k)
          (GenName n _ ) => pure (GenName n k)
          Dummy          => pure (GenName "_" k)

public export
data Context : Type where
     Ctx : SortedMap Var (Term, Maybe Term) -> Context

public export
emptyContext : Context
emptyContext =
  Ctx empty

public export
preludeContext : Context
preludeContext = Ctx $ fromList
 [ (Name "Nat", (Universe 0, Nothing))
 , (Name "z", (TVar (Name "Nat"), Nothing))
 , (Name "s", (Pi Dummy (TVar (Name "Nat")) (TVar (Name "Nat")), Nothing))
 , (Name "three", (TVar (Name "Nat"), Just (App (TVar (Name "s")) (App (TVar (Name "s")) (App (TVar (Name "s")) (TVar (Name "z")))))))
 , (Name "Double", (Universe 0, Nothing))
 , (Name "id", (Pi (Name "a") (Universe 0) (Pi (Name "x") (TVar (Name "a")) (TVar (Name "a"))), Just (Lambda (Name "a") (Universe 0) (Lambda (Name "x") (TVar (Name "a")) (TVar (Name "x"))))))
 ]

pc : SortedMap Var (Term, Maybe Term)
pc =  fromList [ (Name "Nat", (Universe 0, Nothing))
 , (Name "z", (TVar (Name "Nat"), Nothing))
 , (Name "s", (Pi Dummy (TVar (Name "Nat")) (TVar (Name "Nat")), Nothing))
 , (Name "three", (TVar (Name "Nat"), Just (App (TVar (Name "s")) (App (TVar (Name "s")) (App (TVar (Name "s")) (TVar (Name "z")))))))
 , (Name "Double", (Universe 0, Nothing))
 ]


lookupTy : Var -> Context -> Maybe Term
lookupTy v (Ctx ctx) =
  case lookup v ctx of
    (Just (ty, _)) => Just ty
    _              => Nothing

lookupValue : Var -> Context -> Maybe (Maybe Term)
lookupValue v (Ctx ctx) =
  case lookup v ctx of
    (Just (_, val)) => Just val
    Nothing         => Nothing

export
extend : Var -> Term -> Maybe Term -> Context -> Context
extend x t val (Ctx ctx) =
  Ctx (insert x (t, val) ctx)

subst : SortedMap Var Term -> Term -> Eff Term [STATE Int]
subst' : SortedMap Var Term -> Var -> Term -> Term -> Eff (Var, Term, Term) [STATE Int]

subst vars e =
  case e of
    (TVar v) =>
      case lookup v vars of
        Nothing   => return e
        (Just e') => return e'
    (Universe _) => return e
    Pi v e1 e2 =>
      do (v', e1', e2') <- subst' vars v e1 e2
         return $ Pi v' e1' e2'
    Lambda v e1 e2 =>
      do (v', e1', e2') <- subst' vars v e1 e2
         return $ Lambda v' e1' e2'
    App e1 e2 =>
      do e1' <- subst vars e1
         e2' <- subst vars e2
         return $ App e1' e2'

subst' vars x t e =
  do x' <- fresh x
     t' <- subst vars t
     e' <- subst (insert x (TVar x') vars) e
     return (x', t', e')

export
inferType : Context -> Term -> Eff Term [STATE Int, EXCEPTION String]
inferUniverse : Context -> Term -> Eff Nat [STATE Int, EXCEPTION String]
inferPi       : Context -> Term -> Eff (Var, Term, Term) [STATE Int, EXCEPTION String]
equal         : Context -> Term -> Term -> Eff Bool [STATE Int, EXCEPTION String]
export
normalize     : Context -> Term -> Eff Term [STATE Int, EXCEPTION String]
normalize'    : Context -> Var  -> Term -> Term -> Eff (Var, Term, Term) [STATE Int, EXCEPTION String]

tDouble : Term
tDouble = TVar (Name "Double")

inferLiteral : Literal -> Eff Term [STATE Int]
inferLiteral (LDouble _) = pure tDouble

inferPrimitive : Primitive -> Eff Term [STATE Int]
inferPrimitive Plus   = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)
inferPrimitive Minus  = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)
inferPrimitive Times  = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)
inferPrimitive Divide = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)

inferType ctx e =
  case e of
    (TVar x) =>
      case lookupTy x ctx of
        Nothing  => raise ("inferType: unknown identifier: " ++ ppVar x)
        (Just t) => return t

    (Universe k) => return (Universe (k + 1))

    Pi x t1 t2 =>
      do k1 <- inferUniverse ctx t1
         k2 <- inferUniverse (extend x t1 Nothing ctx) t2
         return (Universe (max k1 k2))

    Lambda x t e =>
      do u <- inferUniverse ctx t
         te <- inferType (extend x t Nothing ctx) e
         return (Pi x t te)

    App e1 e2 =>
      do (x, s, t) <- inferPi ctx e1
         te <- inferType ctx e2
         if !(equal ctx s te)
           then subst (fromList [(x, e2)]) t
           else raise ("inferType:\nexpected type: " ++ ppTerm s ++ "\ninferred type: " ++ ppTerm te)

    Lit l => inferLiteral l
    Prim p => inferPrimitive p

inferUniverse ctx t =
  do u  <- inferType ctx t
     u' <- normalize ctx u
     case u' of
       (Universe k) => return k
       _ => raise $ "inferUniverse: type expected. Got: t=" ++ ppTerm t ++ ", u=" ++ ppTerm u ++ ", u'=" ++ ppTerm u'

inferPi ctx e =
  do t <- inferType ctx e
     t' <- normalize ctx t
     case t' of
       (Pi x s t) => return (x, s, t)
       _          => raise $ "inferPi: function expected. Got: " ++ ppTerm t'

normalize ctx e =
  case e of
    (TVar x) =>
      case lookupValue x ctx of
        Nothing => raise $ "normalize: Unknown identifier: " ++ ppVar x
        (Just Nothing) => return (TVar x)
        (Just (Just e)) => normalize ctx e

    (Universe k) => return (Universe k)

    Pi x t e =>
       do (x', t', e') <- normalize' ctx x t e
          return $ Pi x' t' e'

    Lambda x t e =>
       do (x', t', e') <- normalize' ctx x t e
          return $ Lambda x' t' e'
    (App (App (Prim Plus) e1) e2) =>
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) =>
              return $ Lit $ LDouble (d1 + d2)
           _ => return $ App (App (Prim Plus) e1') e2'
    (App (App (Prim Minus) e1) e2) =>
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) =>
              return $ Lit $ LDouble (d1 - d2)
           _ => return $ App (App (Prim Minus) e1') e2'
    (App (App (Prim Times) e1) e2) =>
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) =>
              return $ Lit $ LDouble (d1 * d2)
           _ => return $ App (App (Prim Times) e1') e2'

    (App (App (Prim Divide) e1) e2) =>
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) =>
              return $ Lit $ LDouble (d1 / d2)
           _ => return $ App (App (Prim Divide) e1') e2'

    (App e1 e2) =>
      do e2' <- normalize ctx e2
         case !(normalize ctx e1) of
           (Lambda x _ e1'') =>
             do e1''' <- subst (fromList [(x, e2')]) e1''
                normalize ctx e1'''
           e1'' => return $ App e1'' e2'

    (Lit l) => return (Lit l)

normalize' ctx x t e =
  do t' <- normalize ctx t
     e' <- normalize (extend x t' Nothing ctx) e
     return (x, t', e')

equal ctx e1 e2 =
  do e1' <- normalize ctx e1
     e2' <- normalize ctx e2
     equal' e1' e2'
  where
    equal' : Term -> Term -> InferEff Bool
    equal'' : Var -> Term -> Term -> Var -> Term -> Term -> InferEff Bool
    equal' e1 e2 =
      case (e1, e2) of
        (TVar x1, TVar x2) => return (x1 == x2)
        (App e11 e12, App e21 e22) =>
          return( !(equal' e11 e21) && !(equal' e12 e22))
        (Universe k1, Universe k2) => return (k1 == k2)
        (Pi x1 t1 e1, Pi x2 t2 e2) =>
          equal'' x1 t1 e1 x2 t2 e2
        (Lambda x1 t1 e1, Lambda x2 t2 e2) =>
          equal'' x1 t1 e1 x2 t2 e2
        _ => return False

    equal'' x t1 e1 y t2 e2 =
      do b1  <- equal' t1 t2
         e2' <- subst (fromList [(y, (TVar x))]) e2
         b2  <- equal' e1 e2'
         return (b1 && b2)

export
runInfer : Context -> Term -> Either String Term
runInfer ctx term = run (inferType ctx term)

export
runEval : Context -> Term -> Either String Term
runEval ctx term = run (normalize ctx term)

evalIO : Context -> Term -> IO ()
evalIO ctx term =
  case runEval ctx term of
    (Left err)   => putStrLn $ "error: " ++ err
    (Right term) => putStrLn $ ppTerm term

typeCheck : Context -> Term -> IO ()
typeCheck ctx term =
  case runInfer ctx term of
    (Left err)   => putStrLn $ "error: " ++ err
    (Right term) => putStrLn $ ppTerm term

-- main : IO ()
-- main = eval preludeContext (TVar $ Name "three")

-- Local Variables:
-- idris-packages: ("effects")
-- End:
