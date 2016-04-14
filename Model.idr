module Model

import public Effect.State
import Effect.Exception
import public Effects
import Parser
import Stackulator
-- import Lightyear.Strings

public export
Stack : Type
Stack = List (Term, Term)

public export
record Model where
  constructor MkModel
  context : Context
  stack : Stack

initialModel : Model
initialModel = MkModel emptyContext []

public export
ModelEff : Type -> Type
ModelEff t = Eff t [EXCEPTION String, STATE Model]

export
doLet : Var -> Term -> ModelEff ()
doLet v term =
  do (MkModel ctx terms) <- get
     case runInfer ctx term of
       (Left err) => raise err
       (Right ty) => do put (MkModel (extend v ty (Just term) ctx) terms)
                        pure ()


export
doAssume : Var -> Term -> ModelEff ()
doAssume v term =
  do (MkModel ctx terms) <- get
     put (MkModel (extend v term Nothing ctx) terms)

export
push : Term -> ModelEff ()
push term =
  do (MkModel ctx terms) <- get
     case runInfer ctx term of
          (Left err) => raise err
          (Right ty) => put (MkModel ctx ((term, ty) :: terms))

export
drop : ModelEff ()
drop =
  do (MkModel ctx terms) <- get
     case terms of
       (t::ts) => put (MkModel ctx ts)
       _       => raise "empty stack"

export
swap : ModelEff ()
swap =
  do (MkModel ctx terms) <- get
     case terms of
       (t1::t2::ts) => put (MkModel ctx (t2::t1::ts))
       _            => raise "not enough elements on the stack"

export
dup : ModelEff ()
dup =
  do (MkModel ctx terms) <- get
     case terms of
      (t::terms') => put (MkModel ctx (t::t::terms'))
      []          => raise "empty stack"

export
pushString : String -> ModelEff ()
pushString s =
  if s == ""
    then dup
    else case parseAll pStatement s of
           (Left err)   => raise err
           (Right statement) =>
             case statement of
               (Let v term) => doLet v term
               (Assume v term) => doAssume v term
               (STerm term) => push term

export
apply : ModelEff ()
apply =
  do (MkModel ctx terms) <- get
     case terms of
      ((f, _)::(x, _)::xs) =>
         do let t = App f x
            case runInfer ctx t of
              (Left err) => raise err
              (Right ty) => put (MkModel ctx ((t, ty) :: xs))
      _ => raise "Not enough terms on the stack"

export
eval : ModelEff ()
eval =
  do (MkModel ctx terms) <- get
     case terms of
      ((t,_)::terms) =>
        case runEval ctx t of
          (Left err) => raise err
          (Right t') => do put (MkModel ctx terms)
                           push t'
      _ => raise "Not enough terms on the stack"

{-
apply : ModelEff (Maybe String)
apply =
  do (MkModal ctx terms) <- get
     case terms of
-}


-- Local Variables:
-- idris-packages: ("effects" "lightyear")
-- End:
