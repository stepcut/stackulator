module Model

import Effect.State
import Effect.Exception
import Parser
import Stackulator

Stack : Type
Stack = List (Term, Term)

record Model : Type where
  MkModel : (context : Context) -> (stack : Stack) -> Model

initialModel : Model
initialModel = MkModel emptyContext []

ModelEff : Type -> Type
ModelEff t = { [EXCEPTION String, STATE Model] } Eff t

doLet : Var -> Term -> ModelEff ()
doLet v term =
  do (MkModel ctx terms) <- get
     case runInfer ctx term of
       (Left err) => raise err
       (Right ty) => put (MkModel (extend v ty (Just term) ctx) terms)

push : Term -> ModelEff ()
push term =
  do (MkModel ctx terms) <- get
     case runInfer ctx term of
          (Left err) => raise err
          (Right ty) => put (MkModel ctx ((term, ty) :: terms))

drop : ModelEff ()
drop =
  do (MkModel ctx terms) <- get
     case terms of
       (t::ts) => put (MkModel ctx ts)
       _       => raise "empty stack"

swap : ModelEff ()
swap =
  do (MkModel ctx terms) <- get
     case terms of
       (t1::t2::ts) => put (MkModel ctx (t2::t1::ts))
       _            => raise "not enough elements on the stack"

dup : ModelEff ()
dup =
  do (MkModel ctx terms) <- get
     case terms of
      (t::terms') => put (MkModel ctx (t::t::terms'))
      []          => raise "empty stack"

pushString : String -> ModelEff ()
pushString s =
  if s == ""
    then dup
    else case parseAll pStatement s of
           (Left err)   => raise err
           (Right statement) =>
             case statement of
               (Let v term) => doLet v term
--               (Assume v term) => doAssume v term
               (STerm term) => push term

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
 
 
 
 
