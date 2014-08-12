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

push : Term -> ModelEff ()
push term =
  do (MkModel ctx terms) <- get
     case runInfer ctx term of
          (Left err) => raise err
          (Right ty) => put (MkModel ctx ((term, ty) :: terms))

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
    else case parseAll pTerm s of
           (Left err)   => raise err
           (Right term) => push term

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
 
 
 
 
