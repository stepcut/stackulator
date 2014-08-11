module Main


import Effect.State
import Effect.Exception
import IQuery
import IQuery.Event
import Model
import Stackulator

putModel : Model -> IO ()
putModel model =
  mkForeign (FFun "model = %0" [FAny Model] FUnit) model

getModel : IO Model
getModel =
  mkForeign (FFun "model" [] (FAny Model))

innerHTML : Element -> String -> IO ()
innerHTML e s = setProperty e "innerHTML" s

reportError : String -> IO ()
reportError s =
  do (Just e) <- !(query "div#error") `elemAt` 0
     setText e s

clearError : IO ()
clearError =
  do (Just e) <- !(query "div#error") `elemAt` 0
     setText e ""

renderStackElem : Element -> (Term, Term) -> IO ()
renderStackElem parent (e, ty) =
 do rowDiv <- newElement "div"
    setAttribute rowDiv "class" "row"
    -- expression
    eDiv <- newElement "div"
    setAttribute eDiv "class" "col-md-6"
    setText eDiv (ppTerm e)
    appendChild rowDiv eDiv

    -- type
    tyDiv <- newElement "div"
    setAttribute tyDiv "class" "col-md-6"
    setText tyDiv (ppTerm ty)
    appendChild rowDiv tyDiv

    appendChild parent rowDiv

renderModel : Model -> IO ()
renderModel (MkModel _ stack) =
   do Just stackDiv <- !(query "div#stack") `elemAt` 0
      innerHTML stackDiv ""
      clearError
      traverse_ (renderStackElem stackDiv) (reverse stack)

enter : Element -> Event -> IO Int
enter prompt ev =
  do model <- getModel
     str <- getValue prompt
     let eModel' = the (Either String Model) $ runInit (() :: model :: Nil) $
                     do pushString str
                        get
     case eModel' of
       (Left err) => do reportError err
                        pure 1
       (Right model') =>
         do setValue prompt ""
            renderModel model'
            putModel model'
            pure 1

app : Event -> IO Int
app ev =
  do model <- getModel
     let eModel' = the (Either String Model) $ runInit (() :: model :: Nil) $
                     do apply
                        get
     case eModel' of
       (Left err) => do reportError err
                        pure 1
       (Right model') =>
         do -- setValue prompt ""
            renderModel model'
            putModel model'
            pure 1

main : IO ()
main =
  do p <- newElement "p"
     setText p "hello!"
     Just prompt <- !(query "input#prompt") `elemAt` 0
     Just enterButton <- !(query "input#enter") `elemAt` 0
     Just applyButton <- !(query "input#apply") `elemAt` 0
     let model = MkModel preludeContext [(TVar $ Name "Nat", TVar $ Name "Type")]
     putModel model
     renderModel model
     onClick enterButton (enter prompt)
     onClick applyButton app

-- Local Variables:
-- idris-packages: ("effects" "lightyear" "iquery")
-- End:
