module Main

import Effect.State
import Effect.Exception
import IQuery
import IQuery.Event
import Model
import Stackulator

fromCharCode : Int -> IO String
fromCharCode cc = mkForeign (FFun "String.fromCharCode(%0)" [FInt] FString) cc

putModel : Model -> IO ()
putModel model =
  mkForeign (FFun "model = %0" [FAny Model] FUnit) model

getModel : IO Model
getModel =
  mkForeign (FFun "model" [] (FAny Model))

innerHTML : Element -> String -> IO ()
innerHTML e s = setProperty e "innerHTML" s

appendP : Element -> String -> IO ()
appendP e str =
  do p <- newElement "p"
     setText p str
     appendChild e p

reportError : String -> IO ()
reportError s =
  do (Just e) <- !(query "pre#error") `elemAt` 0
     setText e s

clearError : IO ()
clearError =
  do (Just e) <- !(query "pre#error") `elemAt` 0
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

eval' : Event -> IO Int
eval' ev =
  do model <- getModel
     let eModel' = the (Either String Model) $ runInit (() :: model :: Nil) $
                     do eval
                        get
     case eModel' of
       (Left err) => do reportError err
                        pure 1
       (Right model') =>
         do -- setValue prompt ""
            renderModel model'
            putModel model'
            pure 1

cmd'' : ModelEff () -> IO Bool
cmd'' thecmd =
  do model <- getModel
     let eModel' = the (Either String Model) $ runInit (() :: model :: Nil) $
                     do thecmd
                        get
     case eModel' of
       (Left err) => do reportError err
                        return False
       (Right model') =>
         do -- setValue prompt ""
            renderModel model'
            putModel model'
            return True

cmd' : ModelEff () -> IO ()
cmd' thecmd = cmd'' thecmd $> pure ()

cmd : ModelEff () -> Event -> IO Int
cmd thecmd _ = cmd' thecmd $> pure 1

keyToChar : Key -> IO String
keyToChar k = fromCharCode (toKeyCode k)

appendText : Element -> String -> IO ()
appendText e t' =
  do t <- getText e
     setText e (t ++ t')

onKeyDown : Element -> IO ()
onKeyDown prompt =
  do (Just body) <- !(query "body") `elemAt` 0
     onEvent KeyDown body $ \ev =>
             do mk <- key ev
                case mk of
                  (Just KeyEnter) => enter prompt ev
                  (Just k) =>
                        case !(altKey ev) of
                           False => pure 1
                           True  =>
                             do case k of
                                  KeyA => do str <- getValue prompt
                                             if str /= ""
                                              then do s <- cmd'' $ pushString str
                                                      if s
                                                         then do setValue prompt ""
                                                                 cmd' $ apply
                                                         else return ()
                                              else cmd' $ do apply
                                  KeyE => do cmd' eval
                                  KeyD => do cmd' drop
                                  KeyS => do cmd' swap
                                preventDefault ev
                                pure 0
{-
                  (Just k)        => do c <- keyToChar k
                                        appendText prompt c
                                        pure 1
                                        -}
                  Nothing         => pure 1

main : IO ()
main =
  do p <- newElement "p"
     setText p "hello!"
     Just prompt      <- !(query "input#prompt") `elemAt` 0
     Just enterButton <- !(query "input#enter")  `elemAt` 0
     Just applyButton <- !(query "input#apply")  `elemAt` 0
     Just evalButton  <- !(query "input#eval")   `elemAt` 0
     Just swapButton  <- !(query "input#swap")   `elemAt` 0
     Just dropButton  <- !(query "input#drop")   `elemAt` 0
     let model = MkModel preludeContext []
     putModel model
     renderModel model
     onKeyDown prompt
     onClick enterButton (enter prompt)
     onClick applyButton (cmd apply)
     onClick evalButton  (cmd eval)
     onClick dropButton  (cmd drop)
     onClick swapButton  (cmd swap)

-- Local Variables:
-- idris-packages: ("effects" "lightyear" "iquery")
-- End:
