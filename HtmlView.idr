module Main

import Control.Monad.Identity
import Effects
import Effect.State
import Effect.Exception
import IQuery
import IQuery.Elements
import IQuery.Event
import IQuery.Key
import Model
import Stackulator

fromCharCode : Int -> JS_IO String
fromCharCode cc = jscall "String.fromCharCode(%0)" (Int -> JS_IO String) cc

putModel : Model -> JS_IO ()
putModel model =
  jscall "model = %0" (Ptr -> JS_IO ()) (believe_me model)

getModel : JS_IO Model
getModel =
  believe_me <$> jscall "model" (JS_IO Ptr)

innerHTML : Element -> String -> JS_IO ()
innerHTML e s = setProperty e "innerHTML" s

appendP : Element -> String -> JS_IO ()
appendP e str =
  do p <- newElement "p"
     setText p str
     appendChild e p

reportError : String -> JS_IO ()
reportError s =
  do (Just e) <- !(query "pre#error") `elemAt` 0
     setText e s

clearError : JS_IO ()
clearError =
  do (Just e) <- !(query "pre#error") `elemAt` 0
     setText e ""

renderStackElem : Element -> (Term, Term) -> JS_IO ()
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

renderContextElem : Element -> (Var, (Term, Maybe Term)) -> IO ()
renderContextElem parent (v, (ty, me)) =
 do rowDiv <- newElement "div"
    setAttribute rowDiv "class" "row"
    -- var
    vDiv <- newElement "div"
    setAttribute vDiv "class" "col-md-4"
    setText vDiv (ppVar v)
    appendChild rowDiv vDiv

    -- expr
    tDiv <- newElement "div"
    setAttribute tDiv "class" "col-md-4"
    case me of
      (Just e) => setText tDiv (ppTerm e)
      Nothing  => return ()
    appendChild rowDiv tDiv

    -- type
    tyDiv <- newElement "div"
    setAttribute tyDiv "class" "col-md-4"
    setText tyDiv (ppTerm ty)
    appendChild rowDiv tyDiv

    appendChild parent rowDiv

renderModel : Model -> IO ()
renderModel (MkModel (Ctx ctx) stack) =
   do Just stackDiv <- !(query "div#stack") `elemAt` 0
      Just contextDiv <- !(query "div#context") `elemAt` 0
      innerHTML stackDiv ""
      innerHTML contextDiv ""
      clearError
      traverse_ (renderStackElem stackDiv) (reverse stack)
      traverse_ (renderContextElem contextDiv) (reverse $ toList ctx)

enter : Element -> Event -> JS_IO Int
enter prompt ev =
  do model <- getModel
     str   <- getValue prompt
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
{-
app : Event -> JS_IO Int
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

eval' : Event -> JS_IO Int
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

cmd'' : ModelEff () -> JS_IO Bool
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

cmd' : ModelEff () -> JS_IO ()
cmd' thecmd = cmd'' thecmd *> pure ()

cmd : ModelEff () -> Event -> JS_IO Int
cmd thecmd _ = cmd' thecmd *> pure 1

keyToChar : Key -> JS_IO String
keyToChar k = fromCharCode (toKeyCode k)

appendText : Element -> String -> JS_IO ()
appendText e t' =
  do t <- getText e
     setText e (t ++ t')

onKeyDown : Element -> JS_IO ()
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

main : JS_IO ()
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

     return ()
-- Local Variables:
-- idris-packages: ("effects" "lightyear" "iquery")
-- End:
-}
