module XMonad.Powerbar.Widgets.IBus
    ( iBusWidget
    , iBusNextEngine
    , iBusQuery
    ) where

import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import XMonad.Powerbar.Lens
import Control.Applicative       (empty)
import Data.Maybe                (fromMaybe)
import Data.Text                 (pack)
import XMonad.Powerbar.Widgets.Common (execute, widget, widgetMaybe,
                                       widgetUpdate, widgetState')
import qualified XMonad as X


-- | IBus widget name
iBusName :: String
iBusName = "IBus"


-- | IBus keyboard layout widget
iBusWidget :: [(String, String)] -> X.X View
iBusWidget engines = widget iBusName 30 iBusQuery engines $ do
  engineString <- widgetMaybe $ execute "ibus" ["engine"] ""
  let engine = takeWhile (/= '\n') engineString
      name   = fromMaybe engine (lookup engine engines)
  if engine == ""
  then empty -- disable widget
  else return $ VAction VButtonLeft iBusNextEngine
              . VAttr ".ibus" (String . pack $ name)
              $ textSpaced name


-- | Switch IBus engine to next one
iBusNextEngine :: X.X ()
iBusNextEngine = do
  engines <- use $ widgetState' iBusName ([] :: [(String, String)])
  engineString <- fromMaybe "" <$> execute "ibus" ["engine"] ""
  let engine = takeWhile (/= '\n') engineString
      nextEngine []           = engine  -- engines set is empty (keep current)
      nextEngine es@((e,_):_) =
          case dropWhile ((/= engine) . fst) (es ++ es) of
            (_:(e',_):_) -> e' -- next engine after current one
            _            -> e  -- take first engine (current was not found)
  -- use execute (read output) to make sure language has been switched
  execute "ibus" ["engine", nextEngine engines] ""
  widgetUpdate iBusName


iBusQuery :: StyleQuery ()
iBusQuery = matchAttr ".ibus" >> swapColors
