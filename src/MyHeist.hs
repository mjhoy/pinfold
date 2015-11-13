{-# LANGUAGE OverloadedStrings #-}

module MyHeist where

import           Control.Lens
import           Control.Monad.Trans
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml as X

import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth

------------------------------------------------------------------------------
-- | Splice that returns the "class" attribute for the body tag.
--
-- Requires access to the AuthManager to see whether the user is
-- currently logged in.
bodyClasses :: SnapletLens b (AuthManager b) -> SnapletISplice b
bodyClasses auth = do
  chk <- lift $ withTop auth isLoggedIn
  case chk of
    False -> return mempty
    True  -> textSplice "logged-in"


------------------------------------------------------------------------------
-- | Helper splice that runs if the tag attribute is bound.
--
-- Example:
--
-- <ifBound tag="myBoundTag">
--   This text is only inserted if `myBoundTag' is bound. Otherwise, nothing.
-- </ifBound>
--
-- Requires that the `bind' splice be an interpreted, not a loadtime
-- splice. See the myHeistInit.
ifBound :: SnapletISplice b
ifBound = do
  inp <- getParamNode
  let t = X.getAttribute "tag" inp
  case t of
    Nothing -> return mempty
    Just t' -> do
      st <- getHS
      let s = lookupSplice t' st
      case s of
        Nothing -> return mempty
        Just _x -> runChildren


------------------------------------------------------------------------------
-- | Add helper splices to the app initialization.
addHelperSplices :: HasHeist b =>
                    Snaplet (Heist b) ->
                    SnapletLens b (AuthManager b) ->
                    -- ^ A reference to 'AuthManager'
                    Initializer b v ()
addHelperSplices h auth = addConfig h $ set scInterpretedSplices splices
                          mempty
  where splices = do
          "ifBound" ## ifBound
          "bodyClasses" ## bodyClasses auth


------------------------------------------------------------------------------
-- | Define the Heist snaplet initialization.
myHeistInit :: FilePath -> SnapletInit b (Heist b)
myHeistInit templateDir = heistInit' templateDir config
  where
    sc = set scInterpretedSplices defaultLoadTimeSplices mempty
    config = emptyHeistConfig & hcSpliceConfig .~ sc
                              & hcNamespace .~ ""
                              & hcErrorNotBound .~ True

