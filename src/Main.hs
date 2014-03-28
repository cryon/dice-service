{-# LANGUAGE OverloadedStrings #-}

import System.Random
import System.IO
import Web.Scotty
import Data.Default (def)
import Data.Maybe (fromJust)
import Network.HTTP.Media
import Control.Monad.Trans
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Control.Monad.State as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B

import DiceRoll
import Views
import Args

-- Accept header ---------------------------------------------------------------

mediaMapping :: [(MediaType, ViewT)]
mediaMapping = [ ("text/plain"      , asText)
               , ("text/html"       , asHtml)
               , ("application/json", asJson)
               ]

bestAcceptMatch :: T.Text -> Maybe MediaType
bestAcceptMatch a = parse a >>= match
  where parse = parseAccept . toBs
        match = matchAccept types
        toBs  = B.pack . T.unpack
        types = map fst mediaMapping

viewLookup :: MediaType -> ViewT
viewLookup = fromJust . flip lookup mediaMapping

-- Configuration ---------------------------------------------------------------

setupLogger :: Handle -> ScottyM ()
setupLogger handle = do
  logger <- liftIO $ mkRequestLogger def { destination = Handle handle
                                         , outputFormat = Apache FromSocket
                                         }
  middleware logger

setupStatic :: ScottyM ()
setupStatic = middleware $ staticPolicy (noDots >-> addBase "static")

-- Get shit done! --------------------------------------------------------------

main :: IO ()
main = do
  args  <- execParser opts
  serve (argPort args) stdout

serve :: Int -> Handle -> IO ()
serve port logHandle = scotty port $ do
  setupLogger logHandle
  setupStatic

  get "/:diceRoll" $ do
    dice <- param "diceRoll"
    gen  <- lift newStdGen
    res  <- rollHelper dice gen

    accept <- reqHeader "accept"
    (case bestAcceptMatch . TL.toStrict  =<< accept of
      Just mediaType -> viewLookup mediaType
      Nothing        -> err406) res

rollHelper :: T.Text -> StdGen -> ActionM (Either String (DiceRoll, RollResult))
rollHelper d g = do
  return $ case parseDiceRoll d of
    Right roll -> Right (roll, fst $ S.runState (performDiceRoll roll) g)
    Left  err  -> Left $ show err
