{-# LANGUAGE OverloadedStrings #-}

module Views
       ( ViewT
       , asText
       , asJson
       , asHtml
       , err406
       ) where

import Web.Scotty
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import Network.HTTP.Types (status406)
import Prelude hiding (id)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import DiceRoll

type ViewT = Either String (DiceRoll, RollResult) -> ActionM ()

-- Text ------------------------------------------------------------------------

asText :: ViewT
asText r = text . TL.pack $ case r of
   Right (_, res) -> show res
   Left  err      -> err

-- Json ------------------------------------------------------------------------

asJson :: ViewT
asJson r = json $ case r of
  Right (dr, res) -> diceJsonOk dr res
  Left  err       -> diceJsonErr err

data JsonResponse = JsonResponseOk DiceRoll Int | JsonResponseErr String

instance A.ToJSON JsonResponse where
  toJSON (JsonResponseOk roll res) =
    A.object ["diceroll" .= roll
             , "result"  .= res]
  toJSON (JsonResponseErr msg) =
    A.object ["error" .= msg]

diceJsonOk :: DiceRoll -> Int -> JsonResponse
diceJsonOk = JsonResponseOk

diceJsonErr :: String -> JsonResponse
diceJsonErr = JsonResponseErr

-- HTML ------------------------------------------------------------------------

asHtml :: ViewT
asHtml r = html . renderHtml $ case r of
  Right (dr, res) -> diceHtmlOk dr res
  Left  err       -> diceHtmlErr err

css :: H.Html
css = H.link ! rel "stylesheet" ! type_ "text/css" ! href "css/style.css"

fork :: H.Html
fork = H.a ! href "http://github.com/cryon/dice-service" $
       H.img ! id "fork" ! src "img/fork.png" ! alt "Fork me on GitHub!"

diceHtmlOk :: DiceRoll -> Int -> H.Html
diceHtmlOk roll res = H.docTypeHtml $ do
  H.head $ do
    H.title "Dice as a Service"
    css
  H.body $ do
    fork
    H.div $ do
      H.h1 $ H.toHtml $ toString roll
      H.p ! class_ "flavor" $ H.toHtml (flavor roll)
      H.p ! class_ "flavor" $ do
        H.toHtml (resultFlavor roll)
        H.toHtml $ T.pack " The result is "
        H.span ! class_ "result" $ H.toHtml $ T.concat [intEnglish res
                                                       , " ("
                                                       , T.pack (show res)
                                                       , ")"]
        H.toHtml $ T.pack "!"
    diceHtmlHelp

diceHtmlErr :: String -> H.Html
diceHtmlErr msg = H.docTypeHtml $ do
  H.head $ do
    H.title "Dice as a Service: Error"
    css
  H.body $ do
    fork
    H.div $ do
      H.h1 $ H.toHtml $ T.pack "Death is coming..."
      H.p ! class_ "flavor error" $ do
        H.toHtml $ T.pack "The gods are upset! This is not a dice roll they \
                          \can interpret. You hear a thousand voices in your \
                          \head speaking in an unwordly language:"
      H.p ! class_ "flavor gods" $ do
        H.toHtml msg
    diceHtmlHelp

diceHtmlHelp :: H.Html
diceHtmlHelp = H.div ! class_ "help" $ do
  H.h2 "A few suggestions:"
  H.ul $ do
    H.li $ H.a ! href "/2d6"   $ "Two six-sided dice."
    H.li $ H.a ! href "/d10+4" $ "A ten-sided die with a modifier of +4."
    H.li $ H.a ! href "/d"     $ "A single six-sided die."
    H.li $ H.a ! href "/d1"    $ "The exciting single sided die!"

-- Errors ----------------------------------------------------------------------

err406 :: ViewT
err406 _  = status status406
