 {-# LANGUAGE OverloadedStrings #-}

module DiceRoll
       ( DiceRoll
       , RollResult
       , performDiceRoll
       , parseDiceRoll
       , toString
       , flavor
       , resultFlavor
       , intEnglish
       ) where

import System.Random
import Data.Char (digitToInt)

import Control.Applicative ((<$>), (<*>), (*>))
import Data.Aeson (ToJSON(..), object, (.=))

import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as T

import Data.Maybe (fromJust)
import Text.Numeral.Grammar.Reified (defaultInflection)
import qualified Text.Numeral.Language.EN as N

import qualified Control.Monad.State as S
import Control.Monad (foldM, liftM)

type DiceState = S.State StdGen

type RollResult = Int

data Dice = Dice
            { dSides :: Int
            } deriving (Show)

roll :: Dice -> DiceState RollResult
roll d = do
  gen <- S.get
  let (val, gen') = randomR (1, dSides d) gen
  S.put gen'
  return val

data DiceRoll = DiceRoll
                { drNumber   :: Int
                , drDice     :: Dice
                , drModifier :: Int
                } deriving (Show)

performDiceRoll :: DiceRoll -> DiceState RollResult
performDiceRoll r = do
  dice <- return $ replicate (drNumber r) (drDice r)
  result <- foldM (\a d -> liftM (a +) $ roll d) 0 dice
  return $ result + drModifier r

-- Parser ----------------------------------------------------------------------

parseDiceRoll :: T.Text -> Either ParseError DiceRoll
parseDiceRoll = parse diceRollP "(unknown)"

diceRollP :: Parser DiceRoll
diceRollP = do
  number <- option 1 naturalP
  dice <- diceP
  modifier <- option 0 modifierP
  eof
  return $ DiceRoll number dice modifier

diceP :: Parser Dice
diceP = do
  _ <- oneOf "dD"
  sides <- option 6 naturalP
  return $ Dice sides

modifierP :: Parser Int
modifierP = positive <|> negative
  where positive = char '+' *> naturalP
        negative = char '-' *> (negate <$> naturalP)

naturalP :: Parser Int
naturalP = stringToInt <$> natural'
  where natural'     = (:) <$> nonZeroDigit <*> number
        nonZeroDigit = oneOf "123456789"
        number       = many digit

stringToInt :: String -> Int
stringToInt = foldl collect 0
  where collect acc i = acc * 10 + digitToInt i

-- Json serializtion -----------------------------------------------------------

instance ToJSON DiceRoll where
  toJSON (DiceRoll number (Dice sides) modifier) =
    object ["number"    .= number
           , "sides"    .= sides
           , "modifier" .= modifier]

-- Flavor text generation ------------------------------------------------------

toString :: DiceRoll -> String
toString (DiceRoll n (Dice s) m) = n' ++ "d" ++ (show s) ++ m'
  where n' = if n == 1 then "" else show n
        m' | m == 0 = ""
           | m > 0  = "+" ++ show m
           | otherwise = show m

flavor :: DiceRoll -> T.Text
flavor (DiceRoll number (Dice sides) modifier)
  | number == 1 = T.concat ["You roll your lucky "
                           , intEnglish sides
                           , "-sided die"
                           , modFlavor modifier
                           , if modifier == 0 then "!" else ""
                           ]
  | otherwise   = T.concat ["You carefully select "
                           , intEnglish number
                           , " of your best "
                           , intEnglish sides
                           , "-sided dice"
                           , if modifier == 0 then " and " else ", "
                           , "throw them ferociously on the table"
                           , if modifier == 0 then "!" else ""
                           , modFlavor modifier
                           ]

resultFlavor :: DiceRoll -> T.Text
resultFlavor (DiceRoll num _ _)
  | num > 1   = "You analyze the mess before you."
  | otherwise = "You investigate the die."

modFlavor :: Int -> T.Text
modFlavor m | m == 0    = ""
            | m > 0     = T.concat [" and add a modifier of "
                                   , intEnglish m
                                   , " to the result!"
                                   ]
            | otherwise = T.concat [" and subtract "
                                   , intEnglish (abs m)
                                   , " from the result!"
                                   ]

intEnglish :: Int -> T.Text
intEnglish i = fromJust $ N.us_cardinal defaultInflection $ toInteger i
