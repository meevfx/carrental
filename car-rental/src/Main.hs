{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject, eitherDecode)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Static (staticPolicy, addBase, noDots, (>->))
import Control.Exception (try, SomeException, evaluate)
import Data.Char (toLower)
import Network.HTTP.Types.Status (status400, status500)
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy (pack, toStrict)

-- Rent request
data RentCarRequest = RentCarRequest
  { customerName     :: String
  , carType          :: String
  , carModel         :: String
  , rentalType       :: String
  , rentalDuration   :: Double
  , depositAmount    :: Double
  } deriving Show

instance FromJSON RentCarRequest where
  parseJSON = withObject "RentCarRequest" $ \v ->
    RentCarRequest <$> v .: "customerName"
                   <*> v .: "carType"
                   <*> v .: "carModel"
                   <*> v .: "rentalType"
                   <*> v .: "rentalDuration"
                   <*> v .: "depositAmount"

instance ToJSON RentCarRequest where
  toJSON r = object
    [ "customerName"   .= customerName r
    , "carType"        .= carType r
    , "carModel"       .= carModel r
    , "rentalType"     .= rentalType r
    , "rentalDuration" .= rentalDuration r
    , "depositAmount"  .= depositAmount r
    ]

-- Return request
data ReturnCarRequest = ReturnCarRequest
  { returnDeposit     :: Double
  , kilometersDriven  :: Double
  , duration          :: Double
  , damageCost        :: Double
  , returnRentalType  :: String
  , returnCarType     :: String
  } deriving Show

instance FromJSON ReturnCarRequest where
  parseJSON = withObject "ReturnCarRequest" $ \v ->
    ReturnCarRequest <$> v .: "returnDeposit"
                     <*> v .: "kilometersDriven"
                     <*> v .: "duration"
                     <*> v .: "damageCost"
                     <*> v .: "returnRentalType"
                     <*> v .: "returnCarType"

instance ToJSON ReturnCarRequest where
  toJSON r = object
    [ "returnDeposit"     .= returnDeposit r
    , "kilometersDriven"  .= kilometersDriven r
    , "duration"          .= duration r
    , "damageCost"        .= damageCost r
    , "returnRentalType"  .= returnRentalType r
    , "returnCarType"     .= returnCarType r
    ]

-- Rate calculator (case-insensitive)
getRate :: String -> String -> Double
getRate carType rentalType = getRate' (map toLower carType) (map toLower rentalType)
  where
    getRate' "5-seater" "hour" = 100  -- Adjusted to match ₹55500.00 for 555 hours
    getRate' "5-seater" "day"  = 2200 -- Matches Maruti Baleno rate
    getRate' "5-seater" "week" = 15400
    getRate' "8-seater" "hour" = 150
    getRate' "8-seater" "day"  = 3000
    getRate' "8-seater" "week" = 21000
    getRate' _ _ = 0

-- Main server
main :: IO ()
main = scotty 8080 $ do
  middleware simpleCors
  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $ file "static/index.html"

  post "/rent" $ do
    reqBody <- body
    case eitherDecode reqBody :: Either String RentCarRequest of
      Left err -> do
        liftIO $ putStrLn $ "JSON Parsing Error: " ++ err
        status status400
        json $ object ["error" .= ("Invalid request: " ++ err :: String)]
      Right validReq -> do
        liftIO $ putStrLn $ "Received request: " ++ show validReq
        let rate = getRate (carType validReq) (rentalType validReq)
            totalCost = rate * rentalDuration validReq
        json $ object
          [ "message" .= ("Car rented successfully!" :: String)
          , "customer" .= customerName validReq
          , "carModel" .= carModel validReq
          , "rentalDuration" .= rentalDuration validReq
          , "rentalType" .= rentalType validReq
          , "rentalCost" .= totalCost
          , "depositAmount" .= depositAmount validReq
          ]

  post "/return" $ do
    reqBody <- body
    case eitherDecode reqBody :: Either String ReturnCarRequest of
      Left err -> do
        liftIO $ putStrLn $ "JSON Parsing Error: " ++ err
        status status400
        text $ pack ("JSON Parsing Error: " ++ err)
      Right validReq -> do
        liftIO $ putStrLn $ "Received return request: " ++ show validReq
        liftIO $ putStrLn $ "Debug: returnCarType = '" ++ returnCarType validReq ++ "', returnRentalType = '" ++ returnRentalType validReq ++ "'"
        let rate = getRate (returnCarType validReq) (returnRentalType validReq)
            rentalCost = rate * duration validReq
            distanceCost = kilometersDriven validReq * 30
            damagesCost = damageCost validReq
            totalCost = rentalCost + distanceCost + damagesCost
            balance = returnDeposit validReq - totalCost
        liftIO $ putStrLn $ "Debug: rate = " ++ show rate ++ ", rentalCost = " ++ show rentalCost ++ ", duration = " ++ show (duration validReq)
        json $ object
          [ "totalCost" .= totalCost
          , "balance" .= if balance >= 0 then ("Refund: ₹" ++ show balance)
                         else ("Pay extra: ₹" ++ show (abs balance))
          ]

  get "/health" $ do
    text "Europcar Haskell Backend is running!"
