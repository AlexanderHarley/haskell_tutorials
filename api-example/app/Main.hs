{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config
import Lucid

import Data.Aeson   hiding (json)
import Data.Monoid  ((<>))
import Data.Text    (Text, pack)
import GHC.Generics

data Person = Person {
    name :: Text,
    age  :: Int
} deriving (Generic, Show)

data SuccessResponse = SuccessResponse {
    status :: Text,
    msg    :: Text
} deriving (Generic, Show)

instance ToJSON Person
instance ToJSON SuccessResponse
instance FromJSON Person

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock spockCfg app)

helloSpock :: SpockAction database session state ()
helloSpock = html "Hello, <em>Spock!</em><h1>Hello World!<h1>"

helloSpockHTML :: Html ()
helloSpockHTML =
    html_ $ do
        head_ $ title_ "Hello!"
        body_ $ do
            h1_ "Hello!"
            p_ "Hello, Lucid!"

app :: Api
app = do
        get "/" helloSpock
        get "people" $ json [
                Person { name = "Fry", age = 25 },
                Person { name = "Bender", age = 4}
            ]
        post "people" $ do
            thePerson <- jsonBody' :: ApiAction Person
            json SuccessResponse { status = "Success", msg = "Parsed: " <> pack (show thePerson) }
