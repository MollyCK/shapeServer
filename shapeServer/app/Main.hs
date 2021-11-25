{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shapes
import Render 

import Web.Scotty

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

{-
main :: IO ()
main = scotty 3000 ( do
    get "/" ( do
        html "Hello World"
     )

    get "/greet/:name" ( do
        name <- param "name"
        html (
            mconcat ["Hey there ", name]
         )
     )
 )
-}

exampleDrawing = [(scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle)]


main = renderCustomShape defaultWindow exampleDrawing

{-
-- Sample code from lectures, useful as syntax & structure examples
response :: Text -> Text
response n = do 
    R.renderHtml ( do 
        H.h1 ("Hello " >> H.toHtml n)
     )

longresponse :: Text -> Text
longresponse n = do
    R.renderHtml ( do
        H.head (H.title "Welcome page")
        H.body ( do
            H.h1 "Welcome!"
            H.p ("Welcome to my Scotty app" >> H.toHtml n)
            )
        )

-}