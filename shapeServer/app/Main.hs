{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shapes
import Render 

import Data.Text.Lazy
import Web.Scotty

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

import Network.Wai.Middleware.Static    -- to make accessing images on client-side easier

singleEllipse = [(scale (point 0.5 0.25) <+> translate (point 0.8 0.4), circle, (ColourRGB 255 0 0))]
singleCircle =  [(identity, circle, ColourRGB 200 10 100)]
singleSquare = [(scale (point 0.9 0.9), square, ColourRGB 0 0 255)]
singleRectangle = [(scale (point 0.5 0.25), square, ColourRGB 0 255 255)]
singlePolygon = [(identity, polygon [(point 0.5 0.25), (point 0.75 0.25), (point 0.25 0.5), (point (-0.5) 0.75)], ColourRGB 100 100 10)]


overlapCircle = [(scale (point 0.5 0.25) <+> translate (point 0.8 0.4), circle, (ColourRGB 255 0 0)), (scale (point 0.5 0.25) <+> translate (point 0.4 0.4), circle, (ColourRGB 0 255 0))]
overlapSquare = [(scale (point 0.5 0.25), square, ColourRGB 0 0 255), (scale (point 0.5 0.25) <+> translate (point 0.8 0.4),square, ColourRGB 50 50 50)]
{-
content :: Text
content = do R.renderHTML (
        H.head $ do
        H.title "Shape Server"
    H.body $ do
        H.p "Sample images:"
        H.img H.! A.src "singleCircle.png"
        H.img H.! A.src "overlapCircle.png"
        H.p "The Producing co"
)

-- pre-rendering images 

main = do 
    renderCustomShape "static/singleEllipse.png" defaultWindow singleEllipse
    renderCustomShape "static/singleCircle.png" defaultWindow singleCircle
    renderCustomShape "static/singleSquare.png" defaultWindow singleSquare
    renderCustomShape "static/singleRectangle.png" defaultWindow singleRectangle
    renderCustomShape "static/singlePolygon.png" defaultWindow singlePolygon

    renderCustomShape "static/overlapCircle.png" defaultWindow overlapCircle
    renderCustomShape "static/overlapSquare.png" defaultWindow overlapSquare
-}

--{-
main :: IO ()
main = scotty 3000 ( do
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" ( do
        html "<head> \
        \ <title>Shape Server</title> \
        \ </head> \
        \ <body> \
        \ <h3>Sample single images (ellipse, circle, square, rectangle, polygon)</h3> \
        \ <ol> \
        \ <li> <img src=\"singleEllipse.png\" </li> <li> Produced by: singleEllipse = [(scale (point 0.5 0.25) <+> translate (point 0.8 0.4), circle, (ColourRGB 255 0 0))] / renderCustomShape \"static/singleEllipse.png\" defaultWindow singleEllipse</li>\
        \ <li> <img src=\"singleCircle.png\"> </li> <li>Produced by: singleCircle =  [(identity, circle, ColourRGB 200 10 100)] / renderCustomShape \"static/singleCircle.png\" defaultWindow singleCircle </li> \
        \ <li> <img src=\"singleSquare.png\"> </li> <li>Produced by: singleSquare = [(scale (point 0.9 0.9), square, ColourRGB 0 0 255)] / renderCustomShape \"static/singleSquare.png\" defaultWindow singleSquare </li> \
        \ <li> <img src=\"singleRectangle.png\"> </li> <li> Produced by: singleRectangle = [(scale (point 0.5 0.25), square, ColourRGB 0 255 255)] / renderCustomShape \"static/singleRectangle.png\" defaultWindow singleRectangle</li>\
        \ <li> <img src=\"singlePolygon.png\"> </li> <li>Produced by: singlePolygon = [(identity, polygon [(point 0.5 0.25), (point 0.75 0.25), (point 0.25 0.5), (point (-0.5) 0.75)], ColourRGB 100 100 10)] /     renderCustomShape \"static/singlePolygon.png\" defaultWindow singlePolygon </li> \
        \ </ol> \
        \ <h3>Sample masked images</h3> \
        \ <ol> \ 
        \ <li> <img src=\"overlapCircle.png\"> </li> <li> Produced by: overlapCircle = [(scale (point 0.5 0.25) <+> translate (point 0.8 0.4), circle, (ColourRGB 255 0 0)), (scale (point 0.5 0.25) <+> translate (point 0.4 0.4), circle, (ColourRGB 0 255 0))] / renderCustomShape \"static/overlapCircle.png\" defaultWindow overlapCircle </li>\
        \ <li> <img src=\"overlapSquare.png\"> </li> <li>Produced by: overlapSquare = [(scale (point 0.5 0.25), square, ColourRGB 0 0 255), (scale (point 0.5 0.25) <+> translate (point 0.8 0.4),square, ColourRGB 50 50 50)] / renderCustomShape \"static/overlapSquare.png\" defaultWindow overlapSquare</li>\
        \</body>"
         
     )

    get "/greet/:name" ( do
        name <- param "name"
        html (
            mconcat ["Hey there ", name]
         )
     )
 )

---}
exampleDrawing = [(scale (point 0.5 0.25) <+> translate (point 0.8 0.4), circle, (ColourRGB 255 0 0)), (scale (point 0.5 0.25) <+> translate (point 0.4 0.4), circle, (ColourRGB 0 255 0))]

-- main = renderCustomShape "static/singleCircle.png" defaultWindow exampleDrawing

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