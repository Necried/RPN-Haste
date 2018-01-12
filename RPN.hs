-- A client-side Reverse Polish-Notation Calculator

import Haste
import Haste.DOM
import Haste.Events

import Data.List
import Control.Monad

main :: IO ()
main = do
    header <- newTextElem "A Simple Reverse Polish Calculator"
    input  <- mkInput 100 "Enter an expression here..."
    button <- mkButton "Evaluate!"
    result <- newElem "span"

    -- adds all HTML elements created above in a column 
    column documentBody [header,input,button,result]

    -- on submit, evaluate the expression and return a message wrapped in a 
    -- HTML span element
    onEvent button Click $ \_ -> do
        expr <- getProp input "value"
        printAns result $ rpnEvaluate expr

    focus input

-- takes the result computed by rpnEvaluate and sets the corresponding html element
printAns :: Elem -> Maybe Double -> IO ()
printAns result exprMaybe = do
    case exprMaybe of
        Just expr -> set result [ prop "innerHTML" =: ("Answer: " ++ (toString expr))
                                , style "color" =: "green" ]
        _ -> set result [ prop "innerHTML" =: "Error: Unable to evaluate expression!"
                        , style "color" =: "red" ]



{- helper functions below are for displaying the html layout
Original reference at:
http://www.cse.chalmers.se/edu/year/2017/course/TDA555/Haste/Pages.hs -}

-- mkInput renders a textbox with specified width and initial message
mkInput :: Int -> String -> IO Elem
mkInput width init = do
    newElem "input" `with` [attr "type"  =: "text",
                            attr "size"  =: show width,
                            attr "value" =: init]

-- mkButton creates a button for clicking with a given label
mkButton :: String -> IO Elem
mkButton label = newElem "button" `with` [prop "textContent" =: label]

-- `wrapDiv e` makes a "div" node with `e` as the only child
wrapDiv :: Elem -> IO Elem
wrapDiv e = newElem "div" `with` [children [e]]

-- `appendChildren parent children` adds a list of children to a parent element
appendChildren :: Elem -> [Elem] -> IO ()
appendChildren parent children = sequence_ [appendChild parent c | c <- children]

-- `column parent children` adds the children as a column to the parent
column :: Elem -> [Elem] -> IO ()
column parent children = do
    cs <- sequence [wrapDiv c | c <- children]
    appendChildren parent cs

{- the functions below define the implementation for the calculator 
Source: http://learnyouahaskell.com, Chapter 13 -}

-- readMaybe consumes a singleton string and wraps it in the Maybe monad
readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

-- foldingFunction performs stack computations of the given input
foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction (x:y:ys) "/" = return ((y / x):ys)
foldingFunction (x:y:ys) "^" = return ((y ** x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

-- rpnEvaluate takes a string of a RPN expression and returns the result of
-- the computation wrapped in a Maybe monad
rpnEvaluate :: String -> Maybe Double
rpnEvaluate st = do
    [result] <- foldM foldingFunction [] (words st)
    return result