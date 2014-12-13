---- | Language Pragmas of GHC Language Extensions
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, 
               TemplateHaskell, OverloadedStrings #-}

---- | Main entry point to the App
module Main where

  -- | Yesod Framework Library is the "foundation" (in Hebrew) of the website
  import Yesod

  {-
     | Foundation Data Type definition of Data Constructor taking no arguments or data
     | (similar to Global Variables)
  -}
  data HelloWorld = HelloWorld

  {-
     | Abstract Boilerplate code into 'myYesod' TH Template Function. Noting that TH
     | Generates Textual Code including Type-Checked Abstract Syntax Trees ASTs).
     |
     | 'mkYesod' Template Function takes two args:
     |   - "HelloWorld" String of foundation type (generates glued code)
     |   - mkYesod (Quasi-Quoted code that introduces Embedded Domain-Specific Languages EDSLs)
     |
     | 'mkYesod' introduces the EDSL (Parser Function) called 'parseRoutes' whose mini-language
     | is included between the pipes, containing a list of Routes, Resources, and Methods (for access).
     | Note: 
     |   - '/' identifies website Root.
     |   - HomeR (i.e. Home Resource) is the Route Name (Data Constructor) used for embedded links
     |   - GET is HTTP Request Method to access the web page
     | 
     | 'mkYesod' Dispatches Requests for each Route to implemented Handlers
     | (i.e. Homepage Handler translation is / HomeR GET ==> getHomeR)
  -}
  mkYesod "HelloWorld" [parseRoutes|
  / HomeR GET
  |]

  {- 
     | HelloWorld Type is instance of Yesod Class defined in Yesod Library.
     | Type Class instance is Interface-like with Virtual Functions and Default Implementations.
     | Override Yesod Functions to Customize website feel and behaviour using the 'instance' 
     | Declaration.
  -}
  instance Yesod HelloWorld

  {-
     | Handler Implementation for Homepage Resource Route, where:
     |   - defaultLayout Virtual Function (of Yesod Type Class) called with default implementation 
     |     laying out the HTML page and sends to browser (alternative is to Override it as part of 
     |     the 'instance Yesod' statement).
     |   - whamlet (HTML EDSL) Widget-Hamlet version whose Argument creates the Contents as an 
     |     In-Line Widget (composable abstraction combining HTML, CSS, JS, etc). whamlet is
     |     implemented using is a Quasi-Quotations.
  -}
  getHomeR :: Handler RepHtml
  getHomeR = defaultLayout [whamlet|Hello World!|]

  -- | The main entry point.
  main :: IO ()
  main = do
      -- putStrLn "Welcome to FP Haskell Center!"
      -- putStrLn "Have a good day!"
      {-
        Warp is built-in backend and server written in Haskell
      -}
      warpDebug 3000 HelloWorld
