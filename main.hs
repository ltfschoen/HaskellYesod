---- | Language Pragmas of GHC Language Extensions
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, 
               TemplateHaskell, OverloadedStrings #-}

---- | Main entry point to the App
module Main where

  {-
     | Yesod Framework Library is the "foundation" (in Hebrew) of the website.
     | Yesod is built upon Web Application Interface (WAI) supporting FastCGI, SCGI, Warp, Webkit
  -}
  import Yesod

  import Text.Hamlet (HtmlUrl, shamlet)
  import Text.Blaze.Html.Renderer.String (renderHtml)
  import Data.Char (toLower)
  import Data.List (sort, null, map)
  import Data.Text (Text)

  {-
     | Foundation Data Type definition of Data Constructor taking no arguments or data
     | (similar to Global Variables). Central location to declare Settings Controlling
     | the execution of the app (i.e. Routes, Instance Declaration, Store Initialisation Info,
     | Database Connection Pool, Load Config File Settings, HTTP Connection Manager)
  -}
  data Links  = Links

  data Person = Person
      { name :: String
      , age  :: Int
      } deriving Show

  isAdmin :: Person -> Bool
  isAdmin _ = True

  {-
     | Front Controller Pattern of Yesod where all requests to app enter at same point for routing.
     |
     | Abstract Boilerplate code into 'myYesod' TH Template Function. Noting that TH
     | Generates Textual Code including Type-Checked Abstract Syntax Trees ASTs).
     |
     | 'mkYesod' is a Template Function takes two args:
     |   - "Links" String of foundation type (generates glued code)
     |   - mkYesod (Quasi-Quoted code that introduces Embedded Domain-Specific Languages EDSLs)
     |
     | 'mkYesod' introduces the EDSL (Parser Function) called 'parseRoutes' (which is a
     | Quasi-Quoter) whose mini-language is included between the pipes, containing a list of Routes,
     | Resources, and Methods (for access).
     |
     | Note: 
     |   - '/' identifies website Root.
     |   - HomeR (i.e. Home Resource) is the Route Name (Data Constructor) used for embedded links
     |   - GET is HTTP Request Method to access the web page that it answers requests from
     | 
     | 'mkYesod' Dispatches Requests for each Route to implemented Handlers
     | (i.e. Homepage Handler translation is / HomeR GET ==> getHomeR)
  -}
  mkYesod "Links" [parseRoutes|
  / HomeR GET
  /page1 Page1R GET
  /page2 Page2R GET
  /page4 Page4R GET
  |]

  {- 
     | HelloWorld Type ("foundation" Data Type) is instance of Yesod TypeClass defined in Yesod Library.
     | TypeClass instance is Interface-like with Virtual Functions and Default Implementations.
     | Override Yesod Functions to Customize website feel and behaviour using the 'instance' 
     | Declaration.
  -}
  instance Yesod Links

  {-
     | Handler Implementation for Homepage Resource Route, where:
     |   - defaultLayout Virtual Function (of Yesod Type Class) called with default implementation 
     |     and responds by wrapping given content in a HTML file with doctype, html, head, and body
     |     tags for opening in a browser (alternative is to Override it as part of the 'instance
     |     Yesod' statement).
     |   - whamlet (HTML EDSL) Widget-Hamlet version whose Argument creates the Contents as an 
     |     In-Line Widget (composable abstraction combining HTML, CSS, JS, etc). whamlet is
     |     implemented using is a Quasi-Quotations and converts Hamlet syntax into a Widget
     |     (Modular Component) consisting of HTML, CSS, and JS for reuse.
     |
     | Note: Handler is to process user input, perform DB queries, and create responses (Controller)
     | Note: Hamlet is default HTML templating engine in Yesod
     | Note: Type-Safe URLs used by making each Resource a Data Constructor and interpolating Haskell
     |       values to textual URLs before sending to user (see -ddump-splices output).
     |       Type-Safe URLs flexibility/robustness by allowing URLs to move around without breaking links
  -}
  getHomeR  :: Handler Html -- RepHtml is deprecated
  getHomeR  = defaultLayout [whamlet|<a href=@{Page4R}>Go to page 4!|] -- Type-Safe URLs
  getPage1R = defaultLayout $ do
        -------
        -- HEAD
        -------
        setTitle "Luke's Page Title"
        toWidgetHead [hamlet|
          <meta name=keywords content="haskell yesod test keywords">
        |]
        -------
        -- CSS
        -------
        toWidget [lucius|
          h1 { color: green; }
        |]
        -------
        -- BODY CONTENT
        -------
        [whamlet|
          <a href=@{HomeR}>Go home!
        |]
        toWidget [hamlet| <h1>Click this content for an alert
        |]
        -------
        -- BODY JS
        -------
        toWidgetBody [julius|
          alert("This script in the body itself.");
        |]
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
        toWidget [julius|
          $(function() {
            $("h1").click(function(){
              alert("heading was clicked");
            });
          });
        |]
  getPage2R = defaultLayout [whamlet|
        <a href=@{HomeR}>Go home!
        <a href=@{Page1R}>Go to page 1!
        |]
  -- isAdmin has been hard coded as True
  getPage4R = do
    defaultLayout [whamlet|
      $doctype 5
      $if isAdmin person
        <p>Hello, my name is #{name person} and I am #{show $ age person}.
        <p>
          My name sorted in alphabetical order is: #
          <b>#{sort $ Data.List.map Data.Char.toLower (name person)}
        <p>In 5 years I will be #{show (5 + (age person))} years old.
      $else
        <p style="color:red">No people
      <input type=checkbox :isAdmin person:checked>
      <a href=@{Page2R}>Go to page 2!
    |]
    where
      person = Person "Luke" 33

  -- | The main entry point.
  main :: IO ()
  main = do

      {-
        Warp is built-in backend and server written in Haskell. warpDebug is deprecated
      -}
      warp 3000 Links