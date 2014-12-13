README
============

Web Applications with Haskell using Yesod Framework


Requirements
-------

* Haskell GHCi
* Cabal Yesod Module
* Cabal Alex Module


Continuous Integration
-------

* [Travis CI Tests Status](https://travis-ci.org/ltfschoen/HaskellYesod)


Setup
-------

1. **Setup Haskell Yesod**
  * Clone or fork [HaskellYesod repository](https://github.com/ltfschoen/HaskellYesod)
  * Configure Yesod
    - Install version 3 of Alex Module (required by Yesod JS minifier hjsmin)
    ```
    cabal install alex
    ```
    - Install Yesod Framework and associated dependencies
    ```
    cabal install yesod
    ```
  * Run GHC wth -ddump-splices Option to view generated code
  ```
  ghc main.hs -ddump-splices
  ```
  * Open server on Port 3000 and load http://localhost:3000 in browser
  ```
  runhaskell main.hs
  ```