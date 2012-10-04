---
layout: default
title: Install Haskell and Bayes-Stack
section: Bayes-Stack
---

Bayes-Stack and Network Topic Models
=====================================

Installing Haskell 7.6.1 
-------------------------


The model implementation is in
BayesStack/Models/Topic/SharedTaste.hs

For an example on how to use is, see SharedTasteTest.hs

- Install Haskell Platform from [http://hackage.haskell.org/platform/](http://hackage.haskell.org/platform/)
- Install Haskell GHC 7.6.1 from [http://www.haskell.org/ghc/download_ghc_7_6_1](http://www.haskell.org/ghc/download_ghc_7_6_1) (our software needed a fix that is not available in the latest version of Haskell Platform)
-- $ sudo apt-get install libgmp3c2
-- unzip tarball and cd into directory
-- $ ./configure
-- make install
-- check that you have the right version with $ ghc -V
 should return "The Glorious Glasgow Haskell Compilation System, version 7.6.1"


Installing Bayes-Stack 2.0
----------------------------

- Clone from github: git://github.com/bgamari/bayes-stack.git
- cd into the bayes-stack source directory
- cabal install ./ network-topic-models/
It should finish with "Installing executable(s) in ~/.cabal/bin"

- ensure you have ~/.cabal/ in your $PATH


Testing the Installation
------------------------

- call $ bayes-stack-lda
- If you see the usage information, you are all set.


Bug Reports
------------
If you come across any issues/bugs/trouble, please submit a bug report to our issue tracker:
[https://github.com/bgamari/bayes-stack/issues/new](https://github.com/bgamari/bayes-stack/issues/new)

