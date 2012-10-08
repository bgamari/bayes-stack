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

1. Install Haskell Platform 7.4.1 from [http://hackage.haskell.org/platform/](http://hackage.haskell.org/platform/)
2. Ensure that package libgmp3c2 is available, for instance with `$ sudo apt-get install libgmp3c2`
2. Install Haskell GHC 7.6.1 from [http://www.haskell.org/ghc/download_ghc_7_6_1](http://www.haskell.org/ghc/download_ghc_7_6_1)
  - Ignore the large "STOP" note on the website. Just installing Haskell Platform is not enough, since bayes-stack relies on language features that is currently not available in the GHC version provided by Haskell Platform. Scroll down to "Binary Packages" and download the tar-ball for your platform.
  - unzip tarball and cd into the directory
  - If you are root call `$ ./configure; sudo make install`
  - If you are not root, you can install it to directory of your choice (lets call it `<installdir>`) with `$ ./configure --prefix=<installdir>; make install`  then add `<installdir>/bin` to your `$PATH`
  - check that you have the right version with `ghc -V` respond with version 7.6.1 - *not* 7.4.1!
  - Update the Haskell package manager with `cabal update`



Installing Bayes-Stack
----------------------------

- Clone from github by calling `git clone git://github.com/bgamari/bayes-stack.git`
- cd into the bayes-stack source directory
- Call `$ cabal install ./ network-topic-models/`
- It should finish with "Installing executable(s) in ~/.cabal/bin"
- ensure that ~/.cabal/bin/ is permanently your `$PATH` variable


Testing the Installation
------------------------

- call `$ bayes-stack-lda`
- If you see the usage information, you are all set.


Continue to read about [how to use bayes-stack topic models..](usage.html)


Bug Reports
------------
If you come across any issues/bugs/trouble, please submit a bug report to our issue tracker:
[https://github.com/bgamari/bayes-stack/issues/new](https://github.com/bgamari/bayes-stack/issues/new)

