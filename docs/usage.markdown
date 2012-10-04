---
layout: default
title: Install Haskell and Bayes-Stack
section: Bayes-Stack
---

Using Bayes-Stack Network Topic Models
=======================================

Bayes-stack comes with implementations of a various topic models for network analysis: 
- Shared Taste Model, to analyze topics shared by friends.
- Citation Influence Model, to analyze topics for which a paper is cited.
- Latent Dirichlet allocation, as a baseline that ignores the network structure. 

All models are implemented as a blocked collapsed Gibbs sampler, written for a multi-thread environment using bayes-stack

