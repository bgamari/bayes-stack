---
layout: default
title: Install Haskell and Bayes-Stack
section: Bayes-Stack
---

Using Bayes-Stack Network Topic Models
=======================================

Bayes-stack comes with implementations of a various topic models for social network analysis:
- Shared Taste Model, to analyze topics shared by friends. [Dietz2012]
- Citation Influence Model, to analyze topics for which a paper is cited. [Dietz2007]
- Latent Dirichlet allocation, a baseline topic model that only incorporates text, but ignores the network structure. [Blei2003] 

[Dietz2012]: http://people.cs.umass.edu/~dietz/delayer/dietz-cameraready.pdf "Laura Dietz, Ben Gamari, John Guiver, Edward Snelson, Ralf Herbrich. De-Layering Social Networks by Shared Tastes of Friendships. ICWSM 2012."
[Dietz2007]: http://www.machinelearning.org/proceedings/icml2007/papers/257.pdf "Laura Dietz, Steffen Bickel, Tobias Scheffer. "Unsupervised Prediction of Citation Influences. ICML 2007."
[Blei2003]: http://www.cs.princeton.edu/~blei/papers/BleiNgJordan2003.pdf "David Blei, Andrew Ng, Michael Jordan. Latent Dirichlet Allocation. JMLR 2003."


### Gibbs sampling in Bayes-Stack

All models are implemented using the bayes-stack framework, which makes it very easy to implement multi-threaded blocked collapsed Gibbs samplers for latent variable and parameter estimation. Gibbs sampling starts with a random initialization of unknown variables, which are iteratively updated in sweeps over all variables for a number of iterations. Every `lag` iterations, bayes-stack will dump the model likelihood to `sweeps/likelihood.log` - a diagnostic measure which should improve on average. Bayes-stack will dump a state of latent variables to the `sweeps` directory whenever a new "best" likelihood is achieved. As early iterations are dominated by the random initialization, the first `burnin` iterations won't lead to a dump. Bayes-stack provides support for updating sets of variables from their joint distribution (aka blocked Gibbs sampler) via the concept of an update unit.

### Parallel inference in Bayes-Stack

Bayes-stack aims at multi-core environments, parallelizing the inference across multiple threads. In bayes-stack each worker thread will repeatedly pick an update unit, fetch the current model state, computes a new setting for variables in the update unit. It then prepares a thunk for updating the model state with this new setting (called a `diff`). One global diff-worker will apply the diffs in batches of `update-block`. Notice that the model state may have advanced in the mean time. We can still apply the diff, as Gibbs samplers are robust towards being mildly out-of-date. However, we ensure consistency of updates, such constant counts across all statistics using the diff-worker.

Bayes-stack is different to other parallel topic model frameworks in that *it does not* update disjoint sets of variables in isolation for several iterations. It further supports integrating out parameters with conjugative priors (collapsing). Bayes-stack is applicable any generative model (not just LDA), especially if strong interdependencies between variables and plates exist. Bayes-stack supports arbitrary nesting of plates and conditional draws (aka gates).


[gates]:http://research.microsoft.com/apps/pubs/default.aspx?id=78857 "Gates"

### Hyper-parameter optimization

Bayes-stack supports optimizing symmetric Dirichlet hyper-parameters (`alpha` in LDA) using Tom Minka's fixed point method. If enabled, an optimization phase is inserted every `hyper-lag` iterations (after `hyper-burnin` iterations). Diagnostic information about new settins of hyper-parameters as well as model likelihood before and after the change are written to `sweeps/hyperparams.log`. It is highly recommended to inspect diagnostic information before proceeding.


### Model inference and analysis

For each bayes-stack model, two binaries are provided:

bayes-stack-&lt;model&gt;
: program to run the Gibbs sampler and dump settings to the sweeps directory

bayes-stack-dump-&lt;model&gt;
: program to run after the Gibbs sampler finished to analyse the sweeps directory and compute point-estimates for parameters (e.g. theta and phi) and output as csv files. 


Network Topic models
--------------------

### Latent Dirichlet allocation

Probabistic topic models and its seminal basic variant latent Dirichlet allocation [Blei2003] are an unsupervised method for clustering words into so-called topics, taking their document context into account. The original work was motivated from a perspective of documents, here we assume that each node as a set of words associated. We therefore use the term `node` instead of `document`. Striving towards applications on tags, songs, books, we also use the `item` instead of `word`. 

The intuition of topic model is that two tokens are likely about the same `topic` if they represent the same word, and/or are in the same node. 

Nomenclature:

*thetas*
: for all nodes, the mixture of topics (e.g. document 1 is one third about topic 1, two thirds about topic 5)

*phis*
: for all topics, the mixture of items. (e.g. topic 1 has item "soccer" with 0.1 and item "ball" with 0.05)

To run an LDA topic model with 10 topics using 5 parallel threads call
    bayes-stack-lda --nodes FILE -t10 --sweeps=ldasweeps --threads=5

The nodes file is in the format of

     `node id` \t all items (e.g. words) on one line \n

If a stopwords file is given, those items are ignored from the input.


After the Gibbs sampler finished, inspect `likelihood.log` file (in the sweeps directory) to confirm that the model likelihood converged. If hyperparameter estimation is turned on, also inspect `hyperparams.log` to ensure that parameters are in a reasonable range.

To output the top 20 items for each topic call

    bayes-stack-dump-lda phis -n20 --sweeps ldasweeps

To output the topic mixtures for each node/document call

    bayes-stack-dump-lda thetas --sweeps ldasweeps

The output format for `N` multinomial parameters over support `S1`, `S2`, ... is

<table border="yes">
<tr><td>N</td><td>&nbsp;</td><td>&nbsp;</td></tr>
<tr><td>&nbsp;</td><td>S1</td><td>probability</td></tr>
<tr><td>&nbsp;</td><td>S2</td><td>probability</td></tr>
<tr><td>&nbsp;</td><td>...</td><td>&nbsp;</td></tr>
</table>

### Shared Taste Model

The shared taste model is a probabilistic network topic model to understand shared topics that underlie a friendship. The intuition is that if two friends are using the same items, this represents a shared taste. If the friends use different items that also have been mutually used in other friendships, it is also likely that they represent the shared taste. This is modeled by introducing topic mixtures of friendships (i.e. edges, not nodes!). Each node associates their items with one of their friends, then draws a topic from the shared topic mixture of that friendship to generate the item. In order to be robust against nodes with individual (that is, non-shared) interests, an item can also be associated with its own topic mixture.

Nomenclature:

*lamdas*
: for all edges, the mixture of topics (equivalent to theta in LDA, but shared by two nodes)

*phis*
: for all topics,  mixture of items (as in LDA)

*psis*
: one global mixture over all edges (for convenience, it provides for each user the restricted mixture over friendships a given user is involved in)

*omegas*
: for each node, its own mixture of topics

*gammas*
: for each node, a Bernoulli distribution sharing versus own topics


To run the shared taste model with 10 topics using 5 parallel threads call

     bayes-stack-st --edges FILE --nodes FILE -t10 --sweeps=stsweeps --threads=5 


Two kinds of input data are required. The nodes file has to be in the format

     `node id` \t all items (e.g. words) on one line \n

If a stopwords file is given, those items are ignored from the input.

The edges file has to list each edge as the two nodes it connects. All edges are undirected, so you only have to give one direction, but both directions do not hurt. Follow the format:

     `node id` \t `node id` \n


After the Gibbs sampler finished, inspect `likelihood.log` file (in the sweeps directory) to confirm that the model likelihood converged. If hyperparameter estimation is turned on, also inspect `hyperparams.log` to ensure that parameters are in a reasonable range.

To output any of the multinomial or Bernoulli parameters listed above (say lambdas) call

     bayes-stack-dump-st lambdas --sweeps stsweeps

For multinomial distributions with many dimentions (such as phis) it is advisable to restrict the range, e.g. "-n20".

To identify the influencial friends for a particular user call

     bayes-stack-dump-st influences --sweeps stsweeps

Notice that `influences` is different from `psis`. Any friend that shares a frequent topic with the user will have a high `influence`. But for generating items, those friends would have to compete with each other, which is reflected in `psis`. We recommend to use `influences` for any social network analysis.



### Citation influence model

The citation influence model is designed to understand for which topics a document was cited and to determine a given research paper which its citations had strong influence on it. Notice that even seminal papers, may be cited by papers on which they only have a marginal influence. We treat each document as a node, and each citation as an arc from the citing to the cited paper.

Those local influences on a citing node are modeled by mixture over citations `psi`. Each cited node has a mixture over topics `lambda`. Each item in a citing node are associated with one of its citations, and a topic drawn from that cited documents's `lambda`. Further, each item in a cited node are drawn from its `lambda`. The consequence is that a cited node's topic mixture `lambda` is a shared topic mixture, estimated not only from the node's items, but also some items in citing nodes. This is a crucial distinction to LDA. `psi` and `lambda` influence each other: The more likely items in a citing document fit to a topic mixture, the higher its probability  under `psi`; The more items in citing documents are associated with the cited document, the more `lamba` will be representing it.

Each node in the citation graph will be represented once as a cited document, and once as a citing document. Topics in both duplicates are synchronized via joint influence of `phi`. As some research papers include more novel ideas than others, each citing document also has a topic mixture `omega` for own topics. The propensity to re-use topics from citations versus the introduction of new topics is captured in the Bernoulli parameter `gamma`.

Nomenclature:

*lambdas*
: for each cited node, the mixture of topics (equivalent to theta in LDA, but modeling shared across the cited node and its citations)

*phis*
: for each topic, the mixture of items (as in LDA)

*psis*
: for each citing node, the mixture over its cited nodes

*omegas*
: for each citing node, the own mixture of topics

*gammas*
: for each citing node, a Bernoulli distribution for sharing versus own topics


To run the shared taste model with 10 topics using 5 parallel threads call

     bayes-stack-ci --arcs FILE --nodes FILE -t10 --sweeps=cisweeps --threads=5 


Two kinds of input data are required. The nodes file has to be in the format

     `node id` \t all items (e.g. words) on one line \n

This id is used both as a cited node id as well as a citing node id. If a stopwords file is given, those items are ignored from the input.

The arcs file has to list each arc as the source node and sink node. All arcs are directed. You *can* add cycles. Follow the format:

     `citing node id` \t `cited node id` \n


After the Gibbs sampler finished, inspect `likelihood.log` file (in the sweeps directory) to confirm that the model likelihood converged. If hyperparameter estimation is turned on, also inspect `hyperparams.log` to ensure that parameters are in a reasonable range.

To output any of the multinomial or Bernoulli parameters listed above (say lambdas) call

     bayes-stack-dump-ci lambdas --sweeps cisweeps

For multinomial distributions with many dimentions (such as `phis`) it is advisable to restrict the range, e.g. "-n20".

To identify the influencial cited nodes for a particular citing node call

     bayes-stack-dump-ci influences --sweeps cisweeps

Notice that `influences` is different from `psis`. Any citation that shares a frequent topic with the node will have a high `influence`. But for generating items, those citations would have to compete with each other, which is reflected in `psis`. We recommend to use `influences` for any social network analysis.









