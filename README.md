# ISMiR: Interpretive Structural Modelling (ISM) in R

## What is it?
This R code explores an old graph-partitioning algorithm. The algorithm's end-goal is to find a minimal-edge, hierarchical digraph which preserves the reachability properties of a given digraph. This algorithm used to be a major part of Interpretive Structural Modelling (ISM). I am referring specficially the algorithms described in the work of J.N Warfield:
- Warfield (1976) Societal Systems ISBN 0-471-01569-5 esp. Ch.9 and 10
- Warfield (1974) 10.1109/TSMC.1974.4309336
- Warfield (1973) doi 10.1109/TSMC.1973.4309270

## What's the point?
Today the term ISM has mutated - and now refers to some wishy-washy parody of the original, rigorous approach. Over the past decades, academics have unilaterally decided to strip ISM off its most characteristic computational devices. As rigour decreased, the number of publications claiming to apply ISM grew (or vice versa? but I digress...). Long story short, published ISM research today amounts to little more than a pile rubbish - in my estimation. Think I'm being too harsh? Pick any paper published in the last twenty-odd years and find out by yourself (hint: look for the mystical "conical" matrix - as in "shaped as a cone" - and you'll find out it's not a typo).

Enough digressing - but for more rants feel free to dig some work in progress of mine, available as a preprint at http://dx.doi.org/10.2139/ssrn.3622934 . 
Utilmately, the code is meant for use as supporting materials for such paper.

## If this is old stuff, isn't there something else out there that does it?
There are not many options that I know of to implement ISM calculations transparently on a computer. The J.N. Warfield IP Trust kindly provides ISM software free of charge (see: https://www.jnwarfield.com/ism-software.html). Yet the software is not open source and I'm not sure which parts of the original algorithm are implemented - if any - and how.

Therefore, this code is my two pennies worth contribution to making ISM available to those interested in
- looking under the hood of this technique
- reclaim the rigour of the original procedures, which is lost in the incumbent academic literature - I think; 

## Easter egg (-ish) for academic geeks
The code explores surprising, but unnoticed similarities with other well-known algorithms. Namely, the code includes a function for finding strongly connected components (SCC) by Depth-First Search, which is borrowed from my other repository Tarjan_SCC_in_R.  It also provides a block-triangular permutation of the corresponding adjacency matrix.
There's lots in common between ISM; finding SCC in a digraph; and working out a block triangular permutation of the adjacency matrix of said digraph. You might argue DFS can do that quite efficiently - although it was not quite as popular when the original ISM work came out, and hence never acknowledged. But don't tell the academics I mentioned above - I love that they don't have a clue of what on earth they're trying to do as they go through the moves of something reminiscent of ISM. Let them have a good sweat.
