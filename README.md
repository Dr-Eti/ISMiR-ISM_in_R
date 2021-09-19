# ISMiR-ISM_in_R
A graph-partitioning algorithm that results in a minimal-edge, hierarchical digraph which preserves the reachability properties of the given initial digraph. This algorithm used to be a major part of Interpretive Structural Modelling (ISM). I am referring to the algorithms described in the work of J.N Warfield, specifically
- Warfield (1976) Societal Systems ISBN 0-471-01569-5 esp. Ch.9 and 10
- Warfield (1974) 10.1109/TSMC.1974.4309336
- Warfield (1973) doi 10.1109/TSMC.1973.4309270

Today the term ISM means something different - thanks to academics who decided to strip ISM off its most fundamental algorithm and algebraic devices, opening the door for that pile of rubbish we call academic research in ISM today. Think I'm being too harsh? Pick any paper published in the last twenty-odd years and find out by yourself (hint: look for the mystical "conical" matrix - as in shaped as a cone - and you'll find out it's not a typo).

The code is meant for use as supporting materials for an academic paper of mine - currently unpublished but available as a preprint at http://dx.doi.org/10.2139/ssrn.3622934 

There are not many options that I know of to implement ISM calculations transparently on a computer. Luckily the J.N. Warfield IP Trust kindly provides ISM software free of charge (see: https://www.jnwarfield.com/ism-software.html). Yet the software is not open source and I'm not sure which parts of the original algorithm are implemented, and how.

This code is my two pennies worth contribution to making ISM available to those interested in
- looking under the hood of this technique
- reclaim the rigour of the original procedures, which is lost in the incumbent academic literature (more about this in my paper); 
- explore surprising, but unnoticed similarities with other well-known algorithms e.g. strongly connected components in a digraph; block-triangular permutation of the corresponding adjacency matrix.

Re the last point above - this code includes a function for finding strongly connected components by Depth-First Search, which is borrowed from the repository Tarjan_SCC_in_R
