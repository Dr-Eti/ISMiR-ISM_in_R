# ISMiR Interpretive Structural Modelling (ISM) in R

## What is it?
This R code explores an old graph-partitioning algorithm. The algorithm's end-goald is a minimal-edge, hierarchical digraph which preserves the reachability properties of a given directed graph (digraph). This algorithm used to be a major part of Interpretive Structural Modelling (ISM). I am referring specficially the algorithms described in the work of J.N Warfield:
- Warfield (1976) Societal Systems ISBN 0-471-01569-5 esp. Ch.9 and 10
- Warfield (1974) 10.1109/TSMC.1974.4309336
- Warfield (1973) doi 10.1109/TSMC.1973.4309270

## What's the point?
I kid you now - but today the term ISM means something different. Over the past decades, academics have unilaterally decided to strip ISM off its most fundamental algorithm and algebraic devices. This lazy behaviour opened the door for that pile of steaming rubbish we call academic research in ISM today. Think I'm being too harsh? Pick any paper published in the last twenty-odd years and find out by yourself (hint: look for the mystical "conical" matrix - as in shaped as a cone - and you'll find out it's not a typo).

I dig deeper into this murky academic business in an academic paper of mine - currently unpublished but available as a preprint at http://dx.doi.org/10.2139/ssrn.3622934 . 
The code is meant for use as supporting materials for such paper.

## If this is old stuff, isn't there something else out there that does it?
There are not many options that I know of to implement ISM calculations transparently on a computer. The J.N. Warfield IP Trust kindly provides ISM software free of charge (see: https://www.jnwarfield.com/ism-software.html). Yet the software is not open source and I'm not sure which parts of the original algorithm are implemented - if any - and how.

Therefore, this code is my two pennies worth contribution to making ISM available to those interested in
- looking under the hood of this technique
- reclaim the rigour of the original procedures, which is lost in the incumbent academic literature (more about this in my paper); 
- explore surprising, but unnoticed similarities with other well-known algorithms e.g. strongly connected components in a digraph; block-triangular permutation of the corresponding adjacency matrix.

## Easter egg (-ish) for academic geeks
Re the last point above - this code includes a function for finding strongly connected components (SCC) by Depth-First Search, which is borrowed from the repository Tarjan_SCC_in_R. I'm not expecting you to get too excited to learn that there's lots in common between ISM and finding SCC by DFS. Tell the academics I mentioned above - who still work that stuff out in every ISM paper like it's 1973... and get published.
