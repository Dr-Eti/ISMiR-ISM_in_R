# ISMiR-ISM_in_R
Interpretive Structural Modelling (ISM) from scratch. 

This code is based on my understanding of the algorithms described in the work of J.N Warfield, specifically
- Warfield (1976) Societal Systems ISBN 0-471-01569-5 esp. Ch.9 and 10
- Warfield (1974) 10.1109/TSMC.1974.4309336
- Warfield (1973) doi 10.1109/TSMC.1973.4309270

The code is meant for use as supporting materials for an academic paper of mine - currently unpublished but available as a preprint at http://dx.doi.org/10.2139/ssrn.3622934 

There are few options to implement the computational structure of ISM. Luckily the J.N. Warfield IP trust is kind enough to provide ISM software free of charge (see: https://www.jnwarfield.com/ism-software.html). Yet the software is not open source.

This code is my two pennies worth contribution to making ISM available to those interested in
- looking under the hood of this technique
- reclaim the rigour of the original procedures, which is lost in the incumbent academic literature (more about this in my paper); 
- explore surprising, but unnoticed similarities with other well-known algorithms e.g. strongly connected components in a digraph; block-triangular permutation of the corresponding adjacency matrix.

Re the last point above - this code includes a function for finding strongly connected components by Depth-First Search, which is borrowed from the repository Tarjan_SCC_in_R
