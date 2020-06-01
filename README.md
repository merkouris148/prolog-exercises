# prolog-exercises
Some exercises in logic programming and constraint logic programming writen in [ECLiPSe Prolog](http://eclipseclp.org/). The exercises are divided in two packages. The first one (see Lp20_A.pdf)  is written in vanilla prolog, while the second one (see Lp20_B.pdf) uses the _constraint_ logic programming extension of ECLiPSe Prolog. 

## Contents
A detailed description of the first two exercises is included in the file **Lp20_A.pdf** (in Greek):

1. **listsoflist.pl**: A programme providing an implementation of some matrix operations. The matrices where encoded in Prolog as lists of lists (hence the name of the file).
2. **jobshop.pl**: A programme solving two versions of a task scheduling problem.
   1. In the first version we have the following constraints:  
      * Each task has a _duration_.
      * Each task can only be executed in a specific _machine type_.
      * We have _multiple instances_ of each machine type.
      * Each task must end before the _deadline_.  
   2. In the second version we also add the constraints:
      * Each task needs some _workers_.
      * There is a fixed number of _available workers_.

The description of the rest three exercises is included in the file **Lp20_B.pdf** (in Greek):

1. **vertexcover.pl**: A programme finding a [vertex cover](https://en.wikipedia.org/wiki/Vertex_cover) in a random undirected graph, using constraints. This programme uses **graph.pl** to generate random graphs. 
2. **stable.pl**: A programme finding a [stable match](https://en.wikipedia.org/wiki/Stable_marriage_problem), using the fd constraint library of ECLiPSe. Some example data are given in **stablefd_data.pl**.
3. **jobshop_opt.pl**: A programme solving the optimization variant of the second version of the homonymous task scheduling problem above. Some example data are given in **jobshop_opt_data.pl**.
