dstruct
=======

Having moved onward to using `allocatable` nodes instead of `pointer` in the
AVL tree the code compiles with
 + `ifort` (2021.1 Beta 20201112)
but not anymore with either of
 + `gfortran` (10.2.0)
 + `nvfortran` (20.9-0)

Currently here can be found:
 + AVL tree, general removal is not yet supported, but for some special cases
   you can already remove keys. Tree traversal is not yet implemented
