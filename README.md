# GeomAlgLib

This project contains algorithms and datastructures for some geometric problems.

The code was written in 1998 for a 
[diploma thesis](https://jdinkla.github.io/software-development/1998/10/19/geometric-algorithms-in-haskell.html).

Since 2016 [stack](https://docs.haskellstack.org) is used as a build tool.

The library contains:

- datastructures for points, lines, segments, rays, polygons and functions like calculations of angles, area, etc.
- datastructures for efficient orthogonal range queries: kd-trees and rangetrees
- algorithms for two dimensional convex hulls: Jarvis March, Grahams Scan, Merge-Hull, the algorithm of Kirkpatrick and Seidel and Chan's algorithmus
- algorithms for the triangulation of simple polygons: two simple but inefficient algorithms, the "standard"-algorithm of Garey, Johnson, Preparata and Tarjan and two output-sensitive algorithms of Toussaint
- the quad-edge data structure (QEDS) for planar subdivisions and polyhedra
- algorithms for Delaunay-triangulations and Voronoi-diagrams in two dimensions: the divide & conquer-algorithms of Stolfi and Guibas, which uses the QEDS and a randomised incremental algorithms by Boissonnat and Teillaud, which uses the delaunay-dag.
- and some applications in two dimensions, like all-next-neighbors, the next post office, the largest empty circle and the closest pair in any dimension.

## Building

You can build it with [stack](https://docs.haskellstack.org).

```bash
$ stack build
```

The following programs are created:

- glConvexHulls
- rangeQueries
- voronoiDelaunay
- convexHulls
- nearest
- triangulations

You can execute them with

```bash
$ stack NAME
```

For example

```bash
$ stack rangeQueries
```

## Thanks

Thanks to [Gauthier Segay](https://github.com/smoothdeveloper) for his 
cabal integration and [Schell Carl Scivally](https://github.com/schell) for 
bug fixes.
