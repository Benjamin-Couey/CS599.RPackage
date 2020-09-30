# CS599.RPackage

<!-- badges: start -->
  [![Build Status](https://travis-ci.org/Benjamin-Couey/CS599.RPackage.svg?branch=master)](https://travis-ci.org/Benjamin-Couey/CS599.RPackage)
<!-- badges: end -->

This package was created for a project for CS599 Unsupervised Learning. It
provides a basic implementation of the K-means and hierarchical clustering
algorithms. These will help you gain insight into a data set by sorting the data
into a specific number of clusters.

Note, that while these algorithms work, they are more limited in options and
less efficient than implementations found in other packages such as stats.

# Installation

This package can be installed from GitHub with the following R command.

```
remotes::install_github("Benjamin-Couey/CS599.RPackage”)
```

# Usage

First, you need to import the installed package.

```
library("CS599.RPackage")
```

After that, you can make use of the KMEANS and HCLUST functions.
```
test.matrix <- as.matrix( iris[ 1:50, 1:4 ] )
kmeans.result <- KMEANS( test.matrix, 4 )
kmeans.result$cluster
kmeans.result$centers
```

```
test.matrix <- as.matrix( iris[ 1:50, 1:4 ] )
HCLUST( test.matrix, 4 )
```
