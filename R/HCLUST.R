#' Perform basic heirarchical clustering on a data matrix using single linkage.
#'
#' This function computes its own distance matrix.
#'
#' @param data.matrix A numeric data matrix to perform clustering on.
#' @param K The number of clusters.
#'
#' @return A vector with as many elements as data.matrix has rows where each
#'           element indicate which cluster the row at that index in the
#'           data.matrix is assigned, with there being K different clusters
#'           represented in the vector.
#'
#' @examples
#' test.matrix <- as.matrix( iris[ 1:50, 1:4 ] )
#' HCLUST( test.matrix, 4 )
HCLUST <- function( data.matrix, K ){

  diss.matrix <- matrix( nrow=nrow( data.matrix ), ncol=nrow( data.matrix ) )

  # First, compute a dissimilarity matrix for data.matrix
  for( outer.i in 1:(nrow(data.matrix)) ) {
    # We can assume the distance between a row and itself is 0
    diss.matrix[ outer.i, outer.i ] <- 0

    if( outer.i < nrow(data.matrix) ) {

      for( inner.i in (outer.i+1):nrow(data.matrix) ) {
        # Calculate the Euclidian distance between the first vector and the second
        distance <- sqrt( sum( ( data.matrix[outer.i,] - data.matrix[inner.i,] )^2 ) )
        # Assign it to the positions in the matrix which are diagonally symmetrical
        diss.matrix[outer.i, inner.i] <- distance
        diss.matrix[inner.i, outer.i] <- distance
      }

    }

  }

  # Set the diagonal of the distance matrix to NA since we don't care about
  # comparing points to themselves.
  diag(diss.matrix)=NA

  # Initialize a list which will store the assignments of rows in the data.matrix
  # to clusters
  cluster.list <- as.list( 1:nrow(diss.matrix) )

  while( length(cluster.list) > K ){

    # Find the shortest distance in the distance matrix, ignoring NA entires
    # which are either the distance of a point to itself, or the distance of a
    # point to another point in the same cluster
    shortest <- NULL
    shortest <- which(diss.matrix == min(diss.matrix [ !is.na(diss.matrix) ]),
                      arr.ind = TRUE)

    to.keep.i <- shortest[1,1]
    to.remove.i <- shortest[1,2]

    # Recalculate the distance matrix

    # Take the min of the joined vectors in the data matrix
    # This minimum vector represents the distance to the new cluster using the
    # single linkage method
    min.vector <- pmin( diss.matrix[ to.keep.i, ], diss.matrix[ to.remove.i, ] )

    diss.matrix[ to.keep.i, ] <- min.vector
    diss.matrix[ , to.keep.i ] <- min.vector
    diss.matrix <- diss.matrix[ -to.remove.i, -to.remove.i ]

    # Update the cluster list to reflect that the two points are now clustered
    cluster.list[[ to.keep.i ]] <- c( cluster.list[[ to.keep.i ]],
                                      cluster.list[[ to.remove.i ]] )
    cluster.list <- cluster.list[ -to.remove.i ]
  }

  cluster.vector <- c()

  # Compute the return vector
  for( list.index in 1:length(cluster.list) ){
    cluster.vector[ cluster.list[[list.index]] ] <- list.index
  }

  return( cluster.vector )
}
