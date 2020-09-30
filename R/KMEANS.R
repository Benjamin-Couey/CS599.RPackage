#' Perform basic k-means clustering on a data matrix
#'
#' @param data.matrix A numeric data matrix to perform clustering on.
#' @param K The number of clusters.
#'
#' @return A list which has the following elements:
#'             cluster: A vector of integers (from 1 to k) indicating the cluster
#'                      to which each row of the data matrix is associated.
#'             centers: A matrix of K cluster centers.
#'             withinss: A vector of the within-cluster sum squares for each
#'                       cluster.
#'             tot.withinss: the sum of withinss for all clusters.
#'             size: A vector of integers indicating the number of points
#'                   associated with each cluster.
#'             iter: The number of outer iterations the function took to converge.
#'
#' @export
#'
#' @examples
#' test.matrix <- as.matrix( iris[ 1:50, 1:4 ] )
#' kmeans.result <- KMEANS( test.matrix, 4 )
#' kmeans.result$cluster
#' kmeans.result$centers
KMEANS <- function( data.matrix, K ){

  # Initialize K clustering centers
  centers <- matrix( nrow=K, ncol=ncol( data.matrix ) )

  centers.index <- 1
  # Randomly select K rows from the data.matrix to be the starting clusters
  for( sample.index in sample( x=c( 1:nrow( data.matrix ) ), size=K ) ) {
    centers[ centers.index, ] <- data.matrix[ sample.index, ]
    centers.index <- centers.index + 1
  }

  # Initialize cluster vector
  # This will store the index of the clustering center each row in the matrix
  # is associated with
  clusters <- c()

  # Initialize outer loop iteration counter
  iter <- 0

  moved.center <- TRUE

  while( moved.center ) {
    iter <- iter + 1
    moved.center <- FALSE

    # Initialize a clear association list
    # This will store matrices of all of the vectors associated with a clustering center
    associations <- rep( list(NULL), K)

    # For each data point in the data.matrix
    for( row.i in 1:nrow( data.matrix ) ) {
      # Find the closest clustering center to the point
      nearest.center <- 0
      distance <- -1

      for( center.i in 1:K ){
        temp.dist <- dist( rbind( centers[center.i, ], data.matrix[ row.i, ] ),
                           method="euclidean" )
        if( distance == -1 || temp.dist < distance ){
          nearest.center <- center.i
          distance <- temp.dist
        }
      }

      # Assign that point to that clustering center
      clusters[row.i] <- nearest.center

      # Add that point to the association matrix
      if( is.null(associations[[nearest.center]]) ) {
        associations[[nearest.center]] <-
          matrix( data.matrix[ row.i, ], ncol=ncol( data.matrix ) )
      } else {
        associations[[nearest.center]] <-
          rbind( associations[[nearest.center]], data.matrix[ row.i, ] )
      }
    }


    # Calculate the mean of all points in each cluster
    for( center.i in 1:K ){
      # If this mean is the same as the coordinates of the cluster's center,
      # we're done
      # Otherwise, move the cluster's center to the mean and repeat
      cluster.mean <- colMeans( associations[[center.i]] )
      if( !all( cluster.mean == centers[center.i, ] ) ){
        centers[center.i, ] <- cluster.mean
        moved.center <- TRUE
      }
    }
  }

  # Calculate the return data

  # Calculate within point scatter (withinss)
  withinss <- c()
  tot.withinss <- 0

  for( center.i in 1:K ){

    center.withinss.total <- 0

    for( row.i in 1:nrow( associations[[center.i]] ) ){
      center.withinss.total <-
        center.withinss.total + ( dist( rbind( centers[center.i, ], associations[[center.i]][ row.i, ] ),
                                        method="euclidean" ) )^2
    }

    withinss[[center.i]] <- center.withinss.total
    # Sum to find total within point scatter (tot.withinss)
    tot.withinss <- tot.withinss + center.withinss.total
  }

  # Size, the number of points in each cluster, will just be the number of rows in
  # the association matrix for each center
  size <- lapply( associations, nrow )

  return.list <- list( cluster=clusters, centers=centers, withinss=withinss,
                       tot.withinss=tot.withinss, size=size, iter=iter)
  return( return.list )
}
