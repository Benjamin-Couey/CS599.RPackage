test_that("KMEANS returns a vector of N length with integers 1 to K", {
  N <- 50
  K <- 4
  range.of.features <- 1:4
  test.matrix <- as.matrix( iris[ 1:N, range.of.features ] )
  kmeans.result <- KMEANS( test.matrix, K )
  resulting.clustering <- kmeans.result$cluster

  expect_equal( length( resulting.clustering ), N)
  expect_equal( min( resulting.clustering ), 1 )
  expect_equal( max( resulting.clustering ), K )
  expect_equal( length( unique( resulting.clustering ) ), K )
})
