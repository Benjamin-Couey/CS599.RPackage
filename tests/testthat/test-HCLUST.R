test_that("HCLUST returns a vector of N length with integers 1 to K", {
  N <- 50
  K <- 4
  range.of.features <- 1:4
  test.matrix <- as.matrix( iris[ 1:N, range.of.features ] )
  hclust.result <- HCLUST( test.matrix, K )

  expect_equal( length( hclust.result ), N)
  expect_equal( min( hclust.result ), 1 )
  expect_equal( max( hclust.result ), K )
  expect_equal( length( unique( hclust.result ) ), K )
})
