library(testthat)

test_that("dijkstra computes correct shortest paths (Wikipedia example)", {
  wiki_graph <- data.frame(
    v1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6),
    v2 = c(2, 3, 6, 1, 3, 4, 1, 2, 4, 6, 2, 3, 5, 4, 6, 1, 3, 5),
    w = c(7, 9, 14, 7, 10, 15, 9, 10, 11, 2, 15, 11, 6, 6, 9, 14, 2, 9)
  )
  res_1 <- dijkstra(wiki_graph, 1)
  expect_equal(res_1, c(0, 7, 9, 20, 20, 11))
  res_3 <- dijkstra(wiki_graph, 3)
  expect_equal(res_3, c(9, 10, 0, 11, 11, 2))
})

test_that("works for string-labeled graphs", {
  graph <- data.frame(
    v1 = c("A", "A", "B", "C"),
    v2 = c("B", "C", "C", "A"),
    w = c(1, 4, 2, 7),
    stringsAsFactors = FALSE
  )
  res <- dijkstra(graph, "A")
  expect_equal(res, c(0, 1, 3))
})

test_that("handles disconnected graphs", {
  graph <- data.frame(
    v1 = c(1, 2),
    v2 = c(2, 3),
    w = c(1, 1)
  )
  res <- dijkstra(graph, 1)
  expect_equal(res, c(0, 1, 2))
  expect_equal(dijkstra(graph, 3), c(Inf, Inf, 0))
})

test_that("throws error if graph is not a data.frame", {
  expect_error(dijkstra(matrix(), 1), "must be a data.frame")
})

test_that("throws error if graph has incorrect columns", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_error(dijkstra(df[, 1:2], 1), "must have 3 columns")
  colnames(df) <- c("v1", "v2", "X")
  expect_error(dijkstra(df, 1), "missing columns")
})

test_that("throws error if weights are negative", {
  g <- data.frame(v1 = 1, v2 = 2, w = -1)
  expect_error(dijkstra(g, 1), "non-negative")
})

test_that("throws error for non-scalar init_node", {
  g <- data.frame(v1 = 1:2, v2 = 2:3, w = 1:2)
  expect_error(dijkstra(g, c(1, 2)), "must be an atomic scalar")
})

test_that("throws error for absent init_node", {
  g <- data.frame(v1 = 1:2, v2 = 2:3, w = 1:2)
  expect_error(dijkstra(g, 10), "does not exist")
})

test_that("throws error for wrong types in v1/v2/w", {
  g <- data.frame(v1 = I(list(1, 2)), v2 = I(list(2, 3)), w = c(1, 2))
  expect_error(dijkstra(g, 1), "must be atomic")
  g2 <- data.frame(v1 = 1:2, v2 = 2:3, w = c("a", "b"))
  expect_error(dijkstra(g2, 1), "must be numeric")
})