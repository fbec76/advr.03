# 1.1.2

#' Validate inputs for the dijkstra function
#'
#' @param graph Graph to be traversed
#' @param init_node Initial node to start from
#' @return error message in case input is not valid, empty string otherwise
validate_input_dijkstra <- function(graph, init_node) {
  if (!is.data.frame(graph)) {
    return("'graph' must be a data.frame.")
  }
  if (ncol(graph) != 3) {
    return(sprintf("'graph' must have 3 columns, but has %d.", ncol(graph)))
  }
  if (length(setdiff(c("v1", "v2", "w"), colnames(graph))) > 0) {
    return("'graph' is missing columns.")
  }
  if (!is.atomic(graph$v1)) {
    return("'v1' column must be atomic.")
  }
  if (!is.atomic(graph$v2)) {
    return("'v2' column must be atomic.")
  }
  if (!is.numeric(graph$w)) {
    return("'w' column must be numeric.")
  }
  if (any(graph$w < 0)) {
    return("'w' must be non-negative.")
  }
  if (!is.atomic(init_node) || length(init_node) != 1) {
    return("Input 'init_node' must be an atomic scalar value.")
  }
  if (!(init_node %in% unique(c(graph$v1, graph$v2)))) {
    return(sprintf("'init_node' (%s) does not exist.",
                   as.character(init_node)))
  }
  ""
}

#' Implementation of the Dijkstra's algorithm
#' to find the shortest path between nodes
#' Wikipedia: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#'
#' @importFrom stats setNames
#'
#' @param graph Graph to be traversed
#' @param init_node Initial node to start from
#' @return numeric vector of shortest distances to nodes in given graph
#' @export
dijkstra <- function(graph, init_node) {
  validation_msg <- validate_input_dijkstra(graph, init_node)
  if (nchar(validation_msg) > 0) {
    stop(validation_msg)
  }
  unique_nodes <- unique(c(graph$v1, graph$v2))
  distances <- setNames(rep(Inf, length(unique_nodes)), unique_nodes)
  distances[init_node] <- 0
  visited_nodes <- setNames(rep(FALSE, length(unique_nodes)), unique_nodes)
  while (any(!visited_nodes)) {
    unvisited <- names(visited_nodes)[!visited_nodes]
    unvisited_distances <- distances[unvisited]
    if (min(unvisited_distances) == Inf) {
      break
    }
    curr_node <- unvisited[which.min(unvisited_distances)[1]]
    neighbors_idxs <- which(graph$v1 == curr_node)
    for (n in neighbors_idxs) {
      neighbor <- graph$v2[n]
      new_dist <- distances[curr_node] + graph$w[n]
      if (new_dist < distances[neighbor]) {
        distances[neighbor] <- new_dist
      }
    }
    visited_nodes[curr_node] <- TRUE
  }
  as.numeric(distances)
}