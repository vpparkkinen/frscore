#' plot_submodel_network
#'
#' @param x An object of class \code{"frscore"} or \code{"frscored_cna"}.
#' @param show_clusters Logical; should clusters be visualized?
#' @param directed Logical; should submodel relations be represented as
#'   directional?
#' @param igraphlayout Logical; should igraph layout be used?
#' @param ... additional arguments to \code{visNetwork()}.
#'
#' @return A \code{visNetwork} object.
#' @export
#'
#' @examples
plot_submodel_network <- function(x,
                             show_clusters = TRUE,
                             directed = FALSE,
                             igraphlayout = TRUE,
                             ...
                             ){
  stopifnot(inherits(x, c("frscored_cna", "frscore")))
  adj <- x$submodel_adjacencies
  gmode <- if(directed) "directed" else "max"
  s_graph <- igraph::graph_from_adjacency_matrix(adj,mode = gmode)
  vg <- visNetwork::toVisNetworkData(s_graph)
  if(show_clusters){
    clust <- igraph::cluster_edge_betweenness(s_graph, directed = FALSE)
    vg$nodes$group <- as.character(clust$membership)
  }
  if(directed) vg$edges$arrows <- "from"
  #vn <- visNetwork(nodes = vg$nodes, edges = vg$edges)
  vn <- do.call(visNetwork::visNetwork,
                c(list(nodes = vg$nodes, edges = vg$edges), list(...)))
  if (igraphlayout) vn <- visIgraphLayout(vn)
  return(vn)
}

#' Title
#'
#' @param x An object of class \code{"frscore"} or \code{"frscored_cna"}.
#'
#' @return An \code{igraph} graph.
#' @export
#'
#' @examples
submodel_adjacencies_to_igraph <- function(x){
  stopifnot(inherits(x, c("frscored_cna", "frscore")))
  return(graph_from_adjacency_matrix(x$submodel_adjacencies))
}






