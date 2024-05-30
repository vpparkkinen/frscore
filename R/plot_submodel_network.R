#' Visualize submodel networks
#'
#' @description Visualize the output of `frscore()` and `frscored_cna()` by
#'   generating a network representation of submodel relations among the scored
#'   model types.
#'
#'
#' @param x An object of class \code{"frscore"} or \code{"frscored_cna"}.
#' @param show_clusters Logical; should clusters be visualized?
#' @param directed Logical; should submodel relations be represented as
#'   directional?
#' @param igraphlayout Logical; should igraph layout be used?
#' @param ... additional arguments to \code{visNetwork()}.
#'
#' @details `plot_submodel_network()` takes as input a results object returned
#'   by [`frscore()`][frscore::frscore()] or
#'   [`frscored_cna()`][frscore::frscored_cna()] and uses the included adjacency
#'   matrix of submodel relations among the scored model types to visualize
#'   those submodel relations as a network, using
#'   [`visNetwork()`][visNetwork::visNetwork()]. The nodes of the network
#'   represent unique model types, and an edge between two nodes represents a
#'   submodel relation between those model types. By default, the edges of the
#'   network are undirected. Setting \code{directed = TRUE} produces a network
#'   with directed edges (arrows), where the direction points from a submodel to
#'   a supermodel. As the fr-scores of the model types depend on sub- *and*
#'   supermodel relations they have to other models, the directed network
#'   representation provides no additional information about fr-scores over and
#'   above the undirected network. By default, the network plot represents
#'   clusters of model types based on edge-betweenness, calculated with
#'   [`cluster_edge_betweenness()`][igraph::cluster_edge_betweenness()] from the
#'   `igraph` package. The clusters are always base on undirected
#'   edge-betweenness, as only the presence of submodel-relations, not their
#'   direction, is relevant for fr-scores. The clusters can be turned off
#'   by setting \code{show_clusters = FALSE}.
#'
#'
#'
#' @return A \code{visNetwork} object.
#'
#' @examples
#'
#'
#' @export
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

#' Generate igraph from submodel adjacencies
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






