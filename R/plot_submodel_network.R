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
#'   [`frscored_cna()`][frscore::frscored_cna()], and creates a network
#'   visualization of the submodel relations among the scored models using
#'   [`visNetwork()`][visNetwork::visNetwork()]. The nodes of the network
#'   represent unique model types, and an edge between two nodes represents a
#'   submodel relation between those model types. By default, the edges of the
#'   network are undirected. Setting \code{directed = TRUE} creates a directed
#'   network with edges pointing from submodels to supermodels. As it is only
#'   the presence of and not the direction of submodel relations that matters
#'   for the fr-score of a model type, the directed network provides no
#'   additional information about fr-scores over and above the undirected
#'   network. By default, the network color-codes clusters of model types based
#'   on edge-betweenness, calculated with
#'   [`cluster_edge_betweenness()`][igraph::cluster_edge_betweenness()] from the
#'   package `igraph`. The clusters are always based on undirected
#'   edge-betweenness to reflect the fact that only the presence of
#'   submodel-relations, not their direction, is relevant for fr-scores. The
#'   clusters can be turned off by setting \code{show_clusters = FALSE}. The
#'   network plot uses `igraph` layout by default, this can be changed to
#'   `visNetwork` default by setting \code{igraphlayout = FALSE}. The
#'   visualization can be customized by passing other `visNetwork()` arguments
#'   in `...`, and by using other functions from the `visNetwork` package (see
#'   examples). The purpose of `plot_submodel_network()` is to provide a
#'   convenient way of visualizing submodel relations calculated by the
#'   `frscore` functions, at the expense of limited flexibility. For further
#'   analysis of the submodel network and more visualization options,
#'   [´submodel_adjacencies_to_igraph()´][frscore::submodel_adjacencies_to_igraph()]
#'   produces an `igraph` graph of submodel relations from an adjacency matrix
#'   included in [`frscore()`][frscore::frscore()] and
#'   [`frscored_cna()`][frscore::frscored_cna()] output.
#'
#'
#'
#' @return A \code{visNetwork} object.
#'
#' @examples r <- frscored_cna(d.error)
#' plot_submodel_network(r)
#'
#' # customize or override general options
#' v <- visOptions(r, highlightNearest = T)
#' v
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
  vg$nodes$title <- as.character(vg$nodes$id)
  if(show_clusters){
    clust <- igraph::cluster_edge_betweenness(s_graph, directed = FALSE)
    vg$nodes$group <- as.character(clust$membership)
  }
  if(directed) vg$edges$arrows <- "from"
  vn <- do.call(visNetwork::visNetwork,
                c(list(nodes = vg$nodes, edges = vg$edges), list(...)))
  utitle <- as.character(vn$nodes$id)
  if (igraphlayout) {
    vn <- visNetwork::visIgraphLayout(vn, layout = "layout_with_fr")
    vn <- visNetwork::visOptions(vn,
                                 nodesIdSelection = T)
  }
  return(vn)
}

#' Generate igraph from submodel adjacencies.
#'
#' @param x An object of class \code{"frscore"} or \code{"frscored_cna"}.
#'
#' @return An \code{igraph} graph.
#' @export
#'
#' @details
#' [`frscore()`][frscore::frscore()] and [`frscored_cna()`][frscore::frscored_cna()]
#' output includes an adjacency matrix of submodel relations among
#' the scored model types. `submodel_adjacencies_to_igraph()` is a convenience
#' function that extracts the adjacency matrix and produces an `igraph` graph
#' from it.
#'
#'
#' @examples
#' r <- frscored_cna(d.error)
#' sg <- submodel_adjacencies_to_igraph(r)
#' sg
submodel_adjacencies_to_igraph <- function(x){
  stopifnot(inherits(x, c("frscored_cna", "frscore")))
  return(igraph::graph_from_adjacency_matrix(x$submodel_adjacencies))
}






