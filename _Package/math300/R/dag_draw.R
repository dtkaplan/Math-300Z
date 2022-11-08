#' Draw a DAG
#'
#' Make a simple drawing of a DAG.
#'
#' @details See the igraph package for more details.
#'
#' @param DAG The DAG to draw
#' @param \ldots Additional arguments to plot.igraph()
#'
#' @examples
#' dag_draw(dag03)
#' @export
dag_draw <- function(DAG, ...) {
  ig <- dag_to_igraph(DAG)

  plot(ig, vertex.size=30, vertex.color="green", vertex.shape="none",
       vertex.label.cex=2, vertex.label.family="Courier", ...)
}


