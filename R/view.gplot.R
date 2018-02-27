#' view.gplot
#' @keywords internal
view.gplot <- function(g, n.row, n.col){
  print(g, vp = viewport(layout.pos.row = n.row, layout.pos.col = n.col))
}
