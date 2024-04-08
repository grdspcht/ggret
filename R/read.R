#' Read Network in Extended Newick Format
#'
#' @param file extended newick file
#' @param text character string
#'
#' @return evonet object
#' @export

read.enewick <- function(file = "", text = NULL)

{
  tr <- ape::read.tree(file = file, text = text)
  ret_index <- grep("#", tr$tip.label)
  rets <- match(ret_index, tr$edge[, 2])
  ret_len <- tr$edge.length[rets]

  evo <- ape::as.evonet.phylo(tr)
  evo$ret.length <- ret_len

  evo
}
