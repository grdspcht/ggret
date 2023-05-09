#' Read an extended Newick file (via ape)
#'
#' @param file Path to an extended Newick file.
#'
#' @return ape evonet object
#' @export
#'
#' @examples
#' print("EXAMPLE HERE")


read.enewick2 <- function(file = "")
{
  tr <- ape::read.tree(file = file)
  ret_index <- grep("#", tr$tip.label)
  rets <- match(ret_index, tr$edge[, 2])
  ret_len <- tr$edge.length[rets]

  evo <- ape::as.evonet.phylo(tr)
  evo$ret.length <- ret_len

  evo
}
