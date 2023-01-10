#' Read an extended Newick file
#'
#' @param file Path to an extended Newick file.
#'
#' @return ape evonet object
#' @export
#'
#' @examples
#' print("EXAMPLE HERE")
read.enewick <- function(file=""){
  ape::read.evonet(file)
}

