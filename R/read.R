#' Read an extended Newick file (via ape)
#'
#' @param file Path to an extended Newick file.
#'
#' @return ape evonet object
#' @export
#'
#' @examples
#' print("EXAMPLE HERE")
read.enewick <- function(file=""){
  if (grepl("#", readLines(file))){
    ape::read.evonet(file)
  }else{
  stop("No reticulation edges found.")
  }
}

