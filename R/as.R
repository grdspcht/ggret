#' Evonet object to treedata
#'
#' @description
#' A method for converting evonet objects to treedata objects. Please note:
#' We are currently changing class information to c("phylo", "evonet")
#' even though the other way around is *correct*. This is because the S4 def.
#' of treedata does not allow for evonet objects in the phylo slot. At some
#' point we will resolve this inconsistency.
#'
#' @param evo An evonet object.
#' @param ... Unused, for extensibility.
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom tidytree treedata
#' @importFrom tidytree as.treedata
#' @return A treedata object with evonet object in the phylo slot.
#' @export
#' @method as.treedata evonet
#' @examples

as.treedata.evonet <- function(evo, ...){
  # TODO: We are currently changing class information to c("phylo", "evonet")
  # even though the other way around is *correct*. This is because the S4 def.
  # of treedata does not allow for evonet objects in the phylo slot. At some
  # point we will have to do something about this inconsistency (new class?)

  class(evo) <- c("phylo", "evonet")
  trd <- as.treedata(evo, ...)

  return(trd)
}

#' Treedata object to evonet
#'
#' @param trd A treedata object that contains a phylo slot in evonet format
#'
#' @return An evonet object
#' @export
#' @method as.evonet treedata
#' @examples
as.evonet.treedata <- function(trd){
  evo <- trd@phylo
  class(evo) <- c("evonet", "phylo")

  return(evo)

}
