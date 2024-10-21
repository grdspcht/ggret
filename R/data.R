#' An example reticulation network in evonet format
#'
#' A small reticulation network for testing purposes.
#' @format ## `retnet_evonet`
#' Evolutionary network with 4 reticulations, the base tree has 20 tips and 27 internal nodes:
#' \describe{
#'   \item{edge}{Edge matrix that describes the base tree toplogy}
#'   \item{edge.length}{Edge lengths}
#'   \item{Nnode}{Number of internal nodes}
#'   \item{node.label}{Labels for internal nodes. Root node: "r"}
#'   \item{tip.label}{Labels for network tips. (A-D)}
#'   \item{root.edge}{edge identifier of root}
#'   \item{reticulation}{Edge matrix for reticulations}
#'   \item{ret.length}{Reticulation Lengths}
#'   ...
#' }
"retnet_evonet"

#' An example reticulation network in treedata format
#'
#' A small reticulation network for testing purposes with additional metadata
#' @format ## `retnet_treedata`
#' Evolutionary network with 4 reticulations, the base tree has 20 tips and 27 internal nodes:
#' \describe{
#'   \item{edge}{Edge matrix that describes the base tree toplogy}
#'   \item{edge.length}{Edge lengths}
#'   \item{Nnode}{Number of internal nodes}
#'   \item{node.label}{Labels for internal nodes.}
#'   \item{tip.label}{Labels for network tips. (taxon_1-taxon_20)}
#'   \item{root.edge}{edge identifier of root}
#'   \item{reticulation}{Edge matrix for reticulations}
#'   \item{ret.length}{Reticulation Lengths}
#'   \item{isTip}{Boolean, defines whether node is tip or not}
#'   \item{posterior}{Posterior probality for node position}
#'   \item{height_95_HPD}{Range of the 95% HPD}
#'   \item{conv}{Reticulation edge index}
#'   \item{region}{Position of the affected parts of the genome}
#'   \item{locus}{Locus name}
#'   \item{relSize}{Relative size}
#'   \item{startSite_95_HPD}{95% HPD of the reticulation start site}
#'   \item{endSite_95_HPD}{95% HPD of the reticulation end site}
#'   ...
#' }
"retnet_treedata"
