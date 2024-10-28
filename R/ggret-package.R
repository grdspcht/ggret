#' @keywords internal
#' @aliases ggret-pkg
"_PACKAGE"

## usethis namespace: start
#' @import ggtree
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_split_1
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom treeio read.beast
## usethis namespace: end
NULL

# defining global variables
# ugly solution to avoid magrittr NOTE
# see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(".")
