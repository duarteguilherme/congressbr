#' @title A list of state abbreviations for use with the functions of congressoBR.
#' @description This function prints out a character vector of Brazilian
#' state abbreviations to the console.
#' @author Robert Myles McDonnell & Guilherme Jardim Duarte.
#' @note This is most useful if you already have an idea of what state you want
#'  and its abbreviation. If not, to see the full dataset, use \code{data("statesBR")},
#'  which has the abbreviation, "UF" (\emph{Unidade Federal}) and the full name of the
#'  states.
#' @export
UF <- function(){
  data("statesBR")
  uf <- as.character(statesBR$UF)
  print(uf)
}
