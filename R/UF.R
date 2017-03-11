#' @title A list of state abbreviations for use with the functions of congressbr.
#' @description This function prints out a character vector of Brazilian
#' state abbreviations to the console.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @note This is most useful if you already have an idea of what state you want
#'  and its abbreviation. If not, to see the full dataset, use
#'  \code{data("statesBR")}, which has the abbreviation, "UF"
#'  (\emph{Unidade Federal}) and the full name of the states.
#' @export
UF <- function(){
  uf <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
          "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
          "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
          "SP", "SE")
  print(uf)
}
