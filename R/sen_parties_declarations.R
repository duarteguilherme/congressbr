#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map
#' @importFrom dplyr as_data_frame
#' @importFrom stringi stri_trans_general
#' @title Downloads and tidies information on the political parties in the
#' Federal Senate.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @param ascii \code{logical}. TRUE by default, removes latin-1 characters
#' from returned object.
#' @usage
#' sen_parties(ascii = TRUE)
#' @examples
#' \code{party <- sen_parties(); head(party)}
#' @export
sen_parties <- function(ascii = TRUE){
  x <- httr::GET("http://legis.senado.gov.br/dadosabertos/senador/partidos")
  x <- httr::content(x, "parsed")
  x <- x$ListaPartidos$Partidos$Partido
  x <- purrr::map(x, dplyr::as_data_frame)
  x <- do.call(rbind, x)
  if(ascii == TRUE){
    x$Nome <- stringi::stri_trans_general(x$Nome, "Latin-ASCII")
  }
  return(x)
}



#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies information on the types of declarations senators
#' can make in the Federal Senate.
#' @param return \code{logical}. If TRUE, returns a data frame (tibble).
#' @param ascii \code{logical}. TRUE by default, removes latin-1 characters
#' from returned object.
#' @return The data frame is printed to the console. If \code{return = TRUE} is
#' chosen, the data frame is printed to the console and also returned as an object.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @usage \code{sen_statement_list(return = TRUE, ascii = TRUE)}
#' @examples
#' st <- sen_statement_list(return = T)
#' @export
sen_statement_list <- function(return = FALSE, ascii = TRUE){
  x <- httr::GET("http://legis.senado.gov.br/dadosabertos/senador/lista/tiposPronunciamento")
  x <- httr::content(x, "parsed")
  x <- x$ListaTiposPronunciamento$TiposPronunciamento$TipoPronunciamento
  x <- purrr::map(x, dplyr::as_data_frame)
  x <- do.call(rbind, x)
  if(ascii == TRUE){
    x$Descricao <- stringi::stri_trans_general(x$Descricao, "Latin-ASCII")
  }
  head(x, n = 9L)
  if(return == TRUE){
    return(x)
  }
}
