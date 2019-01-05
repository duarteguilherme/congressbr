#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom lubridate parse_date_time
#' @importFrom stringi stri_trans_general
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @title Downloads and tidies data on the coalitions in the Federal Senate
#' @description Downloads and tidies data on the coalitions in the Federal Senate.
#' @param ascii \code{logical}. If TRUE, names are converted to ascii format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame},
#' with variables:
#' \itemize{
#'  \item{\code{bloc_code: }}{unique code given to each coalition.}
#'  \item{\code{bloc_name: }}{name of the coalition.}
#'  \item{\code{bloc_label: }}{additional label for the coalition.}
#'  \item{\code{date_created: }}{\code{POSIXct}, date the coalition was created.}
#'  }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' coalitions <- sen_coalitions()
#' @export
sen_coalitions <- function(ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/blocoParlamentar/lista"

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaBlocoParlamentar$Blocos$Bloco
  N <- NA_character_

  bloc <- tibble::tibble(
    bloc_code = purrr::map_chr(request, "CodigoBloco", .null = N),
    bloc_name = purrr::map_chr(request, "NomeBloco", .null = N),
    bloc_label = purrr::map_chr(request, "NomeApelido", .null = N),
    date_created = suppressWarnings(
      lubridate::parse_date_time(
      purrr::map_chr(request, "DataCriacao", .null = N),
      orders = "Ymd"))
    )

  if(isTRUE(ascii)){
    bloc <- bloc %>%
      dplyr::mutate(
        bloc_name = stringi::stri_trans_general(
          bloc_name, "Latin-ASCII"),
        bloc_label = stringi::stri_trans_general(
          bloc_label, "Latin-ASCII")
      )
  }

  return(bloc)
}
