#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom stringi stri_trans_general
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @importFrom magrittr "%>%"
#' @title Downloads and tidies data on the bill sponsors in the Federal Senate
#' @description Downloads and tidies data on the bill sponsors in the Federal Senate.
#' @param ascii (\code{logical}). If TRUE, bill sponsor names are converted
#' to ascii format, stripping the latin characters from the names.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame},
#' with variables:
#' \itemize{
#'  \item{\code{sponsor_name: }}{identity of the bill sponsor.}
#'  \item{\code{sponsor_code: }}{code for senator bill sponsors.}
#'  \item{\code{sponsor_title: }}{title of bill sponsor.}
#'  \item{\code{sponsor_party: }}{party of bill sponsor.}
#'  \item{\code{sponsor_state: }}{state of bill sponsor.}
#'  \item{\code{quantity: }}{quantity of bills sponsored by this individual or entity.}
#' }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{
#' bills <- sen_bill_sponsors()
#' }
#' @export
sen_bill_sponsors <- function(ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/autor/lista/atual"

  request <- httr::GET(base_url)
  request <- status(request)

  request <- request$ListaAutores$Totais$Parlamentares
  N <- NA_character_

  result <- tibble::tibble(
    sponsor_name = purrr::map_chr(request, "NomeAutor", .null = N),
    sponsor_id = purrr::map_chr(request, "CodigoParlamentar", .null = N),
    sponsor_title = purrr::map_chr(request, "Tratamento", .null = N),
    sponsor_party = purrr::map_chr(request, "Partido", .null = N),
    sponsor_state = purrr::map_chr(request, "Uf", .null = N),
    quantity = as.numeric(purrr::map_chr(request, "Quantidade"))
  )


  if(isTRUE(ascii)){
    result <- result %>%
      dplyr::mutate(
        sponsor_name = stringi::stri_trans_general(sponsor_name,
                                                   "Latin-ASCII")
      )
  }

  result <- result %>%
    dplyr::mutate(sponsor_party = ifelse(sponsor_party == "S/Partido",
                                          "Independent", sponsor_party)) %>%
    dplyr::filter(!is.na(sponsor_name))

    return(result)
}



#' @title Downloads and tidies data on the types of bill sponsors in the
#' Federal Senate
#' @description Downloads and tidies data on the types of bill sponsors in the
#' Federal Senate.
#' @param ascii (\code{logical}). If TRUE, bill sponsor names are converted
#' to ascii format, stripping the latin characters from the names.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame},
#' with variables:
#' \itemize{
#'  \item{\code{sponsor_abbr: }}{Abbreviation of sponsor type name.}
#'  \item{\code{sponsor_name: }}{Sponsor type name.}
#' }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' types <- sen_sponsor_types()
#' @export
sen_sponsor_types <- function(ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/autor/tiposAutor"

  request <- httr::GET(base_url)
  request <- status(request)

  request <- request$ListaTiposAutor$TiposAutor$TipoAutor

  req <- tibble::tibble(
    sponsor_abbr = purrr::map_chr(request, "SiglaTipo",
                                 .null = NA_character_),
    sponsor_name = purrr::map_chr(request, "Descricao",
                                  .null = NA_character_)
  )

  if(isTRUE(ascii)){
    req <- req %>%
      dplyr::mutate(
        sponsor_abbr = stringi::stri_trans_general(sponsor_abbr,
                                                   "Latin-ASCII"),
        sponsor_name = stringi::stri_trans_general(sponsor_name,
                                                   "Latin-ASCII")
      )
  }
  return(req)
}
