#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom dplyr as_data_frame
#' @importFrom magrittr '%>%'
#' @importFrom stringi stri_trans_general
#' @importFrom purrr map_chr
#' @title Downloads and tidies budget information from the Federal Senate
#' @description Downloads and tidies budget information from the Federal Senate.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{
#' bud <- sen_budget()
#' }
#' @export
sen_budget <- function(ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/orcamento/lista"

  request <- httr::GET(base_url)
  # status checks
  request <- status(request)

  request <- request$ListaLoteEmendas$LotesEmendasOrcamento$LoteEmendasOrcamento

  req <- dplyr::data_frame(
    budget_sponsor = purrr::map_chr(request, "NomeAutorOrcamento"),
    active = purrr::map_chr(request, "IndicadorAtivo"),
    budget_date = lubridate::parse_date_time(
      purrr::map_chr(request, "DataOperacao"), orders = "Ymd"),
    quantity_amendments = purrr::map_chr(request, "QuantidadeEmendas"),
    year_execution = purrr::map_chr(request, "AnoExecucao"),
    number = purrr::map_chr(request, "NumeroMateria"),
    budget_type_abbr = purrr::map_chr(request, "SiglaTipoPlOrcamento"),
    budget_type_description = purrr::map_chr(request, "DescricaoTipoPlOrcamento")
  )
  req <- req %>%
    dplyr::mutate(active = ifelse(active == "Sim", "Yes", "No"))

  if(!isTRUE(ascii)){
    return(req)
  } else{
    req <- req %>%
      dplyr::mutate(
        budget_sponsor = stringi::stri_trans_general(
          budget_sponsor, "Latin-ASCII"),
        budget_type_description = stringi::stri_trans_general(
          budget_type_description, "Latin-ASCII")
        )
    return(req)
  }
}
