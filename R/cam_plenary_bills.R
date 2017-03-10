#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom magrittr '%>%'
#' @title This function lists every bill voted on in plenary.
#' @param initial_data (\code{character}) start date of the period requested.
#' This parameter must be in the format YYYYMMDD (Year-Month-Day). A value for
#' this parameter is necessary, all others are optional.
#' @param end_data (\code{character}) final date for period requested. Format
#' YYYYMMDD.
#' @param house (\code{character}). The acronym for the legislative house
#' for which results are requested. Options are SF (\emph{Senado Federal}, Federal Senate)
#' and CN (\emph{Congresso Nacional}, National Congress - joint meeting of
#' the Senate and Chamber).
#' @param colegiado To Do
#' @param legislator To Do
#' @param details (\code{logical}). If details is equal to TRUE, the data returned
#' is an expanded dataset with additional details. This is not recommended unless
#' necessary.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cam_plenary_bills(year=2008)



cam_plrenary_bills <- function(type="", year) {
  "This function lists every bill voted on in plenary."
  if ( is.null(year) )  {
    stop("Lacking arguments. year is mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoesVotadasEmPlenario?" %p%
    "ano=" %p% year %p%
    "&tipo=" %p% tipo
  data <- read_xml(link) %>%
    xml_find_all('proposicao') %>%
    map_df(extract_plenary_bill)
  return(data)
}


extract_plenary_bill <- function(bill) {
  return(
    dplyr::tibble(
      id_bill = xml_text(xml_find_all(bill, "./codProposicao")),
      name_bill = xml_text(xml_find_all(bill, "./nomeProposicao")),
      date_voted = xml_text(xml_find_all(bill, "./dataProposicao"))
    )
  )
}


