#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom magrittr '%>%'
#' @title This function lists every bill voted on in plenary.
#' @param year (\code{integer}) start year of the period requested.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cam_plenary_bills(year=2008)
#' @export
cam_plenary_bills <- function(type="", year) {
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


