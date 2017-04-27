#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attr
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @title This function lists every bill voted on in plenary.
#' @description This function lists every bill voted on in plenary.
#' @param year (\code{integer}) start year of the period requested.
#' @param type \code{character}. The type of the bill. For example, "PL"
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cham_plenary_bills(year = 2008)
#' @export
cham_plenary_bills <- function(year, type="", ascii=T) {
  "This function lists every bill voted on in plenary."
  if ( is.null(year) )  {
    stop("Lacking arguments. year is mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoesVotadasEmPlenario?" %p%
    "ano=" %p% year %p%
    "&tipo=" %p% type
  data <- read_xml(link) %>%
    xml_find_all('proposicao') %>%
    map_df(extract_plenary_bill)
  if ( ascii==T ) {
    data <- data %>%
      dplyr::mutate_if(is.character, function(x) stringi::stri_trans_general(x, "Latin-ASCII")
      )
  }

  return(data)
}


extract_plenary_bill <- function(bill) {
  return(
    dplyr::tibble(
      bill_id = xml_text(xml_find_all(bill, "./codProposicao")),
      bill_name = xml_text(xml_find_all(bill, "./nomeProposicao")),
      vote_date = xml_text(xml_find_all(bill, "./dataVotacao"))
    )
  )
}


