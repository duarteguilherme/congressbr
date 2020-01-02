#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom purrr map_chr
#' @title Downloads list of authors of a specific bill by providing id of a bill
#' @description Downloads list of authors of a specific bill by providing id of a bill
#' @param bill_id \code{integer}. The id number of the bill.
#' @return A vector of classes \code{character}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{
#' cham_bill_list_authors(14784)
#' }
#' @export
cham_bill_list_authors <- function(bill_id) {
  paste0("http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarAutoresProposicao?codProposicao=", bill_id) %>%
    xml2::read_xml() %>%
    xml2::xml_find_all('autor') %>%
    purrr::map_chr(xml2::xml_text)
}
