#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom tibble tibble
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%>%"
#' @title Downloads details of a specific bill by providing type, number and year
#' @description Downloads details of a specific bill by providing type, number and year
#' @param type \code{character}. The type of the bill. For example, "PL"
#' @param number \code{integer}. The number of the bill
#' @param year \code{integer}. The year of the bill.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cham_bill_info(type = "PL", number = "3962", year = "2008")
#' @export


cham_bill_info <- function(type, number, year, ascii=T) {
  if ( is.null(type) | is.null(number) | is.null(year) ) {
    stop("Lacking arguments. type, number, and year are mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicao?tipo=" %p%
    type %p% "&numero=" %p% number %p% "&ano=" %p% year
  data <- read_xml(link) %>%
#    xml_find_all('proposicao') %>%
    extract_bill_info
  if ( ascii==T ) {
    data <- data %>%
      dplyr::mutate_if(is.character, function(x) stringi::stri_trans_general(x, "Latin-ASCII")
      )
  }
  return(data)

}

extract_bill_info <- function(bill) {
  return(
    dplyr::tibble(
      bill_type = xml_attr(xml_find_all(bill, "/proposicao"), "tipo"),
      bill_number = xml_attr(xml_find_all(bill, "/proposicao"), "numero"),
      bill_year = xml_attr(xml_find_all(bill, "/proposicao"), "ano"),
      bill_id = xml_text(xml_find_all(bill, "idProposicao")),
      bill_name = xml_text(xml_find_all(bill, "nomeProposicao")),
      type_name = xml_text(xml_find_all(bill, "tipoProposicao")),
      bill_subject = xml_text(xml_find_all(bill, "tema")),
      ementa_txt = xml_text(xml_find_all(bill, "Ementa")),
      ementa_explanation = xml_text(xml_find_all(bill, "ExplicacaoEmenta")),
      author_name = xml_text(xml_find_all(bill, "Autor")),
      author_id = xml_text(xml_find_all(bill, "ideCadastro")),
      author_state = xml_text(xml_find_all(bill, "ufAutor")),
      author_party = xml_text(xml_find_all(bill, "partidoAutor")),
      bill_index = xml_text(xml_find_all(bill, "Indexacao")),
      presentation_date = xml_text(xml_find_all(bill, "DataApresentacao")),
      complete_link = xml_text(xml_find_all(bill, "LinkInteiroTeor")),
      situation_desc = xml_text(xml_find_all(bill, "Situacao")),
      last_action_desc = xml_text(xml_find_all(bill, "UltimoDespacho")),
      last_action_date = xml_attr(xml_find_all(bill, "UltimoDespacho"), "Data")

    )
  )
}
