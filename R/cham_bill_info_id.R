#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom tibble tibble
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%>%"
#' @title Downloads details of a specific bill by providing id of a bill
#' @description Downloads details of a specific bill by providing id of a bill
#' @param bill_id \code{integer}. The id number of the bill.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cham_bill_info_id(14784)
#' @export

cham_bill_info_id <- function(bill_id, ascii=T) {
  if ( is.null(bill_id) ) {
    stop("Lacking arguments. bill_id is mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp=" %p%
    bill_id
  print(link)
  data <- read_xml(link) %>%
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
