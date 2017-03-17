#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom tibble tibble
<<<<<<< HEAD
#' @importFrom magrittr '%>%'
=======
#' @importFrom magrittr "%>%"
>>>>>>> e191eb4ef7e5cb66b9bfd686e2c419036cec0161
#' @title Downloads details of a specific bill by providing id of a bill
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cam_bill_info(type="PL", number="3962", year="2008")
#' @export

cam_bill_info <- function(id_bill) {
  if ( is.null(id_bill) ) {
    stop("Lacking arguments. id_bill is mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp=354258" %p%
    id_bill %>%
    extract_bill_info
  return(data)

}

extract_bill_info <- function(bill) {
  return(
    dplyr::tibble(
      type_bill = xml_attr(xml_find_all(bill, "/proposicao"), "tipo"),
      number_bill = xml_attr(xml_find_all(bill, "/proposicao"), "numero"),
      year_bill = xml_attr(xml_find_all(bill, "/proposicao"), "ano"),
      id_bill = xml_text(xml_find_all(bill, "idProposicao")),
      name_bill = xml_text(xml_find_all(bill, "nomeProposicao")),
      name_type = xml_text(xml_find_all(bill, "tipoProposicao")),
      subject_bill = xml_text(xml_find_all(bill, "tema")),
      txt_ementa = xml_text(xml_find_all(bill, "Ementa")),
      explanation_ementa = xml_text(xml_find_all(bill, "ExplicacaoEmenta")),
      name_author = xml_text(xml_find_all(bill, "Autor")),
      id_author = xml_text(xml_find_all(bill, "ideCadastro")),
      abrstate_author = xml_text(xml_find_all(bill, "ufAutor")),
      party_author = xml_text(xml_find_all(bill, "partidoAutor")),
      index_bill = xml_text(xml_find_all(bill, "Indexacao")),
      date_presentation = xml_text(xml_find_all(bill, "DataApresentacao")),
      link_complete = xml_text(xml_find_all(bill, "LinkInteiroTeor")),
      desc_situation = xml_text(xml_find_all(bill, "Situacao")),
      last_action_desc = xml_text(xml_find_all(bill, "UltimoDespacho")),
      last_action_date = xml_attr(xml_find_all(bill, "UltimoDespacho"), "Data")

    )
  )
}
