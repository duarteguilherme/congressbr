#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%>%"
#' @title Downloads and tidies data for lists of bills in Brazilian Chamber of Deputies
#' @description Downloads and tidies data for lists of bills in Brazilian Chamber of Deputies
#' @param type \code{character}. The type of the bill. For example, "PL"
#' @param number \code{integer}. The number of the bill
#' @param year \code{integer}. The year of the bill.
#' @param initial_date (\code{character}) start date of the period requested.
#' This parameter must be in the format YYYYMMDD (Year-Month-Day). A value for
#' this parameter is necessary, all others are optional.
#' @param end_date (\code{character}) final date for period requested. Format
#' YYYYMMDD.
#' @param part_name_author (\code{character}). Filter bills by a part of the author's name
#' @param id_type_author (\code{character}). Filter bills by type of a branch_author for the bill.
#' @param abbreviation_party_author (\code{character}). Filter bills by the party of the author
#' @param abbreviation_st_author (\code{character}). Filter bills by the state of the author
#' @param gender_author (\code{character}). If "M", bills proposed by male authors are filtered. If "F", female authors. If empty, every bill is included.
#' @param cod_state (\code{integer}). Filter bills by a part of the author's name
#' @param cod_branch_state (\code{integer}). Filter by the id of the branch regarding the bill.
#' @param still (\code{integer}). If 1, only bills that are still being discussed are included. If 2, only ended bills. If empty, every bill is included.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \dontrun{
#' cham_bills(type = "PL", year = 1998)
#' }
#' @export


cham_bills <- function(type="",number="",year="", initial_date="", end_date="", part_name_author="", id_type_author="",
                      abbreviation_party_author="", abbreviation_st_author="",gender_author="",
                      cod_state="", cod_branch_state="", still="", ascii=T) {
  " This function lists every bill informations according to the parameters searched"
  if ( part_name_author=="" & ( type=="" | year=="" )  ) {
    stop("Lacking arguments. part_name_author or type and year are mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoes?" %p%
    "sigla=" %p% type %p%
    "&numero=" %p% number %p%
    "&ano=" %p% year %p%
    "&datApresentacaoIni=" %p% initial_date %p%
    "&datApresentacaoFim=" %p% end_date %p%
    "&parteNomeAutor=" %p% part_name_author %p%
    "&idTipoAutor=" %p% id_type_author %p%
    "&siglaPartidoAutor=" %p% abbreviation_party_author %p%
    "&siglaUFAutor=" %p% abbreviation_st_author %p%
    "&generoAutor=" %p% gender_author %p%
    "&codEstado=" %p% cod_state %p%
    "&codOrgaoEstado=" %p% cod_branch_state %p%
    "&emTramitacao=" %p% still
  data <- read_xml(link) %>%
    xml_find_all('proposicao') %>%
    map_df(extract_bill)
  if ( ascii==T ) {
    data <- data %>%
      dplyr::mutate_if(is.character, function(x) stringi::stri_trans_general(x, "Latin-ASCII")
      )
  }

  return(data)
}


extract_bill <- function(bill) {
  return(
    dplyr::tibble(
      bill_id = xml_text(xml_find_all(bill, "./id")),
      bill_name = xml_text(xml_find_all(bill, "./nome")),
      type_id = xml_text(xml_find_all(bill, "./tipoProposicao/id")),
      type_abr = xml_text(xml_find_all(bill, "./tipoProposicao/sigla")),
      type_name = xml_text(xml_find_all(bill, "./tipoProposicao/nome")),
      branchn_id = xml_text(xml_find_all(bill, "./orgaoNumerador/id")),
      branchn_abr = xml_text(xml_find_all(bill, "./orgaoNumerador/sigla")),
      branchn_name = xml_text(xml_find_all(bill, "./orgaoNumerador/nome")),
      presentation_date = xml_text(xml_find_all(bill, "./datApresentacao")),
      ementa_txt = xml_text(xml_find_all(bill, "./txtEmenta")),
      ementa_explanation = xml_text(xml_find_all(bill, "./txtExplicacaoEmenta")),
      procedure_cod = xml_text(xml_find_all(bill, "./regime/codRegime")),
      procedure_txt = xml_text(xml_find_all(bill, "./regime/txtRegime")),
      consideration_id = xml_text(xml_find_all(bill, "./apreciacao/id")),
      consideration_txt = xml_text(xml_find_all(bill, "./apreciacao/txtApreciacao")),
      author1_name = xml_text(xml_find_all(bill, "./autor1/txtNomeAutor")),
      author1_id = xml_text(xml_find_all(bill, "./autor1/idecadastro")),
      author1_party_cod = xml_text(xml_find_all(bill, "./autor1/codPartido")),
      author1_party_abr = xml_text(xml_find_all(bill, "./autor1/txtSiglaPartido")),
      author1_state = xml_text(xml_find_all(bill, "./autor1/txtSiglaUF")),
      number_authors = xml_text(xml_find_all(bill, "./qtdAutores")),
      last_action = xml_text(xml_find_all(bill, "./ultimoDespacho/datDespacho")),
      last_action_txt = xml_text(xml_find_all(bill, "./ultimoDespacho/txtDespacho")),
      situation_id = xml_text(xml_find_all(bill, "./situacao/id")),
      situation_desc = xml_text(xml_find_all(bill, "./situacao/descricao")),
      situation_branch_cod = xml_text(xml_find_all(bill, "./situacao/orgao/codOrgaoEstado")),
      situation_branch_abr = xml_text(xml_find_all(bill, "./situacao/orgao/siglaOrgaoEstado")),
      main_bill_cod = xml_text(xml_find_all(bill, "./situacao/principal/codProposicaoPrincipal")),
      main_bill = xml_text(xml_find_all(bill, "./situacao/principal/proposicaoPrincipal")),
      gender_id = xml_text(xml_find_all(bill, "./indGenero")),
      num_branches_state = xml_text(xml_find_all(bill, "./qtdOrgaosComEstado"))
    )
  )
}

