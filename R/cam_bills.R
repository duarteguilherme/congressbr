#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom magrittr '%>%'
#' @title Downloads and tidies data for lists of bills in Brazilian Chamber of Deputies
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
#' cam_bills(initial_date = "11/01/2015", end_date = "11/01/2016", part_name_author="Oliveira")


cam_bills <- function(type="",number="",year="", initial_date="", end_date="", part_name_author="", id_type_author="",
                      abbreviation_party_author="", abbreviation_st_author="",gender_author="",
                      cod_state="", cod_branch_state="", still="") {
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
  return(data)
}


extract_bill <- function(bill) {
  return(
    dplyr::tibble(
      id_bill = xml_text(xml_find_all(bill, "./id")),
      name_bill = xml_text(xml_find_all(bill, "./nome")),
      id_type = xml_text(xml_find_all(bill, "./tipoProposicao/id")),
      abr_type = xml_text(xml_find_all(bill, "./tipoProposicao/sigla")),
      name_type = xml_text(xml_find_all(bill, "./tipoProposicao/nome")),
      id_branchn = xml_text(xml_find_all(bill, "./orgaoNumerador/id")),
      abr_branchn = xml_text(xml_find_all(bill, "./orgaoNumerador/sigla")),
      name_branchn = xml_text(xml_find_all(bill, "./orgaoNumerador/nome")),
      date_presentation = xml_text(xml_find_all(bill, "./datApresentacao")),
      txt_ementa = xml_text(xml_find_all(bill, "./txtEmenta")),
      explanation_ementa = xml_text(xml_find_all(bill, "./txtExplicacaoEmenta")),
      cod_procedure = xml_text(xml_find_all(bill, "./regime/codRegime")),
      txt_procedure = xml_text(xml_find_all(bill, "./regime/txtRegime")),
      id_consideration = xml_text(xml_find_all(bill, "./apreciacao/id")),
      txt_consideration = xml_text(xml_find_all(bill, "./apreciacao/txtApreciacao")),
      name_author1 = xml_text(xml_find_all(bill, "./autor1/txtNomeAutor")),
      id_author1 = xml_text(xml_find_all(bill, "./autor1/idecadastro")),
      codparty_author1 = xml_text(xml_find_all(bill, "./autor1/codPartido")),
      abrparty_author1 = xml_text(xml_find_all(bill, "./autor1/txtSiglaPartido")),
      abrstate_author1 = xml_text(xml_find_all(bill, "./autor1/txtSiglaUF")),
      number_authors = xml_text(xml_find_all(bill, "./qtdAutores")),
      last_action = xml_text(xml_find_all(bill, "./ultimoDespacho/datDespacho")),
      txt_last_action = xml_text(xml_find_all(bill, "./ultimoDespacho/txtDespacho")),
      id_situation = xml_text(xml_find_all(bill, "./situacao/id")),
      desc_situation = xml_text(xml_find_all(bill, "./situacao/descricao")),
      cod_branch_situation = xml_text(xml_find_all(bill, "./situacao/orgao/codOrgaoEstado")),
      abr_branch_situation = xml_text(xml_find_all(bill, "./situacao/orgao/siglaOrgaoEstado")),
      cod_main_bill = xml_text(xml_find_all(bill, "./situacao/principal/codProposicaoPrincipal")),
      main_bill = xml_text(xml_find_all(bill, "./situacao/principal/proposicaoPrincipal")),
      id_gender = xml_text(xml_find_all(bill, "./indGenero")),
      num_branches_state = xml_text(xml_find_all(bill, "./qtdOrgaosComEstado"))
    )
  )
}


