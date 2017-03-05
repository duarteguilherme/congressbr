#' @import xml2
#' @import lubridate
#' @import magrittr
#' @title Downloads and tidies data on the agenda in the Federal Senate.
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
#' sen_agenda(initial_date = "20161105", end_date = "20161125")
cam_bills <- function(type=""http://sao-paulo.estadao.com.br/noticias/geral,estudante-de-direito-e-presa-pichando-muro-na-regiao-central-de-sp,70001686946?3,number="",year, initial_date, final_date,) {
  " This function lists every bill informations according to the parameters searched"
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListrProposicoes?" %p%
    "sigla=" %p% type %p%
    "&numero=" %p% number %p%
    "&ano=" %p% year %p%
    "&datApresentacaoIni=" %p% initial_date %p%
    "&datApresentacaoFim=" %p% final_date %p%
    "&parteNomeAutor=" %p% part_name_author %p%
    "&idTipoAutor=" %p% id_type_author %p%
    "&siglaPartidoAutor=" %p% abbreviation_party_author %p%
    "&siglaUFAutor=" %p% abbreviation_st_author %p%
    "&generoAutor=" %p% gender_author %p%
    "&codEstado=" %p% cod_state %p%
    "&codOrgaoEstado=" %p% cod_branch_state %p%
    "&emTramitacao=" %p% still

  return(link)
}




