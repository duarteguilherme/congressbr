#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_trim
#' @title Downloads details of a specific bill by providing type, number and year
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cam_get_votes(type="PL", number="1992", year="2007")
#' @export


cam_get_votes <- function(type, number, year) {
  if ( is.null(type) | is.null(number) | is.null(year) ) {
    stop("Lacking arguments. type, number, and year are mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterVotacaoProposicao?tipo=" %p%
    type %p% "&numero=" %p% number %p% "&ano=" %p% year
  data <- read_xml(link) %>%
        xml_find_all('.//Votacao') %>%
    map_df(extract_bill_votes)
  return(data)

}

extract_bill_votes <- function(bill) {
  info_bill <-     dplyr::tibble(
    summary_decision = xml_attr(bill, "Resumo"),
    date_decision = xml_attr(bill, "Data"),
    time_decision = xml_attr(bill, "Hora"),
    subject_vote = xml_attr(bill, "ObjVotacao"),
    id_legislative_session = xml_attr(bill, "codSessao")

  )
  orientation_bill <- bill %>%
    xml_find_all('.//bancada') %>%
    map_df(extract_orientation) %>%
    spread(sigla, orientacao )

  votes_bill <- bill %>%
    xml_find_all('.//Deputado') %>%
    map_df(extract_votes)
  
  data_bill <- bind_cols(info_bill, orientation_bill) %>%
    cbind(votes_bill)
  
  return(data_bill)
    
}


extract_orientation <- function(votacao) {
  return(
    dplyr::tibble(
      sigla="orient_" %p% gsub("[^a-zA-z]", "", xml_attr(votacao, "Sigla")),
      orientacao=str_trim(xml_attr(votacao, "orientacao"))
    )
  )
}


extract_votes <- function(votes) {
  return(
    dplyr::tibble(
      id_legislator =  xml_attr(votes, "ideCadastro"),
      name_legislator =  xml_attr(votes, "Nome"),
      uf_legislator = xml_attr(votes, "UF"),
      party_legislator = xml_attr(votes, "Partido"),
      vote =  str_trim(xml_attr(votes, "Voto"))
    )
  )
}

