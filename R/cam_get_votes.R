#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_trim
#' @importFrom tidyr spread
#' @importFrom dplyr bind_cols
#' @title Downloads votes of a specific bill by providing type, number and year. A bill can have more than one roll call,
#' # and the API does not provide an id to identify them So we provide one (id_rollcall).
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cam_get_votes(type="PL", number="1992", year="2007")
#' @export
cam_get_votes <- function(type, number, year) {
  queue <<- 1:100 # I implemented this queue in order to build ids for rollcalls
  if ( is.null(type) | is.null(number) | is.null(year) ) {
    stop("Lacking arguments. type, number, and year are mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterVotacaoProposicao?tipo=" %p%
    type %p% "&numero=" %p% number %p% "&ano=" %p% year
  data <- read_xml(link) %>%
        xml_find_all('.//Votacao') %>%
    map_df(extract_bill_votes) %>%
    mutate(type = type, number = number, year=year) %>%
    mutate(id_rollcall = type %p% "-" %p% number %p% "-" %p% year %p% "-" %p% id_rollcall)
  return(data)
}


# I'm using this queue to create an id for each rollcall

extract_bill_votes <- function(bill) {
  info_bill <-     dplyr::tibble(
    summary_decision = xml_attr(bill, "Resumo"),
    date_decision = xml_attr(bill, "Data"),
    time_decision = xml_attr(bill, "Hora"),
    subject_vote = xml_attr(bill, "ObjVotacao"),
    id_legislative_session = xml_attr(bill, "codSessao"),
    id_rollcall = queue[1]
  )
  queue <<- queue[-1]

  # Checking Orientation
  orientation <-  bill %>%
    xml_find_all('.//bancada')
  if(length(orientation) == 0) {
    orientation_bill <- NULL
    } else { # Filling orientation
  orientation_bill <- orientation %>%
    map_df(extract_orientation) %>%
    spread(sigla, orientacao )
    }

  votes_bill <- bill %>%
    xml_find_all('.//Deputado') %>%
    map_df(extract_votes)

  data_bill <- bind_cols(info_bill, orientation_bill)

  if ( nrow(votes_bill) > 0 ) {
    data_bill <- data_bill %>%
      cbind(votes_bill)
  }

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

