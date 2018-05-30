#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom magrittr "%>%"
#' @importFrom stats rnorm
#' @importFrom stringr str_detect
#' @importFrom stringr str_trim
#' @importFrom tidyr spread
#' @importFrom tidyr unite
#' @importFrom dplyr bind_cols
#' @importFrom dplyr mutate_if
#' @importFrom httr GET
#' @title Downloads votes of a specific bill by providing type, number and year
#' @description Downloads votes of a specific bill by providing type, number and year. A bill can have more than one roll call,
#' and the API does not provide an id to identify them So we provide one (rollcall_id).
#' @param type \code{character}. The type of the bill. For example, "PL" for law proposal ("projeto de lei"),
#'  "PEC" for constitutional ammendments ("projeto de emenda constitucional"), "PDC" for legislative decree ("decreto legislativo"),
#'  and "PLP" for supplementary laws ("projeto de lei complementar).
#' @param number \code{integer}. The number of the bill.
#' @param year \code{integer}. The year of the bill.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \dontrun{cham_votes(type = "PL", number = "1992", year = "2007")}
#' @export
cham_votes <- function(type, number, year, ascii = TRUE) {
  if ( is.null(type) | is.null(number) | is.null(year) ) {
    stop("Lacking arguments. type, number, and year are mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterVotacaoProposicao?tipo=" %p%
    type %p% "&numero=" %p% number %p% "&ano=" %p% year
  print(link)
  data <- tryCatch({read_xml(link)},
                   error=function(x) {
                     y <- GET(link)
                     # Handling a specific error related to the API
                     msg_erro <- "Esta proposicao eh acessoria e nao foi possivel baixar seu conteudo"
                    if ( str_detect(rawToChar(y$content), msg_erro) ) {
                      stop("This is not a main bill. Download is not possible")
                    }
                     else {
                       stop("HTTP error 500")
                     }
                  }
  )
  data <- data %>%
        xml_find_all('.//Votacao') %>%
    map_df(extract_bill_votes, .id = "rollcall_id") %>%
    mutate(type_bill = type, number_bill = number, year_bill=year)

  data$rollcall_id <- data$type %p% "-" %p% data$number %p% "-" %p% data$year %p% "-" %p% data$rollcall_id


  data <- as_tibble(data)

  if ( ascii==T ) {
    data <- data %>%
      dplyr::mutate_if(is.character, function(x) stringi::stri_trans_general(x, "Latin-ASCII")
      )
  }

  data <- mutate(data, legislator_party = str_trim(legislator_party) )
  return(data)
}


# I'm using this queue to create an id for each rollcall

extract_bill_votes <- function(bill) {
  info_bill <-  dplyr::tibble(
    decision_summary = xml_attr(bill, "Resumo"),
    decision_date = xml_attr(bill, "Data"),
    decision_time = xml_attr(bill, "Hora"),
    rollcall_subject = xml_attr(bill, "ObjVotacao"),
    session_id = xml_attr(bill, "codSessao")
    )

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
      sigla = gsub("[^a-zA-z]", "", xml_attr(votacao, "Sigla")) %p% "_orientation",
      orientacao = str_trim(xml_attr(votacao, "orientacao"))
    )
  )
}


extract_votes <- function(votes) {
    DF <- dplyr::tibble(
      legislator_id =  xml_attr(votes, "ideCadastro"),
      legislator_name =  xml_attr(votes, "Nome"),
      legislator_state = xml_attr(votes, "UF"),
      legislator_party = str_trim(xml_attr(votes, "Partido")),
      legislator_vote =  str_trim(xml_attr(votes, "Voto"))
    ) %>%
      dplyr::mutate(legislator_vote = ifelse(legislator_vote == "-", NA, legislator_vote))
    return(DF)
}

