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
#'  "PEC" for constitutional amendments ("projeto de emenda constitucional"), "PDC" for legislative decree ("decreto legislativo"),
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
#' \donttest{
#' cham_votes(type = "PL", number = "1992", year = "2007")
#' }
#' @export
cham_votes <- function(type, number, year, ascii = TRUE) {
  if (is.null(type) | is.null(number) | is.null(year)) {
    stop("Lacking arguments. type, number, and year are mandatory")
  }
  link <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterVotacaoProposicao?tipo=" %p%
    type %p% "&numero=" %p% number %p% "&ano=" %p% year

  data <- tryCatch(
    {
      read_xml(link)
    },
    error = function(x) {
      y <- httr::GET(link)
      # Handling a specific error related to the API
      msg_erro <- "Esta proposicao eh acessoria e nao foi possivel baixar seu conteudo"
      if (stringr::str_detect(rawToChar(y$content), msg_erro)) {
        stop("This is not a main bill. Download is not possible")
      }
      else {
        stop("HTTP error 500")
      }
    }
  )
  data <- data %>%
    xml_find_all(".//Votacao") %>%
    map_df(cham_extract_bill_votes, .id = "rollcall_id") %>%
    mutate(type_bill = type, number_bill = number, year_bill = year)

  data$rollcall_id <- data$type %p% "-" %p% data$number %p% "-" %p% data$year %p% "-" %p% data$rollcall_id


  data <- as_tibble(data)

  if (ascii == T) {
    data <- data %>%
      dplyr::mutate_if(is.character, function(x) stringi::stri_trans_general(x, "Latin-ASCII"))
  }

  data <- mutate(data, legislator_party = str_trim(legislator_party))
  return(data)
}

#' @importFrom glue glue
#' @importFrom purrr map_df
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom stringr str_trim
#' @importFrom lubridate dmy
#' @importFrom lubridate year
#' @title Returns voting information from the Chamber floor for the year
#' requested
#' @description Returns voting information from the Chamber floor for the year
#' requested.
#' @param year \code{character} or \code{integer}. Format YYYY
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{
#' cham_votes_year("2013")
#' }
#'
#' @export
cham_votes_year <- function(year) {
  print(glue("Downloading data for {year}. This operation might take a few minutes..."))

  # Call cham_plenary_bills to download a list of issues voted in a certain year
  dados <- cham_plenary_bills(year)

  # Obtaining common description for each bill_id
  # Through cham_bill_info_id(bill_id)
  bills <- dados %>%
    distinct(bill_id)
  bills <- bills$bill_id

  print("Checking bills...")
  data_tny <- map_df(
    bills,
    cham_bill_info_id
  ) %>%
    distinct(bill_type, bill_number, bill_year)

  print("Downloading votes...")

  # Now we will iterate for each bill and
  # get data from votes
  # We'll be using for loops in order to
  # handle errors better
  data_year <- NULL
  for (i in 1:nrow(data_tny)) {
    bill_type <- str_trim(data_tny$bill_type[i])
    bill_number <- data_tny$bill_number[i]
    bill_year <- data_tny$bill_year[i]
    extrato <- year_extract_votes(bill_type, bill_number, bill_year)
    data_year <- bind_rows(data_year, extrato)
  }

  # Filtering only those decision taken that year
  # extract_votes return all the decisions regarding
  # one project
  # It's pretty common that one bill returns more than
  # one calls taken in different dates and years
  data_year <- data_year %>%
    mutate(ano = lubridate::year(lubridate::dmy(decision_date))) %>%
    filter(ano == year)

  data_year
}


# create an id for each rollcall
cham_extract_bill_votes <- function(bill) {
  info_bill <- dplyr::tibble(
    decision_summary = xml_attr(bill, "Resumo"),
    decision_date = xml_attr(bill, "Data"),
    decision_time = xml_attr(bill, "Hora"),
    rollcall_subject = xml_attr(bill, "ObjVotacao"),
    session_id = xml_attr(bill, "codSessao")
  )

  # Checking Orientation
  orientation <- bill %>%
    xml_find_all(".//bancada")
  if (length(orientation) == 0) {
    orientation_bill <- NULL
  } else { # Filling orientation
    orientation_bill <- orientation %>%
      map_df(extract_orientation) %>%
      spread(sigla, orientacao)
  }

  votes_bill <- bill %>%
    xml_find_all(".//Deputado") %>%
    map_df(cham_extract_votes)

  data_bill <- bind_cols(info_bill, orientation_bill)

  if (nrow(votes_bill) > 0) {
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


cham_extract_votes <- function(votes) {
  DF <- dplyr::tibble(
    legislator_id = xml_attr(votes, "ideCadastro"),
    legislator_name = xml_attr(votes, "Nome"),
    legislator_state = xml_attr(votes, "UF"),
    legislator_party = str_trim(xml_attr(votes, "Partido")),
    legislator_vote = str_trim(xml_attr(votes, "Voto"))
  ) %>%
    dplyr::mutate(legislator_vote = ifelse(legislator_vote == "-", NA, legislator_vote))
  return(DF)
}


year_extract_votes <- function(type, number, year) {
  # This function extracts votes for a certain bill
  # it takes bill s type, number and year
  print(glue::glue("Downloading votes from {type}-{number}/{year}"))

  #  votes <- cham_votes(type, number, year)
  # This handling error structure is necessary
  # since there are some issues with the API
  votes <- tryCatch(cham_votes(type, number, year),
                    error = function(e) {
                      mes <- e$message
                      if (str_detect(mes, "This is not a main bill. Download is not possible")) {
                        print(mes)
                        return(NULL)
                      }
                      else if (mes == "HTTP error 500") {
                        print(glue(
                          "Download is not possible. This problem usually happens with not included 'requerimentos'"
                        ))
                        return(NULL)
                      }
                      else {
                        stop("nao funcionou")
                      }
                    }
  )

  if (is.null(votes)) {
    return(NULL)
  }
  #   Removing other types of orientation
  orientation_removed <- colnames(votes)[grep("orient_", colnames(votes))]
  orientation_removed <- orientation_removed[(orientation_removed != "orient_GOV")]
  votes <- votes %>% dplyr::select(-dplyr::one_of(orientation_removed))
  return(votes)
}
