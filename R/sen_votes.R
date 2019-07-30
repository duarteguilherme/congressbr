#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom purrr flatten
#' @importFrom purrr map_chr
#' @importFrom purrr is_empty
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Returns voting information from the Senate floor for the date
#' requested
#' @description Returns voting information from the Senate floor for the date
#' requested.
#' @param date \code{character} or \code{integer}. Format YYYYMMDD.
#' @param end_date \code{character} or \code{integer}. Format YYYYMMDD. If
#' \code{end_date} is supplied, information on all votes between the initial date
#' (\code{date}) and \code{end_date} is returned. See the notes below.
#' @param binary \code{logical}. If \code{TRUE}, the default, transforms
#' votes into \code{1} for "yes", \code{0}, for "no" and \code{NA} for everything
#' else. If \code{FALSE}, returns a character vector of vote decisions.
#' @param ascii \code{logical}. If \code{TRUE}, the default, strips Latin
#' characters from the results.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @note The maximum period allowed by the API is currently 60 days. If the
#' period requested is larger than this, an error message is returned. For a full
#' set of votes for each legislature, see \code{data("senate_nominal_votes")}.
#' @examples
#' \dontrun{
#' sen_votes(date = "20130516")
#'
#' # Some votes are secret:
#' ssshhh <- sen_votes("20160301", binary = FALSE)
#'
#' # All votes between two periods (might take a little while):
#' longer <- sen_votes(date = "20160301", end_date = "20160415")
#' }
#' @export
sen_votes <- function(date = NULL, end_date = NULL,
                              binary = TRUE, ascii = TRUE){

  year_now <- Sys.Date() %>% gsub("-", "", .) %>% gsub("[0-9]{4}$", "", .) %>%
    as.numeric()
  year_func <- gsub("[0-9]{4}$", "", date) %>% as.numeric()
  if(is.null(date) || nchar(date) < 8 ||  year_func > year_now){
    stop("Please enter a valid date. Format is YYYYMMDD.")
  }

  if(!is.null(end_date)){
    year_func2 <- gsub("[0-9]{4}$", "", end_date) %>% as.numeric()
    if(nchar(end_date) < 8 ||  year_func2 > year_now){
      stop("Please enter a valid date. Format is YYYYMMDD.")
    }
    date_1 <- lubridate::parse_date_time(date, "Ymd")
    date_2 <- lubridate::parse_date_time(end_date, "Ymd")
    period <- date_2 - date_1
    if(period > 60){
      stop("Period requested is larger than that allowed by the API.")
    }
    base_url <- "http://legis.senado.gov.br/dadosabertos/plenario/lista/votacao/" %p%
      date %p% "/" %p% end_date
  } else if(is.null(end_date)){
    base_url <- "http://legis.senado.gov.br/dadosabertos/plenario/lista/votacao/" %p%
      date
  }

  request <- httr::GET(base_url)
  request <- status(request)
  if(purrr::is_empty(request$ListaVotacoes$Votacoes)){
    stop("No data match your request.")
  }
  if(depth(request$ListaVotacoes$Votacoes$Votacao) == 4){
    request <- request$ListaVotacoes$Votacoes
  } else if(depth(request$ListaVotacoes$Votacoes$Votacao) >= 5){
    request <- request$ListaVotacoes$Votacoes$Votacao
  }

  N <- NA_character_
  votes <- purrr::map(request, "Votos")
  if(length(votes) > 1){
    votes <- votes %>%  purrr::flatten()
  }

  bill_id <- purrr::map_chr(request, .null = N, "CodigoMateria")
  vote_round <- purrr::map_chr(request, .null = N, "SequencialSessao")
  rollcall_id <- purrr::map_chr(request, .null = N, "CodigoSessaoVotacao")

  for(k in 1:length(votes)){
    for(j in 1:length(votes[[k]])){
      votes[[k]][[j]]$bill_id = bill_id[[k]]
      votes[[k]][[j]]$vote_round = vote_round[[k]]
      votes[[k]][[j]]$rollcall_id = rollcall_id[[k]]

    }
  }

  votes <- votes %>% purrr::flatten()

  bill <- tibble::tibble(
    vote_date = purrr::map_chr(request, .null = N, "DataSessao"),
    vote_time = purrr::map_chr(request, .null = N, "HoraInicio"),
    vote_round = purrr::map_chr(request, .null = N, "SequencialSessao"),
    bill_id = purrr::map_chr(request, .null = N, "CodigoMateria"),
    bill_number = purrr::map_chr(request, .null = N, "NumeroMateria"),
    bill_type = purrr::map_chr(request, .null = N, "SiglaMateria"),
    bill_year = purrr::map_chr(request, .null = N, "AnoMateria"),
    bill_description = purrr::map_chr(request, .null = N, "DescricaoVotacao"),
    rollcall_id = purrr::map_chr(request, .null = N, "CodigoSessaoVotacao"),
    vote_result = purrr::map_chr(request, .null = N, "Resultado"),
    vote_secret = purrr::map_chr(request, .null = N, "Secreta")
  )

  vote <- tibble::tibble(
    bill_id = purrr::map_chr(votes, .null = N, "bill_id"),
    vote_round = purrr::map_chr(votes, .null = N, "vote_round"),
    rollcall_id = purrr::map_chr(votes, .null = N, "rollcall_id"),
    senator_id = purrr::map_chr(votes, .null = N, "CodigoParlamentar"),
    senator_name = purrr::map_chr(votes, .null = N, "NomeParlamentar"),
    senator_vote = purrr::map_chr(votes, .null = N, "Voto"),
    senator_gender = purrr::map_chr(votes, .null = N, "SexoParlamentar"),
    senator_party = purrr::map_chr(votes, .null = N, "SiglaPartido"),
    senator_state = purrr::map_chr(votes, .null = N, "SiglaUF")
  )


  Votes <- suppressMessages(dplyr::full_join(bill, vote))


  Votes <- Votes %>%
    dplyr::mutate(
      vote_date = lubridate::parse_date_time(
        vote_date, "Ymd"),
      vote_round = as.numeric(vote_round),
      vote_secret = ifelse(vote_secret == "S", "Yes", "No")
    )

  if(isTRUE(ascii)){
    Votes <- Votes %>%
      dplyr::mutate(
        bill_description = stringi::stri_trans_general(
          bill_description, "Latin-ASCII"),
        senator_name = stringi::stri_trans_general(
          senator_name, "Latin-ASCII"),
        senator_vote = stringi::stri_trans_general(
          senator_vote, "Latin-ASCII")
      )
  }
  if(isTRUE(binary)){
    Votes <- Votes %>%
      dplyr::mutate(
        senator_vote = stringi::stri_trans_general(
          senator_vote, "Latin-ASCII"),
        senator_vote = ifelse(
          vote_secret == "Yes" & senator_vote == "Votou", "Voted",
          ifelse(
            vote_secret == "Yes" & senator_vote != "Votou", "Did not vote",
            ifelse(
              vote_secret == "No" & senator_vote == "Sim", 1,
              ifelse(vote_secret == "No" & senator_vote == "Nao", 0, NA)
            ))))
  } else{
    Votes <- Votes %>%
      dplyr::mutate(
        senator_vote = ifelse(
          vote_secret == "Yes" & senator_vote == "Votou", "Voted",
          ifelse(
            vote_secret == "Yes" & senator_vote != "Votou", "Did not vote",
            ifelse(
              vote_secret == "No" & senator_vote == "Sim", "Yes",
              ifelse(
                vote_secret == "No" & senator_vote == "Nao", "No",
                ifelse(
                  vote_secret == "No" & senator_vote == "Abstencao",
                  "Abstained", "Other"))))))
  }

  return(Votes)
}

#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom progress progress_bar
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom purrr flatten
#' @importFrom purrr map_chr
#' @importFrom purrr is_empty
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Returns voting information from the Senate floor for the year
#' requested
#' @description Returns voting information from the Senate floor for the year
#' requested.
#' @param year \code{character} or \code{integer}. Format YYYY
#' @param binary \code{logical}. If \code{TRUE}, the default, transforms
#' votes into \code{1} for "yes", \code{0}, for "no" and \code{NA} for everything
#' else. If \code{FALSE}, returns a character vector of vote decisions.
#' @param ascii \code{logical}. If \code{TRUE}, the default, strips Latin
#' characters from the results.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{
#' sen_votes_year("2013")
#' }
#'
#' @export
sen_votes_year <- function(year = NULL,
                      binary = TRUE, ascii = TRUE){

  if(is.null(year) || nchar(year) < 4 ){
    stop("Please enter a valid date. Format is YYYY.")
  }

  base_url <- glue::glue("http://legis.senado.leg.br/dadosabertos/dados/ListaVotacoes{year}.xml")


  request <- httr::GET(base_url)
  request <- status2(request)

  rollcalls <- xml2::xml_find_first(request, 'Votacoes')
  rollcalls <- xml2::xml_find_all(rollcalls,'Votacao')

  Votes <- NULL
  pb <- progress::progress_bar$new(total = length(rollcalls))
  cat("Parsing Data... \n\n")
  for (i in 1:length(rollcalls)) {
      pb$tick()

      Votes <- dplyr::bind_rows(Votes, extract_rollcall_data(rollcalls[[i]]))
  }


  Votes <- Votes %>%
    dplyr::mutate(
      vote_date = lubridate::parse_date_time(
        vote_date, "Ymd"),
      vote_round = as.numeric(vote_round),
      vote_secret = ifelse(vote_secret == "S", "Yes", "No")
    )

  if(isTRUE(ascii)){
    Votes <- Votes %>%
      dplyr::mutate(
        bill_description = stringi::stri_trans_general(
          bill_description, "Latin-ASCII"),
        senator_name = stringi::stri_trans_general(
          senator_name, "Latin-ASCII"),
        senator_vote = stringi::stri_trans_general(
          senator_vote, "Latin-ASCII")
      )
  }
  if(isTRUE(binary)){
    Votes <- Votes %>%
      dplyr::mutate(
        senator_vote = stringi::stri_trans_general(
          senator_vote, "Latin-ASCII"),
        senator_vote = ifelse(
          vote_secret == "Yes" & senator_vote == "Votou", "Voted",
          ifelse(
            vote_secret == "Yes" & senator_vote != "Votou", "Did not vote",
            ifelse(
              vote_secret == "No" & senator_vote == "Sim", 1,
              ifelse(vote_secret == "No" & senator_vote == "Nao", 0, NA)
            ))))
  } else{
    Votes <- Votes %>%
      dplyr::mutate(
        senator_vote = ifelse(
          vote_secret == "Yes" & senator_vote == "Votou", "Voted",
          ifelse(
            vote_secret == "Yes" & senator_vote != "Votou", "Did not vote",
            ifelse(
              vote_secret == "No" & senator_vote == "Sim", "Yes",
              ifelse(
                vote_secret == "No" & senator_vote == "Nao", "No",
                ifelse(
                  vote_secret == "No" & senator_vote == "Abstencao",
                  "Abstained", "Other"))))))
  }

  return(Votes)
}


extract_rollcall_data <- function(x) {
  bill <- tibble::tibble(
    vote_date = xml2::xml_text(xml2::xml_find_first(x, "DataSessao")),
    vote_time = xml2::xml_text(xml2::xml_find_first(x, "HoraInicio")),
    vote_round = xml2::xml_text(xml2::xml_find_first(x, "SequencialSessao")),
    bill_id = xml2::xml_text(xml2::xml_find_first(x, "CodigoMateria")),
    bill_number = xml2::xml_text(xml2::xml_find_first(x, "NumeroMateria")),
    bill_type = xml2::xml_text(xml2::xml_find_first(x, "SiglaMateria")),
    bill_year = xml2::xml_text(xml2::xml_find_first(x, "AnoMateria")),
    bill_description = xml2::xml_text(xml2::xml_find_first(x, "DescricaoVotacao")),
    rollcall_id = xml2::xml_text(xml2::xml_find_first(x, "CodigoSessaoVotacao")),
    vote_result = xml2::xml_text(xml2::xml_find_first(x, "Resultado")),
    vote_secret = xml2::xml_text(xml2::xml_find_first(x, "Secreta"))
  )
  votes <- xml2::xml_find_first(x, "Votos")
  if ( purrr::is_empty(votes) ) return(bill)
  votes <- xml2::xml_find_all(votes, "VotoParlamentar")
  votes_data <- dplyr::bind_rows(purrr::map(votes, extract_votes))

  cbind(bill, votes_data)

}


extract_votes <- function(x) {
  vote <- tibble::tibble(
    senator_id = xml2::xml_text(xml2::xml_find_first(x, "CodigoParlamentar")),
    senator_name = xml2::xml_text(xml2::xml_find_first(x, "NomeParlamentar")),
    senator_vote = xml2::xml_text(xml2::xml_find_first(x, "Voto")),
    senator_gender = xml2::xml_text(xml2::xml_find_first(x, "SexoParlamentar")),
    senator_party = xml2::xml_text(xml2::xml_find_first(x, "SiglaPartido")),
    senator_state = xml2::xml_text(xml2::xml_find_first(x, "SiglaUF"))
  )
  vote
}
