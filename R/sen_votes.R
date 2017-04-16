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
#' nominal_vote <- sen_votes(date = "20130516")
#'
#' # Some votes are secret:
#' ssshhh <- sen_votes("20160301", binary = F)
#'
#' # All votes between two periods (might take a little while):
#' longer <- sen_votes(date = "20160301", end_date = "20160415")
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

  bill_id = purrr::map_chr(request, .null = N, "CodigoMateria")
  vote_round = purrr::map_chr(request, .null = N, "SequencialSessao")

  for(k in 1:length(request)){
    for(j in 1:length(votes[[k]])){
      votes[[k]][[j]]$bill_id = bill_id[[k]]
      votes[[k]][[j]]$vote_round = vote_round[[k]]
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
    vote_result = purrr::map_chr(request, .null = N, "Resultado"),
    vote_secret = purrr::map_chr(request, .null = N, "Secreta")
  )

  vote <- tibble::tibble(
    bill_id = purrr::map_chr(votes, .null = N, "bill_id"),
    vote_round = purrr::map_chr(votes, .null = N, "vote_round"),
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

  if(ascii == TRUE){
    Votes <- Votes %>%
      dplyr::mutate(
        bill_description = stringi::stri_trans_general(
          bill_description, "Latin-ASCII"),
        senator_name = stringi::stri_trans_general(
          senator_name, "Latin-ASCII")
      )
  }
  if(binary == TRUE){
    Votes <- Votes %>%
      dplyr::mutate(
        senator_vote = ifelse(
          vote_secret == "Yes" & senator_vote == "Votou", "Voted",
          ifelse(
            vote_secret == "Yes" & senator_vote != "Votou", "Did not vote",
            ifelse(
              vote_secret == "No" & senator_vote == "Sim", 1,
              ifelse(vote_secret == "No" & senator_vote == "N\u00a3o", 0, NA)
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
                vote_secret == "No" & senator_vote == "N\u00a3o", "No",
                ifelse(
                  vote_secret == "No" & senator_vote == "Absten\u00a7\u00a3o",
                  "Abstained", "Other"))))))
  }

  return(Votes)
}



