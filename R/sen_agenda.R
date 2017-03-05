#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom lubridate parse_date_time
#' @importFrom data.table rbindlist
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies data on the agenda in the Federal Senate.
#' @param initial_data (\code{character}) start date of the period requested.
#' This parameter must be in the format YYYYMMDD (Year-Month-Day). A value for
#' this parameter is necessary, all others are optional.
#' @param end_data (\code{character}) final date for period requested. Format
#' YYYYMMDD.
#' @param house (\code{character}). The acronym for the legislative house
#' for which results are requested. Options are SF (\emph{Senado Federal}, Federal Senate) and CN (\emph{Congresso Nacional}, National Congress - joint meeting of the Senate and Chamber).
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
#'
#' @export
sen_agenda <- function(initial_date = NULL, end_date, house,
                       colegiado, legislator, details = FALSE){

  # checks
  if(is.null(initial_date)){
    stop("Please choose a valid initial date. Format is YYYYMMDD.")
  }
  if(!is.null(details) & !is.null(legislator)){
    stop("It is not possible to use the arguments 'details' and 'legislator together.")
  }

  # request data
  base_url <- "http://legis.senado.gov.br/dadosabertos/agenda/"

  # specify urls:
  if(!is.null(end_date) & details == TRUE & !is.null(house)
     & !is.null(colegiado)){
    request <- httr::GET(paste0(base_url, initial_date, "/", end_date,
                                "/", "detalhe?casa=", house,
                                "&detalhe?colegiado=", colegiado))
  } else if(!is.null(end_date) & details == TRUE & !is.null(house)){
    request <- httr::GET(paste0(base_url, initial_date, "/", end_date,
                                "/", "detalhe?casa=", house))
  } else if(!is.null(end_date) & details == TRUE & !is.null(colegiado)){
    request <- httr::GET(paste0(base_url, initial_date, "/", end_date,
                                "/", "detalhe?colegiado=", colegiado))
  } else if(!is.null(end_date) & details == TRUE){
    request <- httr::GET(paste0(base_url, initial_date, "/", end_date,
                                "/", "detalhe"))
  } else if(!is.null(end_date) & details == FALSE){
    request <- httr::GET(paste0(base_url, initial_date, "/", end_date))
  } else if(is.null(end_date) & details == TRUE){
    request <- httr::GET(paste0(base_url, initial_date, "/", "detalhe"))
  } else{
    request <- httr::GET(paste0(base_url, initial_date))
  }
    # status checks
    if(request$status_code != 200){
      stop("GET request failed")
      } else{
      request <- httr::content(request, "parsed")
      }

  ## tidy
  request <- request$Reunioes$Reuniao

  req <- rmNullObs(request)

  for(z in 1:length(req)){
    req[[z]] <- as.data.frame(req[[z]], stringsAsFactors = F)
  }

  req <- data.table::rbindlist(req, fill=TRUE)

  req$Data <- gsub("/", "-", req$Data)
  req$Data <- lubridate::parse_date_time(req$Data, orders = "d!m!Y!")
  req <- dplyr::as_data_frame(req)
  return(req)
  }
