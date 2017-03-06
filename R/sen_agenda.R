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
#' for which results are requested. Options are SF (\emph{Senado Federal},
#' Federal Senate), CN (\emph{Congresso Nacional}, National Congress - joint
#' meeting of the Senate and Chamber), and CA \code{Camara dos Deputados},
#' Chamber of Deputies.
#' @param s_b \code{character}. Name of the commission or supervisory body. A
#' data frame of these can be seen with \code{data("commissions")}.
#' @param legislator \code{integer}. The numeric code given to each senator.
#' A dataframe with these values is returned from the \code{sen_senator_list()}
#' function.
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
#' sen_agenda(initial_date = "20161105", end_date = "20161125",
#' legislator = 4988)
#' @export
sen_agenda <- function(initial_date = NULL, end_date = NULL,
                       house = NULL, s_b = NULL,
                       legislator = 0, details = FALSE){
  # checks
  if(is.null(initial_date)){
    stop("Please choose a valid initial date. Format is YYYYMMDD.")
  }
  if(details == TRUE & !is.null(legislator)){
    warning("Using the arguments 'details' and 'legislator' together will return the same thing as when 'legislator' is not used.")
  }

  base_url <- "http://legis.senado.gov.br/dadosabertos/agenda/" %p%
    initial_date

  if(!is.null(end_date) & details == FALSE){
    base_url <- paste0(base_url, "/", end_date, "?")
  } else if(!is.null(end_date) & details == TRUE){
    base_url <- paste0(base_url, "/", end_date, "/", "detalhe?")
  } else if(is.null(end_date) & details == TRUE){
    base_url <- paste0(base_url, "/", "detalhe?")
  } else{
    base_url <- paste0(base_url, "?")
  }

  # request data
  request <- base_url %p%
    "casa=" %p% house %p%
    "&colegiado=" %p% s_b %p%
    "&parlamentar=" %p% legislator

  request <- httr::GET(request)

  # status checks
  request <- status(request)

  if(is.null(request$Reunioes)){
    stop("No data matches your input.")
  }

  ## tidy
  request <- request$Reunioes$Reuniao

  req <- rmNullObs(request)

  for(z in 1:length(req)){
    req[[z]] <- as.data.frame(req[[z]], stringsAsFactors = F)
  }

  req <- data.table::rbindlist(req, fill = TRUE)

  req$Data <- gsub("/", "-", req$Data)
  req$Data <- lubridate::parse_date_time(req$Data, orders = "d!m!Y!")
  req <- dplyr::as_data_frame(req)
  return(req)
  }
