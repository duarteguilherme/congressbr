#' @import httr
#' @import xml2
#' @import dplyr
#'
#'
#'
#'



## to do:
# what is colegiado?
# list of options for house, colegiado and legislator

sen_agenda <- function(initial_date = NULL, end_date = NULL, details = FALSE,
                       house = NULL, colegiado = NULL, legislator = NULL){

  # 1: checks
  # details & legislator can't be used together
  # dates must be YYYYMMDD
  # the only necessary argument is initial_date
  if(is.null(initial_date)){
    return(message("Error: please choose a valid initial date. Format is YYYYMMDD."))
  }
  if(!is.null(details) & !is.null(legislator)){
    return(message("Error: it is not possible to use the arguments 'details' and 'legislator together."))
  }

  # 2:
  # request data
  base_url <- "http://legis.senado.gov.br/dadosabertos/agenda/"

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
      ## error message here
      } else{
      request <- httr::content(request, "parsed")
      }
  }

### agora, arrumar isso

x <- sen_agenda(initial_date = "20161105", end_date = "20161125")




