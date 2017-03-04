#' @import httr
#' @import xml2
#' @import lubridate
#' @import data.table
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
      return(message("Error: GET request failed"))
      } else{
      request <- httr::content(request, "parsed")
      }

  ## tidy
  request <- request$Reunioes$Reuniao
  # helper function from Josh O'Brien from here:
  #http://stackoverflow.com/questions/26539441/r-remove-null-elements-from-list-of-lists
  is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

  ## Recursively step down into list, removing all such objects
  rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }

  req <- rmNullObs(request)

  for(z in 1:length(req)){
    req[[z]] <- as.data.frame(req[[z]], stringsAsFactors = F)
  }

  req2 <- data.table::rbindlist(req, fill=TRUE)

  req2$Data <- gsub("/", "-", req2$Data)
  req2$Data <- lubridate::parse_date_time(req2$Data, orders = "d!m!Y!")

  return(req2)
  }

X <- sen_agenda(initial_date = "20161105", end_date = "20161125")




### agora, arrumar isso
# what comes back with detalhes?
y <- sen_agenda(initial_date = "20161105", end_date = "20161125", details = T)
# first, it is 37 times bigger.
listviewer::jsonedit(y)
# list of 1, then 87, then 13. of the 13, it is "Partes" that is the big difference. It has one list of 9. This list of 9 has "Finalidade" which is a not-small character vector, Itens (a list), Evento (a list) and 4 other lists. one of them Anexo, is a list of lists.
