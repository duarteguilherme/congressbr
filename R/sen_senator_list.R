#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom lubridate parse_date_time
#' @importFrom data.table rbindlist
#' @importFrom dplyr as_data_frame
#' @import util
#' @title Downloads and tidies information on the Senators in the Federal Senate.
#' @param present \code{logical}. If TRUE, downloads data on the legislature
#' currently sitting in the Federal Senate.
#' @param start two-digit integer representing the legislature.
#' @param end two-digit integer representing the legislature.
#' @param state two-letter abbreviation of Brazilian state. A list of these is
#' available with the function \code{UF()}.
#' @param status \code{character}, either "T" or "S", representing
#' \emph{titular} or \emph{suplente} (stand-in senator), respectively.
#' @param serving
#' @param withdrawn
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note
#' @author Robert Myles McDonnell & Guilherme Jardim Duarte.
#' @examples
#'
#'
#' @export
sen_senator_list <- function(present, start, end, state,
                     status, serving, withdrawn = FALSE){

  # start is the number of the legislature (53); end likewise
  # serving is "S" (yes); "N", (nao). I think no refers to those elected who haven't entered yet (exercicio)
  # status is "T" (titular); "S", suplente (participacao)
  # legislatures: list of legis and number, with first available

  # base url
  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/lista/"

  # checks
  '%ni%' <- Negate('%in%')
  if(present == FALSE & withdrawn == FALSE & is.null(start) &
     is.null(end) & is.null(state) & is.null(status) & is.null(serving)){
    return(message("Error: no valid parameters to function call."))
  }
  if(!is.null(serving) & serving %ni% c("yes", "no")){
    return(message("Error: 'serving' must be equal to either 'yes' or 'no'."))
  }

  if(length(start) > 2 | length(end) > 2){
    return(message("Error: 'start' and/or 'end' must be two digit numbers"))
  }
  if(present == TRUE & is.null(state) & is.null(status)){
    request <- httr::GET(paste0(base_url, "atual"))
  } else if(present == TRUE & !is.null(state) & is.null(status)){
    state <- tolower(state)
    request <- httr::GET(paste0(base_url, "atual?uf=", state))
  } else if(present == TRUE & is.null(state) & !is.null(status)){
    if(status %ni% c("T", "S")){
      return(message("Error: status must be either 'T' or 'S'."))
    } else{
      status <- tolower(status)
      request <- httr::GET(paste0(base_url, "atual?participacao=", status))
      }
  } else if(present == TRUE & !is.null(state) & !is.null(status)){
    state <- tolower(state)
    status <- tolower(status)
    request <- httr::GET(paste0(base_url, "atual?uf=", state,
                                "&participacao=", status))
  } else if(withdrawn == TRUE){
    if(is.null(state) & is.null(status) & present==F & is.null(start)
       & is.null(end) & is.null(serving)){
      request <- httr::GET(paste0(base_url, "afastados"))
    }
  } else if(!is.null(start)){
    if(is.null(end) & is.null(state) & is.null(status) & is.null(serving)){
      request <- httr::GET(paste0(base_url, "legislatura/", start))
    } else if(!is.null(end) & is.null(state) & is.null(status) &
              is.null(serving)){
      request <- httr::GET(paste0(base_url, "legislatura/", start,
                                  "/", end))
    } else if(!is.null(end) & !is.null(state) & is.null(status) &
              is.null(serving)){
      state <- tolower(state)
      request <- httr::GET(paste0(base_url, "legislatura/", start,
                                  "/", end, "?uf=", state))
    } else if(!is.null(end) & !is.null(state) & !is.null(status) &
              is.null(serving)){
      state <- tolower(state)
      status <- tolower(status)
      request <- httr::GET(paste0(base_url, "legislatura/", start,
                                  "/", end, "?uf=", state,
                                  "&participacao=", status))
    } else if(!is.null(end) & !is.null(state) & !is.null(status) &
              !is.null(serving)){
      state <- tolower(state)
      status <- tolower(status)
      serving <- ifelse(serving == "yes", "S", "N")
      request <- httr::GET(paste0(base_url, "legislatura/", start,
                                  "/", end, "?uf=", state,
                                  "&participacao=", status,
                                  "&exercicio=", serving))
    }
  }


  # status checks
  if(request$status_code != 200){
    return(message("Error: GET request failed"))
  } else{
    request <- httr::content(request, "parsed")
  }




  GET /senador/lista/tiposPronunciamento  ## ??? maybe do with list of UFs


}

#' @export
sen_parties <- function(){
  GET /senador/partidos
}










