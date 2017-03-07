#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom purrr compact
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies information on the senators in the Federal Senate.
#' @param present \code{logical}. If \code{TRUE}, downloads data on the legislature
#' currently sitting in the Federal Senate.
#' @param start two-digit integer representing the first legislature of the
#' time period requested.
#' @param end two-digit integer representing the final legislature of the time
#'  period requested.
#' @param state two-letter abbreviation of Brazilian state. A list of these is
#' available with the function \code{UF()}.
#' @param status \code{character}, either "T" or "S", representing
#' \emph{titular} or \emph{suplente} (stand-in senator), respectively.
#' @param serving is the Senator currently serving his/her mandate? Options are
#'  "yes" or "no". "no" returns information on senators who have been elected but
#'  who have not yet entered office.
#' @param withdrawn \code{logical}. If TRUE, returns information on senators who
#' were elected but who are not serving (due to health reasons, for example).
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#'
#'
#' @export
sen_senator_list <- function(present = TRUE, start = NULL, end = NULL,
                             state = NULL, status = NULL, serving = "yes",
                             withdrawn = FALSE){

  # base url
  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/lista/"

  # checks
  '%ni%' <- Negate('%in%')
  if(present == FALSE & withdrawn == FALSE & is.null(start) &
     is.null(end) & is.null(state) & is.null(status) & is.null(serving)){
    stop("No valid parameters to function call.")
  }
  if(!is.null(serving) & serving %ni% c("yes", "no")){
    stop("'serving' must be equal to either 'yes' or 'no'.")
  }

  if(length(start) > 2 | length(end) > 2){
    stop("'start' and/or 'end' must be two digit numbers")
  }
  if(present == TRUE & is.null(state) & is.null(status)){
    request <- httr::GET(paste0(base_url, "atual"))
  } else if(present == TRUE & !is.null(state) & is.null(status)){
    state <- tolower(state)
    request <- httr::GET(paste0(base_url, "atual?uf=", state))
  } else if(present == TRUE & is.null(state) & !is.null(status)){
    if(status %ni% c("T", "S")){
      stop("Status must be either 'T' or 'S'.")
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
  request <- status(request)

  request <- request$ListaParlamentarEmExercicio$Parlamentares$Parlamentar

  req <- purrr::compact(request)

  if(length(req) == 0){
    stop("No data matches your request.")
  }

  for(z in 1:length(req)){
    req[[z]][[3]] <- NULL
  }

  for(z in 1:length(req)){
    req[[z]][["Mandato"]] <- as.data.frame(req[[z]][["Mandato"]],
                                           stringsAsFactors = F)
  }
  parl <- purrr::map_df(req, "IdentificacaoParlamentar")
  mand <- purrr::map_df(req, "Mandato")

  req <- cbind(parl, mand)
  if(length(grep("UfParlamentar", colnames(req))) > 1){
    x <- grep("UfParlamentar", colnames(req))
    req <- req[, -x[2]]
  }
  req <- dplyr::as_data_frame(req)
  return(req)
}
