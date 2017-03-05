#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies specific information related to Senators in
#' the Federal Senate.
#' @param code \code{integer}
#' @param active \code{character}. Possible values are "yes" or "no". If left
#' blank, returns all the positions held by the Senator. If "yes", just active
#' positions, if "no", finished positions.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell & Guilherme Jardim Duarte.
#' @examples
#'
#'
#'
#' @export
sen_senator_positions <- function(code = 0, active = "both"){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/"

  # param checks

  if(is.null(code)){
    return(message("Error: 'code' is necessary."))
  } else if(!is.null(code) & !is.numeric(code)){
    return(message("Error: 'code' must be an integer."))
  }

  if(active == "yes"){
    request <- httr::GET(paste0(base_url, code, "/cargos?indAtivos=s"))
  } else if(active == "no"){
    request <- httr::GET(paste0(base_url, code, "/cargos?indAtivos=n"))
  } else if(active == "both"){
    request <- httr::GET(paste0(base_url, code, "/cargos"))
  } else if(!is.null(active) & active != "yes" & active != "no"){
    return(message("Error: 'active' must be NULL, 'yes' or 'no'"))
  }

  # status checks
  if(request$status_code != 200){
    return(message("Error: GET request failed"))
  } else{
    request <- httr::content(request, "parsed")
  }
  if(length(request) < 4){
    return(message("Error: either this Senator has not held any congressional positions or the code you entered is incorrect."))
  }

    request <- request$CargoParlamentar$Parlamentar
    request$UrlGlossario <- NULL

    req <- rmNullObs(request)

    req_P <- dplyr::as_data_frame(req$IdentificacaoParlamentar)

    ## to do: tidy Cargos

    req <- req$Cargos
    req <- as.data.frame(t(as.data.frame(purrr::flatten(request))))
    req$Variable <- row.names(req)
    colnames(req)[1] <- "Value"
    req <- dplyr::select(req, Variable, Value)
    row.names(req) <- NULL
    req <- dplyr::as_data_frame(req)
    return(req)
}

