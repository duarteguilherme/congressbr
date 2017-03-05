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
#' @param wide \code{logical}. Default is TRUE, which returns a dataframe of one
#' row with details on the Senator. When FALSE, returns a two-column dataframe with
#' the variables (column names when TRUE) in the first column and the values in the
#' second.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' romario <- sen_senator_positions(5322)
#' head(romario)
#'
#' @export
sen_senator_positions <- function(code = 0, active = "both", wide = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/"

  # param checks

  if(is.null(code)){
    stop("'code' is necessary.")
  } else if(!is.null(code) & !is.numeric(code)){
    stop("'code' must be an integer.")
  }

  if(active == "yes"){
    request <- httr::GET(paste0(base_url, code, "/cargos?indAtivos=s"))
  } else if(active == "no"){
    request <- httr::GET(paste0(base_url, code, "/cargos?indAtivos=n"))
  } else if(active == "both"){
    request <- httr::GET(paste0(base_url, code, "/cargos"))
  } else if(!is.null(active) & active != "yes" & active != "no"){
    return(stop("'active' must be 'both', 'yes' or 'no'"))
  }

  # status checks
  if(request$status_code != 200){
    stop("GET request failed")
  } else{
    request <- httr::content(request, "parsed")
  }
  if(length(request$CargoParlamentar) < 4){
    stop("Either this Senator has not held any congressional positions or the code you entered is incorrect.")
  }

    request <- request$CargoParlamentar$Parlamentar
    request$UrlGlossario <- NULL

    req <- rmNullObs(request)

    req_P <- dplyr::as_data_frame(req$IdentificacaoParlamentar)

    req <- req$Cargos
    for(i in 1:length(req$Cargo)){
      names(req$Cargo)[i] <- paste0("Cargo_", i)
    }
    req <- as.data.frame(purrr::flatten(req))
    req <- cbind(req_P, req)
    if(wide == FALSE){
      req <- as.data.frame(t(req))
      req$Variable <- row.names(req)
      colnames(req)[1] <- "Value"
      req <- dplyr::select(req, Variable, Value)
      row.names(req) <- NULL
      req <- dplyr::as_data_frame(req)
      return(req)
    } else{
      req <- dplyr::as_data_frame(req)
      return(req)
    }
}
