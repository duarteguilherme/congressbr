#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr flatten
#' @importFrom lubridate parse_date_time
#' @importFrom data.table rbindlist
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies information on the Senators in the Federal Senate.
#' @param code \code{integer}. This number represents the code of the senator
#' you wish to get information on. These codes can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'CodigoParlamentar'.
#' @param wide \code{logical}. Default is TRUE, which returns a one-row data
#' frame. FALSE returns a two-column dataframe with the variables (column names
#' when TRUE) in the first column and the values in the second. This may be
#' useful in some cases because the wide dataframe can have many columns, as
#' the API stores this information in \emph{very} deeply nested lists.
#' @param list \code{logical}. If TRUE, returns the deeply nested list directly
#' from the API. \code{wide} will be set to FALSE automatically
#' when this option is selected.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso sen_senator_list
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' Acir_G <- sen_senator_details(code = 4981)
#'
#' @export
sen_senator_details <- function(code = 0, wide = TRUE, list = FALSE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/"

  # param checks
  if(wide == TRUE & list == TRUE){
    wide <- FALSE
  }
  if(is.null(code)){
    return(message("'code' is necessary."))
  } else if(!is.null(code) & !is.numeric(code)){
    stop("'code' must be an integer.")
  } else {
    request <- httr::GET(paste0(base_url, code))
  }
  # status checks
  if(request$status_code != 200){
    stop("GET request failed")
  } else{
    request <- httr::content(request, "parsed")
  }

  request <- request$DetalheParlamentar$Parlamentar
  request$UrlGlossario <- NULL
  req <- rmNullObs(request)

  if(list == TRUE){
    return(req)
  }

  if(wide == FALSE){
    req <- as.data.frame(t(as.data.frame(purrr::flatten(request))))
    req$Variable <- row.names(req)
    colnames(req)[1] <- "Value"
    req <- dplyr::select(req, Variable, Value)
    row.names(req) <- NULL
    req <- dplyr::as_data_frame(req)
    return(req)
  } else{
    req <- as.data.frame(purrr::flatten(request))
  }
}
