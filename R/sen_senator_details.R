#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom lubridate parse_date_time
#' @importFrom data.table rbindlist
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies information on the Senators in the Federal Senate.
#' @param code (\code{integer}). This number represents the code of the senator
#' you wish to get information on. These codes can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'CodigoParlamentar'.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso sen_senator_list
#' @author Robert Myles McDonnell & Guilherme Jardim Duarte.
#' @examples
#' Acir_G <- sen_senator_details(code = 4981)
#'
#' @export
sen_senator_details <- function(code = 0){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/"

  # param checks
  if(is.null(code)){
    return(message("Error: 'code' is necessary."))
  } else if(!is.null(code) & !is.numeric(code)){
    return(message("Error: 'code' must be an integer."))
  } else {
    request <- httr::GET(paste0(base_url, code))
  }
  # status checks
  if(request$status_code != 200){
    return(message("Error: GET request failed"))
  } else{
    request <- httr::content(request, "parsed")
  }

  request <- request$DetalheParlamentar$Parlamentar
  request$UrlGlossario <- NULL
  req <- as.data.frame(t(as.data.frame(purrr::flatten(request))))
  req$Variable <- row.names(req)
  colnames(req)[1] <- "Value"
  req <- dplyr::select(req, Variable, Value)
  row.names(req) <- NULL
  req <- dplyr::as_data_frame(req)
  return(req)
}
