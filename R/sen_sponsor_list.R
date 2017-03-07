#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom stringi stri_trans_general
#' @importFrom data.table rbindlist
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies data on the bill sponsors in the Federal Senate.
#' @param ascii (\code{logical}). If TRUE, bill sponsor names are converted
#' to ascii format, stripping the latin characters from the names.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame},
#' with variables:
#' \itemize{
#'  \item{\code{request_date: }}{\code{POSIXct}, date and time the request was made.}
#'  \item{\code{sponsor_name: }}{identity of the bill sponsor.}
#'  \item{\code{sponsor_code: }}{code for senator bill sponsors.}
#'  \item{\code{title: }}{title of bill sponsor.}
#'  \item{\code{party: }}{party of bill sponsor.}
#'  \item{\code{state: }}{state of bill sponsor.}
#'  \item{\code{quantity: }}{quantity of bills sponsored by this sponsor.}
#' }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' bills <- sen_bill_sponsors()
#' @export
sen_bill_sponsors <- function(ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/autor/lista/atual"

  request <- httr::GET(base_url)

  # status checks
  request <- status(request)

  request <- request$ListaAutores$Totais$Parlamentares
  request <- rmNullObs(request)

  for(z in 1:length(request)){
    request[[z]] <- as.data.frame(request[[z]], stringsAsFactors = F)
  }

  req <- data.table::rbindlist(request, fill = TRUE)

  if(ascii == TRUE){
    result <- dplyr::data_frame(
      sponsor_name = stringi::stri_trans_general(req$NomeAutor, "Latin-ASCII"),
      sponsor_code = req$CodigoParlamentar,
      title = req$Tratamento,
      party = req$Partido,
      state = req$Uf,
      quantity = as.numeric(req$Quantidade)
    )
    result$party <- ifelse(result$party == "S/Partido", "Independent",
                           result$party)
    return(result)
  } else {
    result <- dplyr::data_frame(
      sponsor_name = req$NomeAutor,
      sponsor_code = req$CodigoParlamentar,
      title = req$Tratamento,
      party = req$Partido,
      state = req$Uf,
      quantity = as.numeric(req$Quantidade)
    )
    result$party <- ifelse(result$party == "S/Partido", "Independent",
                           result$party)
    return(result)
  }
}



#' @title Downloads and tidies data on the types ofbill sponsors in the
#' Federal Senate.
#' @param ascii (\code{logical}). If TRUE, bill sponsor names are converted
#' to ascii format, stripping the latin characters from the names.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' types <- sen_sponsor_types()
#' @export
sen_sponsor_types <- function(ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/autor/tiposAutor"

  request <- httr::GET(base_url)

  # status checks
  request <- status(request)

  request <- request$ListaTiposAutor$TiposAutor$TipoAutor
  request <- rmNullObs(request)

  for(z in 1:length(request)){
    request[[z]] <- as.data.frame(request[[z]], stringsAsFactors = F)
  }

  req <- data.table::rbindlist(request, fill = TRUE)
  req <- dplyr::as_data_frame(req)
  colnames(req) <- c("Abbreviation", "Description", "Date_created_in_database")
  req$Date_created_in_database <- lubridate::parse_date_time(
    req$Date_created_in_database, orders = "Ymd"
  )
  if(ascii == TRUE){
    req$Abbreviation <- stringi::stri_trans_general(req$Abbreviation,
                                                    "Latin-ASCII")
    req$Description <- stringi::stri_trans_general(req$Description,
                                                   "Latin-ASCII")
    return(req)
  } else{
    return(req)
  }
}
