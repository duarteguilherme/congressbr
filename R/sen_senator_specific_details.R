#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom dplyr as_data_frame
#' @title Downloads information on positions a certain senator has held in
#' the Federal Senate.
#' @param code \code{integer}.This number represents the code of the senator
#' you wish to get information on. These codes can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'CodigoParlamentar'.
#' @param active \code{character}. Possible values are "yes" or "no". If left
#' blank, returns all the positions held by the senator. If "yes", just active
#' positions, if "no", finished positions.
#' @param wide \code{logical}. Default is TRUE, which returns a dataframe of one
#' row with details on the senator. When FALSE, returns a two-column dataframe
#' with the variables (column names when TRUE) in the first column and the values
#'  in the second. This may be useful in some cases because the wide dataframe
#'  can have many columns, as the API stores this information in \emph{very}
#'  deeply nested lists.
#' @param list \code{logical}. If TRUE, returns the deeply nested list object
#' directly from the API. \code{wide} will be set to FALSE automatically
#' when this option is selected.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' romario <- sen_senator_positions(code = 5322)
#' head(romario)
#'
#' @export
sen_senator_positions <- function(code = 0, active = "both",
                                  wide = TRUE, list = FALSE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/"

  # param checks

  if(is.null(code)){
    stop("'code' is necessary.")
  } else if(!is.null(code) & !is.numeric(code)){
    stop("'code' must be an integer.")
  }

  if(wide == TRUE & list == TRUE){
    wide <- FALSE
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

    if(list == TRUE){
      return(req)
    }

    if(wide == FALSE){
      req <- as.data.frame(t(as.data.frame(purrr::flatten(req),
                                           stringsAsFactors = F)),
                           stringsAsFactors = F)
      req$Variable <- row.names(req)
      colnames(req)[1] <- "Value"
      req <- dplyr::select(req, Variable, Value)
      row.names(req) <- NULL
      req <- dplyr::as_data_frame(req)
      return(req)
    } else{
      req <- as.data.frame(purrr::flatten(req), stringsAsFactors = F)
      return(req)
    }
}


#' @title Get party affiliations of a senator.
#' @param code \code{integer}. This number represents the code of the senator
#' you wish to get information on. These codes can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'CodigoParlamentar'.
#' @param wide \code{logical}. Default is TRUE, which returns a dataframe of one
#' row with details on the senator. When FALSE, returns a two-column dataframe
#' with the variables (column names when TRUE) in the first column and the values
#'  in the second. This may be useful in some cases because the wide dataframe
#'  can have many columns, as the API stores this information in \emph{very}
#'  deeply nested lists.
#' @param list \code{logical}. If TRUE, returns the deeply nested list object
#' directly from the API. \code{wide} will be set to FALSE automatically
#' when this option is selected.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' dias <- sen_senator_affiliations(code = 945)
#' @export
sen_senator_affiliations <- function(code = 0, wide = TRUE,
                                     list = FALSE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/"

  # param checks
  if(is.null(code)){
    stop("'code' is necessary.")
  } else if(!is.null(code) & !is.numeric(code)){
    stop("'code' must be an integer.")
  }
  if(wide == TRUE & list == TRUE){
    wide <- FALSE
  }
  request <- httr::GET(paste0(base_url, code, "/filiacoes"))
  # status checks
  if(request$status_code != 200){
    stop("GET request failed")
  } else{
    request <- httr::content(request, "parsed")
  }
  request <- request$FiliacaoParlamentar$Parlamentar
  request$UrlGlossario <- NULL
  req <- rmNullObs(request)
  if(wide == TRUE){
    req <- as.data.frame(purrr::flatten(req),
                         stringsAsFactors = F)
    req <- dplyr::as_data_frame(req)
    return(req)
  } else if(list == TRUE){
    return(request)
  } else{
    req <- as.data.frame(t(as.data.frame(purrr::flatten(req),
                                         stringsAsFactors = F)),
                         stringsAsFactors = F)
    req$Variable <- row.names(req)
    colnames(req)[1] <- "Value"
    req <- dplyr::select(req, Variable, Value)
    row.names(req) <- NULL
    req <- dplyr::as_data_frame(req)
  }
}

#' @title Get information on the terms served by a senator.
#' @param code \code{integer}. This number represents the code of the senator
#' you wish to get information on. These codes can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'CodigoParlamentar'.
#' @param wide \code{logical}. Default is TRUE, which returns a dataframe of one
#' row with details on the senator. When FALSE, returns a two-column dataframe
#' with the variables (column names when TRUE) in the first column and the values
#'  in the second. This may be useful in some cases because the wide dataframe
#'  can have many columns, as the API stores this information in \emph{very}
#'  deeply nested lists.
#' @param list \code{logical}. If TRUE, returns the deeply nested list object
#' directly from the API. \code{wide} will be set to FALSE automatically
#' when this option is selected.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' lira <- sen_senator_terms(code = 3823)
#' @export
sen_senator_terms <- function(code = 0, wide = TRUE,
                              list = FALSE){
  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/"

  # param checks
  if(is.null(code)){
    stop("'code' is necessary.")
  } else if(!is.null(code) & !is.numeric(code)){
    stop("'code' must be an integer.")
  }
  if(wide == TRUE & list == TRUE){
    wide <- FALSE
  }
  request <- httr::GET(paste0(base_url, code, "/mandatos"))
  # status checks
  if(request$status_code != 200){
    stop("GET request failed")
  } else{
    request <- httr::content(request, "parsed")
  }
  request <- request$MandatoParlamentar$Parlamentar
  request$UrlGlossario <- NULL
  req <- rmNullObs(request)
  if(wide == TRUE){
    req <- as.data.frame(purrr::flatten(req),
                         stringsAsFactors = F)
    req <- dplyr::as_data_frame(req)
    return(req)
  } else if(list == TRUE){
    return(request)
  } else{
    req <- as.data.frame(t(as.data.frame(purrr::flatten(req),
                                         stringsAsFactors = F)),
                         stringsAsFactors = F)
    req$Variable <- row.names(req)
    colnames(req)[1] <- "Value"
    req <- dplyr::select(req, Variable, Value)
    row.names(req) <- NULL
    req <- dplyr::as_data_frame(req)
  }
}



#' @title Get information on the commissions a senator served on.
#' @param code \code{integer}. This number represents the code of the senator
#' you wish to get information on. These codes can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'CodigoParlamentar'.
#' @param commission \code{character}. Abbreviation of the name of the
#' commission.
#' @param wide \code{logical}. Default is TRUE, which returns a dataframe of one
#' row with details on the senator. When FALSE, returns a two-column dataframe
#' with the variables (column names when TRUE) in the first column and the values
#'  in the second. This may be useful in some cases because the wide dataframe
#'  can have many columns, as the API stores this information in \emph{very}
#'  deeply nested lists.
#' @param list \code{logical}. If TRUE, returns the deeply nested list object
#' directly from the API. \code{wide} will be set to FALSE automatically
#' when this option is selected.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' lira <- sen_senator_
#' @export
#sen_senator_comms <- function()


