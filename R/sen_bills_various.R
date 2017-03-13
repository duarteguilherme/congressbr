#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom dplyr data_frame
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Downloads and tidies information on the subtypes of legislation in
#' the Federal Senate.
#' @param active \code{character}. Options are "Yes", "No" or \code{NULL}, the
#' default. "Yes" returns active subtypes, "No" returns inactive subtypes,
#' while the default returns both.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_subtypes()
#' @export
sen_bills_subtypes <- function(active = NULL, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/subtipos"
  if(active == "Yes"){
    base_url <- base_url %p% "?indAtivos=S"
  } else if(active == "No"){
    base_url <- base_url %p% "?indAtivos=N"
  }
  request <- httr::GET(base_url)
  request <- status(request)
  subtypes <- request$ListaSubtiposMateria$SubtiposMateria$SubtipoMateria

  subs <- dplyr::data_frame(
    subtype_abbr = purrr::map_chr(subtypes, "SiglaMateria", .null = NA),
    subtype_description = purrr::map_chr(subtypes,
                                         "DescricaoSubtipoMateria", .null = NA),
    subtype_date_created = purrr::map_chr(subtypes, "DataCriacao", .null = NA)
    )
  subs$subtype_date_created <- lubridate::parse_date_time(
    subs$subtype_date_created, orders = "Ymd"
  )
  if(ascii == TRUE){
    subs <- subs %>%
      dplyr::mutate(subtype_description = stringi::stri_trans_general(
        subtype_description, "Latin-ASCII"
      ))
  }
  return(subs)
}



#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom dplyr data_frame
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Downloads and tidies information on the types of deadline and time
#' limits for legislation in the Federal Senate.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_limits()
#' @export
sen_bills_limits <- function(ascii = TRUE){
  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/tiposPrazo"
  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaTiposPrazo$TiposPrazo$TipoPrazo

  lims <- dplyr::data_frame(
    limit_code = purrr::map_chr(request, "CodigoTipoPrazo", .null = NA),
    limit_description = purrr::map_chr(request, "DescricaoTipoPrazo",
                                       .null = NA)
  )
  if(ascii == TRUE){
    lims <- lims %>%
      dplyr::mutate(limit_description = stringi::stri_trans_general(
        limit_description, "Latin-ASCII"))
  }
  return(lims)
}




#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom dplyr data_frame
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Downloads and tidies information on the topics of legislation in
#' the Federal Senate.
#' @param active \code{character}. Options are "Yes", "No" or \code{NULL}, the
#' default. "Yes" returns active subtypes, "No" returns inactive subtypes,
#' while the default returns both.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_topics()
#' @export
sen_bills_topics <- function(active = NULL, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/assuntos"
  if(active == "Yes"){
    base_url <- base_url %p% "?indAtivos=S"
  } else if(active == "No"){
    base_url <- base_url %p% "?indAtivos=N"
  }
  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaAssuntos$Assuntos$Assunto

  themes <- dplyr::data_frame(
    topic_code = purrr::map_chr(request, "Codigo"),
    topic_general = purrr::map_chr(request, "AssuntoGeral"),
    topic_specific = purrr::map_chr(request, "AssuntoEspecifico")
  )
  if(ascii == TRUE){
    themes <- themes %>%
      dplyr::mutate(
        topic_general = stringi::stri_trans_general(
          topic_general, "Latin-ASCII"),
        topic_specific = stringi::stri_trans_general(
          topic_specific, "Latin-ASCII"))
  }
  return(themes)
}




#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom dplyr data_frame
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Downloads and tidies information on legislation that is under
#' consideration in the Federal Senate.
#' @param year Format YYYY. Returns legislation from this year.
#' @param date date requested, format YYYYMMDD. Returns information on
#' legislation on or after this time.
#' @param time Format hour-minute-second (HHmmSS). Returns information on
#' legislation on or after this time.
#' @param number bill number.
#' @param type type of legislation.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_passing()
#' # All MPVs (provisional presidential decrees) in 2001:
#' mpvs <- sen_bills_passing(year = "2001", type = "MPV")
#' # Bills from a certain date:
#' march01_2017 <- sen_bills_passing(date = 20170301)
#' @export
sen_bills_passing <- function(year = NULL,  number = NULL,
                              type = NULL, date = NULL,
                              time = NULL){

  if(!is.null(year)){
    if(nchar(year) > 4){
      stop("Please enter a valid year. Format is YYYY.")
    }
  }
  if(!is.null(time) & is.null(date)){
    stop("'time' can only be used with a 'date' argument.")
  }

  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/tramitando?"

  if(!is.null(year)){
    base_url <- base_url %p% "ano=" %p% year
  }
  if(!is.null(number)){
    base_url <- base_url %p% "&numero=" %p% number
  }
  if(!is.null(type)){
    base_url <- base_url %p% "&sigla=" %p% type
  }
  if(!is.null(date)){
    base_url <- base_url %p% "&data=" %p% date
  }
  if(!is.null(time)){
    base_url <- base_url %p% "&hora=" %p% time
  }

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaMateriasTramitando$Materias$Materia
  request <- purrr::map(request, "IdentificacaoMateria")

  pass <- dplyr::data_frame(
    bill_id = purrr::map_chr(request, "CodigoMateria", .null = NA),
    bill_number = purrr::map_chr(request, "NumeroMateria", .null = NA),
    bill_year = purrr::map_chr(request, "AnoMateria", .null = NA),
    bill_type = purrr::map_chr(request, "SiglaSubtipoMateria", .null = NA)
  )
  return(pass)
}




#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom dplyr data_frame
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Downloads and tidies information on legislation from the current
#' legislature of the Federal Senate.
#' @param year Format YYYY. Returns legislation from this year.
#' @param date date requested, format YYYYMMDD. Returns information on
#' legislation on or after this time.
#' @param time Format hour-minute-second (HHmmSS). Returns information on
#' legislation on or after this time.
#' @param number bill number.
#' @param type type of legislation.
#' @param passing is the bill currently under consideration in the Senate?
#' Possible values are \code{NULL} (the default), "Yes" or "No". \code{NULL}
#' returns the information obtained from "Yes" and "No".
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' all <- sen_bills_current()
#' plc_2015 <- sen_bills_current(year = 2015, type = "PLS")
#' @export
sen_bills_current <- function(year = NULL, date = NULL,
                              time = NULL, number = NULL,
                              type = NULL, passing = NULL){

  if(!is.null(time) & is.null(date)){
    stop("'time' can only be used with a 'date' argument.")
  }
  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/legislaturaatual?"

  if(!is.null(year)){
    if(nchar(year) != 4){
      stop("Please enter a valid year, format is YYYY.")
    }
    base_url <- base_url %p% "ano=" %p% year
  }
  if(!is.null(date)){
    base_url <- base_url %p% "&data=" %p% date
  }
  if(!is.null(time)){
    base_url <- base_url %p% "&hora=" %p% time
  }
  if(!is.null(number)){
    base_url <- base_url %p% "&numero=" %p% number
  }
  if(!is.null(type)){
    base_url <- base_url %p% "&sigla=" %p% type
  }
  if(!is.null(passing)){
    if(passing == "Yes"){
      passing <- "S"
    } else{
      passing <- "N"
    }
    base_url <- base_url %p% "&tramitando=" %p% passing
  }


  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaMateriasLegislaturaAtual$Materias$Materia
  request <- purrr::map(request, "IdentificacaoMateria")

  bills <- dplyr::data_frame(
    bill_id = purrr::map_chr(request, "CodigoMateria", .null = NA),
    bill_number = purrr::map_chr(request, "NumeroMateria", .null = NA),
    bill_year = purrr::map_chr(request, "AnoMateria", .null = NA),
    bill_type = purrr::map_chr(request, "SiglaSubtipoMateria", .null = NA)
  )
  return(bills)
}
