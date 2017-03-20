#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr flatten
#' @importFrom purrr map_chr
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @title Returns results from the plenary in the Federal Senate for a
#' specified date.
#' @param date \code{character}. Format YYYYMMDD.
#' @param ascii \code{logical}. If \code{TRUE}, the default, strips Latin
#' characters from the results.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' # get info for the 3rd of March 2016:
#' x <- sen_plenary_result(date = "20160303")
#' # Earlier periods may not have information:
#' \dontrun{
#' y <- sen_plenary_result(date = "19910105")
#' }
#' @export
sen_plenary_result <- function(date = NULL, ascii = TRUE){

  year_now <- Sys.Date() %>% gsub("-", "", .) %>% gsub("[0-9]{4}$", "", .) %>%
    as.numeric()
  year_func <- gsub("[0-9]{4}$", "", date) %>% as.numeric()

  if(is.null(date) || nchar(date) < 8 ||  year_func > year_now){
    stop("Please enter a valid date. Format is YYYYMMDD.")
  }

  base_url <- "http://legis.senado.gov.br/dadosabertos/plenario/resultado/" %p%
    date

  request <- httr::GET(base_url)
  request <- status(request)
  N <- NA_character_

  if(depth(request) > 7){
    request <- request$ResultadoPlenario$Sessoes$Sessao
    for(x in 1:length(request)){
      for(z in 1:length(request[[x]]$Itens$Item)){
        request[[x]]$Itens$Item[[z]]$session_id = request[[x]]$codigoSessao
      }
    }
  } else if(depth(request) < 4){
    stop("No data match your search.")
  } else{
    request <- request$ResultadoPlenario$Sessoes
    for(z in 1:length(request$Sessao$Itens$Item)){
        request$Sessao$Itens$Item[[z]]$session_id = request$Sessao$codigoSessao
      }
  }

  items <- purrr::map(request, "Itens") %>% purrr::flatten() %>%
    purrr::flatten()

  req <- tibble::tibble(
    session_id = purrr::map_chr(request, .null = N, "codigoSessao"),
    session_number = purrr::map_chr(request, .null = N, "numeroSessao"),
    session_date = purrr::map_chr(request, .null = N, "dataSessao"),
    session_time = purrr::map_chr(request, .null = N, "horaSessao"),
    session_type = purrr::map_chr(request, .null = N, "descricaoTipoSessao"),
    session_type_abbr = purrr::map_chr(request, .null = N, "tipoSessao"),
    session_house = purrr::map_chr(request, .null = N, "siglaCasa")
  )

  req <- req %>%
    dplyr::mutate(
      session_date = lubridate::parse_date_time(
        session_date, "Ymd"
      )
    )
  if(ascii == TRUE){
    req <- req %>%
      dplyr::mutate(
        session_type = stringi::stri_trans_general(
          session_type, "Latin-ASCII"
        )
      )
  }

  item <- tibble::tibble(
    session_id = purrr::map_chr(items, .null = N, "session_id"),
    bill_id = purrr::map_chr(items, .null = N, "codigoMateria"),
    bill = purrr::map_chr(items, .null = N, "identificacao"),
    bill_house = purrr::map_chr(items, .null = N, "siglaCasaMateria"),
    bill_type = purrr::map_chr(items, .null = N, "siglaMateria"),
    bill_number = purrr::map_chr(items, .null = N, "numeroMateria"),
    bill_year = purrr::map_chr(items, .null = N, "anoMateria"),
    bill_report = purrr::map_chr(items, .null = N, "parecer"),
    bill_details = purrr::map_chr(items, .null = N, "ementaPapeleta"),
    bill_result = purrr::map_chr(items, .null = N, "textoResultado"),
    bill_sponsor = purrr::map_chr(items, .null = N, "autorMateria")
  )

  if(ascii == TRUE){
    item <- item %>%
      dplyr::mutate(
        bill = stringi::stri_trans_general(
          bill, "Latin-ASCII"),
        bill_report = stringi::stri_trans_general(
          bill_report, "Latin-ASCII"),
        bill_details = stringi::stri_trans_general(
          bill_details, "Latin-ASCII"),
        bill_result = stringi::stri_trans_general(
          bill_result, "Latin-ASCII"),
        bill_sponsor = stringi::stri_trans_general(
          bill_sponsor, "Latin-ASCII")
      )
  }
  result <- suppressMessages(dplyr::full_join(req, item))
  return(result)
}




#' @title Returns results from the plenary in the Federal Senate for a
#' specified date.
#' @param active . Default "Yes", which returns session types which are
#' still used. Otherwise, returns all types of sessions.
#' @param ascii \code{logical}. If \code{TRUE}, the default, strips Latin
#' characters from the results.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sessions <- sen_plenary_sessions()
#' @export
sen_plenary_sessions <- function(active = c("Yes", "No"), ascii = TRUE){

  act <- match.arg(active)
  if(act == "Yes"){
    base_url <- "http://legis.senado.gov.br/dadosabertos/plenario/tiposSessao"
  } else{
    base_url <- "http://legis.senado.gov.br/dadosabertos/plenario/tiposSessao" %p%
      "?indAtivos=N"
  }


  request <- httr::GET(base_url)
  request <- status(request)
  N <- NA_character_
  request <- request$ListaTiposSessao$TiposSessao$TipoSessao

  req <- tibble::tibble(
    session_id = purrr::map_chr(request, "Codigo", .null = N),
    session_house = purrr::map_chr(request, "SiglaCasa", .null = N),
    session_description = purrr::map_chr(request, "Descricao",
                                         .null = N)
  )

  if(ascii == TRUE){
    req <- req %>%
      dplyr::mutate(
        session_description = stringi::stri_trans_general(
          session_description, "Latin-ASCII"
        )
      )
  }

  return(req)
}




#' @title Returns results from the plenary in the Federal Senate for a
#' specified date.
#' @param period \code{character}. If "month" is selected, all information
#' available from the date specified with \code{date} to the end of the month
#' is returned. Otherwise, information returned from the day of \code{date} only
#' is returned.
#' @param date \code{character}. Format YYYYMMDD.
#' @param ascii \code{logical}. If \code{TRUE}, the default, strips Latin
#' characters from the results.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' # get info from the second half of March 2014:
#' sessions <- sen_plenary_agenda(period = "month", date = "20140315")
#' # or from the first of April 2016:
#' sessions <- sen_plenary_agenda(period = "day", date = "20160401")
#' @export
sen_plenary_agenda <- function(period = c("month", "day"),
                               date = NULL, ascii = TRUE){

  if(is.null(date)){
    stop("Please enter a valid date. Format is YYYYMMDD.")
  }

  per <- match.arg(period)
  if(per == "month"){
    base_url <- "http://legis.senado.gov.br/dadosabertos/plenario/agenda/mes/" %p%
      date
    request <- httr::GET(base_url)
    request <- status(request)
    request <- request$AgendaPlenario$Sessoes$Sessao
  } else if(per == "day"){
    base_url <- "http://legis.senado.gov.br/dadosabertos/plenario/agenda/dia/" %p%
      date
    request <- httr::GET(base_url)
    request <- status(request)
    request <- request$PautaPlenario$Sessoes
  } else{
    stop("'period' can be 'month' or 'day'.")
  }

  N <- NA_character_
  event <- purrr::map(request, "Evento", .null = N)

  req <- tibble::tibble(
    date = purrr::map_chr(request, .null = N, "Data"),
    time = purrr::map_chr(request, .null = N, "Hora"),
    session_number = purrr::map_chr(request, .null = N,
                                                   "NumeroSessao"),
    session_type = purrr::map_chr(request, .null = N,
                                                 "TipoSessao"),
    house = purrr::map_chr(request, .null = N, "Casa"),
    legislature = purrr::map_chr(request, .null = N,
                                                "Legislatura")
  )

  if(length(event) > 1){
    req <- req %>%
      dplyr::mutate(
        event_type = purrr::map_chr(event, .null = N,
                                    "DescricaoTipoEvento"),
        event_description = purrr::map_chr(event, .null = N, "DescricaoEvento")
      )
    if(ascii == TRUE){
      req <- req %>%
        dplyr::mutate(
          event_type = stringi::stri_trans_general(
            event_type, "Latin-ASCII"),
          event_description = stringi::stri_trans_general(
            event_description, "Latin-ASCII")
        )
    }
  }

  req <- req %>%
    dplyr::mutate(
      date = lubridate::parse_date_time(
        date, "Ymd"
      )
    )

  if(ascii == TRUE){
    req <- req %>%
      dplyr::mutate(
        session_number = stringi::stri_trans_general(
          session_number, "Latin-ASCII"),
        session_type = stringi::stri_trans_general(
          session_type, "Latin-ASCII")
        )
  }
  return(req)
}



