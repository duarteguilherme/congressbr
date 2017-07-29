#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @importFrom magrittr '%>%'
#' @importFrom purrr discard
#' @importFrom purrr is_empty
#' @importFrom purrr flatten
#' @title Downloads and tidies information on the types of legislation in
#' the Federal Senate
#' @description Downloads and tidies information on the types of legislation in
#' the Federal Senate.
#' @param active \code{character}. Options are "Yes", "No" or \code{NULL}, the
#' default. "Yes" returns active types, "No" returns inactive types,
#' while the default returns both.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' legis <- sen_bills_types()
#' @export
sen_bills_types <- function(active = NULL, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/subtipos"
  if(!is.null(active)){
    if(active == "Yes"){
      base_url <- base_url %p% "?indAtivos=S"
    } else if(active == "No"){
      base_url <- base_url %p% "?indAtivos=N"
    }
  }

  request <- httr::GET(base_url)
  request <- status(request)
  subtypes <- request$ListaSubtiposMateria$SubtiposMateria$SubtipoMateria

  subs <- tibble::tibble(
    bill_type_abbr = purrr::map_chr(subtypes, "SiglaMateria", .null = NA),
    bill_type_description = purrr::map_chr(subtypes,
                                           "DescricaoSubtipoMateria", .null = NA),
    bill_type_date_created = purrr::map_chr(subtypes, "DataCriacao", .null = NA)
  )
  subs$bill_type_date_created <- lubridate::parse_date_time(
    subs$bill_type_date_created, orders = "Ymd"
  )
  if(isTRUE(ascii)){
    subs <- subs %>%
      dplyr::mutate(bill_type_description = stringi::stri_trans_general(
        bill_type_description, "Latin-ASCII"
      ))
  }
  return(subs)
}



#' @title Downloads and tidies information on the types of deadline and time
#' limits for legislation in the Federal Senate
#' @description Downloads and tidies information on the types of deadline and time
#' limits for legislation in the Federal Senate.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' lims <- sen_bills_limits()
#' @export
sen_bills_limits <- function(ascii = TRUE){
  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/tiposPrazo"
  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaTiposPrazo$TiposPrazo$TipoPrazo

  lims <- tibble::tibble(
    limit_code = purrr::map_chr(request, "CodigoTipoPrazo", .null = NA),
    limit_description = purrr::map_chr(request, "DescricaoTipoPrazo",
                                       .null = NA)
  )
  if(isTRUE(ascii)){
    lims <- lims %>%
      dplyr::mutate(limit_description = stringi::stri_trans_general(
        limit_description, "Latin-ASCII"))
  }
  return(lims)
}



#' @title Downloads and tidies information on the topics of legislation in
#' the Federal Senate
#' @description Downloads and tidies information on the topics of legislation in
#' the Federal Senate.
#' @param active \code{character}. Options are "Yes", "No" or \code{NULL}, the
#' default. "Yes" returns active subtypes, "No" returns inactive subtypes,
#' while the default returns both.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' top <- sen_bills_topics()
#' @export
sen_bills_topics <- function(active = NULL, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/assuntos"
  if(!is.null(active)){
    if(active == "Yes"){
      base_url <- base_url %p% "?indAtivos=S"
    } else if(active == "No"){
      base_url <- base_url %p% "?indAtivos=N"
    }
  }

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaAssuntos$Assuntos$Assunto

  themes <- tibble::tibble(
    topic_code = purrr::map_chr(request, "Codigo"),
    topic_general = purrr::map_chr(request, "AssuntoGeral"),
    topic_specific = purrr::map_chr(request, "AssuntoEspecifico")
  )
  if(isTRUE(ascii)){
    themes <- themes %>%
      dplyr::mutate(
        topic_general = stringi::stri_trans_general(
          topic_general, "Latin-ASCII"),
        topic_specific = stringi::stri_trans_general(
          topic_specific, "Latin-ASCII"))
  }
  return(themes)
}




#' @title Downloads and tidies information on legislation that is under
#' consideration in the Federal Senate
#' @description Downloads and tidies information on legislation that is under
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
#' \dontrun{
#' sen_bills_passing()
#' # All MPVs (provisional presidential decrees) in 2001:
#' mpvs <- sen_bills_passing(year = "2001", type = "MPV")
#' # Bills from a certain date:
#' march01_2017 <- sen_bills_passing(date = 20170301)
#' }
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
  if(purrr::is_empty(request$ListaMateriasTramitando$Materias$Materia)){
    stop("No data match your query.")
  } else{
    request <- request$ListaMateriasTramitando$Materias$Materia
    request <- purrr::map(request, "IdentificacaoMateria")
  }


  pass <- tibble::tibble(
    bill_id = purrr::map_chr(request, "CodigoMateria", .null = NA),
    bill_number = purrr::map_chr(request, "NumeroMateria", .null = NA),
    bill_year = purrr::map_chr(request, "AnoMateria", .null = NA),
    bill_type = purrr::map_chr(request, "SiglaSubtipoMateria", .null = NA)
  )
  return(pass)
}




#' @title Downloads and tidies information on legislation from the current
#' legislature of the Federal Senate
#' @description Downloads and tidies information on legislation from the current
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
#' \dontrun{
#' all <- sen_bills_current()
#' plc_2015 <- sen_bills_current(year = 2015, type = "PLS")
#' }
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

  bills <- tibble::tibble(
    bill_id = purrr::map_chr(request, "CodigoMateria", .null = NA),
    bill_number = purrr::map_chr(request, "NumeroMateria", .null = NA),
    bill_year = purrr::map_chr(request, "AnoMateria", .null = NA),
    bill_type = purrr::map_chr(request, "SiglaSubtipoMateria", .null = NA)
  )
  return(bills)
}





#' @title Downloads and tidies information on legislation from the current
#' legislature of the Federal Senate
#' @description Downloads and tidies information on legislation from the current
#' legislature of the Federal Senate.
#' @param bill_id \code{integer}. This number is the id given to each bill in the
#' Senate database. For example, running \code{sen_bills_current()} will return a
#'  dataframe with the variable \code{bill_id} in the first column. These numbers
#'  can be used as this id.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_status(bill_id = 80406)
#' @export
sen_bills_status <- function(bill_id = NULL, ascii = TRUE){

  if(is.null(bill_id)){
    stop("Please enter a value for 'bill_id'.")
  }
  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/situacaoatual/" %p%
    bill_id

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$SituacaoAtualMateria$Materias
  Null <- NA_character_
  req <- request$Materia$Autuacoes

  stat <- tibble::tibble(
    bill_id = purrr::map_chr(request, "Codigo", .null = Null),
    bill_type = purrr::map_chr(request, "Subtipo", .null = Null),
    bill_number = purrr::map_chr(request, "Numero", .null = Null),
    bill_year = purrr::map_chr(request, "Ano", .null = Null),
    bill_details = purrr::map_chr(request, "Ementa", .null = Null),
    bill_in_passage = purrr::map_chr(request, "Tramitando", .null = Null),
    bill_situation = purrr::map_chr(req, "DescricaoSituacao", .null = Null),
    situation_date = purrr::map_chr(req, "DataSituacao", .null = Null),
    situation_place = purrr::map_chr(req, "NomeLocal", .null = Null)
  )

  stat <- stat %>%
    dplyr::mutate(situation_date = lubridate::parse_date_time(
      situation_date, orders = "Ymd"),
      bill_in_passage = dplyr::case_when(
        grepl("Nao|N\u00a3o", stat$bill_in_passage) ~ "No",
        grepl("Sim", stat$bill_in_passage) ~ "Yes",
        TRUE ~ "Not recorded")
    )

  if(isTRUE(ascii)){
    stat <- stat %>%
      dplyr::mutate(
        bill_details = stringi::stri_trans_general(bill_details,
                                                   "Latin-ASCII"),
        bill_situation = stringi::stri_trans_general(bill_situation,
                                                     "Latin-ASCII"),
        situation_place = stringi::stri_trans_general(situation_place,
                                                      "Latin-ASCII")
      )
  }
  return(stat)
}




#' @title Downloads and tidies information on the possible locations a piece
#' of legislation can currently be passing through the Federal Senate
#' @description Downloads and tidies information on the possible locations a piece
#' of legislation can currently be passing through the Federal Senate.
#' @param active \code{character}. Options are "Yes", "No" or \code{NULL}, the
#' default. "Yes" returns active subtypes, "No" returns inactive subtypes,
#' while the default returns both.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \dontrun{
#' sen_bills_locations()
#' }
#' @export
sen_bills_locations <- function(active = NULL, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/locais"

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaLocais$Locais$Local
  N = NA_character_

  req <- tibble::tibble(
    loc_id = purrr::map_chr(request, "CodigoLocal", .null = N),
    loc_abbr = purrr::map_chr(request, "SiglaLocal", .null = N),
    loc_name = purrr::map_chr(request, "NomeLocal", .null = N),
    loc_type = purrr::map_chr(request, "TipoLocal", .null = N),
    loc_type_descr = purrr::map_chr(request, "DescricaoTipoLocal",
                                    .null = N),
    loc_house = purrr::map_chr(request, "NomeCasaLocal", .null = N),
    loc_date_created = purrr::map_chr(request, "DataCriacaoLocal",
                                      .null = N)
  )

  req <- req %>%
    dplyr::mutate(
      loc_date_created = lubridate::parse_date_time(
        loc_date_created, "Ymd")
    ) %>%
    dplyr::filter(!is.na(loc_id))  ## last line returns NA for most fields

  if(isTRUE(ascii)){
    req <- req %>%
      dplyr::mutate(
        loc_name = stringi::stri_trans_general(loc_name, "Latin-ASCII"),
        loc_type_descr = stringi::stri_trans_general(loc_type_descr,
                                                     "Latin-ASCII"),
        loc_house = stringi::stri_trans_general(loc_house, "Latin-ASCII")
      )
  }
  return(req)
}



#' @title Downloads and tidies information on the possible locations a piece
#' of legislation can currently be passing through
#' @description  Downloads and tidies information on the possible locations a piece
#' of legislation can currently be passing through.
#' @param bill_id \code{integer}. This number is the id given to each bill in the
#' Senate database. For example, running \code{sen_bills_current()} will return a
#'  dataframe with the variable \code{bill_id} in the first column. These numbers
#'  can be used as this id.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_passage(bill_id = 9123)
#' @export
sen_bills_passage <- function(bill_id = NULL, ascii = TRUE){

  if(is.null(bill_id)){
    stop("Please enter an bill_id number.")
  }
  base_url <- "http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/" %p%
    bill_id

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$MovimentacaoMateria$Materia
  N = NA_character_

  sit <- request$SituacaoAtual$Autuacoes$Autuacao
  tram <- request$Tramitacoes$Tramitacao %>% purrr::flatten()
  tram_o <- purrr::map(tram, "OrigemTramitacao") %>% purrr::flatten()
  tram_d <- purrr::map(tram, "DestinoTramitacao") %>% purrr::flatten()

  req <- tibble::tibble(
    bill_id = purrr::map_chr(request, .null = N, "CodigoMateria") %>%
      disc(),
    bill_number = purrr::map_chr(request, .null = N,
                                 "NumeroMateria") %>%
      disc(),
    bill_year = purrr::map_chr(request, .null = N,
                               "AnoMateria") %>%
      disc(),
    bill_type_abbr = purrr::map_chr(request, .null = N,
                                    "SiglaSubtipoMateria") %>%
      disc(),
    bill_type = purrr::map_chr(request, .null = N,
                               "DescricaoSubtipoMateria") %>%
      disc(),
    bill_house = purrr::map_chr(request, .null = N,
                                "NomeCasaIdentificacaoMateria") %>%
      disc(),
    bill_house_abbr = purrr::map_chr(request, .null = N,
                                     "SiglaCasaIdentificacaoMateria") %>%
      disc(),
    bill_in_passage = purrr::map_chr(request, .null = N,
                                     "IndicadorTramitando") %>%
      disc(),
    bill_situation = purrr::map_chr(sit, .null = N, "DescricaoSituacao") %>%
      disc(),
    bill_situation_date = purrr::map_chr(sit, .null = N, "DataSituacao") %>%
      disc(),
    bill_location_id = purrr::map_chr(sit, .null = N,
                                      "CodigoLocal") %>% disc(),
    bill_location_type = purrr::map_chr(sit, .null = N,
                                        "TipoLocal") %>% disc(),
    bill_location = purrr::map_chr(sit, .null = N, "NomeLocal") %>% disc(),
    bill_location_house = purrr::map_chr(sit, .null = N,
                                         "NomeCasaLocal") %>% disc(),
    bill_location_house_abbr = purrr::map_chr(sit, .null = N,
                                              "SiglaCasaLocal") %>% disc(),
    bill_passage_id = purrr::map_chr(tram, .null = N, "CodigoTramitacao"),
    bill_passage_date = purrr::map_chr(tram, .null = N, "DataTramitacao"),
    bill_passage_text = purrr::map_chr(tram, .null = N, "TextoTramitacao"),
    bill_passage_origin = purrr::map_chr(tram_o, .null = N, "NomeCasaLocal"),
    bill_passage_orig_location = purrr::map_chr(tram_o, .null = N,
                                                "NomeLocal"),
    bill_passage_destination = purrr::map_chr(tram_d, .null = N,
                                              "NomeCasaLocal"),
    bill_passage_dest_location = purrr::map_chr(tram_d, .null = N,
                                                "NomeLocal")
  )

  req <- req %>%
    dplyr::mutate(
      bill_situation_date = lubridate::parse_date_time(
        bill_situation_date, "Ymd"),
      bill_passage_date = lubridate::parse_date_time(
        bill_passage_date, "Ymd")
    )

  if(isTRUE(ascii)){
    req <- req %>%
      dplyr::mutate(
        bill_type = stringi::stri_trans_general(bill_type, "Latin-ASCII"),
        bill_house = stringi::stri_trans_general(bill_house, "Latin-ASCII"),
        bill_in_passage = stringi::stri_trans_general(
          bill_in_passage, "Latin-ASCII"),
        bill_situation = stringi::stri_trans_general(bill_situation,
                                                     "Latin-ASCII"),
        bill_location = stringi::stri_trans_general(bill_location,
                                                    "Latin-ASCII"),
        bill_location_house = stringi::stri_trans_general(
          bill_location_house, "Latin-ASCII"),
        bill_passage_text = stringi::stri_trans_general(
          bill_passage_text, "Latin-ASCII"),
        bill_passage_origin = stringi::stri_trans_general(
          bill_passage_origin, "Latin-ASCII"),
        bill_passage_orig_location = stringi::stri_trans_general(
          bill_passage_orig_location, "Latin-ASCII"),
        bill_passage_destination = stringi::stri_trans_general(
          bill_passage_destination, "Latin-ASCII"),
        bill_passage_dest_location = stringi::stri_trans_general(
          bill_passage_dest_location, "Latin-ASCII")
      )
  }
  return(req)
}





#' @title Downloads and tidies information on the possible situations a bill
#' can be in
#' @description Downloads and tidies information on the possible situations a bill
#' can be in.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_situations()
#' @export
sen_bills_situations <- function(ascii = TRUE){

  base_url <- "http://legis.senado.leg.br/dadosabertos/materia/situacoes"

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaSituacoes$Situacoes$Situacao
  N <- NA_character_

  sit <- tibble::tibble(
    sit_id = purrr::map_chr(request, "Codigo", .null = N),
    sit_abbr = purrr::map_chr(request, "Sigla", .null = N),
    sit_description = purrr::map_chr(request, "Descricao", .null = N)
  )

  if(isTRUE(ascii)){
    sit <- sit %>%
      dplyr::mutate(sit_description = stringi::stri_trans_general(
        sit_description, "Latin-ASCII"
      ))
  }
  return(sit)
}




#' @title Downloads and tidies information on bills that have been recently
#'  updated in the Federal Senate
#' @description Downloads and tidies information on bills that have been recently
#'  updated in the Federal Senate.
#' @param update \code{character}. This is the type of update that can be applied
#' to a bill. For a dataframe of these, use the \code{sen_bills_update_types()}
#'  function, and the variable \code{update_name} that is returned.
#' @param year \code{character}. Year of the bill, if a specific bill is
#' requested. Format YYYY.
#' @param number bill number.
#' @param type type of legislation.
#' @param days \code{integer}. The number of days to consider when requesting
#' information on recent updates. The maximum is 30 and the default is 5.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' # Bills from 2014 that have had a "despacho" update in the last 15 days, if
#' # they exist:
#' \dontrun{
#' desp_2014 <- sen_bills_updates(update = "Despacho", year = 2014, days = 15)
#'
#' # PLS bills that have been updated in the last 10 days, if they exist:
#' pls <- sen_bills_updates(type = "PLS", days = 10)
#' }

#' @export
sen_bills_updates <- function(update = NULL, year = NULL,
                              number = NULL, type = NULL,
                              days = 5, ascii = TRUE){

  # checks
  if(days > 30){
    stop("30 is the maximum number of days allowed.")
  }

  base_url <- "http://legis.senado.leg.br/dadosabertos/materia/atualizadas?"

  if(!is.null(update)){
    base_url <- base_url %p% "alteracao=" %p% update
  }
  if(!is.null(year)){
    base_url <- base_url %p% "&ano=" %p% year
  }
  if(!is.null(number)){
    base_url <- base_url %p% "&numero=" %p% number
  }
  if(!is.null(type)){
    base_url <- base_url %p% "&sigla=" %p% type
  }
  if(days != 5){
    base_url <- base_url %p% "&numdias=" %p% days
  }

  request <- httr::GET(base_url)
  request <- status(request)
  if(purrr::is_empty(request$ListaMateriasAtualizadas$Materias)){
    stop("No data match your request.")
  }
  if(depth(request) > 6){
    request <- request$ListaMateriasAtualizadas$Materias$Materia
  } else{
    request <- request$ListaMateriasAtualizadas$Materias
  }

  N = NA_character_

  id <- purrr::map(request, "IdentificacaoMateria")
  dep <- function(x) depth(x) > 1
  at <- purrr::map(request, "AtualizacoesRecentes") %>% purrr::flatten() %>%
    purrr::map_if(dep, purrr::flatten) ## this just takes the first
  ## out of a nested list for now

  req <- tibble::tibble(
    bill_id = purrr::map_chr(id, .null = N, "CodigoMateria"),
    bill_number = purrr::map_chr(id, .null = N, "NumeroMateria"),
    bill_year = purrr::map_chr(id, .null = N, "AnoMateria"),
    bill_type = purrr::map_chr(id, .null = N, "SiglaSubtipoMateria"),
    bill_house = purrr::map_chr(id, .null = N, "NomeCasaIdentificacaoMateria"),
    bill_passing = purrr::map_chr(id, .null = N, "IndicadorTramitando"),
    update_effect = purrr::map_chr(at, .null = N, "InformacaoAtualizada"),
    update_date = purrr::map_chr(at, .null = N, "DataUltimaAtualizacao")
  )

  req <- req %>%
    dplyr::mutate(
      bill_passing = ifelse(bill_passing == "Sim", "Yes", "No"),
      update_date = lubridate::parse_date_time(
        update_date, "Ymd HMS")
    )

  if(isTRUE(ascii)){
    req <- req %>%
      dplyr::mutate(
        bill_house = stringi::stri_trans_general(bill_house, "Latin-ASCII")
      )
  }
  return(req)
}




#' @title Downloads and tidies information on the types of updates that can be
#' applied to bills in the Federal Senate
#' @description Downloads and tidies information on the types of updates that can be
#' applied to bills in the Federal Senate.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame},
#' with variables:
#' \itemize{
#'  \item{\code{update_name: }}{the name in the database for the type of update.}
#'  \item{\code{update_effects: }}{the type of item that is affected by the update. "MovimentacaoMateria" refers to the passage of the bill; "DetalheMateria" refers to the details of the bill itself; "RelatoriaMateria" refers to reports on the bill; "VotacaoMateria" refers to votes on the bill.}
#' }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_bills_update_types()
#' @export
sen_bills_update_types <- function(){

  base_url <- "http://legis.senado.leg.br/dadosabertos/materia/tiposatualizacoes"

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaTiposAtualizacao$TiposAtualizacao$Atualizacao

  req <- tibble::tibble(
    update_name = purrr::map_chr(request, "InformacaoAtualizada",
                                 .null = NA_character_),
    update_effects = purrr::map_chr(request, "NomeServicoAfetado",
                                    .null = NA_character_)
  )
  return(req)
}



