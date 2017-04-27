#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr compact
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr as_data_frame
#' @title Downloads and tidies data on the agenda in the Federal Senate
#' @description Downloads and tidies data on the agenda in the Federal Senate.
#' @param initial_date (\code{character}) start date of the period requested.
#' This parameter must be in the format YYYYMMDD (Year-Month-Day). A value for
#' this parameter is necessary, all others are optional.
#' @param end_date (\code{character}) final date for period requested. Format
#' YYYYMMDD.
#' @param house (\code{character}). The acronym for the legislative house
#' for which results are requested. Options are SF (\emph{Senado Federal},
#' Federal Senate), CN (\emph{Congresso Nacional}, National Congress - joint
#' meeting of the Senate and Chamber), and CA \code{Camara dos Deputados},
#' Chamber of Deputies.
#' @param supervisory \code{character}. Name of the commission or supervisory body. A
#' data frame of these can be seen with \code{data("commissions")}.
#' @param legislator \code{integer}. The numeric code given to each senator.
#' A dataframe with these values is returned from the \code{sen_senator_list()}
#' function.
#' @param details (\code{logical}). If details is equal to TRUE, the data returned
#' is an expanded dataset with additional details. This is not recommended unless
#' necessary.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @note Requesting data from a long period of time with \code{details = TRUE} will
#' return a large object in terms of memory. It will also be rather unwieldy, with
#' many columns.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' sen_agenda(initial_date = "20161105", end_date = "20161125")
#' sen_agenda(initial_date = "20161105", end_date = "20161125",
#' legislator = 4988)
#' @export
sen_agenda <- function(initial_date = NULL, end_date = NULL,
                       house = NULL, supervisory = NULL,
                       legislator = NULL, details = FALSE,
                       ascii = TRUE){
  # checks
  if(is.null(initial_date)){
    stop("Please choose a valid initial date. Format is YYYYMMDD.")
  }
  if(details == TRUE & !is.null(legislator)){
    warning("Using the arguments 'details' and 'legislator' together will result in the latter being ignored.")
    legislator <- NULL
  }


  base_url <- "http://legis.senado.gov.br/dadosabertos/agenda/" %p%
    initial_date

  if(is.null(end_date)){
    if(details == FALSE){
      initial_date <- initial_date %p% "?"
      if(!is.null(house)){
        base_url <- base_url %p% "&casa=" %p% house
      }
      if(!is.null(supervisory)){
        base_url <- base_url %p% "&colegiado=" %p% supervisory
      }
      if(!is.null(legislator)){
        base_url <- base_url %p% "&parlamentar=" %p% legislator
      }
    } else{
      base_url <- base_url %p% "/detalhe?"
      if(!is.null(house)){
        base_url <- base_url %p% "&casa=" %p% house
      }
      if(!is.null(supervisory)){
        base_url <- base_url %p% "&colegiado=" %p% supervisory
      }
    }
  }else{
    base_url <- "http://legis.senado.gov.br/dadosabertos/agenda/" %p%
      initial_date %p% "/" %p% end_date

    if(details == FALSE){
      base_url <- base_url %p% "?"
      if(!is.null(house)){
        base_url <- base_url %p% "&casa=" %p% house
      }
      if(!is.null(supervisory)){
        base_url <- base_url %p% "&colegiado=" %p% supervisory
      }
      if(!is.null(legislator)){
        base_url <- base_url %p% "&parlamentar=" %p% legislator
      }
    } else{
      base_url <- base_url %p% "/detalhe?"
      if(!is.null(house)){
        base_url <- base_url %p% "&casa=" %p% house
      }
      if(!is.null(supervisory)){
        base_url <- base_url %p% "&colegiado=" %p% supervisory
      }
    }
  }

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$Reunioes$Reuniao
  N <- NA_character_

  if(!is.null(legislator)){
    partic <- purrr::map(request, "ParticipacaoParlamentar", .null = N)
  }

  comms <- purrr::map(request, "Comissoes", .null = N) %>%
    purrr::flatten()
  part <- purrr::map(request, "Partes", .null = N) %>% purrr::flatten()

  agenda <- tibble::tibble(
    agenda_id = purrr::map_chr(request, .null = N, "Codigo"),
    agenda_title = purrr::map_chr(request, .null = N, "TituloDaReuniao"),
    agenda_name = purrr::map_chr(part, .null = N, "NomeFantasia"),
    agenda_type = purrr::map_chr(request, .null = N, "Tipo"),
    agenda_date= purrr::map_chr(request, .null = N, "Data"),
    agenda_time = purrr::map_chr(request, .null = N, "Hora"),
    agenda_status = purrr::map_chr(request, .null = N, "Situacao"),
    agenda_place = purrr::map_chr(request, .null = N, "Local"),
    agenda_commission_house = purrr::map_chr(comms, .null = N, "Casa"),
    agenda_commission_abbr = purrr::map_chr(comms, .null = N, "Sigla"),
    agenda_commission_id = purrr::map_chr(comms, .null = N, "CodigoColegiado"),
    agenda_commission_meeting_number = purrr::map_chr(comms, .null = N,
                                                      "NumeroReuniao")
    )

  agenda <- agenda %>%
    dplyr::mutate(agenda_date = lubridate::parse_date_time(
      agenda_date, "dmY"
    ))

  if(!is.null(legislator)){
    agenda <- agenda %>%
      dplyr::mutate(
        legislator_name = purrr::map_chr(partic, .null = N, "Nome"),
        agenda_commission_member = purrr::map_chr(partic, .null = N,
                                                  "IsMembroComissao"),
        agenda_commission_relator = purrr::map_chr(partic, .null = N,
                                                   "IsRelator")
      )
  }


  if(ascii == TRUE){
    if(!is.null(legislator)){
      agenda <- agenda %>%
        dplyr::mutate(
          legislator_name = stringi::stri_trans_general(legislator_name,
                                                        "Latin-ASCII"),
          agenda_commission_member = stringi::stri_trans_general(
            agenda_commission_member, "Latin-ASCII"),
          agenda_commission_relator = stringi::stri_trans_general(
            agenda_commission_relator, "Latin-ASCII")
        )
    }
    agenda <- agenda %>%
      dplyr::mutate(
        agenda_title = stringi::stri_trans_general(agenda_title, "Latin-ASCII"),
        agenda_name = stringi::stri_trans_general(agenda_name, "Latin-ASCII"),
        agenda_type = stringi::stri_trans_general(agenda_type, "Latin-ASCII"),
        agenda_status = stringi::stri_trans_general(agenda_status,
                                                    "Latin-ASCII"),
        agenda_place = stringi::stri_trans_general(agenda_place, "Latin-ASCII")
      )
  }
  return(agenda)
}
