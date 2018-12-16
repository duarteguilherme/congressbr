#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr full_join
#' @importFrom magrittr '%>%'
#' @importFrom stringi stri_trans_general
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_if
#' @importFrom purrr is_empty
#' @title Downloads and tidies persnonal information on senators from the
#' Federal Senate
#' @description Downloads and tidies persnonal information on senators from the
#' Federal Senate.
#' @param id \code{integer}. Unique id for a senator. A dataframe of these
#' is available from \code{sen_senator_list()}.
#' @param affiliations \code{logical}. If TRUE, the default, returns information
#'  on party affiliation.
#' @param mandates \code{logical}. If TRUE, the default, returns information on
#' terms served by the senator.
#' @param absences \code{logical}. If TRUE, returns information on leaves of
#' absence taken by the senator.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @note Setting \code{affiliations}, \code{mandates} and particularly
#' \code{absences} to \code{TRUE} will result in a rather bloated data frame.
#' @examples
#' \donttest{
#' benedito <- sen_senator(id = 3823)
#' aecio <- sen_senator(id = 391, absences = TRUE)
#' juc <- sen_senator(73)
#' }
#' @export
sen_senator <- function(id = NULL, affiliations = TRUE,
                        mandates = TRUE, absences = FALSE,
                        ascii = TRUE){

  if(is.null(id)){
    stop("Please choose a valid senator ID.")
  }
  base_url <- "http://legis.senado.gov.br/dadosabertos/parlamentar/" %p%
    id

  request <- httr::GET(base_url)
  request <- status(request)
  N <- NA_character_

  dob <- purrr::map_chr(request, "dataNascimento", .null = N)
  licenca <- purrr::map(request, "licencas", .null = N)
  if(depth(licenca) > 3){
    licenca <- licenca  %>% purrr::flatten() %>% purrr::flatten()
  } else{
    licenca <- purrr::flatten(licenca)
  }

  fili <- purrr::map(request, "filiacoes")
  if(depth(fili) > 4){
    fili <- fili %>% purrr::flatten() %>% purrr::flatten()
  } else{
    fili <- purrr::flatten(fili)
  }

  exer <- purrr::map(request, "exercicios") %>% purrr::flatten()

  sen <- tibble::tibble(
    id = purrr::map_chr(request, "idParlamentar", .null = N),
    name_full = purrr::map_chr(request, "nomeCompleto", .null = N),
    name_senator = purrr::map_chr(request, "nomeParlamentar", .null = N),
    gender = purrr::map_chr(request, "sexoParlamentar", .null = N),
    date_of_birth = suppressWarnings(
      lubridate::parse_date_time(dob, orders = "Ymd")),
    place_of_birth = purrr::map_chr(request, "nomeCidadeNatural",
                                    .null = N),
    state_of_birth = purrr::map_chr(request, "siglaUfNatural",
                                    .null = N),
    country_of_birth = purrr::map_chr(request, "DescricaoPaisNascimento",
                                      .null = N),
    office_address = purrr::map_chr(request, "local", .null = N),
    office_phone = purrr::map_chr(request, "fone", .null = N),
    office_email = purrr::map_chr(request, "email", .null = N)
  )

  if(isTRUE(ascii)){
    sen <- sen %>%
      dplyr::mutate(
        name_full = stringi::stri_trans_general(
          name_full, "Latin-ASCII"
        ),
        name_senator = stringi::stri_trans_general(
          name_senator, "Latin-ASCII"
        ),
        place_of_birth = stringi::stri_trans_general(
          place_of_birth, "Latin-ASCII"
        ),
        country_of_birth = stringi::stri_trans_general(
          country_of_birth, "Latin-ASCII"
        ),
        office_address = stringi::stri_trans_general(
          office_address, "Latin-ASCII"
        )
      )
  }

  if(isTRUE(affiliations)){
    party <- purrr::map(fili, "partido", .null = N)

    parties <- tibble::tibble(
        id = purrr::map_chr(fili, "idParlamentar", .null = N),
        affil_id = purrr::map_chr(fili, .null = N, "idFiliacao"),
        affil_party_id = purrr::map_chr(fili, .null = N, "idPartido"),
        affil_party = purrr::map_chr(party, .null = N, "siglaPartido"),
        affil_party_name = purrr::map_chr(party, .null = N, "nomePartido"),
        affil_party_date_joined = purrr::map_chr(party, .null = N,
                                                  "dataCriacao"),
        affil_party_date_left = purrr::map_chr(party, .null = N,
                                                  "dataExtincao")
      )
    parties <- parties %>%
      dplyr::mutate(
        affil_party_date_joined = suppressWarnings(
          lubridate::parse_date_time(
          affil_party_date_joined, "Ymd"
        )),
        affil_party_date_left = suppressWarnings(
          lubridate::parse_date_time(
            affil_party_date_left, "Ymd"
          )
        )
      )

    if(isTRUE(ascii)){
      parties <- parties %>%
        dplyr::mutate(
          affil_party_name = stringi::stri_trans_general(
            affil_party_name, "Latin-ASCII"
          )
        )
    }
    sen <- suppressMessages(dplyr::full_join(sen, parties))
  }

  if(isTRUE(mandates)){
    if(depth(exer) > 3){
      exer <- purrr::flatten(exer)
    }
      mand <- exer %>% purrr::flatten()

    ex <- tibble::tibble(
      id = purrr::map_chr(exer, .null = N, "idParlamentar"),
      mandate_id = purrr::map_chr(exer, .null = N, "idMandato"),
      mandate_house = purrr::map_chr(mand, .null = N, "siglaCasa") %>% disc(),
      mandate_state = purrr::map_chr(mand, .null = N, "siglaUf") %>% disc(),
      mandate_start = purrr::map_chr(exer, .null = N, "dataInicio"),
      mandate_end = purrr::map_chr(exer, .null = N, "dataFim")
    )

    ex <- ex %>%
      dplyr::mutate(
        mandate_start = suppressWarnings(
          lubridate::parse_date_time(
          mandate_start, "Ymd"
        )),
        mandate_end = suppressWarnings(
          lubridate::parse_date_time(mandate_end, "Ymd")
        )
      )

    sen <- suppressMessages(dplyr::full_join(sen, ex))
  }

  if(isTRUE(absences)){

    ab <- tibble::tibble(
      id = purrr::map_chr(licenca, .null = N, "idParlamentar"),
      absence_id = purrr::map_chr(licenca, .null = N, "idLicenca"),
      absence_start = purrr::map_chr(licenca, .null = N, "dataInicio"),
      absence_end = purrr::map_chr(licenca, .null = N, "dataTermino"),
      absence_description = purrr::map_chr(licenca, .null = N,
                                           "descricaoFinalidade")
    )
    ab <- ab %>%
      dplyr::mutate(
        absence_start = suppressWarnings(
          lubridate::parse_date_time(
          absence_start, "Ymd"
        )),
        absence_end = suppressWarnings(
          lubridate::parse_date_time(
          absence_end, "Ymd"
        ))
      )
    if(isTRUE(ascii)){
      ab <- ab %>%
        dplyr::mutate(
          absence_description = stringi::stri_trans_general(
            absence_description, "Latin-ASCII"
          )
        )
    }

    sen <- suppressMessages(dplyr::full_join(sen, ab))
  }
  sen <- sen %>%
    dplyr::filter(!is.na(id))
  return(sen)
}

