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
#' Federal Senate.
#' @param code \code{integer}. Unique code for a senator. A dataframe of these
#' is available from \code{sen_senator_list()}.
#' @param affiliations \code{logical}. If TRUE, the default, returns information
#'  on party affiliation.
#' @param mandates \code{logical}. If TRUE, the default, returns information on
#' terms served by the senator.
#' @param leaderships \code{logical}. If TRUE, returns information on leadership
#' positions the senator has held.
#' @param absences \code{logical}. If TRUE, returns information on leaves of
#' absence taken by the senator.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @note Setting all parameters to TRUE will return a rather bloated data frame
#' and is not recommended unless necessary.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' benedito <- sen_senator(3823)
#' @export
sen_senator <- function(code = 0, affiliations = TRUE,
                        mandates = TRUE, leaderships = FALSE,
                        absences = FALSE, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/parlamentar/" %p%
    code

  request <- httr::GET(base_url)
  # status checks
  request <- status(request)


  dob <- safe_map(request, "dataNascimento")


  sen <- dplyr::data_frame(
    id = purrr::map_chr(request, "idParlamentar"),
    name_full = purrr::map_chr(request, "nomeCompleto"),
    name_senator = purrr::map_chr(request, "nomeParlamentar"),
    gender = safe_map(request, "sexoParlamentar"),
    date_of_birth = suppressWarnings(
      lubridate::parse_date_time(dob, orders = "Ymd")),
    place_of_birth = safe_map(request, "nomeCidadeNatural"),
    state_of_birth = safe_map(request, "siglaUfNatural"),
    country_of_birth = safe_map(request, "DescricaoPaisNascimento"),
    office_address = safe_map(request, "local"),
    office_phone = safe_map(request, "fone"),
    office_email = safe_map(request, "email"))



  if(ascii == TRUE){
    sen <- sen %>%
      dplyr::mutate(
        name_full = stringi::stri_trans_general(name_full, "Latin-ASCII"),
        name_senator = stringi::stri_trans_general(name_senator,
                                                   "Latin-ASCII"),
        place_of_birth = stringi::stri_trans_general(place_of_birth,
                                                     "Latin-ASCII"),
        state_of_birth = stringi::stri_trans_general(state_of_birth,
                                                     "Latin-ASCII"),
        office_address = stringi::stri_trans_general(office_address,
                                                     "Latin-ASCII"))
  }

  if(leaderships == TRUE){
    # info on leadership positions:
    if(!is.null(request$parlamentar$liderancas$lideranca)){

      lid <- request$parlamentar$liderancas$lideranca
      if(depth(lid) < 3){
        lid <- request$parlamentar$liderancas
      }
      # uni <- purrr::map(lid, "unidadeLideranca") %>%
      #   purrr::map_if(purrr::is_empty, ~ NA_character_)
      ### NOTE: in future versions, we can utilise 'uni' perhaps.
      ## need more info. What do the numeric codes represent?

      start <- safe_map(lid, "dataDesignacao")
      end <- safe_map(lid, "dataTermino")

      lead <- dplyr::data_frame(
        id = safe_map(lid, "idParlamentar"),
        id_leadership = safe_map(lid, "idLideranca"),
        house = safe_map(lid, "siglaCasa"),
        leadership_type = safe_map(lid, "tipoLideranca"),
        leadsership_date_assumed = suppressWarnings(
          lubridate::parse_date_time(start, orders = "Ymd")),
        leadership_date_terminated =  suppressWarnings(
          lubridate::parse_date_time(end, orders = "Ymd")))
    }
    sen <- suppressMessages(dplyr::full_join(sen, lead))
  }

  if(affiliations == TRUE){
    # info on party affiliation:
    if(!is.null(request$parlamentar$filiacoes$filiacao)){
      parties <- request$parlamentar$filiacoes$filiacao

      if(depth(parties) == 3){
        p <- purrr::map(parties, "partido")
        desf <- safe_map(parties, "dataDesfiliacao")
        join <- safe_map(parties, "dataFiliacao")

        party <- dplyr::data_frame(
          id = safe_map(parties, "idParlamentar"),
          party_id = safe_map(parties, "idPartido"),
          party_abbr = safe_map(p, "siglaPartido"),
          party_name = safe_map(p, "nomePartido"),
          party_date_joined = suppressWarnings(
            lubridate::parse_date_time(join, orders = "Ymd")),
          party_date_left = suppressWarnings(
            lubridate::parse_date_time(desf, orders = "Ymd")))
      } else{
        parties <- request$parlamentar$filiacoes
        names(parties) <- NULL
        p <- purrr::map(parties, "partido")
        desf <- safe_map(parties, "dataDesfiliacao")
        join <- safe_map(parties, "dataFiliacao")

        party <- dplyr::data_frame(
          id = safe_map(parties, "idParlamentar"),
          party_id = safe_map(parties, "idPartido"),
          party_abbr = safe_map(p, "siglaPartido"),
          party_name = safe_map(p, "nomePartido"),
          party_date_joined = suppressWarnings(
            lubridate::parse_date_time(join, orders = "Ymd")),
          party_date_left = suppressWarnings(
            lubridate::parse_date_time(desf, orders = "Ymd")))
      }
      if(ascii == TRUE){
        party$party_name <- stringi::stri_trans_general(party$party_name,
                                                        "Latin-ASCII")
      }
      sen <- suppressMessages(dplyr::full_join(sen, party))
    }
  }

  if(mandates == TRUE){
    # info on mandates:
    if(!is.null(request$parlamentar$exercicios$exercicio)){
      terms <- request$parlamentar$exercicios
      if(depth(terms) == 3){
        names(terms) <- NULL
        mand <- purrr::map(terms, "mandato")
        term <- dplyr::data_frame(
          id = purrr::map_chr(terms, "idParlamentar"),
          id_term = purrr::map_chr(terms, "idExercicio"),
          id_titular = purrr::map_chr(mand, "idTitular"),
          house = purrr::map_chr(mand, "siglaCasa"),
          initial_legislature = purrr::map_chr(mand, "numeroLegislaturaInicio"),
          end_legislature = safe_map(mand, "mumeroLegislaturaFim")
        )
      } else{
        terms <-terms$exercicio
        mand <- purrr::map(terms, "mandato")
        term <- dplyr::data_frame(
          id = purrr::map_chr(terms, "idParlamentar"),
          mandate_id = purrr::map_chr(terms, "idExercicio"),
          id_titular = safe_map(mand, "idTitular"),
          mandate_house = purrr::map_chr(mand, "siglaCasa"),
          mandate_initial_legislature = safe_map(mand,
                                                       "numeroLegislaturaInicio"),
          mandate_end_legislature = safe_map(mand,
                                                   "mumeroLegislaturaFim")
        )
      }
    }
    sen <- suppressMessages(dplyr::left_join(sen, term))
  }


  if(absences == TRUE){
    # info on leaves of absence:
    if(!is.null(request$parlamentar$licencas$licenca)){

      lic <- request$parlamentar$licencas$licenca

      start <- safe_map(lic, "dataInicio")
      leave <- safe_map(lic, "dataTermino")

      lic <- dplyr::data_frame(
        id = purrr::map_chr(lic, "idParlamentar"),
        id_absence = safe_map(lic, "idLicenca"),
        leave_start_date = suppressWarnings(
          lubridate::parse_date_time(start, orders = "Ymd")),
        leave_end_date = suppressWarnings(
          lubridate::parse_date_time(leave, orders = "Ymd")))
    }
    sen <- suppressMessages(dplyr::left_join(sen, lic))
  }
  return(sen)
}

