#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr flatten
#' @importFrom purrr map
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr distinct
#' @title Downloads and tidies personal information on the senators in the
#' Federal Senate
#' @description Downloads and tidies personal information on the senators in the
#' Federal Senate.
#' @param id \code{integer}. This number represents the id of the senator
#' you wish to get information on. These ids can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'id'.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso \code{sen_senator_list()}
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' Acir_G <- sen_senator_details(id = 4981)
#'
#' @export
sen_senator_details <- function(id = 0, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/" %p% id

  if(is.null(id)){
    stop("'id' is necessary.")
  }

  request <- httr::GET(base_url)
  request <- status(request)

  if(purrr::is_empty(request$DetalheParlamentar$Parlamentar)){
    stop("No data match your request. Have you checked the 'id' argument?")
  }
  request <- request$DetalheParlamentar$Parlamentar
  N <- NA_character_

  mand <- request$MandatoAtual
  party <- request$FiliacaoAtual

  req <- tibble::tibble(
    senator_id = purrr::map_chr(request, .null = N,
                                "CodigoParlamentar") %>% disc(),
    senator_name = purrr::map_chr(request, .null = N, "NomeParlamentar") %>%
      disc(),
    senator_party_name = purrr::map_chr(party, .null = N,
                                        "NomePartido") %>% disc(),
    senator_party_abbr = purrr::map_chr(request, .null = N,
                                        "SiglaPartidoParlamentar") %>% disc(),
    senator_party_date_joined = purrr::map_chr(request, .null = N,
                                               "DataFiliacao") %>% disc(),
    senator_state = request$IdentificacaoParlamentar$UfParlamentar,
    senator_status = purrr::map_chr(request, .null = N,
                                    "DescricaoParticipacao") %>% disc(),
    senator_gender = purrr::map_chr(request, .null = N,
                                    "SexoParlamentar") %>% disc(),
    senator_date_of_birth = purrr::map_chr(request, .null = N,
                                           "DataNascimento") %>% disc(),
    senator_state_of_birth = purrr::map_chr(request, .null = N,
                                            "UfNaturalidade") %>% disc(),
    senator_title = purrr::map_chr(request, .null = N, "FormaTratamento") %>%
      disc(),
    senator_mandates_first_legis =
      mand$PrimeiraLegislaturaDoMandato$NumeroLegislatura,
    senator_mandates_second_legis =
      mand$SegundaLegislaturaDoMandato$NumeroLegislatura,
    senator_url = purrr::map_chr(request, .null = N,
                                 "UrlPaginaParlamentar") %>% disc(),
    senator_email = purrr::map_chr(request, .null = N,
                                   "EmailParlamentar") %>% disc(),
    senator_office_address = purrr::map_chr(request, .null = N,
                                            "EnderecoParlamentar") %>%
      disc()
  ) %>% dplyr::distinct(.keep_all = TRUE)

  if(isTRUE(ascii)){
    req <- req %>%
      dplyr::mutate(
        senator_name = stringi::stri_trans_general(
          senator_name, "Latin-ASCII"
        ),
        senator_party_name = stringi::stri_trans_general(
          senator_party_name, "Latin-ASCII"
        ),
        senator_office_address = stringi::stri_trans_general(
          senator_office_address, "Latin-ASCII"
        )
      )
  }
  req <- req %>%
    dplyr::mutate(
      senator_party_date_joined = lubridate::parse_date_time(
        senator_party_date_joined, "Ymd"
      ),
      senator_date_of_birth = lubridate::parse_date_time(
        senator_date_of_birth, "Ymd"
      )
    )
  return(req)
}


#' @title Downloads and tidies information on bills that certain senators
#' have sponsored/authored in the Federal Senate
#' @description Downloads and tidies information on bills that certain senators
#' have sponsored/authored in the Federal Senate.
#' @param id \code{integer}. This number represents the id of the senator
#' you wish to get information on. These ids can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'id'.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso \code{sen_senator_list()}
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' Ataides <- sen_senator_bills(id = 5164)
#'
#' @export
sen_senator_bills <- function(id = 0, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/" %p% id

  if(is.null(id)){
    stop("'id' is necessary.")
  }

  request <- httr::GET(base_url)
  request <- status(request)

  if(purrr::is_empty(request$DetalheParlamentar$Parlamentar)){
    stop("No data match your request. Have you checked the 'id' argument?")
  }
  request <- request$DetalheParlamentar$Parlamentar
  N <- NA_character_

  party <- request$FiliacaoAtual
  bills <- request$MateriasDeAutoriaTramitando

  req <- tibble::tibble(
    senator_id = purrr::map_chr(request, .null = N,
                                "CodigoParlamentar") %>% disc(),
    senator_name = purrr::map_chr(request, .null = N, "NomeParlamentar") %>%
      disc(),
    senator_party_name = purrr::map_chr(party, .null = N,
                                        "NomePartido") %>% disc(),
    senator_party_abbr = purrr::map_chr(request, .null = N,
                                        "SiglaPartidoParlamentar") %>% disc(),
    senator_state = request$IdentificacaoParlamentar$UfParlamentar,
    senator_bills_number = bills$Materia %>%
      purrr::flatten() %>%
      purrr::map_chr(.null = N, "NumeroMateria") %>% disc(),
    senator_bills_type = bills$Materia %>%
      purrr::flatten() %>%
      purrr::map_chr(.null = N, "SiglaSubtipoMateria") %>% disc(),
    senator_bills_year = bills$Materia %>%
      purrr::flatten() %>%
      purrr::map_chr(.null = N, "AnoMateria") %>% disc(),
    senator_bills_details = bills$Materia %>%
      purrr::map_chr(.null = N, "EmentaMateria") %>% disc()
  ) %>% dplyr::distinct(.keep_all = TRUE)

  if(isTRUE(ascii)){
    req <- req %>%
      dplyr::mutate(
        senator_name = stringi::stri_trans_general(
          senator_name, "Latin-ASCII"
        ),
        senator_party_name = stringi::stri_trans_general(
          senator_party_name, "Latin-ASCII"
        ),
        senator_bills_details = stringi::stri_trans_general(
          senator_bills_details, "Latin-ASCII"
        )
      )
  }
  return(req)
}





#' @title Downloads and tidies information on the commissions on which
#' senators have served or are serving in the Federal Senate
#' @description Downloads and tidies information on the commissions on which
#' senators have served or are serving in the Federal Senate.
#' @param id \code{integer}. This number represents the id of the senator
#' you wish to get information on. These ids can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'id'.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso \code{sen_senator_list()}
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' Armando <- sen_senator_commissions(id = 715)
#'
#' @export
sen_senator_commissions <- function(id = 0, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/" %p% id

  if(is.null(id)){
    stop("'id' is necessary.")
  }

  request <- httr::GET(base_url)
  request <- status(request)

  if(purrr::is_empty(request$DetalheParlamentar$Parlamentar)){
    stop("No data match your request. Have you checked the 'id' argument?")
  }
  request <- request$DetalheParlamentar$Parlamentar
  N <- NA_character_

  party <- request$FiliacaoAtual
  coms <- request$MembroAtualComissoes
  jobs <- request$CargosAtuais


  req <- tibble::tibble(
    senator_id = purrr::map_chr(request, .null = N,
                                "CodigoParlamentar") %>% disc(),
    senator_name = purrr::map_chr(request, .null = N, "NomeParlamentar") %>%
      disc(),
    senator_party_name = purrr::map_chr(party, .null = N,
                                        "NomePartido") %>% disc(),
    senator_party_abbr = purrr::map_chr(request, .null = N,
                                        "SiglaPartidoParlamentar") %>% disc(),
    senator_state = request$IdentificacaoParlamentar$UfParlamentar,
    senator_commissions_name = coms$Comissao %>%
      purrr::flatten() %>%
      purrr::map_chr(.null = N, "NomeComissao") %>% disc(),
    senator_commissions_abbr = coms$Comissao %>%
      purrr::flatten() %>%
      purrr::map_chr(.null = N, "SiglaComissao") %>% disc(),
    senator_commissions_id = coms$Comissao %>%
      purrr::flatten() %>%
      purrr::map_chr(.null = N, "CodigoComissao") %>% disc(),
    senator_commissions_house = coms$Comissao %>%
      purrr::flatten() %>%
      purrr::map_chr(.null = N, "NomeCasaComissao") %>% disc(),
    senator_commissions_participation = coms$Comissao %>%
      purrr::map_chr(.null = N, "DescricaoParticipacao") %>% disc(),
    senator_commissions_date_joined = coms$Comissao %>%
      purrr::map_chr(.null = N, "DataInicio") %>% disc(),
    senator_positions_commission_desc = purrr::map_chr(jobs,
                                                       .null = N,
                                                       "DescricaoCargo") %>%
      disc(),
    senator_positions_commission_date_start = purrr::map_chr(jobs,
                                                             .null = N,
                                                             "DataInicio") %>%
      disc()
  ) %>% dplyr::distinct(.keep_all = TRUE)

  if(isTRUE(ascii)){
    req <- req %>%
      dplyr::mutate(
        senator_name = stringi::stri_trans_general(
          senator_name, "Latin-ASCII"
        ),
        senator_party_name = stringi::stri_trans_general(
          senator_party_name, "Latin-ASCII"
        ),
        senator_commissions_name = stringi::stri_trans_general(
          senator_commissions_name, "Latin-ASCII"
        )
      )
  }

  req <- req %>%
    dplyr::mutate(
      senator_commissions_date_joined = lubridate::parse_date_time(
        senator_commissions_date_joined, "Ymd"
      ),
      senator_positions_commission_date_start = lubridate::parse_date_time(
        senator_positions_commission_date_start, "Ymd"
      )
    )
  return(req)
}



#' @title Downloads and tidies information on titular senators and their
#' \emph{suplentes} in the Federal Senate
#' @description Downloads and tidies information on titular senators and their
#' \emph{suplentes} in the Federal Senate.
#' @param id \code{integer}. This number represents the id of the senator
#' you wish to get information on. These ids can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'id'.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso \code{sen_senator_list()}
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' # A titular senator, José Serra:
#' Serra <- sen_senator_suplentes(id = 90)
#'
#' # Or one of his suplentes:
#' suplente <- sen_senator_suplentes(id = 878)
#'
#' @export
sen_senator_suplentes <- function(id = 0, ascii = TRUE){
  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/" %p% id

  if(is.null(id)){
    stop("'id' is necessary.")
  }

  request <- httr::GET(base_url)
  request <- status(request)

  if(purrr::is_empty(request$DetalheParlamentar$Parlamentar)){
    stop("No data match your request. Have you checked the 'id' argument?")
  }
  request <- request$DetalheParlamentar$Parlamentar
  N <- NA_character_
  party <- request$FiliacaoAtual


  status <- request$MandatoAtual$DescricaoParticipacao
  if(is.null(status)){
    status <- "Suplente"
  }
  if(status == "Titular"){
    sup <- request$MandatoAtual$Suplentes %>% purrr::flatten()
    sups <- tibble::tibble(
      senator_id = purrr::map_chr(request, .null = N,
                                  "CodigoParlamentar") %>% disc(),
      senator_name = purrr::map_chr(request, .null = N, "NomeParlamentar") %>%
        disc(),
      senator_party_name = purrr::map_chr(party, .null = N,
                                          "NomePartido") %>% disc(),
      senator_party_abbr = purrr::map_chr(request, .null = N,
                                          "SiglaPartidoParlamentar") %>% disc(),
      senator_state = request$IdentificacaoParlamentar$UfParlamentar,
      senator_suplente = purrr::map_chr(sup, .null = N,
                                        "DescricaoParticipacao"),
      senator_suplente_name = purrr::map_chr(sup, .null = N,
                                             "NomeParlamentar"),
      senator_suplente_id = purrr::map_chr(sup, .null = N,
                                           "CodigoParlamentar")
    )
    if(isTRUE(ascii)){
      sups <- sups %>%
        dplyr::mutate(
          senator_name = stringi::stri_trans_general(
            senator_name, "Latin-ASCII"
          ),
          senator_suplente_name = stringi::stri_trans_general(
            senator_suplente_name, "Latin-ASCII"
          ),
          senator_party_name = stringi::stri_trans_general(
            senator_party_name, "Latin-ASCII"
          )
        )
    }
  } else{
    titular <- sup <- request$UltimoMandato$Titular

    sups <- tibble::tibble(
      senator_id = purrr::map_chr(request, .null = N,
                                  "CodigoParlamentar") %>% disc(),
      senator_name = purrr::map_chr(request, .null = N, "NomeParlamentar") %>%
        disc(),
      senator_party_name = purrr::map_chr(party, .null = N,
                                          "NomePartido") %>% disc(),
      senator_party_abbr = purrr::map_chr(request, .null = N,
                                          "SiglaPartidoParlamentar") %>% disc(),
      senator_state = purrr::map_chr(request, .null = N,
                                      "UfParlamentar") %>% disc(),
      senator_titular_name = titular$NomeParlamentar,
      senator_titular_id = titular$CodigoParlamentar
    )
    if(isTRUE(ascii)){
      sups <- sups %>%
        dplyr::mutate(
          senator_name = stringi::stri_trans_general(
            senator_name, "Latin-ASCII"
          ),
          senator_titular_name = stringi::stri_trans_general(
            senator_titular_name, "Latin-ASCII"
          ),
          senator_party_name = stringi::stri_trans_general(
            senator_party_name, "Latin-ASCII"
          )
        )
    }
  }
  return(sups)
}





#' @title Downloads and tidies information on senators' votes in the Federal
#' Senate
#' @description Downloads and tidies information on senators' votes in the Federal
#' Senate.
#' @param id \code{integer}. This number represents the id of the senator
#' you wish to get information on. These ids can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'id'.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso \code{sen_senator_list()}
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{
#' ant <- sen_senator_votes(id = 5529)
#'
#' # some have never voted, as they are suplentes:
#' sen_senator_votes(898)
#' }
#' @export
sen_senator_votes <- function(id = 0, ascii = TRUE){

  base_url <- "http://legis.senado.leg.br/dadosabertos/senador/" %p%
    id %p% "/votacoes"

  request <- httr::GET(base_url)
  request <- status(request)
  N <- NA_character_

  if(purrr::is_empty(request$VotacaoParlamentar$Parlamentar)){
    stop("Your search returned no results.")
  }

  request <- request$VotacaoParlamentar
  votes <- purrr::map(request, "Votacoes") %>%
    .$Parlamentar %>%
    .$Votacao


  Vote <- tibble::tibble(
    vote_date = purrr::map(votes, .null = N,
                                   "SessaoPlenaria") %>%
      purrr::map_chr(.null = N,"DataSessao"),
    vote_house = purrr::map(votes, .null = N,
                                    "SessaoPlenaria") %>%
      purrr::map_chr(.null = N,"NomeCasaSessao"),
    vote_session_id = purrr::map(votes, .null = N,
                                      "SessaoPlenaria") %>%
      purrr::map_chr(.null = N,"CodigoSessao"),
    vote_session = purrr::map(votes, .null = N,
                                      "SessaoPlenaria") %>%
      purrr::map_chr(.null = N,"SiglaTipoSessao"),
    bill_id =  purrr::map(votes, .null = N,
                                       "IdentificacaoMateria") %>%
      purrr::map_chr(.null = N,"CodigoMateria"),
    bill_number = purrr::map(votes, .null = N,
                                        "IdentificacaoMateria") %>%
      purrr::map_chr(.null = N,"NumeroMateria"),
    bill_type = purrr::map(votes, .null = N,
                                        "IdentificacaoMateria") %>%
      purrr::map_chr(.null = N,"SiglaSubtipoMateria"),
    bill_year = purrr::map(votes, .null = N,
                                        "IdentificacaoMateria") %>%
      purrr::map_chr(.null = N,"AnoMateria"),
    secret_vote = purrr::map_chr(votes, .null = N,
                                     "IndicadorVotacaoSecreta"),
    bill_result = purrr::map_chr(votes, .null = N,
                                 "DescricaoResultado"),
    bill_details = purrr::map_chr(votes, .null = N,
                                  "DescricaoVotacao"),
    senator_vote = purrr::map_chr(votes, .null = N,
                                  "DescricaoVoto")
  )

  Vote <- Vote %>%
    dplyr::mutate(
      vote_date = lubridate::parse_date_time(
        vote_date, "Ymd"
      )
    )
  if(isTRUE(ascii)){
    Vote <- Vote %>%
      dplyr::mutate(
        secret_vote = stringi::stri_trans_general(
          secret_vote, "Latin-ASCII"
        ),
        bill_result = stringi::stri_trans_general(
          bill_result, "Latin-ASCII"
        ),
        bill_details = stringi::stri_trans_general(
          bill_details, "Latin-ASCII"
        ),
        senator_vote = stringi::stri_trans_general(
          senator_vote, "Latin-ASCII"
        )
      )
  }

  Vote <- Vote %>%
    dplyr::mutate(
      secret_vote = ifelse(secret_vote == "Sim", "Yes", "No"),
      bill_result = ifelse(bill_result == "Aprovado", "Passed",
                           ifelse(bill_result == "Rejeitado", "Rejected",
                                  "No information"))
    )
  return(Vote)
}






#' @title Downloads and tidies information on senators' mandates in the Federal
#' Senate
#' @description Downloads and tidies information on senators' mandates in the Federal
#' Senate.
#' @param id \code{integer}. This number represents the id of the senator
#' you wish to get information on. These ids can be extracted from the API
#' using the \code{sen_senator_list()} function, where they will appear as the
#' first column in the data frame returned, under the name 'id'.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @seealso \code{sen_senator_list()}
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' terms <- sen_senator_mandates(id = 4763)
#' terms <- sen_senator_mandates(id = 3398)
#' @export
sen_senator_mandates <- function(id = 0, ascii = TRUE){

  url <- "http://legis.senado.leg.br/dadosabertos/senador/" %p%
    id %p% "/mandatos"

  req <- httr::GET(url)
  req <- status(req)
  N <- NA_character_

  req <- req$MandatoParlamentar$Parlamentar
  if(!purrr::is_empty(req$Mandatos$Mandato)){
    if(depth(req$Mandatos) < 6){
      mand <- req$Mandatos
    } else{
      mand <- req$Mandatos %>% purrr::flatten()
    }
    prim <- purrr::map(mand, .null = N, "PrimeiraLegislaturaDoMandato")
    seg <- purrr::map(mand, .null = N, "SegundaLegislaturaDoMandato")

    terms <- tibble::tibble(
      senator_id = purrr::map_chr(req, .null = N, "CodigoParlamentar") %>%
        disc(),
      senator_name = purrr::map_chr(req, .null = N, "NomeParlamentar") %>%
        disc(),
      senator_party = purrr::map_chr(req, .null = N,
                                     "SiglaPartidoParlamentar") %>% disc(),
      senator_state = purrr::map_chr(req, .null = N, "UfParlamentar") %>%
        disc(),
      mandate_id = purrr::map_chr(mand, .null = N, "CodigoMandato"),
      mandate_status = purrr::map_chr(mand, .null = N, "DescricaoParticipacao"),
      mandate_state = purrr::map_chr(mand, .null = N, "UfParlamentar"),
      legislature_initial = purrr::map_chr(prim, .null = N,
                                           "NumeroLegislatura"),
      legislature_initial_start_date = purrr::map_chr(prim, .null = N,
                                                      "DataInicio"),
      legislature_initial_end_date = purrr::map_chr(prim, .null = N,
                                                    "DataFim"),
      legislature_second = purrr::map_chr(seg, .null = N,
                                          "NumeroLegislatura"),
      legislature_second_initial_date = purrr::map_chr(seg, .null = N,
                                                   "DataInicio"),
      legislature_second_end_date = purrr::map_chr(seg, .null = N,
                                                   "DataFim")
    ) %>%
      dplyr::mutate(
        legislature_initial_start_date = lubridate::parse_date_time(
          legislature_initial_start_date, "Ymd"
        ),
        legislature_initial_end_date = lubridate::parse_date_time(
          legislature_initial_end_date, "Ymd"
        ),
        legislature_second_initial_date = suppressWarnings(
          lubridate::parse_date_time(
          legislature_second_initial_date, "Ymd"
        )
        ),
        legislature_second_end_date = suppressWarnings(
          lubridate::parse_date_time(
          legislature_second_end_date, "Ymd"
        )
        )) %>%
      dplyr::select_if(colSums(!is.na(.)) > 0)


    return(terms)
  } else{
    terms <- tibble::tibble(
      senator_id = purrr::map_chr(req, .null = N, "CodigoParlamentar") %>%
        disc(),
      senator_name = purrr::map_chr(req, .null = N, "NomeParlamentar") %>%
        disc(),
      senator_party = purrr::map_chr(req, .null = N,
                                     "SiglaPartidoParlamentar") %>% disc(),
      mandate_status = "Suplente"
    )
    return(terms)
  }
}
