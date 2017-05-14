#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom magrittr '%>%'
#' @importFrom stringi stri_trans_general
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr flatten
#' @importFrom tibble tibble
#' @importFrom lubridate parse_date_time
#' @title Information on positions (jobs) that legislators may occupy
#' in commissions in the Federal Senate
#' @description Information on positions (jobs) that legislators may occupy
#' in commissions in the Federal Senate.
#' @param active \code{character}. If "No", returns all positions, otherwise
#' returns positions that are presently active.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @details Returns a data frame with the following variables:
#' \itemize{
#'  \item{\code{comm_position_id: }}{unique code for each position.}
#'  \item{\code{comm_position: }}{name of the position.}
#'  \item{\code{comm_position_active: }}{whether the position is currently active or not.}
#' }
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' jobs <- sen_commission_positions(active = "No")
#' @export
sen_commission_positions <- function(active = c("Yes", "No"),
                                     ascii = TRUE){


  act <- match.arg(active)
  if(act == "No") {
    url <- "http://legis.senado.gov.br/dadosabertos/comissao/lista/tiposCargo?indAtivos=N"
  } else{
    url <- "http://legis.senado.gov.br/dadosabertos/comissao/lista/tiposCargo"
  }

  req <- httr::GET(url)
  req <- status(req)
  req <- req$ListaTiposCargo$Cargos$Cargo
  N <- NA_character_

  jobs <- purrr::map_df(req, tibble::as_tibble) %>%
    stats::setNames(c('comm_position_id', 'comm_position', 'comm_position_active'))

  if(ascii == TRUE){
    jobs <- jobs %>%
      dplyr::mutate(comm_position_active = ifelse(
        comm_position_active == "S", "Yes", "No"
      ),
      comm_position = stringi::stri_trans_general(
        comm_position, "Latin-ASCII"
      ))
    return(jobs)
  } else{
    jobs <- jobs %>%
      dplyr::mutate(comm_position_active = ifelse(
        comm_position_active == "S", "Yes", "No"
      ))
    return(jobs)
  }
}






#' @title Information on commissions in the Federal Senate
#' @description Information on commissions in the Federal Senate.
#' @param active \code{character}. Options are "Yes" or "No". If "Yes", returns
#' only active commissions, otherwise all commissions.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @details Returns a data frame with the following variables:
#' \itemize{
#'  \item{\code{commission_id: }}{unique code for each commission.}
#'  \item{\code{commission_abbr: }}{Abbreviated name of the commission.}
#'  \item{\code{commission_name: }}{Commission name.}
#'  \item{\code{commission_purpose: }}{Objective of the commission.}
#'  \item{\code{commission_initial_date: }}{Commission starting date.}
#'  \item{\code{commission_public: }}{Whether the commission is public or not.}
#'  \item{\code{commission_house: }}{Legislative house where the commission is based.}
#'  \item{\code{commission_type: }}{Permanent, temporary, or a parliamentary inquiry.}
#' }
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' comms <- sen_commissions()
#' @export
sen_commissions <- function(active = c("Yes", "No"),
                                     ascii = TRUE){
  act <- match.arg(active)
  if(act == "No"){
    url <- "http://legis.senado.gov.br/dadosabertos/comissao/lista/colegiados?=indAtivos=N"
  } else{
    url <- "http://legis.senado.gov.br/dadosabertos/comissao/lista/colegiados"
  }

  req <- httr::GET(url)
  req <- status(req)
  req <- req$ListaColegiados$Colegiados$Colegiado
  N <- NA_character_

  coms <- tibble::tibble(
    commission_id = purrr::map_chr(req, .null = N, "Codigo"),
    commission_abbr = purrr::map_chr(req, .null = N, "Sigla"),
    commission_name = purrr::map_chr(req, .null = N, "Nome"),
    commission_purpose = purrr::map_chr(req, .null = N, "Finalidade"),
    commission_initial_date = purrr::map_chr(req, .null = N, "DataInicio"),
    commission_public = purrr::map_chr(req, .null = N, "Publica"),
    commission_house = purrr::map_chr(req, .null = N, "SiglaCasa"),
    commission_type = purrr::map_chr(req, .null = N,
                                     "DescricaoTipoColegiado")
  )

  coms <- coms %>%
    dplyr::mutate(commission_initial_date = lubridate::parse_date_time(
      commission_initial_date, "Ymd"),
    commission_public = ifelse(commission_public == "S", "Yes", "No")
    )

  if(ascii == TRUE){
    coms <- coms %>%
      dplyr::mutate(commission_type = stringi::stri_trans_general(
        commission_type, "Latin-ASCII"
      ),
      commission_name = stringi::stri_trans_general(
        commission_name, "Latin-ASCII"
      ),
      commission_purpose = stringi::stri_trans_general(
        commission_purpose, "Latin-ASCII"
      ))
    return(coms)
  } else{
    return(coms)
  }
}





#' @title Information on commissions in the Federal Senate, by commission type
#' @description Information on commissions in the Federal Senate, by commission type.
#' @param type \code{character}. Options are permanent, cpi and temporary.See details.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @details Returns a data frame with the following variables:
#' \itemize{
#'  \item{\code{commission_id: }}{unique code for each commission.}
#'  \item{\code{commission_name: }}{name of the commission.}
#'  \item{\code{commission_type: }}{permanent commission, temporary, or a parliamentary inquiry \emph{Comissão Parlamentar de Inquérito}.}
#'  \item{\code{commission_house: }}{Legislative house where the commission is based.}
#'  \item{\code{active: }}{Whether the commission is active or not.}
#' }
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' cpi <- sen_commissions_type(type = "cpi")
#' @export
sen_commissions_type <- function(type = c("permanent", "cpi", "temporary"),
                            ascii = TRUE){

  tipo <- match.arg(type)
  if(tipo == "permanent"){
    ty <- "permanente"
  } else if(tipo == "cpi"){
    ty <- "cpi"
  } else{
    ty <- "temporaria"
  }
  url <- "http://legis.senado.gov.br/dadosabertos/comissao/lista/" %p%
    ty

  req <- httr::GET(url)
  req <- status(req)
  if(ty %in% c("cpi", "temporaria")){
    req <- req$ListaBasicaComissoes$colegiado$colegiados
  } else if(ty == "permanente"){
    req <- req$ListaBasicaComissoes$colegiado$colegiados$colegiado
  }

  N <- NA_character_

  type_c <- purrr::map(req, .null = N, "tipocolegiado")

  comms <- tibble::tibble(
    commission_id = purrr::map_chr(req, .null = N, "CodigoColegiado"),
    commission_name = purrr::map_chr(req, .null = N, "NomeColegiado"),
    commission_abbr = purrr::map_chr(req, .null = N, "SiglaColegiado"),
    commission_type = purrr::map_chr(type_c, .null = N,
                                     "DescricaoTipoColegiado"),
    commission_house = purrr::map_chr(type_c, .null = N, "SiglaCasa"),
    active = purrr::map_chr(type_c, .null = N, "IndicadorAtivo")
  ) %>%
    dplyr::mutate(
      active = ifelse(active == "S", "Yes", "No")
    )

  if(ascii == TRUE){
    comms <- comms %>%
      dplyr::mutate(
        commission_name = stringi::stri_trans_general(
          commission_name, "Latin-ASCII"
        ),
        commission_type = stringi::stri_trans_general(
          commission_type, "Latin-ASCII"
        )
      )
    return(comms)
  } else{
    return(comms)
  }
}






#' @title Information on the senators who serve on a certain commission in the
#'  Federal Senate
#' @description Information on the senators who serve on a certain commission in the
#'  Federal Senate.
#' @param code \code{character}. Character code (abbreviation) of the
#' commission requested. A list of these may be obtained with \code{sen_commissions()}, although not all of the abbreviations in this data frame will return information.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @details Returns a data frame with the following variables:
#' \itemize{
#'  \item{\code{commission: }}{name of the commission.}
#'  \item{\code{commission_abbr: }}{abbreviated name of the commission.}
#'  \item{\code{senator_id: }}{unique code for each senator.}
#'  \item{\code{senator_name}}
#'  \item{\code{senator_party}}
#'  \item{\code{senator_state}}
#' }
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' # get info on the senators who serve on the CCJ commission:
#' ccj <- sen_commissions_senators(code = "CCJ")
#'
#' @export
sen_commissions_senators <- function(code = NULL, ascii = TRUE){

  if(!is.null(code)){
    code <- toupper(code)
  }

  url <- "http://legis.senado.gov.br/dadosabertos/materia/distribuicao/relatoria/" %p%
    code

  req <- httr::GET(url)
  req <- status(req)
  N <- NA_character_
  req <- req$DistribuicaodeRelatoria$Totais$Parlamentares

  com <- tibble::tibble(
    commission = purrr::map_chr(req, .null = N, "Comissao"),
    commission_abbr = purrr::map_chr(req, .null = N, "SiglaComissao"),
    senator_id = purrr::map_chr(req, .null = N, "CodigoParlamentar"),
    senator_name = purrr::map_chr(req, .null = N, "Parlamentar"),
    senator_party = purrr::map_chr(req, .null = N, "Partido"),
    senator_state = purrr::map_chr(req, .null = N, "Uf")
  ) %>%
    dplyr::mutate(
      senator_name = gsub("Senador ", "", senator_name),
      senator_name = gsub("Senadora ", "", senator_name)
    )

  if(ascii == TRUE){
    com <- com %>%
      dplyr::mutate(
        commission = stringi::stri_trans_general(
          commission, "Latin-ASCII"
        ),
        senator_name = stringi::stri_trans_general(
          senator_name, "Latin-ASCII"
      )
      )
    return(com)
  } else{
    return(com)
  }
}



