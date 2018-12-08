#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom lubridate parse_date_time
#' @importFrom stringi stri_trans_general
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr modify_depth
#' @title Downloads and tidies data on specific coalitions in the Federal Senate
#' @description Downloads and tidies data on specific coalitions in the Federal Senate.
#' @param code \code{integer}. Code of the coalition. If not known (the most likely
#' case), these codes can be obtained from the \code{sen_coalitions()} function.
#' @param ascii \code{logical}. If TRUE, names are converted to ascii format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame},
#' with variables:
#' \itemize{
#'  \item{\code{bloc_code: }}{unique code given to each coalition.}
#'  \item{\code{house: }}{legislative house for the coalition.}
#'  \item{\code{bloc_name: }}{name of the coalition.}
#'  \item{\code{bloc_label: }}{additional label for the coalition.}
#'  \item{\code{date_created: }}{\code{POSIXct}, date the coalition was created.}
#'  \item{\code{member_code: }}{party code.}
#'  \item{\code{member_date_joined: }}{\code{POSIXct}, date when the party first joined the coalition.}
#'  \item{\code{member_abbr: }}{party acronym.}
#'  \item{\code{member_name: }}{party name.}
#'  \item{\code{member_date_created:: }}{\code{POSIXct}, date when the coalition was created.}
#' }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' moderador <- sen_coalition_info(code = 200)
#' @export
sen_coalition_info <- function(code = NULL, ascii = TRUE){

  if(is.null(code)){
    stop("'code' is necessary.")
  }

  url <- "http://legis.senado.gov.br/dadosabertos/blocoParlamentar/" %p%
    code

  request <- httr::GET(url)
  request <- status(request)

  blocos <- request$blocos$bloco

  result <- tibble::tibble(
    bloc_code = blocos$idBloco,
    house = blocos$siglaBloco,
    bloc_name = blocos$nomeBloco,
    bloc_label = blocos$nomeApelidoBloco,
    date_created = lubridate::parse_date_time(
      blocos$dataCriacao, orders = "dmY"))

  compo <- blocos$composicaoBloco$composicao_bloco

  comp <- tibble::tibble(
    coalition_id = purrr::map_chr(compo, "@id"),
    bloc_code = purrr::map_chr(compo, "idBloco"),
    member_code = purrr::map_chr(compo, "idPartido"),
    member_data_joined = lubridate::parse_date_time(
      purrr::map_chr(compo, "dataAdesao"), orders = "dmY")
  )

  parties <- purrr::modify_depth(compo, 1, "partido")

  party <- tibble::tibble(
    member_code = purrr::map_chr(parties, "idPartido"),
    member_abbr = purrr::map_chr(parties, "siglaPartido"),
    member_name = purrr::map_chr(parties, "nomePartido"),
    member_date_created = lubridate::parse_date_time(
      purrr::map_chr(parties, "dataCriacao"), orders = "dmY")
  )

  cp <- suppressMessages(dplyr::full_join(comp, party))

  result <- suppressMessages(dplyr::full_join(result, cp))

  if(!isTRUE(ascii)){
    return(result)
  } else{
    result <- result %>%
      dplyr::mutate(bloc_name = stringi::stri_trans_general(bloc_name,
                                                     "Latin-ASCII"),
             member_name = stringi::stri_trans_general(member_name,
                                                       "Latin-ASCII"))
    return(result)
  }
}
