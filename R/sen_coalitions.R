#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom lubridate parse_date_time
#' @importFrom stringi stri_trans_general
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom purrr map_if
#' @importFrom purrr flatten
#' @importFrom purrr map_chr
#' @importFrom purrr at_depth
#' @importFrom purrr compact
#' @title Downloads and tidies data on the coalitions in the Federal Senate.
#' @param members \code{logical}. If FALSE, returns only the first four columns
#' of the data frame.
#' @param ascii \code{logical}. If TRUE, names are converted to ascii format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame},
#' with variables:
#' \itemize{
#'  \item{\code{bloc_code: }}{unique code given to each coalition.}
#'  \item{\code{bloc_name: }}{name of the coalition.}
#'  \item{\code{bloc_label: }}{additional label for the coalition.}
#'  \item{\code{date_created: }}{\code{POSIXct}, date the coalition was created.}
#'  \item{\code{bloc_member_abbr: }}{party acronym.}
#'  \item{\code{bloc_member_name: }}{party name.}
#'  \item{\code{bloc_member_date_joined: }}{\code{POSIXct}, date when the party first joined the coalition.}
#'  \item{\code{member_date_left:: }}{\code{POSIXct}, date when the party left the coalition.}
#' }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' coalitions <- sen_coalitions()
#' coalitions_detail <- sen_coalitions(members = TRUE)
#' @export
sen_coalitions <- function(members = FALSE, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/blocoParlamentar/lista"

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaBlocoParlamentar$Blocos$Bloco
  N <- NA_character_

  bloc <- tibble::tibble(
    bloc_code = purrr::map_chr(request, "CodigoBloco", .null = N),
    bloc_name = purrr::map_chr(request, "NomeBloco", .null = N),
    bloc_label = purrr::map_chr(request, "NomeApelido", .null = N),
    date_created = suppressWarnings(
      lubridate::parse_date_time(
      purrr::map_chr(request, "DataCriacao", .null = N),
      orders = "Ymd"))
    )

  if(ascii == TRUE){
    bloc <- bloc %>%
      dplyr::mutate(
        bloc_name = stringi::stri_trans_general(
          bloc_name, "Latin-ASCII"),
        bloc_label = stringi::stri_trans_general(
          bloc_label, "Latin-ASCII")
      )
  }

  # Get members:

  if(members == TRUE){

    members <- purrr::at_depth(request, 1, "Membros")
    names(members) <- bloc$bloc_code
    members <- purrr::compact(members)
    part <- purrr::at_depth(members, 1, "Membro") %>%
      purrr::at_depth(2, "Partido")
    # variable to join with:
    for(i in 1:length(part)){
      for(j in 1:length(part[[i]])){
        part[[i]][[j]]$bloc_code <- names(part)[[i]]
      }
    }
    part <- purrr::flatten(part)
    parties <- tibble::tibble(
      bloc_code = purrr::map_chr(part, "bloc_code"),
      bloc_member_abbr = purrr::map_chr(part, "SiglaPartido"),
      bloc_member_name = purrr::map_chr(part, "NomePartido")
    )
    date_j <- purrr::at_depth(members, 3, "DataAdesao") %>%
      purrr::flatten() %>% purrr::flatten() %>%
      purrr::map_if(is.null, ~NA)
    parties$bloc_member_date_joined <- suppressWarnings(
      lubridate::parse_date_time(date_j, orders = "Ymd")
    )
    if(ascii == TRUE){
      parties <- parties %>%
        dplyr::mutate(
          bloc_member_name = stringi::stri_trans_general(
            bloc_member_name, "Latin-ASCII"
          )
        )
    }

    bloc <- suppressMessages(dplyr::full_join(bloc, parties))
  }
  return(bloc)
}
