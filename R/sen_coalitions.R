#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom lubridate parse_date_time
#' @importFrom stringi stri_trans_general
#' @importFrom magrittr '%>%'
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom purrr map
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
#'  \item{\code{member_code: }}{party code.}
#'  \item{\code{member_abbr: }}{party acronym.}
#'  \item{\code{member_name: }}{party name.}
#'  \item{\code{member_date_joined: }}{\code{POSIXct}, date when the party first joined the coalition.}
#'  \item{\code{member_date_left:: }}{\code{POSIXct}, date when the party left the coalition.}
#' }
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' coalitions <- sen_coalitions()
#' @export
sen_coalitions <- function(members =TRUE, ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/blocoParlamentar/lista"

  request <- httr::GET(base_url)
  # status checks
  request <- status(request)
  request <- request$ListaBlocoParlamentar$Blocos$Bloco

  bloc <- dplyr::data_frame(
    bloc_code = purrr::map_chr(request, "CodigoBloco"),
    bloc_name = purrr::map_chr(request, "NomeBloco"),
    bloc_label = purrr::map_chr(request, "NomeApelido"),
    # bloc_abbr = purrr::map_chr(request, "SiglaBloco"),
    # this just returns "Bloco", it's probably due to the
    # API being in development. For future versions of this
    # package.
    date_created = lubridate::parse_date_time(
      purrr::map_chr(request, "DataCriacao"), orders = "Ymd")
  )

  if(members == FALSE & ascii == FALSE){
    return(bloc)
  } else if(members == FALSE & ascii == TRUE){
    bloc$bloc_name <- stringi::stri_trans_general(bloc$bloc_name,
                                                  "Latin-ASCII")
    return(bloc)
  } else{
    members <- purrr::at_depth(request, 1, "Membros")
    names(members) <- bloc$bloc_code
    members <- purrr::compact(members)
    part <- purrr::at_depth(members, 1, "Membro")
    part <- purrr::at_depth(part, 2, "Partido")
    # variable to join with:
    for(i in 1:length(part)){
      for(j in 1:length(part[[i]])){
        part[[i]][[j]]$bloc_code <- names(part)[[i]]
      }
    }

    part <- purrr::flatten(part)

    parties <- dplyr::data_frame(
      bloc_code = purrr::map_chr(part, "bloc_code"),
      member_code = purrr::map_chr(part, "CodigoPartido"),
      member_abbr = purrr::map_chr(part, "SiglaPartido"),
      member_name = purrr::map_chr(part, "NomePartido")
    )

    date_j <- purrr::at_depth(members, 3, "DataAdesao")
    date_j <- purrr::flatten(date_j)
    date_j <- purrr::flatten(date_j)
    date_j <- purrr::map_if(date_j, is.null, ~NA)
    parties$member_date_joined <- suppressWarnings(
      lubridate::parse_date_time(date_j, orders = "Ymd"))

    date_l <- purrr::at_depth(members, 3, "DataDesligamento")
    date_l <- purrr::flatten(date_l)
    date_l <- purrr::flatten(date_l)
    date_l <- purrr::map_if(date_l, is.null, ~NA)
    parties$member_date_left <- suppressWarnings(
      lubridate::parse_date_time(date_l, orders = "Ymd"))

    bloco <- suppressMessages(dplyr::full_join(bloc, parties))

    if(ascii == FALSE){
      return(bloco)
    } else{
      bloco <- bloco %>%
        dplyr::mutate(bloc_name = stringi::stri_trans_general(bloc_name,
                                                       "Latin-ASCII"),
                      member_name = stringi::stri_trans_general(member_name,
                                                         "Latin-ASCII"))
      return(bloco)
    }
  }
  }
