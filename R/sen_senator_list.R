#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom lubridate parse_date_time
#' @title Downloads and tidies information on the senators in the Federal Senate
#' @description Downloads and tidies information on the senators in the Federal Senate.
#' @param present \code{logical}. If \code{TRUE}, downloads data on the legislature
#' currently sitting in the Federal Senate, otherwise returns information on
#' senators who are currently absent.
#' @param state \code{character}. Two-letter abbreviation of Brazilian state. A list of these is
#' available with the function \code{UF()}.
#' @param status \code{character}, either "T" or "S", representing
#' \emph{titular} or \emph{suplente} (stand-in senator), respectively.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \dontrun{all <- sen_senator_list()}
#'
#' # Who represents Rio de Janeiro?
#' \dontrun{
#' rj <- sen_senator_list(state = "RJ")
#' }
#' @export
sen_senator_list <- function(present = TRUE, state = NULL,
                             status = NULL, ascii = TRUE){


  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/lista/"

  if(present == TRUE){
    present <- "atual?"
    base_url <- base_url %p% present
  } else{
    present <- "afastados"
    base_url <- base_url %p% present
  }

  if(!is.null(state)){
    ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
             "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
             "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
             "SP", "SE", "TO")
    if(state %ni% ufs){
      stop("Please enter a valid state. A list can be obtained from 'UF()'.")
    }
    base_url <- base_url %p% "uf=" %p% state
  }
  if(!is.null(state) & !is.null(status)){
    if(status %ni% c("T", "S")){
      stop("Please enter a valid status argument, 'T' or 'S'.")
    }
    base_url <- base_url %p% "uf=" %p% state %p% "&participacao=" %p% status
  }
  if(is.null(state) & !is.null(status)){
    base_url <- base_url %p% "participacao=" %p% status
  }

  request <- httr::GET(base_url)
  request <- status(request)

  if(present == "afastados"){
    request <- request$AfastamentoAtual$Parlamentares$Parlamentar
  } else{
    request <- request$ListaParlamentarEmExercicio$Parlamentares$Parlamentar
  }

  par <- purrr::map(request, "IdentificacaoParlamentar")
  null <- NA_character_

  parl <- tibble::tibble(
    id = purrr::map_chr(par, "CodigoParlamentar", .null = null),
    name_full = purrr::map_chr(par, "NomeCompletoParlamentar",
                               .null = null),
    name_senator = purrr::map_chr(par, "NomeParlamentar",
                                  .null = null),
    gender = purrr::map_chr(par, "SexoParlamentar",
                            .null = null),
    foto_url = purrr::map_chr(par, "UrlFotoParlamentar",
                              .null = null),
    page_url = purrr::map_chr(par, "UrlPaginaParlamentar",
                              .null = null),
    office_email = purrr::map_chr(par, "EmailParlamentar",
                                  .null = null),
    party_abbr = purrr::map_chr(par, "SiglaPartidoParlamentar",
                                .null = null)
  )

  mand <- purrr::map(request, "Mandato")
  prim <- purrr::map(mand, "PrimeiraLegislaturaDoMandato")
  seg <- purrr::map(mand, "SegundaLegislaturaDoMandato")

  mandate <- tibble::tibble(
    id_mandate = purrr::map_chr(mand, "CodigoMandato", .null = null),
    state = purrr::map_chr(mand, "UfParlamentar", .null = null),
    status = purrr::map_chr(mand, "DescricaoParticipacao"),
    num_legislature_first_term = purrr::map_chr(prim, "NumeroLegislatura",
                                                .null = null),
    first_term_start = suppressWarnings(
      lubridate::parse_date_time(
        purrr::map_chr(prim, "DataInicio", .null = null),
        orders = "Ymd")),
    first_term_end = suppressWarnings(
      lubridate::parse_date_time(
        purrr::map_chr(prim, "DataFim", .null = null),
        orders = "Ymd")),
    num_legislature_second_term = purrr::map_chr(seg, "NumeroLegislatura",
                                                 .null = null),
    second_term_start = suppressWarnings(
      lubridate::parse_date_time(
        purrr::map_chr(seg, "DataInicio", .null = null),
        orders = "Ymd")),
    second_term_end = suppressWarnings(
      lubridate::parse_date_time(
        purrr::map_chr(seg, "DataFim", .null = null),
        orders = "Ymd")))

  result <- dplyr::bind_cols(parl, mandate)

  if(ascii == TRUE){
    result <- result %>%
      mutate(name_full = stringi::stri_trans_general(name_full,
                                                     "Latin-ASCII"),
             name_senator = stringi::stri_trans_general(name_senator,
                                                        "Latin-ASCII"))
  }
  return(result)
}



#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom lubridate parse_date_time
#' @title Downloads and tidies information on the senators in the Federal Senate
#' @description Downloads and tidies information on the senators in the Federal Senate.
#' @param start two-digit integer representing the first legislature of the
#' time period requested.
#' @param end two-digit integer representing the final legislature of the time
#'  period requested.
#' @param state \code{character}. Two-letter abbreviation of Brazilian state. A list of these is
#' available with the function \code{UF()}.
#' @param status \code{character}, either "T" or "S", representing
#' \emph{titular} or \emph{suplente} (stand-in senator), respectively.
#' @param ascii \code{logical}. If \code{TRUE}, strips Latin characters from
#' strings.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \dontrun{
#' all <- sen_senator_legis(start = 50)
#' }
#' @export
sen_senator_legis <- function(start = NULL, end = NULL,
                              state = NULL, status = NULL,
                              ascii = TRUE){

  base_url <- "http://legis.senado.gov.br/dadosabertos/senador/lista/legislatura/"

  # checks:
  if(!is.null(state)){
    ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
             "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
             "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
             "SP", "SE", "TO")
    if(state %ni% ufs){
      stop("Please enter a valid state. A list can be obtained from 'UF()'.")
    }
  }
  if(!is.null(status)){
    if(status %ni% c("T", "S")){
      stop("Please enter a valid status argument, 'T' or 'S'.")
    }
  }

  if(!is.null(start) ){
    if(nchar(start) > 2){
      stop("'start' must be a two-digit number.")
    }
  }
  if(!is.null(end) ){
   if(nchar(end) > 2){
    stop("'end' must be a two-digit number.")
   }
  }
  if(is.null(start) & !is.null(end)){
    stop("'end' cannot be used without 'start'.")
  }

  # urls
  if(!is.null(start) & is.null(end)){
    base_url <- base_url %p% start %p% "?"
  } else if(!is.null(start) & !is.null(end)){
    base_url <- base_url %p% start %p% "/" %p% end %p% "?"
  }

  if(!is.null(state) & !is.null(status)){
    base_url <- base_url %p% "uf=" %p% state %p% "&participacao=" %p%
      status
  } else if(!is.null(state) & is.null(status)){
    base_url <- base_url %p% "uf=" %p% state
  } else if(is.null(state) & !is.null(status)){
    base_url <- base_url %p% "participacao=" %p% status
  }

  request <- httr::GET(base_url)
  request <- status(request)
  request <- request$ListaParlamentarLegislatura$Parlamentares$Parlamentar

  par <- purrr::map(request, "IdentificacaoParlamentar")
  null <- NA_character_

  parl <- tibble::tibble(
    id = purrr::map_chr(par, "CodigoParlamentar", .null = null),
    name_full = purrr::map_chr(par, "NomeCompletoParlamentar",
                               .null = null),
    name_senator = purrr::map_chr(par, "NomeParlamentar",
                                  .null = null),
    gender = purrr::map_chr(par, "SexoParlamentar",
                            .null = null),
    foto_url = purrr::map_chr(par, "UrlFotoParlamentar",
                              .null = null),
    page_url = purrr::map_chr(par, "UrlPaginaParlamentar",
                              .null = null),
    office_email = purrr::map_chr(par, "EmailParlamentar",
                                  .null = null),
    party_abbr = purrr::map_chr(par, "SiglaPartidoParlamentar",
                                .null = null)
  )
  mand <- purrr::map(request, "Mandatos")
  mand <- purrr::map(mand, "Mandato")
  prim <- purrr::map(mand, "PrimeiraLegislaturaDoMandato")
  seg <- purrr::map(mand, "SegundaLegislaturaDoMandato")

  mandate <- tibble::tibble(
    id_mandate = purrr::map_chr(mand, "CodigoMandato", .null = null),
    state = purrr::map_chr(mand, "UfParlamentar", .null = null),
    status = purrr::map_chr(mand, "DescricaoParticipacao"),
    num_legislature_first_term = purrr::map_chr(prim, "NumeroLegislatura",
                                            .null = null),
    first_term_start = suppressWarnings(
      lubridate::parse_date_time(
      purrr::map_chr(prim, "DataInicio", .null = null),
      orders = "Ymd")),
    first_term_end = suppressWarnings(
      lubridate::parse_date_time(
        purrr::map_chr(prim, "DataFim", .null = null),
        orders = "Ymd")),
    num_legislature_second_term = purrr::map_chr(seg, "NumeroLegislatura",
                                                 .null = null),
    second_term_start = suppressWarnings(
      lubridate::parse_date_time(
        purrr::map_chr(seg, "DataInicio", .null = null),
        orders = "Ymd")),
    second_term_end = suppressWarnings(
      lubridate::parse_date_time(
        purrr::map_chr(seg, "DataFim", .null = null),
        orders = "Ymd")))

  result <- dplyr::bind_cols(parl, mandate)

  if(ascii == TRUE){
    result <- result %>%
      mutate(name_full = stringi::stri_trans_general(name_full,
                                                     "Latin-ASCII"),
             name_senator = stringi::stri_trans_general(name_senator,
                                                        "Latin-ASCII"))
  }

  return(result)
}
