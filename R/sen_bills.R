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
#'
#' @export
sen_bills_timeframe <- function(code = 0, commission = NULL,
                                legislator = NULL, type = NULL){

  base_url <- "http://legis.senado.gov.br/dadosabertos/materia/lista/prazo/" %p%
    code %p%
    "?siglaComissao=" %p% commission %p%
    "?parlamentar=" %p% legislator %p%
    "?tipoMateria=" %p% type

  request <- httr::GET(base_url)
  # status checks
  request <- status(request)

  #check exists!!!
  request <- request$MateriasCumprindoPrazo$Materias$Materia

  ids <- purrr::map_df(request, "IdentificacaoMateria")
  #check cols!!!
  colnames(ids) <- c("bill_code", "house_abbr", "house_name", "bill_type_abbr",
                     "bill_type_name", "bill_number", "bill_year", "in_passage")


  t_frames <- purrr::map(request, "Prazos")
  names(t_frames) <- ids$bill_code
  # variable to join with:
  for(i in 1:length(t_frames)){
    for(j in 1:length(t_frames[[i]])){
      t_frames[[i]][[j]]$bill_code <- names(t_frames)[[i]]
    }
  }
  t_frames <- t_frames %>%
    purrr::at_depth(2, purrr::flatten) %>%
    purrr::flatten()



  names(t_frames) <- NULL

  tf <- dplyr::data_frame(
    bill_code = purrr::map_chr(t_frames, "bill_code"),
    tf_code = purrr::map_chr(t_frames, "CodigoTipoPrazo"),
    tf_description = purrr::map_chr(t_frames, "DescricaoTipoPrazo"),
    tf_foundation = purrr::map_chr(t_frames, "DescricaoTipoFundamento"),
    tf_start = lubridate::parse_date_time(
      purrr::map_chr(t_frames, "DataInicioPrazo"), orders = "Ymd"),
    tf_end = lubridate::parse_date_time(
      purrr::map_chr(t_frames, "DataFimPrazo"), orders = "Ymd"),
    tf_description = purrr::map_chr(t_frames, "DescricaoPrazo"),
    tf_commission_code = purrr::map_chr(t_frames, "CodigoComissao"),
    tf_commission_abbr = purrr::map_chr(t_frames, "SiglaComissao"),
    tf_commission_name = purrr::map_chr(t_frames, "NomeComissao"),
    tf_commission_house = purrr::map_chr(t_frames, "SiglaCasaComissao")
  )

}
