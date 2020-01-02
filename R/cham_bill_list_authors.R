#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @title Downloads list of authors of a specific bill by providing id of a bill
#' @description Downloads list of authors of a specific bill by providing id of a bill
#' @param bill_id \code{integer}. The id number of the bill.
#' @return A data-frame.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{
#' cham_bill_list_authors(321182)
#' }
#' @export
cham_bill_list_authors <- function(bill_id) {
  paste0("https://dadosabertos.camara.leg.br/api/v2/proposicoes/", bill_id, "/autores") %>%
    jsonlite::fromJSON() %>%
    `[[`('dados') %>%
    dplyr::select(legislator_id = .data$uri,
                  legislator_name = .data$nome,
                  legislator_type = .data$tipo,
                  signature_order = .data$ordemAssinatura,
                  main_author = .data$proponente) %>%
    dplyr::mutate(legislator_id = sub("https://dadosabertos.camara.leg.br/api/v2/deputados/",
                                 "", .data$legislator_id),
                  bill_id = bill_id)
}
