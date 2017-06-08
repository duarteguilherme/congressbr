#' @importFrom httr POST
#' @importFrom xml2 read_html
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom tibble tibble
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate year
#' @export
sp_city_bill_authors <- function(year = "", type = "", number = ""){

  # nothing returns everything for current year

  link <- "http://splegisws.camara.sp.gov.br/ws/ws2.asmx?op=ProjetosAutores"

  url <- paste0(link, "&ano=", ano, "&tipo=", tipo, "&numero=", numero)

  res <- httr::GET(url)

  x <- xml2::read_xml(res)
  xml2::html_structure(x)
  xml2::xml_ns(x)

}
