#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom stringr str_trim
#' @title Downloads and tidies information on the legislators in the Chamber of Deputies
#' @description Downloads and tidies information on the legislators in the Federal Senate.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' \donttest{all <- cham_legislator_list()}
#'
#' @export
#'
cham_legislator_list <- function() {


  base_url <- "http://www.camara.leg.br/SitCamaraWS/Deputados.asmx/ObterDeputados"

  request <- GET(base_url)
  request <- content(request)

  legislators <- xml_find_all(request, './/deputado')
  return(map_df(legislators, extract_legislator_data))
}



extract_legislator_data <- function(legislator_data) {
  tibble(
    legislator_id = xml_find_all(legislator_data, 'ideCadastro') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_status = xml_find_all(legislator_data, 'condicao') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_name = xml_find_all(legislator_data, 'nome') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_cham_name = xml_find_all(legislator_data, 'nomeParlamentar') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_photo_url = xml_find_all(legislator_data, 'urlFoto') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_gender = xml_find_all(legislator_data, 'sexo') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_state = xml_find_all(legislator_data, 'uf') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_party = xml_find_all(legislator_data, 'partido') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_cabinet = xml_find_all(legislator_data, 'gabinete') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_building_address = xml_find_all(legislator_data, 'anexo') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_telephone_number = xml_find_all(legislator_data, 'fone') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_email = xml_find_all(legislator_data, 'email') %>% xml_text %>% str_trim %>% paste0(., ""),
    legislator_comissions = xml_find_all(legislator_data, 'comissoes') %>% xml_text %>% str_trim %>% paste0(., "")


  )
}


