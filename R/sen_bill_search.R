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
#' @title Search for data on legislation in the Brazilian Federal Senate
#' @description Search for data on legislation in the Brazilian Federal Senate.
#' @param year \code{integer}. Four-digit year, such as \code{2013}.
#' @param year_law \code{integer}. Year of introduction of the law, such as \code{2013}.
#' @param topic_id \code{}.
#' @param situation_id \code{}.
#' @param date_presented_init \code{}.
#' @param date_presented_end \code{}.
#' @param date_situation_init \code{}.
#' @param date_situation_end \code{}.
#' @param complementary \code{}.
#' @param present \code{}.
#' @param rapporteur \code{}.
#' @param author \code{}.
#' @param number \code{}.
#' @param type \code{}.
#' @param law_number \code{}.
#' @param keyword \code{}.
#' @param party_abbr_author \code{}.
#' @param author_type \code{}.
#' @param law_type \code{}.
#' @param author_state \code{}.
#' @param in_passage \code{}.
#' @param ascii \code{logical}. If TRUE, certain strings are converted to ascii
#' format.
#' @return A tibble, of classes \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @examples
#' # search for legislation from 2014:
#' two14 <- sen_bill_search(year = 2014)
#' @export
sen_bill_search <- function(year = "", year_law = "", topic_id = "",
                        situation_id = "", date_presented_init = "",
                        date_presented_end = "", date_situation_init = "",
                        date_situation_end = "", complementary = "",
                        present = "", rapporteur = "",
                        author = "", number = "", type = "",
                        law_number = "", keyword = "",
                        party_abbr_author = "", author_type = "",
                        law_type = "", author_state = "",
                        in_passage = "", ascii = TRUE){

  url <- "http://legis.senado.gov.br/dadosabertos/materia/pesquisa/lista?" %p%
    "ano=" %p% year %p%
    "&anoNorma=" %p% year_law %p%
    "&codigoAssunto=" %p% topic_id %p%
    "&codigoSituacao=" %p% situation_id %p%
    "&dataFimApresentacao=" %p% date_presented_end %p%
    "&dataFimSituacao=" %p% date_situation_end %p%
    "&dataInicioApresentacao=" %p% date_presented_init %p%
    "&dataInicioSituacao=" %p% date_situation_init %p%
    "&indicadorComplementar=" %p% complementary %p%
    "&indicadorSituacaoAtual=" %p% present %p%
    "&nomeAutor=" %p% author %p%
    "&nomeRelator=" %p% rapporteur %p%
    "&numero=" %p% number %p%
    "&numeroNorma=" %p% law_number %p%
    "&palavraChave=" %p% keyword %p%
    "&sigla=" %p% type %p%
    "&siglaPartidoAutor=" %p% party_abbr_author %p%
    "&tipoAutor=" %p% author_type %p%
    "&tipoNorma=" %p% law_type %p%
    "&tramitando=" %p% in_passage %p%
    "&ufAutor=" %p% author_state

  req <- httr::GET(url)
  req <- status(req)
  req <- req$PesquisaBasicaMateria$Materias$Materia
  N <- NA_character_

  authors <- purrr::map(req, .null = N, "AutoresPrincipais") %>%
    purrr::flatten()
  situation <- purrr::map(req, .null = N, "SituacaoAtual") %>%
    purrr::map(.null = N, "Autuacoes") %>% purrr::flatten()
  sit <- purrr::map(situation, .null = N, "Situacao")
  local <- purrr::map(situation, .null = N, "Local")

  votes <- tibble::tibble(
    bill_id = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "CodigoMateria") %>%
      disc(),
    bill_house = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "NomeCasaIdentificacaoMateria") %>%
      disc(),
    bill_number = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "NumeroMateria") %>%
      disc(),
    bill_type = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "DescricaoSubtipoMateria") %>%
      disc(),
    bill_year = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "AnoMateria") %>%
      disc(),
    bill_in_passage = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "IndicadorTramitando") %>%
      disc(),
    bill_complementary = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "IndicadorComplementar") %>%
      disc(),
    bill_date_presented = purrr::flatten(req) %>%
      purrr::map_chr(.null = N, "DataApresentacao") %>%
      disc(),
    bill_details = purrr::map(req, .null = N, "DadosBasicosMateria") %>%
      purrr::map_chr(.null = N, "EmentaMateria"),
    bill_topics = purrr::map(req, .null = N, "DadosBasicosMateria") %>%
    purrr::map_chr(.null = N,  "IndexacaoMateria"),
    bill_author = purrr::map_chr(authors, .null = N,
                                 "NomeAutor"),
    bill_situation = purrr::map_chr(sit, .null = N,
                                    "DescricaoSituacao"),
    bill_situation_date = purrr::map_chr(sit, .null = N,
                                         "DataSituacao"),
    bill_situation_id = purrr::map_chr(sit, .null = N,
                                       "SiglaSituacao"),
    bill_situation_place = purrr::map_chr(local, .null = N,
                                          "NomeLocal"),
    bill_situation_place_abbr = purrr::map_chr(local, .null = N,
                                               "SiglaLocal"),
    bill_situation_place_date = purrr::map_chr(local, .null = N,
                                               "DataLocal"),
    bill_situation_place_house = purrr::map_chr(local, .null = N,
                                                "NomeCasaLocal")
  )

  if(ascii == TRUE){
    votes <- votes %>%
      dplyr::mutate(
        bill_type = stringi::stri_trans_general(
          bill_type, "Latin-ASCII"
        ),
        bill_in_passage = stringi::stri_trans_general(
          bill_in_passage, "Latin-ASCII"
        ),
        bill_complementary = stringi::stri_trans_general(
          bill_complementary, "Latin-ASCII"
        ),
        bill_details = stringi::stri_trans_general(
          bill_details, "Latin-ASCII"
          ),
        bill_topics = stringi::stri_trans_general(
          bill_topics, "Latin-ASCII"
        ),
        bill_author = stringi::stri_trans_general(
          bill_author, "Latin-ASCII"
        ),
        bill_situation = stringi::stri_trans_general(
          bill_situation, "Latin-ASCII"
        ),
        bill_situation_place = stringi::stri_trans_general(
          bill_situation_place, "Latin-ASCII"
        )
      )
  }

  votes <- votes %>%
    dplyr::mutate(
      bill_date_presented = lubridate::parse_date_time(
        bill_date_presented, "Ymd"
      ),
      bill_situation_date = lubridate::parse_date_time(
        bill_situation_date, "Ymd"
      )
    )
  return(votes)
}
