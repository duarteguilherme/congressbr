# capturing responses for tests

# parameters taken from examples

# chamber:
camara <- c(
  rep("http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/", 6),
  "http://www.camara.leg.br/SitCamaraWS/Deputados.asmx/"
  )

camara_params <- c(
  "ObterProposicao?tipo=PL&numero=3962&ano=2008",                     # cham_bill_info
  "ObterProposicaoPorID?IdProp=14784",                                # cham_bill_info_id
  "ListarProposicoes?sigla=PL&ano=2011",                              # cham_bills
  "ListarProposicoesVotadasEmPlenario?tipo=PL&ano=2008&numero=1247",  # cham_plenary_bills
  "ListarTiposAutores",                                               # cham_typeauthors_bills
  "ObterVotacaoProposicao?tipo=PL&numero=1992&ano=2007",              # cham_votes
  "ObterDeputados"                                                    # cham_deputy_list
)


# capture response
#start_capturing()
purrr::map2(camara, camara_params, paste0) %>% purrr::map(httr::GET)


# senate:
senate <- c("http://legis.senado.gov.br/dadosabertos/")

senate_params <- c(
  "agenda/20161105/20161125",            # sen_agenda
  "materia/pesquisa/lista?ano=2017",     # sen_bill_search
  "materia/110428",                      # sen_bills

)
# sen_agenda
GET("http://legis.senado.gov.br/dadosabertos/agenda/20161105/20161125")
# sen_bill_search
GET("http://legis.senado.gov.br/dadosabertos/materia/pesquisa/lista?ano=2017")
# sen_bills
GET("http://legis.senado.gov.br/dadosabertos/materia/110428")
# sen_bills_various
GET("http://legis.senado.gov.br/dadosabertos/materia/subtipos")
GET("http://legis.senado.gov.br/dadosabertos/materia/tiposPrazo")
GET("http://legis.senado.gov.br/dadosabertos/materia/assuntos")
GET("http://legis.senado.gov.br/dadosabertos/materia/tramitando?")
GET("http://legis.senado.gov.br/dadosabertos/materia/legislaturaatual?")
GET("http://legis.senado.gov.br/dadosabertos/materia/situacaoatual/80406")
GET("http://legis.senado.gov.br/dadosabertos/materia/locais")
GET("http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/9123")
GET("http://legis.senado.leg.br/dadosabertos/materia/tiposatualizacoes")
# sen_budget
GET("http://legis.senado.gov.br/dadosabertos/orcamento/lista")
# sen_coalition_info
GET("http://legis.senado.gov.br/dadosabertos/blocoParlamentar/200")
# sen_coalitions
GET("http://legis.senado.gov.br/dadosabertos/blocoParlamentar/lista")
# sen_commission_list
GET("http://legis.senado.gov.br/dadosabertos/comissao/lista/tiposCargo")
GET("http://legis.senado.gov.br/dadosabertos/comissao/lista/colegiados")
GET("http://legis.senado.gov.br/dadosabertos/comissao/lista/cpi")
GET("http://legis.senado.gov.br/dadosabertos/materia/distribuicao/relatoria/CCJ")
# sen_plenary
GET("http://legis.senado.gov.br/dadosabertos/plenario/resultado/20160303")
GET("http://legis.senado.gov.br/dadosabertos/plenario/tiposSessao")
GET("http://legis.senado.gov.br/dadosabertos/plenario/agenda/mes/20140315")
GET("http://legis.senado.gov.br/dadosabertos/plenario/lista/liderancas")
# sen_senator
GET("http://legis.senado.gov.br/dadosabertos/parlamentar/3823")
# sen_senator_details
GET("http://legis.senado.leg.br/dadosabertos/senador/4763/mandatos")



#stop_capturing()
