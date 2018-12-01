# capturing responses for tests
# start_capturing()
start_capturing()
# cham_bill_info
GET("http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicao?tipo=PL&numero=3962&ano=2008")
# cham_bill_info_id
GET("http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp=14784")
# cham_bills
GET("http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoes?sigla=PL&ano=1998")


# sen_agenda
GET("http://legis.senado.gov.br/dadosabertos/agenda/20161105/20161125")


stop_capturing()
