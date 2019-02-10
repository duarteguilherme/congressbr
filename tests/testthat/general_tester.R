library(dplyr); library(tibble); library(purrr); library(magrittr)

test_table <- tribble(
  ~funcs, ~params,
  "cham_bill_info", list(type = "PL", number = "3962", year = "2008"),
  "cham_bill_info_id", list(14784),
  "cham_bills", list(type = "PL", year = 2011, number = 2718,
                     initial_date="2011-16-11"),
  "cham_legislator_list", list(),
  "cham_plenary_bills", list(year = 2008),
  "cham_typeauthors_bills", list(),
  "cham_votes", list(type = "PL", number = "1992", year = "2007"),
  "sen_agenda", list(initial_date = "20161105", end_date = "20161125"),
  "sen_bill_search", list(year = 2014),
  "sen_bills", list(type = "PLS", number = 5, year = 2010),
  "sen_bills_types", list(),
  "sen_bills_limits", list(),
  "sen_bills_topics", list(),
  "sen_bills_passing", list(year = "2001", type = "MPV"),
  "sen_bills_current", list(year = 2015, type = "PLS"),
  "sen_bills_status", list(bill_id = 80406),
  "sen_bills_locations", list(),
  "sen_bills_passage", list(bill_id = 9123),
  "sen_bills_situations", list(),
  "sen_bills_updates", list(type = "PLS", days = 10),
  "sen_bills_update_types", list(),
  "sen_budget", list(),
  "sen_coalition_info", list(code = 200),
  "sen_coalitions", list(),
  "sen_commission_positions", list(active = "No"),
  "sen_commissions", list(),
  "sen_commissions_type", list(type = "permanent"),
  "sen_commissions_senators", list(code = "CCJ"),
  "sen_plenary_result", list(date = "20110405"),
  "sen_plenary_sessions", list(),
  "sen_plenary_agenda", list(period = "day", date = "20160401"),
  "sen_plenary_leaderships", list(),
  "sen_senator", list(id = 3823),
  "sen_senator_details", list(id = 4981),
  "sen_senator_bills", list(id = 5164),
  "sen_senator_commissions", list(id = 715),
  "sen_senator_suplentes", list(id = 90),
  "sen_senator_votes", list(id = 5529),
  "sen_senator_mandates", list(id = 4763),
  "sen_senator_list", list(),
  "sen_senator_legis", list(start = 50),
  "sen_bill_sponsors", list(),
  "sen_sponsor_types", list(),
  "sen_parties", list(),
  "sen_statement_list", list(),
  "sen_bills_list", list(active = TRUE),
  "sen_votes_year", list("2013"),
 ) %>%
  mutate(result = tryCatch(invoke_map(funcs, params),
                           error = function(e) {
                             print(funcs)
                             }
                           ))

result = list()
#for(i in 1:length(test_table$funcs)){
for(i in 3){
  result[[i]] = possibly(
    invoke_map(test_table$funcs[[i]], test_table$params[[i]]),
    otherwise = "error")
}

names(result) <- test_table$funcs
