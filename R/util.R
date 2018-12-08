#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Emulate '+' python function
`%p%` <- function(e1, e2) return(paste0(e1, e2))


# response status check
status <- function(x){
  if(x$status_code != 200){
    stop("GET request failed. Please check the validity of the information you requested.")
  } else{
    xx <- httr::content(x, as = "parsed")

  }
  if(is.null(xx)){
    stop("No data matches your search.")
  } else{
    return(xx)
  }
}

status2 <- function(x){
  if(x$status_code != 200){
    stop("GET request failed. Please check the validity of the information you requested.")
  } else{
    xx <- httr::content(x, as = "parsed", encoding = "latin1")

  }
  if(is.null(xx)){
    stop("No data matches your search.")
  } else{
    return(xx)
  }
}

'%ni%' <- Negate('%in%')


# depth of list check
depth <- function(x) ifelse(is.list(x), 1L + max(sapply(x, depth)), 0L)
#(http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r/13433689)

# discard NA in vector
disc <- function(x){
  x <- as.character(x) %>% purrr::discard(is.na)
  if(purrr::is_empty(x)){
    x <- NA
  }
  return(x)
}


## goddamn you global variables check
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "senator_bills_details", "senator_commissions_date_joined",
    "senator_commissions_name", "senator_date_of_birth", "senator_name",
    "senator_office_address", "senator_party_date_joined", "senator_party_name",
    "senator_positions_commission_date_start", "senator_suplente_name",
    "senator_titular_name", "senator_vote session_date", "session_description",
    "session_number", "session_type", "sigla", "sit_description", "situation_date",
    "situation_place", "sponsor_abbr", "sponsor_name", "sponsor_party", "topic_general",
    "topic_specific", "unit_name", "update_date", "vote_date", "vote_secret",
    'country_of_birth', 'event_description', 'event_type', 'head', 'id_rollcall',
    'leader_type', 'legislator_name', 'legislature_initial_end_date',
    'legislature_initial_start_date', 'legislature_second_end_date',
    'legislature_second_initial_date', 'limit_description', 'loc_date_created',
    'loc_house', 'loc_id', 'loc_name', 'loc_type_descr', 'mandate_end',
    'mandate_start','member_name', 'name', 'name_full', 'name_senator',
    'office_address', 'orientacao', 'party', 'party_name', 'place_of_birth',
    'queue', 'secret_vote', 'senator_vote',
    'session_date', 'agenda_commission_member', 'agenda_date', 'legislator_name',
    'agenda_commission_member', "bill_house", "bill_id", "bill_in_passage",
    "bill_indexing", "bill_location", "bill_location_house", "bill_number",
    "bill_passage_date", "vote_round",
    "bill_passage_dest_location", "bill_passage_destination",
    "bill_passage_orig_location",
    "bill_passage_origin", "bill_passage_text", "bill_passing", "bill_report",
    "bill_result", "bill_situation", "bill_situation_date", "bill_situation_place",
    "bill_sponsor", "bill_topic_general", "bill_topic_specific", "bill_topics",
    "bill_type", "bill_type_description", "bill_year", "bloc_label", "bloc_member_name",
    "bloc_name", "budget_sponsor", "budget_type_description", "comm_position",
    "comm_position_active", "commission", "commission_initial_date", "commission_name",
    "commission_public", "commission_purpose", "commission_type", "absence_description",
    "absence_end", "absence_start", "active", "affil_party_date_joined",
    "affil_party_date_left", "affil_party_name", "agenda_commission_relator",
    "agenda_name", "type", "number", "year", "id_bill",
    "agenda_place", "agenda_status", "agenda_title", "agenda_type", "bill",
    "bill_author", "bill_complementary", "bill_date_presented", "bill_description",
    "bill_details", "bill_details_short", "agenda_commission_relator", "agenda_title",
    "agenda_name", "agenda_type", "agenda_status", "agenda_place",
    "absence_description", "sen_bills_list", 'type', 'number', 'year',
    'initial_date', 'end_date', 'part_name_author', 'id_type_author',
    'abbreviation_party_author', 'abbreviation_st_author', 'legislator_vote',
    "bill_details", "legislator_party"))

