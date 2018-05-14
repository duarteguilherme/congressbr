#' Nominal votes in the Brazilian Chamber of Deputies
#' @description This is a dataset of the nominal votes in the Brazilian
#' Chamber of Deputies, from all those available on the API from 1991 onwards.
#' @note These data can easily be grouped by legislature if so desired, using the
#' \code{legislature} variable.
#' @return
#' \itemize{
#'  \item{\code{vote_date: }}{\code{POSIXct}, date the vote took place.}
#'  \item{\code{rollcall_id: }}{id of the rollcall}
#'  \item{\code{bill: }}{bill type, year and number.}
#'  \item{\code{legislator_id: }}{unique id of the senator.}
#'  \item{\code{legislator_name: }}{the legislator's name.}
#'  \item{\code{legislator_vote: }}\code{numeric}{vote cast. 1 = "yes"; 0 = "no", NA = other.}
#'  \item{\code{legislator_party: }}{political party the legislator was in when the vote took place.}
#'  \item{\code{legislator_state: }}{state the senator represented when the vote took place.}
#'  \item{\code{GOV_orientation: }}{the leader of government's orientation. }
#' }
#'
#' @format A data frame with 60691 rows and 8 variables
"cham_nominal_votes"
#> [1] "cham_nominal_votes"
