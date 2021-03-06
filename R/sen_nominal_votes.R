#' Nominal votes in the Brazilian Federal Senate
#' @description This is a dataset of the nominal votes in the Brazilian
#' Federal Senate, from all those available on the API from 1991 onwards.
#' @note These data can easily be grouped by legislature if so desired, using the
#' \code{legislature} variable.
#' @return
#' \itemize{
#'  \item{\code{vote_date: }}{\code{POSIXct}, date the vote took place.}
#'  \item{\code{bill_id: }}{id of the bill in the Senate API database.}
#'  \item{\code{bill: }}{bill type, year and number.}
#'  \item{\code{legislature: }}{legislature number.}
#'  \item{\code{senator_id: }}{unique id of the senator.}
#'  \item{\code{senator_name: }}{the senator's name.}
#'  \item{\code{senator_vote: }}\code{numeric}{vote cast. 1 = "yes"; 0 = "no", NA = other.}
#'  \item{\code{senator_party: }}{political party the senator was in when the vote took place.}
#'  \item{\code{senator_state: }}{state the senator represented when the vote took place.}
#' }
#'
#' @format A data frame with 60691 rows and 8 variables
"sen_nominal_votes"
#> [1] "sen_nominal_votes"
