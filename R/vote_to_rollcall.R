#' @importFrom pscl rollcall
#' @title Create rollcall or matrix format data structures from voting records
#' @description Returns votes from the Senate or Chamber of Deputies in a format
#' that can be easily used with other R packages (wnominate, pscl, MCMCpack) or
#' with other modelling languages such as JAGS.
#' @param votes \code{integer}. The recorded nominal votes. Must be in the format
#' 1, 0, where 1 indicates a 'Yes' vote, 0 a 'No' vote. Missing values must be
#' \code{NA}.
#' All of the \code{votes}, \code{legislators} and \code{bills} parameters must
#' be vectors.
#' @param legislators This may be a vector of legislator names or id numbers.
#' @param bills A vector of bill numbers or ids.
#' @param ideal If TRUE, the default, returns a \code{rollcall} object for use with
#' pscl and/or wnominate. If FALSE, returns a matrix suitable for use with MCMCpack or
#' the JAGS modelling language.
#' @examples
#' # get votes:
#' data(senate_nominal_votes)
#' votes <- senate_nominal_votes %>%
#'     dplyr::filter(vote_date >= "2005-03-01", vote_date <= "2007-12-15")
#' rc <- vote_to_rollcall(votes$senator_vote,
#'                        legislators = votes$senator_name,
#'                        bills = votes$bill_id)
#' @author Robert Myles McDonnell, Guilherme Jardim Duarte & Danilo Freire.
#' @export
vote_to_rollcall <- function(votes = NULL, legislators = NULL, bills = NULL,
                             ideal = TRUE){

  if(is.null(votes) | is.null(legislators) | is.null(bills)){
    stop("'votes', 'bills' and 'legislators' must have values.")
  }
  if(!is.vector(votes) | !is.vector(legislators) | !is.vector(bills)){
    stop("'votes', 'bills' and 'legislators' must be vectors.")
  }


  nameID <- unique(legislators); n <- length(nameID)
  voteID <- unique(bills); m <- length(voteID)

  rollCallMatrix <- matrix(NA, n, m)
  row_names <- match(legislators, nameID)
  col_names <- match(bills, voteID)

  for(i in 1:length(legislators)){
    rollCallMatrix[row_names[i], col_names[i]] <- votes[i]
  }

  rollCallMatrix <- matrix(as.numeric(unlist(rollCallMatrix)),
                           nrow = nrow(rollCallMatrix))
  dimnames(rollCallMatrix) <- list(unique(nameID), unique(voteID))

  if(!isTRUE(ideal)){
    return(rollCallMatrix)
  }


  if(isTRUE(ideal)){
    names <- unlist(dimnames(rollCallMatrix)[1])

    rollcalls <- pscl::rollcall(data = rollCallMatrix,
                                legis.names = names
                                )
    return(rollcalls)
  }
}
