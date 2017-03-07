
# Emulate '+' python function
`%p%` <- function(e1,e2) return(paste0(e1,e2))


# response status check
status <- function(x){
  if(x$status_code != 200){
    stop("GET request failed")
  } else{
    xx <- httr::content(x, "parsed")
    return(xx)
  }
}
