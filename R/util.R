
# Emulate '+' python function
`%p%` <- function(e1,e2) return(paste0(e1,e2))


# response status check
status <- function(x){
  if(x$status_code != 200){
    stop("GET request failed")
  } else{
    xx <- httr::content(x, "parsed")

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
