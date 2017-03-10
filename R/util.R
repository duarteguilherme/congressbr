# safe map_chr:
# for filling dataframes when we don't know if the information exists in the API
safe_map_chr <- purrr::safely(purrr::map_chr, otherwise = NA_character_)
safe_map <- function(x, y){
  z <- safe_map_chr(x, y)$result
  return(z)
}


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

# depth of list check
depth <- function(x) ifelse(is.list(x), 1L + max(sapply(x, depth)), 0L)
#(http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r/13433689)
