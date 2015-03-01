#' @export
setPreference <- function(p, catA, catB, preference) {
  p[p$a==catA   & p$b==catB   ,"pref"] <- preference
  invisible (p)
}

#' @export
printForInput <- function(preferenceTable) {
  variableName <- as.list(match.call()[-1])[1]
  nca <- max(nchar(as.character(pp$a)))
  ncb <- max(nchar(as.character(pp$b)))
  do.call(function(a, b, pref) printLineForInput(as.character(a), as.character(b), pref, variableName, nca, ncb), preferenceTable)
}


padRight <- function(x, l) {
  arg <- paste0("%-", l, "s")
  sprintf(arg, x)
}

stringRep <- function(x, l) {
  
  sapply(l, function(y) paste0(rep(x, y), collapse = ""))
}

printLineForInput <- function(a, b, pref, variableName, nca, ncb) {
  cat(paste0(variableName, 
             " <- setPreference(", 
             variableName, 
             ", \"", 
             a, 
             "\"", 
             stringRep(" ", nca - nchar(a)) ,
             " , \"", 
             b,
             "\"", 
             stringRep(" ", ncb - nchar(b)) ,
             " , ",
             pref, 
             ")\n"),
      sep = ""
      )
}