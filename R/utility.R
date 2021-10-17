#' TBD
as.numeric.factor <- function(x) {
  return(as.numeric(as.character(x[,])))
}

#' TBD
as.idx.colname <- function(data, name) {
  return(grep(paste("^",name,"$", sep=""), colnames(data)))
}