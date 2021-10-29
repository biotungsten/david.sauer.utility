#' TBD
#' @export
my.as.numeric.factor <- function(x) {
  return(as.numeric(as.character(x[,])))
}

#' TBD
#' @export
as.idx.colname <- function(data, name) {
  return(grep(paste("^",name,"$", sep=""), colnames(data)))
}

#' TBD
#' @export
as.idx.rowname <- function(data, name) {
  return(grep(paste("^",name,"$", sep=""), rownames(data)))
}
