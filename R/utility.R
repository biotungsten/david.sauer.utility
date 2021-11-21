#' TBD
#' @export
my.as.numeric.factor <- function(x) {
  try(return(as.numeric(as.character(x[]))))
  try(return(as.numeric(as.character(x[,]))))
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
