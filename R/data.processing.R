#' TBD
#' @export
filter.data.by.substances <- function(data, substance.names, substance.colname = "substance", concentration.colname = "concentration", include.control = TRUE, concentrations.factors = TRUE) {
  substance.colindex = grep(substance.colname, colnames(data))
  concentration.colindex = grep(concentration.colname, colnames(data))
  if (include.control) {
    substance.names <- c("control", substance.names)
  }
  filtered.data <- data[data[,substance.colindex] %in% substance.names, ]
  if (concentrations.factors) {
    filtered.data[concentration.colindex] <- my.as.numeric.factor(filtered.data[concentration.colindex][,])
  }
  return(filtered.data)
}

#' Cleans a dataframe of normalized data from values bigger than 1.
#' @param mode What to do with data bigger 1 (1 - is capped at 1, 2- is set to NA, default - do nothing)
#' @note It is persumed, that control data is marked by teh entry 'control' in a column named 'substance'
#' @export
cap.normalized.data <- function(data, mode, normal.colname = "response"){
  data <- apply(data, 1, function(x) {
    if (as.numeric(x[as.idx.colname(data, normal.colname)]) > 1 &
        !is.na(x[as.idx.colname(data, normal.colname)]) &
        x[as.idx.colname(data, "substance")] != "control") {
      if(mode == 1) {
        x[as.idx.colname(data, normal.colname)] <- 1
      }
      if (mode == 2) {
        x[as.idx.colname(data, normal.colname)] <- NA
      }
    }
    return(x)
  })
  return(data.frame(t(data))) #we have to transpose the return due to the workings of lapply
}
