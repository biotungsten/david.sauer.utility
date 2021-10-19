#' TBD
filter.data.by.substances <- function(data, substance.names, substance.colname = "substance", concentration.colname = "concentration", include.control = TRUE, concentrations.factors = TRUE) {
  substance.colindex = grep(substance.colname, colnames(data))
  concentration.colindex = grep(concentration.colname, colnames(data))
  if (include.control) {
    substance.names <- c("control", substance.names)
  }
  filtered.data <- data[data[,substance.colindex] %in% substance.names, ]
  if (concentrations.factors) {
    filtered.data[concentration.colindex] <- as.numeric.factor(filtered.data[concentration.colindex])
  }
  return(filtered.data)
}
