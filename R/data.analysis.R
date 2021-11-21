#' Fits a hill model using \code{nls}. The formula for the hill model is \eqn{\frac{1}{1+\frac{c}{EC_{50}^n}}.
#' @param data A data frame providing the data, to which the model will be fitted. A column with concentrations (named concentrations) is required. It should contain in the same unit the concentrations for each measurement.
#' @return A model object (nlsModel) fitted to the data provided.
#' @export
fit.hill.model <- function(data, response.variable.name = "response", guesses = list(ec50=1, n=1), lower = list(ec50 = 0, n = 0), print.summary = TRUE, algorithm = "default", trace = FALSE, control = nls.control()) {
  fit.generic.model("1/(1+(concentration/ec50)^n)", data, response.variable.name, guesses, lower, print.summary, algorithm, trace, control)
}

#' TBD
#' @export
fit.exp.model <- function(data, response.variable.name = "response", guesses = list(a=1), lower=list(a=0), print.summary = TRUE, algorithm = "default", trace = FALSE, control = nls.control()) {
  fit.generic.model("exp(-a*concentration)", data, response.variable.name, guesses, print.summary, algorithm, trace, control)
}

#' TBD
#' @export
fit.generic.model <- function(equation, provided.data, response.variable.name, guesses, lower, print.summary, algorithm, trace, control) {
  hill.formula <- as.formula(paste(response.variable.name, "~", equation))
  model <- nls(hill.formula,
               data = provided.data,
               start = guesses,
               lower = lower,
               algorithm = algorithm,
               trace = trace,
               control = control)
  if (print.summary) {
    print(summary(model))
  }
  return(model)
}

#' TBD. If use.normalize.value is FALSE, certain rows out of data are selected and the mean of the values in their colname.normalize column is used as normalize.value.
#' @param data
#' @param colname.normalize The name of the column in which the data, data should be normalized are located. Further if use.normalize.value is false, this column will be used to calculate normalize.value.
#' @param colname.selection The name of the column in which the object.selection object should be matched.
#' @param object.selection The object which must be matched in the column specified by colname.selection for rows to be included in the normalize.value.
#' @param normalize.value The value which to use for normalizing. Is ignored if use.normalize.value is FALSE.
#' @param use.normalize.value A boolean to indicate whether or not the normalize.value parameter should be used.
#' @export
normalize.data <- function(data, colname.normalize, colname.selection, object.selection, normalize.value = 1, use.normalize.value = FALSE) {
  if (!use.normalize.value) {
    normalize.data.points <- data[data[as.idx.colname(data, colname.selection)] == object.selection, ]
    normalize.value <- as.numeric(apply(normalize.data.points[as.idx.colname(data, colname.normalize)], 2, mean))
  }
  data[as.idx.colname(data, colname.normalize)] <- apply(data, 1, function(x) {
    return(as.numeric(x[as.idx.colname(data, colname.normalize)])/normalize.value)
  })
  return(data)
}
