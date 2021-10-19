#' Converts a label of the format X...-Y where X is the substance abbreviation and Y is the concentration. CTRL is treated as abbreviation for control.
appl2.label.as.vector <- function(label){
  if(str_starts(label, "CTRL")){
    return(c("control", 0))
  }
  substance.abbreviation <- str_sub(label, 1, 1)
  substance.name <- switch(substance.abbreviation,
                           C = "catechol",
                           R = "resorcinol",
                           H = "hydrochinon")
  concentration <- as.numeric(str_split(label, "-")[[1]][2])
  return(c(substance.name, concentration))
}

#' TBD
appl2.plot.dose.response <- function(aggregated.data, model.data, predicted.data, substance.name, x.upper.lim = 100) {
  plot.dose.response(aggregated.data = aggregated.data,
                     model.data = model.data,
                     predicted.data = predicted.data,
                     title = paste(substance.name, "+ 0.1% (v/v) Tween20 nach 7 Tagen"),
                     x.upper.lim = x.upper.lim,
                     xlab = "Konzentration (mM)",
                     ylab = "normalisiertes relatives Wachstum",
                     filename = substance.name)
}
