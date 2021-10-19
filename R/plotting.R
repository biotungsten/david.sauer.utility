#' TBD
#' @export
dose.response.plot <- function(aggregated.data, model.data, title, x.upper.lim, xlab, ylab, filename) {
  remove.ctrl <- function(df) {
    return(df[df$substance != "control", ])
  }

  prediction.concentrations <- seq(0, x.upper.lim, 0.1)
  predicted.data <- data.frame(concentrations = prediction.concentration,
                               response = predict(model.data, data.frame(concentrations = prediction.concentrations)))

  ec50 <- coefficients(model.data)[1]
  n <- coefficients(model.data)[2]
  ec50.confint.upper <- round(confint(model.data)[1,][2], 2)
  ec50.confint.lower <- round(confint(model.data)[1,][1], 2)
  n.confint.upper <- round(confint(model.data)[2,][2], 2)
  n.confint.lower <- round(confint(model.data)[2,][1], 2)

  p <- ggplot(data = predicted.data,
              mapping = aes(x = concentration,
                            y = response)) +
    scale_x_log10(limits = c(NA,x.upper.lim),
                  expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = c(0,0)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.margin = unit(c(1,1,1,1),"cm")) +
    geom_line() +
    geom_errorbar(data = remove.ctrl(aggregated.data),
                  mapping = aes(ymin = ratio.mean-ratio.sd,
                                ymax = ratio.mean+ratio.sd),
                  color = "turquoise4",
                  alpha = 0.7,
                  size = .25) +
    geom_point(data = remove.ctrl(aggregated.data),
               mapping = aes(x = concentration,
                             y = ratio.mean),
               shape = "+",
               size = 5,
               stroke = .15,
               color = "turquoise4") +
    labs(x = xlab,
         y = ylab,
         title = title,
         subtitle = paste("EC50 (95%): ", ec50.confint.lower, " - ", ec50.confint.upper, ", n (95%): ", n.confint.lower, " - ", n.confint.upper, sep="")) +
    geom_segment(mapping = aes(x = ec50,
                               y = 0,
                               xend = ec50,
                               yend = 0.5),
                 linetype = "dotted",
                 size = .25) +
    coord_cartesian(clip = "off")
  ggsave(paste(filename, "plot", format(Sys.time(), "%d-%m-%Y.jpg"), sep="_"), dpi = "retina", device = "jpg")
}
