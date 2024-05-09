options(scipen = 9999)

initialize_plot = function(data, aesthetics = NA, custom_theme = theme_bw) {
  p = NA

  if(anyNA(aesthetics)) {
    p = ggplot(data)
  } else {
    p = ggplot(data, aesthetics)
  }

  p =
    p +

    custom_theme() +

    theme(legend.title = element_blank(),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 9),
          legend.position = "top",
          legend.key.width  = unit(.4, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text.x = element_text(size = 9)) +

    theme(plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "points")) +

    theme(#panel.grid.minor.x = element_line(size = 0.3, color = "lightgrey")
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(size = 0.3, color = "lightgrey"),
      panel.grid.minor.y = element_line(size = 0.3, color = "lightgrey", linetype = "dashed"),
      panel.grid.major.y = element_line(size = 0.3, color = "lightgrey"))

  return (p)
}

calculate_delta = function(min, max) {
  #Magnitude of the max catch by label value
  magnitude = round(log10(max))

  #Delta corresponds to the previous order magnitude

  #delta = max(10, 10^(magnitude - 1))
  delta = 10^(magnitude - 1)

  return(delta)
}

calculate_limit = function(min, max) {
  delta = calculate_delta(min, max)

  #Calculates the upper range of the Y axis, rounded to the closest multiple of the magnitude
  return (ceiling(max / delta) * delta)
}

breaks_for = function(min, max) {
  maxx  = calculate_limit(min, max)
  delta = calculate_delta(min, max)

  #The step is twice as wide as the delta
  step =  2 * delta

  #return (c(seq(0, maximum_value, ifelse(magnitude == 3, 200, 50)), round(maximum_value)))
  return (c(seq(0, maxx, step), maxx))
}

limit_for = function(min, max) {
  breaks = breaks_for(min, max)

  return(c(min(breaks), max(breaks)))
  #return (c(0, calculate_limit(min, max) * 1.1))
}
