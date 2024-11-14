#' Generic Pareto charts for T1 nominal catch data, providing two distinct categorizations: one for the X axis, and one for the
#' catch components. The final chart also includes a superimposed cumulative (relative) catch plot.
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param x_column the category column for the-X axis, chosen among the columns included in the T1 nominal catch dataset
#' @param x_name the category name for the X-axis
#' @param category_column the category column for the catch components, chosen among the columns included in the T1 nominal catch dataset
#' @param category_name the name of the category
#' @param category_levels the list of reference category values
#' @param category_colors a color table for the catch component categories, i.e., a table with three columns: \code{_categoryColumn_CODE}, \code{FILL}, \code{OUTLINE}, with the latter two being the RGB colors for the fill and outline components of each bar
#' @param max_x the maximum number of categories being displayed on the X axis. Everything else will go under the _All others_ label
#' @param max_categories the maximum number of catch categories. Everything else will go under the _All others_ label
#' @param vertical to display the chart vertically
#' @param rotate_x_labels to rotate the X-axis labels vertically
#' @return a Pareto chart of T1 nominal catches configured according to the provided parameters
#' @export
t1nc.plot.pareto = function(t1nc_data,
                            x_column, x_name,
                            category_column, category_name,
                            category_levels, category_colors,
                            max_x, max_categories,
                            vertical,
                            rotate_x_labels = FALSE) {
  RED      = "#AA0000"
  DARK_RED = "#770000"

  ALL_OTHERS = "All others"

  T1NC = copy(t1nc_data)

  colnames(T1NC)[which(colnames(T1NC) == x_column)]        = "X_CODE"
  colnames(T1NC)[which(colnames(T1NC) == category_column)] = "CATEGORY_CODE"

  T1NC_X = T1NC[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(X_CODE)][order(-CATCH)]

  if(is.na(max_x))
    top_x = T1NC_X$X_CODE
  else
    top_x = head(T1NC_X, n = max_x)$X_CODE

  T1NC_C = T1NC[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(CATEGORY_CODE)][order(-CATCH)]

  if(!is.na(max_categories)) {
    top_c = head(T1NC_C, n = max_categories)$CATEGORY_CODE
  } else {
    top_c = T1NC_C$CATEGORY_CODE
  }

  T1NC[!X_CODE %in% top_x,        X_CODE        := ALL_OTHERS]
  T1NC[!CATEGORY_CODE %in% top_c, CATEGORY_CODE := ALL_OTHERS]

  T1NC$CATEGORY_CODE =
    factor(
      T1NC$CATEGORY_CODE,
      levels = append(category_levels, ALL_OTHERS),
      labels = append(category_levels, ALL_OTHERS),
      ordered = TRUE
    )

  category_colors = copy(category_colors)
  colnames(category_colors)[1] = "CODE"

  category_colors =
    rbind(category_colors,
          data.table(CODE = ALL_OTHERS, FILL = "#444444", COLOR = darken("#444444", amount = 0.4)))

  T1NC =
    T1NC[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(X_CODE, CATEGORY_CODE)]

  LEVELS = append(top_x, ALL_OTHERS)

  if(vertical) LEVELS = rev(LEVELS)

  T1NC$X_CODE =
    factor(
      T1NC$X_CODE,
      levels = LEVELS,
      labels = LEVELS,
      ordered = TRUE
    )

  T1NC_X  = copy(T1NC)[, .(CATCH = sum(CATCH)), keyby = .(X_CODE)]
  T1NC_XR = T1NC_X[, CATCH_R := round(CATCH * 100 / sum(CATCH), 2)]

  if(vertical) {
    T1 =
      initialize_plot(
        T1NC,
        aes(
          x = CATCH,
          y = X_CODE,
          fill  = CATEGORY_CODE,
          color = CATEGORY_CODE,
          group = CATEGORY_CODE
        )
      )
  } else {
    T1 =
      initialize_plot(
        T1NC
      )
  }

  if(vertical) {
    T1 = T1 +
      geom_bar(
        aes(
          x = CATCH,
          y = X_CODE,
          fill  = CATEGORY_CODE,
          color = CATEGORY_CODE,
          group = CATEGORY_CODE
        ),
        stat      = "identity",
        position  = "stack",
        width     = 0.9,
        linewidth = 0.2
      )
  } else {
    T1 = T1 +
      geom_bar(
        aes(
          x = X_CODE,
          y = CATCH,
          fill  = CATEGORY_CODE,
          color = CATEGORY_CODE,
          group = CATEGORY_CODE
        ),
        stat      = "identity",
        position  = "stack",
        width     = 0.9,
        linewidth = 0.2
      )
  }

  MIN_C = min(T1NC_X$CATCH)
  MAX_C = max(T1NC_X$CATCH)

  if(!vertical) {
    T1NC_XR_CUMULATIVE = copy(T1NC_XR)[, CATCH_PERC_CUMULATIVE := cumsum(CATCH_R)]

    T1 = T1 +
      geom_line(
        data = T1NC_XR_CUMULATIVE[X_CODE != ALL_OTHERS],
        aes(
          x = X_CODE,
          y = CATCH_PERC_CUMULATIVE / 100 * MAX_C
        ),
        colour = RED,
        group  = 1
      ) +
      geom_point(
        data = T1NC_XR_CUMULATIVE[X_CODE != ALL_OTHERS],
        aes(
          x = X_CODE,
          y = CATCH_PERC_CUMULATIVE / 100 * MAX_C
        ),
        colour = RED
      ) +
      geom_text(
        data = T1NC_XR_CUMULATIVE[X_CODE != ALL_OTHERS],
        aes(
          x = X_CODE,
          y = CATCH_PERC_CUMULATIVE / 100 * MAX_C,
          label = paste(round(CATCH_PERC_CUMULATIVE), "%", sep = "")
        ),
        hjust = 0.40,
        vjust = -1,
        colour = DARK_RED
      )
  }

  T1 = T1 +
    scale_fill_manual (values = category_colors[CODE %in% unique(T1NC$CATEGORY_CODE)]$FILL) +
    scale_color_manual(values = category_colors[CODE %in% unique(T1NC$CATEGORY_CODE)]$COLOR, guide = guide_none())

  T1 = T1 +
    theme_bw() +
    labs(x = ifelse(vertical, "Catches (t)", x_name),
         y = ifelse(vertical, x_name,        "Catches (t)"))

  BREAKS = breaks_for(MIN_C, MAX_C)
  LIMS   = limit_for (MIN_C, MAX_C)

  EXPAND   = c(.01, .01, .01, .01)
  LABELLER = function(x) { format(x, big.mark = ",", scientific = FALSE) }

  if(vertical) {
    T1 = T1 +
      scale_x_continuous(expand = EXPAND,
                         breaks = BREAKS,
                         labels = LABELLER,
                         sec.axis = dup_axis(name = element_blank())) +
      coord_cartesian(xlim = LIMS)
  } else {
    T1 = T1 +
      scale_x_discrete(guide = guide_axis(n.dodge = ifelse(rotate_x_labels, 1, 2))) +
      scale_y_continuous(expand = EXPAND,
                         breaks = BREAKS,
                         labels = LABELLER,
                         sec.axis = sec_axis(breaks = seq(0, 100, 10),
                                             ~. * 100 / MAX_C, name = "% cumulative total value")
      ) +
      coord_cartesian(ylim = c(LIMS[1], LIMS[2] * 1.1)) +
      theme(
        axis.line.y.right  = element_line(color = RED),
        axis.ticks.y.right = element_line(color = RED),
        axis.text.y.right  = element_text(color = RED),
        axis.title.y.right = element_text(color = RED, angle = 270)
      )
  }

  if(rotate_x_labels)
    T1 = T1 +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))

  T1 = T1 +
    theme(legend.position = "right") +
    guides(fill = guide_legend(title = category_name),
           color = guide_none())

  return(
    T1
  )
}

#' Default Pareto charts of T1 nominal catch data, with fleets as X-axis categories, and gear groups as catch component categories.
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param max_x the maximum number of categories being displayed on the X axis. Everything else will go under the _All others_ label
#' @param max_categories the maximum number of catch categories. Everything else will go under the _All others_ label
#' @param vertical to display of the chart vertically
#' @return a Pareto chart of T1 nominal catches configured according to the provided parameters
#' @export
t1nc.plot.pareto_fleet_gears = function(t1nc_data,
                                        max_x = 10, max_categories = 10,
                                        vertical = FALSE) {
  return(
    t1nc.plot.pareto(
      t1nc_data,
      x_column = "FlagName", x_name = "Flag",
      category_column = "GearGrp", category_name = "Gear group",
      category_levels = REF_GEAR_GROUPS$CODE, category_colors = iccat.pub.aes::REF_GEAR_GROUPS_COLORS,
      max_x = max_x, max_categories = max_categories,
      vertical = vertical
    )
  )
}
