
#' TBD
#'
#' @param t1nc_data TBD
#' @param category_column TBD
#' @param x_breaks_every TBD
#' @param colors TBD
#' @return TBD
t1nc.plot.line = function(t1nc_data, category_column = "GearGrp", x_breaks_every = 10, colors) {
  T1NC = copy(t1nc_data)
  colnames(T1NC)[which(colnames(T1NC) == category_column)] = "CATEGORY_CODE"

  T1NC = T1NC[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(YEAR = YearC, CATEGORY_CODE)]

  xMin = min(T1NC$YEAR)
  xMax = max(T1NC$YEAR)

  dMin = floor(xMin / x_breaks_every) * x_breaks_every
  dMax = floor(xMax / x_breaks_every) * x_breaks_every

  x_breaks = c(xMin, seq(dMin, dMax, x_breaks_every))

  if(x_breaks_every == 1 | ( xMax - dMax ) > 1)
    x_breaks = append(x_breaks, xMax)

  x_breaks = unique(x_breaks)

  T1_y = T1NC[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(YEAR)]

  T1 =
    initialize_plot(
      T1NC,
      aes(
        x = YEAR,
        y = CATCH,
        color = CATEGORY_CODE,
        group = CATEGORY_CODE
      )
    )

  T1 = T1 +
    geom_line(
      linewidth = .7
    ) +
    geom_point(
      aes(
        x = YEAR,
        y = CATCH,
        color = CATEGORY_CODE
      )
    )

  T1 = T1 +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       guide = guide_axis(n.dodge = 2))

  T1 = T1 +
    scale_color_manual(values = colors$FILL) # The FILL color is lighter... Logically here we should have used COLOR instead

  y_breaks = breaks_for(min(T1_y$CATCH), max(T1_y$CATCH))
  y_lims   = limit_for (min(T1_y$CATCH), max(T1_y$CATCH))

  T1 = T1 +
    theme_bw() +
    labs(x = "Year",
         y = "Catches (t)") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +
    coord_cartesian(ylim = y_lims) +
    theme(legend.position = "right")

  return(
    T1
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @param max_categories TBD
#' @return TBD
#' @export
t1nc.plot.line_gears = function(t1nc_data, max_categories = NA) {
  T1NC = copy(t1nc_data)

  ref_gear_groups = iccat.pub.data::REF_GEAR_GROUPS$CODE

  if(!is.na(max_categories)) {
    T1_C = T1NC[, .(Qty_t = sum(Qty_t, na.rm = TRUE)), keyby = .(GearGrp)][order(-Qty_t)]

    if(nrow(T1_C) > max_categories - 1) {
      top_categories = head(T1_C$GearGrp, max_categories - 1)

      T1NC[!GearGrp %in% top_categories, GearGrp := "OT"]

      ref_gear_groups = ref_gear_groups[which(ref_gear_groups %in% top_categories)]
      ref_gear_groups = append(ref_gear_groups, "OT")
    }
  }

  T1NC$GearGrp =
    factor(
      T1NC$GearGrp,
      labels = ref_gear_groups,
      levels = ref_gear_groups,
      ordered = TRUE
    )

  gear_group_colors = iccat.pub.aes::REF_GEAR_GROUPS_COLORS[GEAR_GROUP_CODE %in% unique(T1NC$GearGrp)]

  return(
    t1nc.plot.line(
      T1NC, "GearGrp", colors = gear_group_colors
    ) +
      guides(
        color =
          guide_legend(
            title = "Gear group"
          )
      )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @return TBD
#' @export
t1nc.plot.line_catch_types = function(t1nc_data) {
  T1NC = copy(t1nc_data)

  colors =
    data.table(
      CATCH_TYPE_CODE = iccat.pub.data::REF_CATCH_TYPES$CODE
    )

  T1NC$CatchTypeCode =
    factor(
      T1NC$CatchTypeCode,
      labels = iccat.pub.data::REF_CATCH_TYPES$CODE,
      levels = iccat.pub.data::REF_CATCH_TYPES$CODE,
      ordered = TRUE
    )

  catch_type_colors = iccat.pub.aes::REF_CATCH_TYPES_COLORS[CATCH_TYPE_CODE %in% unique(T1NC$CatchTypeCode)]

  return(
    t1nc.plot.line(
      T1NC, "CatchTypeCode", colors = catch_type_colors
    ) +
      guides(
        color =
          guide_legend(
            title = "Catch type"
          )
      )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @return TBD
#' @export
t1nc.plot.line_stocks = function(t1nc_data) {
  T1NC = t1nc_data

  stock_colors =
    data.table(
      STOCK_AREA_CODE = unique(t1nc_data[order(Stock)]$Stock)
    )

  T1NC$Stock =
    factor(
      T1NC$Stock,
      labels = stock_colors$STOCK_AREA_CODE,
      levels = stock_colors$STOCK_AREA_CODE,
      ordered = TRUE
    )

  stock_colors$FILL = hue_pal()(nrow(stock_colors))
  stock_colors[, COLOR := darken(FILL, amount = .3)]

  return(
    t1nc.plot.line(
      T1NC, "Stock", colors = stock_colors
    ) +
      guides(
        color =
          guide_legend(
            title = "Stock"
          )
      )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @return TBD
#' @export
t1nc.plot.line_sampling_areas = function(t1nc_data) {
  T1NC = t1nc_data

  sampling_area_colors =
    data.table(
      SAMPLING_AREA_CODE = unique(t1nc_data[order(SampAreaCode)]$SampAreaCode)
    )

  T1NC$SampAreaCode =
    factor(
      T1NC$SampAreaCode,
      labels = sampling_area_colors$SAMPLING_AREA_CODE,
      levels = sampling_area_colors$SAMPLING_AREA_CODE,
      ordered = TRUE
    )

  sampling_area_colors = sampling_area_colors[SAMPLING_AREA_CODE != "unkn"]

  sampling_area_colors$FILL = hue_pal()(nrow(sampling_area_colors))
  sampling_area_colors =
    rbind(
      sampling_area_colors,
      data.table(SAMPLING_AREA_CODE = "unkn",
                 FILL = "#666666")
    )

  sampling_area_colors[, COLOR := darken(FILL, amount = .3)]

  return(
    t1nc.plot.line(
      T1NC, "SampAreaCode", colors = sampling_area_colors
    ) +
      guides(
        color =
          guide_legend(
            title = "Sampling area"
          )
      )
  )
}

