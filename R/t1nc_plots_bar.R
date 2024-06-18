#' TBD
#'
#' @param t1nc_data TBD
#' @param relative TBD
#' @param category_column TBD
#' @param x_breaks_every TBD
#' @param colors TBD
#' @return TBD
t1nc.plot.bar = function(t1nc_data, relative = FALSE, category_column = "GearGrp", x_breaks_every = 10, colors) {
  T1NC = copy(t1nc_data)
  colnames(T1NC)[which(colnames(T1NC) == category_column)] = "CATEGORY_CODE"

  T1NC = T1NC[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(YEAR = YearC, CATEGORY_CODE)]

  if(relative) {
    T1NC = T1NC[, CATCH_YEAR := sum(CATCH, na.rm = TRUE), by = .(YEAR)]
    T1NC[, CATCH := CATCH * 100 / CATCH_YEAR]
  }

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
        fill = CATEGORY_CODE,
        color = CATEGORY_CODE,
        group = CATEGORY_CODE
      )
    )

  T1 = T1 +
    geom_bar(
      stat      = "identity",
      position  = "stack",
      width     = 1.0,
      linewidth = 0.2
    )

  T1 = T1 +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       guide = guide_axis(n.dodge = 2))

  T1 = T1 +
    scale_fill_manual (values = colors$FILL) +
    scale_color_manual(values = colors$COLOR, guide = guide_none())

  y_breaks = breaks_for(min(T1_y$CATCH), max(T1_y$CATCH))
  y_lims   = limit_for (min(T1_y$CATCH), max(T1_y$CATCH))

  if(relative) {
    y_breaks = seq(0, 100, 10)
    y_lims   =   c(0, 100)
  }

  T1 = T1 +
    theme_bw() +
    labs(x = "Year",
         y = paste0("Catches (", ifelse(relative, "%", "t"), ")")) +
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
#' @param relative TBD
#' @return TBD
#' @export
t1nc.plot.bar_gears = function(t1nc_data, max_categories = NA, relative = FALSE) {
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
    t1nc.plot.bar(
      T1NC, relative, "GearGrp", colors = gear_group_colors
    ) +
    guides(
      fill =
        guide_legend(
          title = "Gear group"
        )
    )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @param relative TBD
#' @return TBD
#' @export
t1nc.plot.bar_catch_types = function(t1nc_data, relative = FALSE) {
  T1NC = copy(t1nc_data)

  colors =
    data.table(
      CATCH_TYPE_CODE = iccat.pub.data::REF_CATCH_TYPES$CODE
    )

  T1NC$CatchTypeCode =
    factor(
      T1NC$CatchTypeCode,
      labels = iccat.pub.data::REF_CATCH_TYPES$CODE,
      levels = iccat.pub.data::REF_CATCH_TYPES$CODE
    )

  catch_type_colors = iccat.pub.aes::REF_CATCH_TYPES_COLORS[CATCH_TYPE_CODE %in% unique(T1NC$CatchTypeCode)]

  return(
    t1nc.plot.bar(
      T1NC, relative, "CatchTypeCode", colors = catch_type_colors
    ) +
      guides(
        fill =
          guide_legend(
            title = "Catch type"
          )
      )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @param relative TBD
#' @return TBD
#' @export
t1nc.plot.bar_stocks = function(t1nc_data, relative = FALSE) {
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
    t1nc.plot.bar(
      T1NC, relative, "Stock", colors = stock_colors
    ) +
    guides(
      fill =
        guide_legend(
          title = "Stock"
        )
    )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @param relative TBD
#' @return TBD
#' @export
t1nc.plot.bar_sampling_areas = function(t1nc_data, relative = FALSE) {
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

  if(nrow(sampling_area_colors) > 0) sampling_area_colors$FILL = hue_pal()(nrow(sampling_area_colors))
  else sampling_area_colors = data.table(SAMPLING_AREA_CODE = character(), FILL = character())

  sampling_area_colors =
    rbind(
      sampling_area_colors,
      data.table(SAMPLING_AREA_CODE = "unkn",
                 FILL = "#666666")
    )

  sampling_area_colors[, COLOR := darken(FILL, amount = .3)]

  return(
    t1nc.plot.bar(
      T1NC, relative, "SampAreaCode", colors = sampling_area_colors
    ) +
      guides(
        fill =
          guide_legend(
            title = "Sampling area"
          )
      )
  )
}
