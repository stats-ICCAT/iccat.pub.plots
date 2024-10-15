
#' TBD
#'
#' @param t1nc_data TBD
#' @param category_column TBD
#' @param ref_categories TBD
#' @param max_categories TBD
#''@param other_category_label TBD
#''@param legend_title TBD
#' @param x_breaks_every TBD
#' @param colors TBD
#' @return TBD
t1nc.plot.line = function(t1nc_data,
                          relative = FALSE,
                          category_column,
                          ref_categories = NULL,
                          max_categories = NA,
                          other_category_label = NA,
                          legend_title,
                          x_breaks_every = 10,
                          colors) {
  T1NC = copy(t1nc_data)
  colnames(T1NC)[which(colnames(T1NC) == category_column)] = "CATEGORY_CODE"

  if(is.null(ref_categories)) {
    ref_categories = sort(unique(T1NC$CATEGORY_CODE))
  }

  if(!is.na(max_categories)) {
    if(is.na(other_category_label))
      other_category_label = "OT"

    T1NC_c = T1NC[, .(Qty_t = sum(Qty_t, na.rm = TRUE)), keyby = .(CATEGORY_CODE)][order(-Qty_t)]

    if(nrow(T1NC_c) > max_categories - 1) {
      top_categories = head(T1NC_c$CATEGORY_CODE, max_categories - 1)

      T1NC[!CATEGORY_CODE %in% top_categories, CATEGORY_CODE := other_category_label]

      ref_categories = ref_categories[which(ref_categories %in% top_categories)]
      ref_categories = append(ref_categories, other_category_label)
    }
  }

  T1NC$CATEGORY_CODE =
    factor(
      T1NC$CATEGORY_CODE,
      labels = ref_categories,
      levels = ref_categories,
      ordered = TRUE
    )

  category_colors = colors[CODE %in% unique(T1NC$CATEGORY_CODE)]

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
    scale_color_manual(values = category_colors$FILL) # The FILL color is lighter... Logically here we should have used COLOR instead

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

    theme(legend.position = "right") +

    guides(
      fill =
        guide_legend(
          title = legend_title
        )
    )

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
t1nc.plot.line_gear_groups = function(t1nc_data, max_categories = NA) {
  return(
    t1nc.plot.line(
      t1nc_data,
      category_column = "GearGrp",
      ref_categories = iccat.pub.data::REF_GEAR_GROUPS$CODE,
      max_categories = max_categories,
      other_category_label = "OT",
      legend_title = "Gear group",
      colors = iccat.pub.aes::REF_GEAR_GROUPS_COLORS[, .(CODE = GEAR_GROUP_CODE, FILL, COLOR)]
    )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @param max_categories TBD
#' @param relative TBD
#' @return TBD
#' @export
t1nc.plot.bar_species_gear_groups = function(t1nc_data, max_categories = NA, relative = FALSE) {
  return(
    t1nc.plot.line(
      t1nc_data,
      category_column = "SpcGearGrp",
      ref_categories = iccat.pub.data::REF_SPECIES_GEAR_GROUPS[SPECIES_GEAR_GROUP %in% t1nc_data$SpcGearGrp, .(SPECIES_GEAR_GROUP_ORDER = max(SPECIES_GEAR_GROUP_ORDER)), keyby = .(SPECIES_GEAR_GROUP)][order(SPECIES_GEAR_GROUP_ORDER, SPECIES_GEAR_GROUP)]$SPECIES_GEAR_GROUP,
      max_categories = max_categories,
      other_category_label = "Others",
      legend_title = "Species gear group",
      colors = iccat.pub.aes::REF_SPECIES_GEAR_GROUPS_COLORS[, .(CODE = SPECIES_GEAR_GROUP, FILL, COLOR)]
    )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @return TBD
#' @export
t1nc.plot.line_catch_types = function(t1nc_data) {
  return(
    t1nc.plot.line(
      t1nc_data,
      category_column = "CatchTypeCode",
      ref_categories = iccat.pub.data::REF_CATCH_TYPES$CODE,
      legend_title = "Catch type",
      colors = iccat.pub.aes::REF_CATCH_TYPES_COLORS[, .(CODE = CATCH_TYPE_CODE, FILL, COLOR)]
    )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @return TBD
#' @export
t1nc.plot.line_stocks = function(t1nc_data) {
  stock_codes = sort(unique(t1nc_data$Stock))

  stock_colors =
    data.table(
      CODE = stock_codes,
      FILL = hue_pal()(length(stock_codes))
    )

  stock_colors[, COLOR := darken(FILL, amount = .3)]

  return(
    t1nc.plot.line(
      t1nc_data,
      category_column = "Stock",
      ref_categories  = stock_codes,
      legend_title    = "Stock",
      colors          = stock_colors
    )
  )
}

#' TBD
#'
#' @param t1nc_data TBD
#' @return TBD
#' @export
t1nc.plot.line_sampling_areas = function(t1nc_data) {
  sampling_area_codes = sort(unique(t1nc_data$SampAreaCode))
  sampling_area_codes = sampling_area_codes[which(sampling_area_codes != "unkn")]

  sampling_area_colors =
    data.table(
      CODE = sampling_area_codes,
      FILL = hue_pal()(length(sampling_area_codes))
    )

  sampling_area_colors =
    rbind(sampling_area_colors,
          data.table(CODE = c("unkn",    "Other areas"),
                     FILL = c("#666666", "#000000")
          )
    )

  sampling_area_colors[, COLOR := darken(FILL, amount = .3)]

  sampling_area_codes = append(sampling_area_codes, c("unkn", "Other areas"))

  return(
    t1nc.plot.line(
      t1nc_data,
      category_column = "SampAreaCode",
      ref_categories  = sampling_area_codes,
      max_categories  = max_categories,
      other_category_label = "Other areas",
      legend_title    = "Sampling area",
      colors          = sampling_area_colors
    )
  )
}

