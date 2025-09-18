#' #' Generic plots of annual time series T1 nominal catch data as area charts (either absolute or relative) categorized
#' #' through one of the T1 nominal catch attributes.
#' #'
#' #' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' #' @param relative whether or not the areas should represent the relative composition of annual catches, or their absolute values
#' #' @param category_column the category column among those included in the T1 nominal catch dataset
#' #' @param ref_categories an optional list of reference category values, If not provided, it is calculated from the actual data
#' #' @param max_categories the maximum number of categories to display. Everything else will be placed under the _Other_ category
#' #''@param other_category_label the _Other_ category label
#' #''@param legend_title the legend title
#' #' @param x_breaks_every the number of years at which X axis breaks should be placed
#' #' @param colors a color table for the selected category, i.e., a table with three columns: \code{_categoryColumn_CODE}, \code{FILL}, \code{OUTLINE}, with the latter two being the RGB colors for the fill and outline components of each bar
#' #' @return an area chart of the provided data with the given configuration
#' t1nc.plot.area = function(t1nc_data,
#'                           relative = FALSE,
#'                           category_column,
#'                           ref_categories = NULL,
#'                           max_categories = NA,
#'                           other_category_label = NA,
#'                           legend_title,
#'                           x_breaks_every = 5,
#'                           colors) {
#'   T1NC = copy(t1nc_data)
#'   colnames(T1NC)[which(colnames(T1NC) == category_column)] = "CATEGORY_CODE"
#'
#'   if(is.null(ref_categories)) {
#'     ref_categories = sort(unique(T1NC$CATEGORY_CODE))
#'   }
#'
#'   if(!is.na(max_categories)) {
#'     if(is.na(other_category_label))
#'       other_category_label = "OT"
#'
#'     T1NC_c = T1NC[, .(Qty_t = sum(Qty_t, na.rm = TRUE)), keyby = .(CATEGORY_CODE)][order(-Qty_t)]
#'
#'     if(nrow(T1NC_c) > max_categories - 1) {
#'       top_categories = head(T1NC_c$CATEGORY_CODE, max_categories - 1)
#'
#'       T1NC[!CATEGORY_CODE %in% top_categories, CATEGORY_CODE := other_category_label]
#'
#'       ref_categories = ref_categories[which(ref_categories %in% top_categories)]
#'       ref_categories = append(ref_categories, other_category_label)
#'     }
#'   }
#'
#'   T1NC$CATEGORY_CODE =
#'     factor(
#'       T1NC$CATEGORY_CODE,
#'       labels = ref_categories,
#'       levels = ref_categories,
#'       ordered = TRUE
#'     )
#'
#'   category_colors = colors[CODE %in% unique(T1NC$CATEGORY_CODE)]
#'
#'   T1NC = T1NC[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(YEAR = YearC, CATEGORY_CODE)]
#'
#'   ### This first pre-processing (categories only appearing once in the time series) might be redundant
#'   ### now that we added the two additional post-processing for categories appearing / disappearing late
#'   ### in the time series...
#'
#'   # Necessary to handle all those categories that only have one value, and that will be
#'   # otherwise not displayed by geom_area
#'   T1NC_C = T1NC[, .(NUM = .N), keyby = .(CATEGORY_CODE)]
#'
#'   # Basically, adds a new data point for all these categories, with YEAR = < previous year, compared to the only observation >
#'   # and CATCH = 0
#'   for(category in T1NC_C[NUM == 1]$CATEGORY_CODE) {
#'     year = min(T1NC[CATEGORY_CODE == category]$YEAR)
#'
#'     T1NC = rbind(T1NC,
#'                  data.table(YEAR = year - 1,
#'                             CATEGORY_CODE = category,
#'                             CATCH = 0)
#'            )
#'   }
#'
#'   # Necessary to handle all those categories that appear / disappear halfway in the time series
#'   # otherwise not displayed by geom_area
#'   T1NC_Y = T1NC[, .(YEAR_m = min(YEAR), YEAR_M = max(YEAR)), keyby = .(CATEGORY_CODE)]
#'
#'   # Adds a new data point for all these categories appearing late in the time series, with
#'   # YEAR = < previous year, compared to the first observation > and CATCH = 0
#'   for(category in T1NC_Y[YEAR_m > min(T1NC$YEAR)]$CATEGORY_CODE) {
#'     year = T1NC_Y[CATEGORY_CODE == category]$YEAR_m
#'
#'     T1NC = rbind(T1NC,
#'                  data.table(YEAR = year - 1,
#'                             CATEGORY_CODE = category,
#'                             CATCH = 0)
#'     )
#'   }
#'
#'   # Adds a new data point for all these categories disappearing late in the time series, with
#'   # YEAR = < next year, compared to the last observation > and CATCH = 0
#'   for(category in T1NC_Y[YEAR_M < max(T1NC$YEAR)]$CATEGORY_CODE) {
#'     year = T1NC_Y[CATEGORY_CODE == category]$YEAR_M
#'
#'     T1NC = rbind(T1NC,
#'                  data.table(YEAR = year + 1,
#'                             CATEGORY_CODE = category,
#'                             CATCH = 0)
#'     )
#'   }
#'
#'   if(relative) {
#'     T1NC = T1NC[, CATCH_YEAR := sum(CATCH, na.rm = TRUE), by = .(YEAR)]
#'     T1NC[, CATCH := CATCH * 100 / CATCH_YEAR]
#'   }
#'
#'   xMin = min(T1NC$YEAR)
#'   xMax = max(T1NC$YEAR)
#'
#'   dMin = floor(xMin / x_breaks_every) * x_breaks_every
#'   dMax = floor(xMax / x_breaks_every) * x_breaks_every
#'
#'   x_breaks = c(xMin, seq(dMin, dMax, x_breaks_every))
#'
#'   if(x_breaks_every == 1 | ( xMax - dMax ) > 1)
#'     x_breaks = append(x_breaks, xMax)
#'
#'   x_breaks = unique(x_breaks)
#'
#'   T1_y = T1NC[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(YEAR)]
#'
#'   T1 =
#'     initialize_plot(
#'       T1NC
#'     )
#'
#'   T1 = T1 +
#'     geom_area(
#'       mapping = aes(
#'         x = YEAR,
#'         y = CATCH,
#'         fill  = CATEGORY_CODE,
#'         color = CATEGORY_CODE,
#'         group = CATEGORY_CODE
#'       ),
#'       #stat = "identity",
#'       stat      = "align",
#'       position  = "stack",
#'       linewidth = 0.2,
#'       alpha     = 0.7
#'     )
#'
#'   T1 = T1 +
#'     scale_x_continuous(expand = c(0, 0),
#'                        breaks = x_breaks,
#'                        guide  = guide_axis(n.dodge = 1))
#'
#'   T1 = T1 +
#'     scale_fill_manual (values = category_colors$FILL) +
#'     scale_color_manual(values = category_colors$COLOR, guide = guide_none())
#'
#'   y_breaks = breaks_for(min(T1_y$CATCH), max(T1_y$CATCH))
#'   y_lims   = limit_for (min(T1_y$CATCH), max(T1_y$CATCH))
#'
#'   if(relative) {
#'     y_breaks = seq(0, 100, 10)
#'     y_lims   =   c(0, 100)
#'   }
#'
#'   T1 = T1 +
#'     theme_bw() +
#'
#'     labs(x = "Year",
#'          y = paste0("Catches (", ifelse(relative, "%", "t"), ")")) +
#'
#'     scale_y_continuous(expand = c(0, 0),
#'                        breaks = y_breaks,
#'                        labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
#'                        sec.axis = dup_axis(name = element_blank())) +
#'     coord_cartesian(ylim = y_lims) +
#'
#'     theme(legend.position = "right") +
#'
#'     guides(
#'       fill =
#'         guide_legend(
#'           title = legend_title
#'         )
#'     )
#'
#'   return(
#'     T1
#'   )
#' }

#' Generic plots of annual time series T1 nominal catch data as area charts (either absolute or relative) categorized
#' through one of the T1 nominal catch attributes.
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param relative whether or not the areas should represent the relative composition of annual catches, or their absolute values
#' @param category_column the category column among those included in the T1 nominal catch dataset
#' @param ref_categories an optional list of reference category values, If not provided, it is calculated from the actual data
#' @param max_categories the maximum number of categories to display. Everything else will be placed under the _Other_ category
#' @param other_category_label the _Other_ category label
#' @param legend_title the legend title
#' @param x_breaks_every the number of years at which X axis breaks should be placed
#' @param colors a color table for the selected category, i.e., a table with three columns: CODE, FILL, COLOR
#' @return an area chart of the provided data with the given configuration
t1nc.plot.area = function(t1nc_data,
                          relative = FALSE,
                          category_column,
                          ref_categories = NULL,
                          max_categories = NA,
                          other_category_label = NA,
                          legend_title,
                          x_breaks_every = 5,
                          colors) {
  T1NC = data.table::copy(t1nc_data)
  colnames(T1NC)[which(colnames(T1NC) == category_column)] = "CATEGORY_CODE"

  if (is.null(ref_categories)) {
    ref_categories = sort(unique(T1NC$CATEGORY_CODE))
  }

  if (!is.na(max_categories)) {
    if (is.na(other_category_label))
      other_category_label = "OT"

    T1NC_c = T1NC[, .(Qty_t = sum(Qty_t, na.rm = TRUE)), keyby = .(CATEGORY_CODE)][order(-Qty_t)]

    if (nrow(T1NC_c) > max_categories - 1) {
      top_categories = head(T1NC_c$CATEGORY_CODE, max_categories - 1)

      T1NC[!CATEGORY_CODE %in% top_categories, CATEGORY_CODE := other_category_label]

      ref_categories = ref_categories[ref_categories %in% top_categories]
      ref_categories = append(ref_categories, other_category_label)
    }
  }

  # Facteur ordonné = ordre d'empilement et de légende
  T1NC$CATEGORY_CODE =
    factor(
      T1NC$CATEGORY_CODE,
      labels = ref_categories,
      levels = ref_categories,
      ordered = TRUE
    )

  # Aligner la table de couleurs sur l'ordre des niveaux
  level_order <- levels(T1NC$CATEGORY_CODE)
  category_colors <- colors[CODE %in% level_order]
  category_colors <- category_colors[match(level_order, category_colors$CODE)]
  fill_values  <- stats::setNames(category_colors$FILL,  category_colors$CODE)
  color_values <- stats::setNames(category_colors$COLOR, category_colors$CODE)

  # Agréger à YEAR × CATEGORY
  T1NC = T1NC[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(YEAR = YearC, CATEGORY_CODE)]

  # --- Catégories avec une seule observation: ajouter (year-1)=0
  T1NC_C = T1NC[, .(NUM = .N), keyby = .(CATEGORY_CODE)]
  for (category in T1NC_C[NUM == 1]$CATEGORY_CODE) {
    year = min(T1NC[CATEGORY_CODE == category]$YEAR)
    T1NC = data.table::rbindlist(list(
      T1NC,
      data.table::data.table(YEAR = year - 1L, CATEGORY_CODE = category, CATCH = 0)
    ), use.names = TRUE, fill = TRUE)
  }

  # --- Apparition/disparition tardive
  T1NC_Y = T1NC[, .(YEAR_m = min(YEAR), YEAR_M = max(YEAR)), keyby = .(CATEGORY_CODE)]

  # Apparition tardive -> (first-1)=0
  for (category in T1NC_Y[YEAR_m > min(T1NC$YEAR)]$CATEGORY_CODE) {
    year = T1NC_Y[CATEGORY_CODE == category]$YEAR_m
    T1NC = data.table::rbindlist(list(
      T1NC,
      data.table::data.table(YEAR = year - 1L, CATEGORY_CODE = category, CATCH = 0)
    ), use.names = TRUE, fill = TRUE)
  }

  # Disparition tardive -> (last+1)=0
  for (category in T1NC_Y[YEAR_M < max(T1NC$YEAR)]$CATEGORY_CODE) {
    year = T1NC_Y[CATEGORY_CODE == category]$YEAR_M
    T1NC = data.table::rbindlist(list(
      T1NC,
      data.table::data.table(YEAR = year + 1L, CATEGORY_CODE = category, CATCH = 0)
    ), use.names = TRUE, fill = TRUE)
  }

  # --- FIX réapparitions : ajouter des points 0 à chaque borne de segment (first-1 / last+1)
  data.table::setorder(T1NC, CATEGORY_CODE, YEAR)
  min_year_overall <- min(T1NC$YEAR, na.rm = TRUE)
  max_year_overall <- max(T1NC$YEAR, na.rm = TRUE)

  # Détecter les runs consécutifs par catégorie
  T1NC[, gap := YEAR - data.table::shift(YEAR), by = CATEGORY_CODE]
  T1NC[, seg := cumsum(is.na(gap) | gap > 1L), by = CATEGORY_CODE]

  bounds <- T1NC[, .(first = min(YEAR), last = max(YEAR)), by = .(CATEGORY_CODE, seg)]

  to_add_before <- bounds[first > min_year_overall,
                          .(CATEGORY_CODE, YEAR = as.integer(first - 1L), CATCH = 0)]
  to_add_after  <- bounds[last  < max_year_overall,
                          .(CATEGORY_CODE, YEAR = as.integer(last + 1L),  CATCH = 0)]

  to_add <- unique(data.table::rbindlist(list(to_add_before, to_add_after),
                                         use.names = TRUE, fill = TRUE))

  if (nrow(to_add)) {
    data.table::setkey(T1NC, CATEGORY_CODE, YEAR)
    data.table::setkey(to_add, CATEGORY_CODE, YEAR)
    to_add <- to_add[!T1NC]                     # anti-join pour éviter les doublons
    if (nrow(to_add)) {
      T1NC <- data.table::rbindlist(list(T1NC, to_add), use.names = TRUE, fill = TRUE)
    }
  }

  # On n'utilise PAS 'seg' dans le group esthétique (empilement stable)
  data.table::setorder(T1NC, YEAR, CATEGORY_CODE)

  if (relative) {
    T1NC = T1NC[, CATCH_YEAR := sum(CATCH, na.rm = TRUE), by = .(YEAR)]
    T1NC[, CATCH := CATCH * 100 / CATCH_YEAR]
  }

  xMin = min(T1NC$YEAR)
  xMax = max(T1NC$YEAR)
  dMin = floor(xMin / x_breaks_every) * x_breaks_every
  dMax = floor(xMax / x_breaks_every) * x_breaks_every

  x_breaks = c(xMin, seq(dMin, dMax, x_breaks_every))
  if (x_breaks_every == 1 | (xMax - dMax) > 1) x_breaks = append(x_breaks, xMax)
  x_breaks = unique(x_breaks)

  T1_y = T1NC[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(YEAR)]

  T1 = initialize_plot(T1NC)

  T1 = T1 +
    ggplot2::geom_area(
      data = T1NC,
      mapping = ggplot2::aes(
        x = YEAR,
        y = CATCH,
        fill  = CATEGORY_CODE,
        color = CATEGORY_CODE,
        group = CATEGORY_CODE           # <-- empilement stable (même ordre partout)
      ),
      stat      = "align",
      position  = "stack",
      linewidth = 0.2,
      alpha     = 0.7
    )

  T1 = T1 +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = x_breaks,
                                guide  = ggplot2::guide_axis(n.dodge = 1)) +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::scale_color_manual(values = color_values, guide = ggplot2::guide_none())

  y_breaks = breaks_for(min(T1_y$CATCH), max(T1_y$CATCH))
  y_lims   = limit_for (min(T1_y$CATCH), max(T1_y$CATCH))

  if (relative) {
    y_breaks = seq(0, 100, 10)
    y_lims   = c(0, 100)
  }

  T1 = T1 +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Year",
                  y = paste0("Catches (", ifelse(relative, "%", "t"), ")")) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = y_breaks,
      labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
      sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
    ) +
    ggplot2::coord_cartesian(ylim = y_lims) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title))

  return(T1)
}


#' Default plot function to display annual time series of T1 nominal catch data as an area chart categorized by gear group
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param max_categories the maximum number of categories to display. Everything else will be placed under the _Other_ category
#' @param relative whether or not the areas should represent the relative composition of annual catches, or their absolute values
#' @return an area chart of the provided data with the given configuration
#' @export
t1nc.plot.area_gear_groups = function(t1nc_data, max_categories = NA, relative = FALSE, x_breaks_every = 5) {
  return(
    t1nc.plot.area(
      t1nc_data,
      relative = relative,
      category_column = "GearGrp",
      ref_categories = iccat.pub.data::REF_GEAR_GROUPS$CODE,
      max_categories = max_categories,
      other_category_label = "OT",
      legend_title = "Gear group",
      colors = iccat.pub.aes::REF_GEAR_GROUPS_COLORS[, .(CODE = GEAR_GROUP_CODE, FILL, COLOR)],
      x_breaks_every = x_breaks_every
    )
  )
}

#' Default plot function to display annual time series of T1 nominal catch data as an area chart categorized by species' gear group
#' (see the \code{GearGrpBySpecies} and \code{Species} table in \code{\link{iccat.dev.base::DATABASE_T1}})
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param max_categories the maximum number of categories to display. Everything else will be placed under the _Other_ category
#' @param relative whether or not the areas should represent the relative composition of annual catches, or their absolute values
#' @return an area chart of the provided data with the given configuration
#' @export
t1nc.plot.area_species_gear_groups = function(t1nc_data, max_categories = NA, relative = FALSE, x_breaks_every = 5) {
  return(
    t1nc.plot.area(
      t1nc_data,
      relative = relative,
      category_column = "SpcGearGrp",
      ref_categories = iccat.pub.data::REF_SPECIES_GEAR_GROUPS[SPECIES_GEAR_GROUP %in% t1nc_data$SpcGearGrp, .(SPECIES_GEAR_GROUP_ORDER = max(SPECIES_GEAR_GROUP_ORDER)), keyby = .(SPECIES_GEAR_GROUP)][order(SPECIES_GEAR_GROUP_ORDER, SPECIES_GEAR_GROUP)]$SPECIES_GEAR_GROUP,
      max_categories = max_categories,
      other_category_label = "Others",
      legend_title = "Species gear group",
      colors = iccat.pub.aes::REF_SPECIES_GEAR_GROUPS_COLORS[, .(CODE = SPECIES_GEAR_GROUP, FILL, COLOR)],
      x_breaks_every = x_breaks_every
    )
  )
}

#' Default plot function to display annual time series of T1 nominal catch data as an area chart categorized by catch type (retained / landed / discarded dead / etc.)
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param relative whether or not the areas should represent the relative composition of annual catches, or their absolute values
#' @return an area chart of the provided data with the given configuration
#' @export
t1nc.plot.area_catch_types = function(t1nc_data, relative = FALSE, x_breaks_every = 5) {
  return(
    t1nc.plot.area(
      t1nc_data,
      relative = relative,
      category_column = "CatchTypeCode",
      ref_categories = iccat.pub.data::REF_CATCH_TYPES$CODE,
      legend_title = "Catch type",
      colors = iccat.pub.aes::REF_CATCH_TYPES_COLORS[, .(CODE = CATCH_TYPE_CODE, FILL, COLOR)],
      x_breaks_every = x_breaks_every
    )
  )
}

#' Default plot function to display annual time series of T1 nominal catch data as an area chart categorized by species' stocks
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param relative whether or not the areas should represent the relative composition of annual catches, or their absolute values
#' @return an area chart of the provided data with the given configuration
#' @export
t1nc.plot.area_stocks = function(t1nc_data, relative = FALSE, x_breaks_every = 5) {
  stock_codes = sort(unique(t1nc_data$Stock))

  stock_colors =
    data.table(
      CODE = stock_codes,
      FILL = hue_pal()(length(stock_codes))
    )

  stock_colors[, COLOR := darken(FILL, amount = .3)]

  return(
    t1nc.plot.area(
      t1nc_data,
      relative = relative,
      category_column = "Stock",
      ref_categories  = stock_codes,
      legend_title    = "Stock",
      colors          = stock_colors,
      x_breaks_every  = x_breaks_every
    )
  )
}

#' Default plot function to display annual time series of T1 nominal catch data as an area chart categorized by species' sampling areas
#'
#' @param t1nc_data the T1 nominal catch data as retrieved through \code{\link{iccat.dev.data::t1nc}}
#' @param relative whether or not the areas should represent the relative composition of annual catches, or their absolute values
#' @param max_categories the maximum number of categories to display. Everything else will be placed under the _Other_ category
#' @return an area chart of the provided data with the given configuration
#' @export
t1nc.plot.area_sampling_areas = function(t1nc_data, relative = FALSE, max_categories = 16, x_breaks_every = 5) {
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
    t1nc.plot.area(
      t1nc_data,
      relative = relative,
      category_column = "SampAreaCode",
      ref_categories  = sampling_area_codes,
      max_categories  = max_categories,
      other_category_label = "Other areas",
      legend_title    = "Sampling area",
      colors          = sampling_area_colors,
      x_breaks_every  = x_breaks_every
    )
  )
}
