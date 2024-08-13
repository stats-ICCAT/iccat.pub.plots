# See: https://stackoverflow.com/questions/23252231/r-data-table-breaks-in-exported-functions
.datatable.aware = TRUE

#' TBD
#'
#' @param base_map TBD
#' @param catdis_data TBD
#' @param gears_to_keep TBD
#' @param default_radius TBD
#' @param max_catch TBD
#' @param center_pies TBD
#' @param legend.x TBD
#' @param legend.y TBD
#' @return TBD
#' @export
catdis.plot.piemap = function(base_map = map.atlantic(), catdis_data, gears_to_keep = NULL, default_radius = pi, max_catch = NA, center_pies = TRUE, legend.x = -90, legend.y = -25) {
  if(is.null(catdis_data) | nrow(catdis_data) == 0) stop("No catdis data provided!")

  if(!is.null(gears_to_keep)) {
    catdis_data[!GearGrp %in% gears_to_keep, GearGrp := 'OT']
  }

  if(center_pies) {
    catdis_data =
      merge(catdis_data, iccat.pub.maps::GRIDS_5x5_RAW_GEOMETRIES,
            by.x = "CWPCode", by.y = "CODE",
            all.x = TRUE)

    catdis_data = catdis_data[, .(CATCH = as.numeric(sum(Catch_t, na.rm = TRUE))), keyby = .(LON = CENTER_LON, LAT = CENTER_LAT, GEAR = GearGrp)]
  } else
    catdis_data = catdis_data[, .(CATCH = as.numeric(sum(Catch_t, na.rm = TRUE))), keyby = .(LON = xLon5ctoid, LAT = yLat5ctoid, GEAR = GearGrp)]

  all_gears = unique(catdis_data$GEAR)

  if(length(all_gears) == 1) {
    # Otherwise, for datasets with only one gear, the geom_scatterpie function will yield an error...
    catdis_data = rbind(catdis_data, data.table(LON = 0, LAT = 0, GEAR = ifelse(all_gears[1] != "OT", "OT", "foo"), CATCH = 0))
  }

  catdis_data_W =
    dcast.data.table(
      catdis_data,
      LON + LAT ~ GEAR,
      fun.aggregate = sum,
      value.var = "CATCH"
    )

  catdis_data_W[, RADIUS     := rowSums(catdis_data_W[, 3:ncol(catdis_data_W)])]
  catdis_data_W[, RADIUS_REL := default_radius * sqrt(RADIUS / ifelse(is.na(max_catch), max(RADIUS), max_catch))]

  fill_colors = REF_GEAR_GROUPS_COLORS[GEAR_GROUP_CODE %in% unique(catdis_data$GEAR)][order(GEAR_GROUP_CODE)]$FILL #brewer.pal(n = length(unique(catdis_data$GEAR)), name = "Set2")

  return(
    base_map +

      #new_scale("fill") +
      #new_scale("Color") +

      geom_scatterpie(
        data = catdis_data_W,
        aes(x = LON,
            y = LAT,
            r = RADIUS_REL
        ),
        linewidth = .3,
        alpha = .7,
        cols = as.character(sort(unique(catdis_data$GEAR))),
        long_format = FALSE
      ) +

      geom_scatterpie_legend(
        catdis_data_W$RADIUS_REL,
        x = legend.x,
        y = legend.y,
        labeller = function(x) {
          paste(prettyNum(round((x / default_radius) ^ 2 * ifelse(is.na(max_catch), max(catdis_data_W$RADIUS), max_catch)), big.mark = ","), " t")
        },
        breaks = c(0, default_radius / sqrt(2), default_radius),
        size = 2
      ) +

      scale_fill_manual("Gear group", values = fill_colors) +
      guides(
        fill = guide_legend(
          position = "bottom"
        )
      )
  )
}

#' TBD
#'
#' @param base_map TBD
#' @param catdis_data TBD
#' @param default_radius TBD
#' @param max_catch TBD
#' @param center_pies TBD
#' @param legend.x TBD
#' @param legend.y TBD
#' @return TBD
#' @export
catdis.plot.piemap.school = function(base_map = map.atlantic(), catdis_data, default_radius = pi, max_catch = NA, center_pies = TRUE, legend.x = -90, legend.y = -25) {
  if(is.null(catdis_data) | nrow(catdis_data) == 0) stop("No catdis data provided!")

  catdis_data[!SchoolType %in% c("FAD", "FSC"), SchoolType := "UNK"]
  catdis_data$SchoolType =
    factor(
      catdis_data$SchoolType,
      levels = c("FAD", "FSC", "UNK"),
      labels = c("FAD", "FSC", "UNK"),
      ordered = TRUE
    )

  if(center_pies) {
    catdis_data =
      merge(catdis_data, iccat.pub.maps::GRIDS_5x5_RAW_GEOMETRIES,
            by.x = "CWPCode", by.y = "CODE",
            all.x = TRUE)

    catdis_data = catdis_data[, .(CATCH = as.numeric(sum(Catch_t, na.rm = TRUE))), keyby = .(LON = CENTER_LON, LAT = CENTER_LAT, SCHOOL_TYPE = SchoolType)]
  } else
    catdis_data = catdis_data[, .(CATCH = as.numeric(sum(Catch_t, na.rm = TRUE))), keyby = .(LON = xLon5ctoid, LAT = yLat5ctoid, SCHOOL_TYPE = SchoolType)]

  all_school_types = unique(catdis_data$SCHOOL_TYPE)

  if(length(all_school_types) == 1) {
    # Otherwise, for datasets with only one school type, the geom_scatterpie function will yield an error...
    catdis_data = rbind(catdis_data, data.table(LON = 0, LAT = 0, SCHOOL_TYPE = ifelse(all_school_types[1] != "UNK", "UNK", "foo"), CATCH = 0))
  }

  catdis_data_W =
    dcast.data.table(
      catdis_data,
      LON + LAT ~ SCHOOL_TYPE,
      fun.aggregate = sum,
      value.var = "CATCH"
    )

  catdis_data_W[, RADIUS     := rowSums(catdis_data_W[, 3:ncol(catdis_data_W)])]
  catdis_data_W[, RADIUS_REL := default_radius * sqrt(RADIUS / ifelse(is.na(max_catch), max(RADIUS), max_catch))]

  fill_colors = data.table(
    SCHOOL_TYPE_CODE = c("FAD", "FSC", "UNK"),
    FILL             = c("yellow", "red", "gray")
  )

  fill_colors[, COLOR := darken(FILL, amount = 0.2)]

  fill_colors = fill_colors[SCHOOL_TYPE_CODE %in% unique(catdis_data$SCHOOL_TYPE)][order(SCHOOL_TYPE_CODE)]$FILL

  return(
    base_map +

      #new_scale("fill") +
      #new_scale("Color") +

      geom_scatterpie(
        data = catdis_data_W,
        aes(x = LON,
            y = LAT,
            r = RADIUS_REL
        ),
        linewidth = .3,
        alpha = .7,
        cols = as.character(sort(unique(catdis_data$SCHOOL_TYPE))),
        long_format = FALSE
      ) +

      geom_scatterpie_legend(
        catdis_data_W$RADIUS_REL,
        x = legend.x,
        y = legend.y,
        labeller = function(x) {
          paste(prettyNum(round((x / default_radius) ^ 2 * ifelse(is.na(max_catch), max(catdis_data_W$RADIUS), max_catch)), big.mark = ","), " t")
        },
        breaks = c(0, default_radius / sqrt(2), default_radius),
        size = 2
      ) +

      scale_fill_manual("School types", values = fill_colors) +
      guides(
        fill = guide_legend(
          position = "bottom"
        )
      )
  )
}

catdis.create_breaks = function(values, num_intervals) {
  dp = 1.0 / num_intervals

  return(unique(quantile(values, probs = seq(0, 1, dp), na.rm = TRUE, type = 6)))
}

catdis.create_breaks_uniform = function(values, num_intervals) {
  max_value = max(values)

  return(
    seq(0, max_value, max_value * 1.0 / num_intervals - 1)
  )
}

catdis.labels_for_breaks = function(breaks) {
  labels = c()

  for(v in c(1:(length(breaks) - 1)))
    labels =
      append(
        labels,
        paste("(", prettyNum(round(breaks[v  ][[1]]), big.mark = ","),
              "-", prettyNum(round(breaks[v+1][[1]]), big.mark = ","),
              "]")
      )

  return(labels)
}

#' TBD
#'
#' @param base_map TBD
#' @param catdis_data TBD
#' @param gears_to_keep TBD
#' @param gear TBD
#' @param num_breaks TBD
#' @return TBD
#' @export
catdis.plot.heatmap = function(base_map = map.atlantic(), catdis_data, gears_to_keep = NULL, gear, num_breaks = 5) {
  if(is.null(catdis_data) | nrow(catdis_data) == 0) stop("No catdis data provided!")

  if(!is.null(gears_to_keep)) {
    catdis_data[!GearGrp %in% gears_to_keep, GearGrp := 'OT']
  }

  catdis_data = catdis_data[, .(CATCH = as.numeric(sum(Catch_t, na.rm = TRUE))), keyby = .(GEAR = GearGrp, CWP_CODE = CWPCode)]

  break_values = catdis.create_breaks(catdis_data$CATCH, num_breaks)
  break_labels = catdis.labels_for_breaks(break_values)

  catdis_data = catdis_data[GEAR == gear]

  if(nrow(catdis_data) == 0) stop(paste0("No catdis data available for gear ", gear, "!"))

  catdis_data = merge(
    st_as_sf(
      iccat.pub.maps::GRIDS_5x5_RAW_GEOMETRIES,
      crs = 4326,
      wkt = "GEOMETRY_WKT"
    ),
    catdis_data,
    by.y = "CWP_CODE", by.x = "CODE",
    all.y = TRUE
  )

  # Unused
  catdis_data$FILL =
    cut(
      catdis_data$CATCH,
      include.lowest = TRUE,
      right = FALSE,
      breaks = break_values,
      labels = break_labels,
      extend = TRUE
    )

  return(
    base_map +
      geom_sf(
        data = catdis_data,
        aes(alpha = CATCH),
        fill  = REF_GEAR_GROUPS_COLORS[GEAR_GROUP_CODE == gear]$FILL,
        color = "transparent"
      ) +
      scale_alpha_continuous(#breaks = seq(0, max(catdis_data$CATCH), max(catdis_data$CATCH) / num_breaks),
        labels = scales::comma) +
      guides(
        alpha = guide_legend(
          title = paste0(gear, " catches (t)"),
          position = "bottom"
        )
      )
  )
}

#' TBD
#'
#' @param base_map TBD
#' @param catdis_data TBD
#' @param school_type TBD
#' @param num_breaks TBD
#' @return TBD
#' @export
catdis.plot.heatmap.school = function(base_map = map.atlantic(), catdis_data, school_type, num_breaks = 5) {
  if(is.null(catdis_data) | nrow(catdis_data) == 0) stop("No catdis data provided!")

  catdis_data[!SchoolType %in% c("FAD", "FSC"), SchoolType := "UNK"]
  catdis_data$SchoolType =
    factor(
      catdis_data$SchoolType,
      levels = c("FAD", "FSC", "UNK"),
      labels = c("FAD", "FSC", "UNK"),
      ordered = TRUE
    )

  catdis_data = catdis_data[, .(CATCH = as.numeric(sum(Catch_t, na.rm = TRUE))), keyby = .(SCHOOL_TYPE = SchoolType, CWP_CODE = CWPCode)]

  break_values = catdis.create_breaks(catdis_data$CATCH, num_breaks)
  break_labels = catdis.labels_for_breaks(break_values)

  catdis_data = catdis_data[SCHOOL_TYPE == school_type]

  if(nrow(catdis_data) == 0) stop(paste0("No catdis data available for school type ", school_type, "!"))

  fill_colors = data.table(
    SCHOOL_TYPE_CODE = c("FAD", "FSC", "UNK"),
    FILL             = c("yellow", "red", "gray")
  )

  fill_colors[, COLOR := darken(FILL, amount = 0.2)]

  fill_colors = fill_colors[SCHOOL_TYPE_CODE == school_type]

  catdis_data = merge(
    st_as_sf(
      iccat.pub.maps::GRIDS_5x5_RAW_GEOMETRIES,
      crs = 4326,
      wkt = "GEOMETRY_WKT"
    ),
    catdis_data,
    by.y = "CWP_CODE", by.x = "CODE",
    all.y = TRUE
  )

  # Unused
  catdis_data$FILL =
    cut(
      catdis_data$CATCH,
      include.lowest = TRUE,
      right = FALSE,
      breaks = break_values,
      labels = break_labels,
      extend = TRUE
    )

  return(
    base_map +
      geom_sf(
        data = catdis_data,
        aes(alpha = CATCH),
        fill  = fill_colors$FILL,
        color = "transparent"
      ) +
      scale_alpha_continuous(#breaks = seq(0, max(catdis_data$CATCH), max(catdis_data$CATCH) / num_breaks),
        labels = scales::comma) +
      guides(
        alpha = guide_legend(
          title = paste0(school_type, " catches (t)"),
          position = "bottom"
        )
      )
  )
}

