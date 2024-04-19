#' TBD
#'
#' @export
DEFAULT_TRENDS_REL_DIFF_LIMITS =
  list(
    T0_100      = list(LOW = .9),
    T100_1000   = list(LOW = .5 , MED = .9),
    T1000_10000 = list(LOW = .1,  MED = .5, HIGH = .9),
    T10000_     = list(LOW = .05, MED = .1, HIGH = .5, VERY_HIGH = .9)
  )

#' TBD
#'
#' @param t1nc_data TBD
#' @param by_species TBD
#' @param by_stock TBD
#' @param by_gear TBD
#' @param rel_diff_limits TBD
#' @param sensitivity TBD
#' @param colorize_gears TBD
#' @return TBD
#' @export
t1nc.viz.trends = function(t1nc_data,
                           by_species = TRUE, by_stock = TRUE, by_gear = TRUE,
                           rel_diff_limits = DEFAULT_TRENDS_REL_DIFF_LIMITS, sensitivity = 0,
                           colorize_gears = FALSE) {
  sensitivity = min(1, max(0, 1 - sensitivity))

  T1NC_proc_m = t1nc.summarise(t1nc_data, by_species, by_stock, by_gear)$raw

  T1NC_proc_m[, PREV_YEAR := YEAR - 1]

  formula_components = c()
  grouped_columns = 0

  formula_components = append(formula_components, "FLAG_CODE")
  grouped_columns = grouped_columns + 1

  if(by_species) {
    formula_components = append(formula_components, "SPECIES_CODE")
    grouped_columns = grouped_columns + 1
  }

  if(by_gear) {
    formula_components = append(formula_components, "GEAR_GROUP_CODE")
    grouped_columns = grouped_columns + 1
  }

  if(by_stock) {
    formula_components = append(formula_components, "STOCK_CODE")
    grouped_columns = grouped_columns + 1
  }

  formula = paste0(formula_components, collapse = " + ")
  formula = paste0(formula, " ~ YEAR")

  T1NC_proc_m =
    merge(T1NC_proc_m, T1NC_proc_m,
          by.x = append(formula_components, "PREV_YEAR"), #c("SPECIES_CODE", "PREV_YEAR", "STOCK_CODE", "FLAG_CODE", "GEAR_GROUP_CODE"),
          by.y = append(formula_components, "YEAR"),      #c("SPECIES_CODE", "YEAR",      "STOCK_CODE", "FLAG_CODE", "GEAR_GROUP_CODE"),
          all.x = TRUE, allow.cartesian = TRUE)

  colnames(T1NC_proc_m)[which(colnames(T1NC_proc_m) == "CATCH.x")] = "CATCH"
  colnames(T1NC_proc_m)[which(colnames(T1NC_proc_m) == "CATCH.y")] = "CATCH_PREV"

  T1NC_proc_m[ is.na(CATCH), DIFF := NA_real_ ]
  T1NC_proc_m[!is.na(CATCH), DIFF := CATCH - CATCH_PREV ]
  T1NC_proc_m[ is.na(CATCH), DIFF_REL := NA_real_ ]
  T1NC_proc_m[!is.na(CATCH) & !is.na(CATCH_PREV), DIFF_REL := abs(DIFF) / ifelse(CATCH > CATCH_PREV, CATCH, CATCH_PREV)]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV), DIFF_REL := 1]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV), DIFF_REL := 1]

  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV), LOG := floor(log10(CATCH_PREV))]
  T1NC_proc_m[!is.na(CATCH) &  is.na(CATCH_PREV), LOG := floor(log10(CATCH))]
  T1NC_proc_m[!is.na(CATCH) & !is.na(CATCH_PREV), LOG := floor(log10(ifelse(CATCH > CATCH_PREV, CATCH, CATCH_PREV)))]

  T1NC_proc_m[ is.na(CATCH), CHANGE := ""]
  T1NC_proc_m[!is.na(CATCH), CHANGE := "="]

  T1NC_proc_m[!is.na(CATCH) & LOG <= 1 & CATCH != 0 & CATCH_PREV != 0 & DIFF_REL >= rel_diff_limits$T0_100$LOW * sensitivity, CHANGE := "+"]

  T1NC_proc_m[!is.na(CATCH) & LOG >= 2 & DIFF_REL >= rel_diff_limits$T100_1000$LOW * sensitivity    , CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 2 & DIFF_REL >= rel_diff_limits$T100_1000$MED * sensitivity    , CHANGE := "++"]

  T1NC_proc_m[!is.na(CATCH) & LOG >= 3 & DIFF_REL >= rel_diff_limits$T1000_10000$LOW * sensitivity  , CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 3 & DIFF_REL >= rel_diff_limits$T1000_10000$MED * sensitivity  , CHANGE := "++"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 3 & DIFF_REL >= rel_diff_limits$T1000_10000$HIGH * sensitivity , CHANGE := "+++"]

  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$LOW * sensitivity      , CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$MED * sensitivity      , CHANGE := "++"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$HIGH * sensitivity     , CHANGE := "+++"]
  T1NC_proc_m[!is.na(CATCH) & LOG >= 4 & DIFF_REL >= rel_diff_limits$T10000_$VERY_HIGH * sensitivity, CHANGE := "++++"]

  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 0, CHANGE := ""]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 1, CHANGE := "+"]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 2, CHANGE := "++"]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG == 3, CHANGE := "+++"]
  T1NC_proc_m[ is.na(CATCH) & !is.na(CATCH_PREV) & LOG >= 4, CHANGE := "++++"]

  T1NC_proc_m[!is.na(CATCH) & is.na(CATCH_PREV)  & LOG == 0 & YEAR != min(YEAR), CHANGE := "="]
  T1NC_proc_m[!is.na(CATCH) & is.na(CATCH_PREV)  & LOG == 1 & YEAR != min(YEAR), CHANGE := "+"]
  T1NC_proc_m[!is.na(CATCH) & is.na(CATCH_PREV)  & LOG == 2 & YEAR != min(YEAR), CHANGE := "++"]
  T1NC_proc_m[!is.na(CATCH) & is.na(CATCH_PREV)  & LOG == 3 & YEAR != min(YEAR), CHANGE := "+++"]
  T1NC_proc_m[!is.na(CATCH) & is.na(CATCH_PREV)  & LOG >= 4 & YEAR != min(YEAR), CHANGE := "++++"]

  T1NC_proc_m[!is.na(CATCH) & is.na(CATCH_PREV)  & CATCH == 0, CHANGE := "="]

  T1NC_proc_m[YEAR == min(YEAR) & !is.na(CATCH), CHANGE := "="]

  T1NC_proc_m_w =
    dcast.data.table(
      T1NC_proc_m,
      formula = as.formula(formula), #SPECIES_CODE + STOCK_CODE + FLAG_CODE + GEAR_GROUP_CODE ~ YEAR,
      fun.aggregate = sum, #function(x) ifelse(is.na(x), NA_real_, x),
      value.var = "CATCH",
      drop = TRUE, fill = FALSE
    )

  T1NC_proc_m_d_w =
    dcast.data.table(
      T1NC_proc_m,
      formula = as.formula(formula), #SPECIES_CODE + STOCK_CODE + FLAG_CODE + GEAR_GROUP_CODE ~ YEAR,
      fun.aggregate = function(x) ifelse(is.na(x), "", x),
      value.var = "CHANGE",
      drop = TRUE, fill = FALSE
    )

  last_row_by_flag = T1NC_proc_m_w[, .(ROW = max(.N)), keyby = .(FLAG_CODE)]
  last_row_by_flag[, ROW := cumsum(ROW)]

  bg_matrix = T1NC_proc_m_d_w[, (grouped_columns + 1):ncol(T1NC_proc_m_d_w)]

  bg_matrix =
    ifelse(bg_matrix == "", "lightgray",
           ifelse(bg_matrix == "+", "yellow",
                  ifelse(bg_matrix == "++", "orange",
                         ifelse(bg_matrix == "+++", "red",
                                ifelse(bg_matrix == "++++", "darkred",
                                       "white")
                         )
                  )
           )
    )

  fg_matrix = T1NC_proc_m_d_w[, (grouped_columns + 1):ncol(T1NC_proc_m_d_w)]

  fg_matrix =
    ifelse(fg_matrix == "++++" | fg_matrix == "+++", "white", "black")

  if(by_gear) {
    gear_group_bg_matrix =
      lighten(
        merge(
          T1NC_proc_m_w[, .(GEAR_GROUP_CODE)],
          iccat.pub.aes::REF_GEAR_GROUPS_COLORS,
          by = "GEAR_GROUP_CODE",
          all.x = TRUE,
          sort = FALSE
        )$FILL,
        amount = .3
      )
  }

  to_merge = c()

                 to_merge = append(to_merge, "FLAG_CODE")
  if(by_species) to_merge = append(to_merge, "SPECIES_CODE")
  #if(by_stock)   to_merge = append(to_merge, "STOCK_CODE")
  if(by_gear )   to_merge = append(to_merge, "GEAR_GROUP_CODE")

  T1NC_FT =
    flextable(T1NC_proc_m_w) %>%
    bg(part = "all", bg = "white") %>% # Default BG color
    set_header_labels(values = list(SPECIES_CODE    = "Species",
                                    STOCK_CODE      = "Stock",
                                    FLAG_CODE       = "Flag name",
                                    GEAR_GROUP_CODE = "Gear group")) %>%
    bold(part = "header") %>%

    line_spacing(part = "all", space   = 0) %>%
    padding     (part = "all", padding = 5)

  T1NC_FT = T1NC_FT %>% merge_v(to_merge, combine = TRUE)

  if(by_species)
    T1NC_FT = T1NC_FT %>% merge_v(j = c("FLAG_CODE", "SPECIES_CODE"), combine = TRUE)

  T1NC_FT = T1NC_FT %>% merge_v(j = c("FLAG_CODE")) # Necessary to properly format the output

  T1NC_FT = T1NC_FT %>%
    valign(j = "FLAG_CODE",                               part = "body",   valign = "top") %>%
    valign(j =  to_merge,                                 part = "body",   valign = "top") %>%
    valign(j = (grouped_columns + 1):ncol(T1NC_proc_m_w), part = "all",    valign = "center") %>%
    align( j = (grouped_columns + 1):ncol(T1NC_proc_m_w), part = "header",  align = "center") %>%

    bg    (                                               part = "header", bg    = "grey") %>%
    bg    (j = (grouped_columns + 1):ncol(T1NC_proc_m_w), part = "body",   bg    = bg_matrix) %>%
    color (j = (grouped_columns + 1):ncol(T1NC_proc_m_w), part = "body",   color = fg_matrix) %>%

    border(part = "header", border.left = fp_border(),   border.right = fp_border()) %>%
    border(part = "body",   border.bottom = fp_border(), border.right = fp_border()) %>%
    border(part = "body",   j = 1, border.left = fp_border()) %>%

    border(part = "body", i = last_row_by_flag$ROW, border.bottom = fp_border(width = 2)) %>%

    fontsize(part = "all", size = 7) %>%
    autofit() %>%
    fix_border_issues()

    if(colorize_gears & by_gear)
      T1NC_FT = T1NC_FT %>% bg(j = "GEAR_GROUP_CODE", part = "body", bg = gear_group_bg_matrix)

  return(
    T1NC_FT
  )
}
