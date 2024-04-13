options(scipen = 9999)

t1nc.barplot = function(t1nc_data, relative = FALSE) {
  T1NC = t1nc_data[, .(CATCH = sum(Qty_t, na.rm = TRUE)), keyby = .(YEAR = YearC, GEAR_GROUP_CODE = GearGrp)]

  T1NC$GEAR_GROUP_CODE =
    factor(
      T1NC$GEAR_GROUP_CODE,
      labels = iccat.pub.data::REF_GEAR_GROUPS$CODE,
      levels = iccat.pub.data::REF_GEAR_GROUPS$CODE
    )

  T1 =
    ggplot(
      T1NC,
      aes(
        x = YEAR,
        y = CATCH,
        fill = GEAR_GROUP_CODE,
        color = GEAR_GROUP_CODE,
        group = GEAR_GROUP_CODE
      )
    )

  T1 = T1 +
    geom_bar(
      stat = "identity",
      position = "stack",
      width = 1,
      linewidth = .2
    )

  T1 = T1 +
    scale_x_continuous(expand = c(0, 0),
                       breaks =
                         sort(
                           unique(
                             c(min(T1NC$YEAR),
                               seq(min(T1NC$YEAR), max(T1NC$YEAR), 5),
                               max(T1NC$YEAR)
                              )
                           )
                         ),
                       guide = guide_axis(n.dodge = 2))

  T1 = T1 +
    scale_fill_manual (values = REF_GEAR_GROUPS_COLORS[GEAR_GROUP_CODE %in% unique(T1NC_$GEAR_GROUP_CODE)]$FILL) +
    scale_color_manual(values = REF_GEAR_GROUPS_COLORS[GEAR_GROUP_CODE %in% unique(T1NC_$GEAR_GROUP_CODE)]$COLOR, guide = guide_none())

  T1 = T1 +
    theme_bw() +
    labs(x = "Year", y = "Catches (t)")

  return(
    T1
  )
}
