# ICCAT `plots` `public` library

A set of functions to produce basic catch plots (bar charts, area charts. line charts, Pareto charts) for different categories (gear group, catch type, etc.)

This library is meant for public usage, and for this reason it does not have dependencies from the (development) ICCAT libraries that provide access to the databases.

## Artifacts that can be produced using the functions provided by the library

1) Bar charts (both absolute / relative)
+ Generic
+ By gear group
+ By species-specific gear group
+ By catch type
+ By stock
+ By sampling area 
2) Area charts (both absolute / relative)
+ Generic
+ By gear group
+ By species-specific gear group
+ By catch type
+ By stock
+ By sampling area 
3) Line charts
+ Generic
+ By gear group
+ By species-specific gear group
+ By catch type
+ By stock
+ By sampling area 
4) Pareto charts (both horizontal / vertical)
+ Generic
+ By fleet and gear
## External dependencies (CRAN) <a name="external_deps"></a>
+ `data.table`
+ `ggplot2`
+ `ggthemes`
+ `ggnewscale`
+ `colorspace`
+ `scales`
+ `scatterpie`
+ `sf`

### Installation
```
install.packages(c("ggplot2", "ggthemes", "ggnewscale", "colorspace", "scales", "sf", "maps", "mapdata", "mapplots", "scatterpie"))
```

## Internal dependencies <a name="internal_deps"></a>
+ [iccat.pub.data](https://github.com/stats-ICCAT/iccat.pub.data)
+ [iccat.pub.aes](https://github.com/stats-ICCAT/iccat.pub.aes)

### Installation (straight from GitHub)
```
library(devtools)

install_github("stats-ICCAT/iccat.pub.plots")
```
# Building the library

Assuming that all [external](#external_deps) and [internal](#internal_deps) dependencies are already installed in the R environment, and that the `devtools` package and [RTools](https://cran.r-project.org/bin/windows/Rtools/) are both available, the building process can be either started within R studio by selecting the Build > Build Source Package menu entry:

![image](https://github.com/user-attachments/assets/f209d8d4-568c-4200-bcf2-fb1fa0e1d2ef)

or by executing the following statement:

`devtools::document(roclets = c('rd', 'collate', 'namespace'))`

## Usage examples

### Loading the library

For the examples to work, the following statement should be executed once per session:

```
library(iccat.pub.plots)
```

> To run these examples we assume that the `T1NC` object contains all T1 nominal catch data as retrieved using the `iccat.dev.data::t1nc` function (i.e., `T1 = t1nc()`).
 
### T1 nominal catch bar charts 

#### Absolute catches by gear group (all years / gears / fleets / species) 
```
t1nc.plot.bar_gear_groups(T1NC)
```
![image](https://github.com/user-attachments/assets/3667bc46-030b-43df-8f1c-5edadaad2799)

#### Relative catches by gear group (all years / gears / fleets / species)
```
t1nc.plot.bar_gear_groups(T1NC, relative = TRUE)
```
![image](https://github.com/user-attachments/assets/9c22b87e-bc06-42a7-8ea2-977cf7eda5c4)

#### Absolute catches by gear group (all years / gears / fleets / species) limited to 10 categories
```
t1nc.plot.bar_gear_groups(T1NC, max_categories = 10)
```
![image](https://github.com/user-attachments/assets/eab45aaf-41ba-4b51-97b0-b1826d2ef253)

#### Absolute catches by catch type (all years / gears / fleets / species) 
```
t1nc.plot.bar_catch_types(T1NC)
```
![image](https://github.com/user-attachments/assets/2043dfc3-5483-4705-9eae-339e9ca5dd66)

#### Absolute catches by species-specific gear group (all years / gears / fleets / BUM) 

```
t1nc.plot.bar_species_gear_groups(T1NC[Species == "BUM"])
```
![image](https://github.com/user-attachments/assets/f1e6f01c-f9ac-489e-8555-c82436cdd877)

#### Absolute catches by species-specific stocks (all years / gears / fleets / ALB) 
```
t1nc.plot.bar_stocks(T1NC[Species == "ALB"])
```
![image](https://github.com/user-attachments/assets/de38022b-7aa3-4973-8179-348d34c55fcd)

#### Absolute catches by species-specific sampling areas (all years / gears / fleets / ALB) 
```
t1nc.plot.bar_sampling_areas(T1NC[Species == "ALB"])
```
![image](https://github.com/user-attachments/assets/b4002869-807f-4837-b7c2-d9d30500f2f9)

### T1 nominal catch area charts 

#### Absolute catches by gear group (all years / gears / fleets / species) 
```
t1nc.plot.area_gear_groups(T1NC)
```
![image](https://github.com/user-attachments/assets/cf918b40-974c-45b4-a3b5-120bc4e512f3)

#### Relative catches by gear group (all years / gears / fleets / species)  
```
t1nc.plot.area_gear_groups(T1NC, relative = TRUE)
```
![image](https://github.com/user-attachments/assets/a40ef446-0efb-455f-bde8-41e10e7b88f7)

### T1 nominal catch line charts 

#### Absolute catches by species-specific gear group (all years / gears / fleets / species) 
```
t1nc.plot.line_species_gear_groups(T1NC)
```
![image](https://github.com/user-attachments/assets/9d37c35f-a9cc-48ba-b582-83352d05f624)

### T1 nominal catch Pareto charts 

#### Cumulative catches by fleet and gear group (years from 1994 onwards / all gears / fleets / species)
```
t1nc.plot.pareto_fleet_gears(T1NC[YearC >= 1994])
```
![image](https://github.com/user-attachments/assets/58770e43-a6bd-4a60-91b3-b72127ffebeb)

#### Vertically oriented cumulative catches by fleet and gear group (years from 1994 onwards / all gears / fleets / species)
```
t1nc.plot.pareto_fleet_gears(T1NC[YearC >= 1994], vertical = TRUE)
```
![image](https://github.com/user-attachments/assets/07e92ab5-3366-462a-9af8-617f9bd50474)

#### Cumulative catches by fleet and catch type, explicitly providing category levels and colors
```
t1nc.plot.pareto(
  T1NC[YearC >= 1994],
  x_column = "FlagName", x_name = "Flag", max_x = 10,
  category_column = "CatchTypeCode", category_name = "Catch type", max_categories = 10,
  category_levels = iccat.pub.data::REF_CATCH_TYPES$CODE,
  category_colors = iccat.pub.aes::REF_CATCH_TYPES_COLORS,
  vertical = FALSE
)
```
![image](https://github.com/user-attachments/assets/c976adf8-e2fa-442e-8813-26ae139807f7)

## Future extensions
+ [ ] improve the way in which additional `geom_sf` layers are included in the map outputs
+ [ ] standardize function signatures' for CATDIS piemaps and heatmaps  

