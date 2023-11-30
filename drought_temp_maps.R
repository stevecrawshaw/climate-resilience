pacman::p_load(tidyverse,
               sf,
               fs,
               glue,
               janitor,
               tmap)

# Read the drought and temperature projections from geojson
# data are geographically pre filtered from the arcgis portal
# Map with tmap to inspect local impacts

# 12-month rainfall deficits provided as a percentage of the mean annual climatological total rainfall (1981–2000) for that location. It measures the severity of a drought, not the frequency.

drought_sf <- st_read('data/Drought_Severity_Index%2C_12-Month_Accumulations_-_Projections.geojson') %>% 
  st_transform(crs = 27700)
 
# This dataset shows the change in summer maximum air temperature for a range of global warming levels, including the recent past (2001-2020), compared to the 1981-2000 baseline period. Here, summer is defined as June-July-August. 

temp_sf <- st_read('data/Summer_Maximum_Temperature_Change_-_Projections_(12km).geojson') %>% 
  st_transform(crs = 27700)
# boundary of weca + NS
lep_sf <- st_read("../lnrs/data/lep_boundary.geojson")

# Utility Functions

ren_end <- function(string){
# clean up the endings of the temp data names
    case_when(
    str_ends(string = string, "me") ~ glue("{string}dian"),
    str_ends(string = string, "up") ~ glue("{string}per"),
    str_ends(string = string, "medi") ~ glue("{string}an"),
    .default = string
  )
}

rename_round_lep <- function(sf_obj, buffer_size = 5000){

  rename_drought <- function(drought_col){
    stripped <- str_remove(drought_col, "DSI12_|tasmax_")
    stripp = if_else(str_starts(stripped, "[0-9]"),
                     glue("Future_{stripped}"),
                     if_else(str_starts(stripped, "geometry"),
                             stripped,
                             str_to_sentence(stripped)
                     ))
    stripp
  }
    
  lep_bbox <- st_bbox(lep_sf) %>% 
    st_as_sfc(crs = 27700) %>% 
    st_buffer(buffer_size,
              endCapStyle = "SQUARE")
  
# 
    sf_obj %>% 
    rename_with(rename_drought) %>% 
    mutate(across(where(is.numeric), ~round(.x, 1))) %>%
    st_intersection(lep_bbox)

}

# Select the metrics of interest - subset each dataset

drought_subset_sf <- drought_sf %>% 
  select(DSI12_baseline_81_00_median,
         DSI12_baseline_81_00_upper,
         DSI12_baseline_00_17_median,
         DSI12_baseline_00_17_upper,
         DSI12_20_median,
         DSI12_20_upper,
         DSI12_40_median,
         DSI12_40_upper) %>% 
  rename_round_lep()

temp_subset_sf <- temp_sf %>% 
  select(tasmax_summer_baseline_81_00_me,
         tasmax_summer_baseline_81_00_up,
         tasmax_summer_change_01_20_medi,
         tasmax_summer_01_20_upper,
         tasmax_summer_change_20_median,
         tasmax_summer_change_20_upper,
         tasmax_summer_change_40_median,
         tasmax_summer_change_40_upper
         ) %>% 
  rename_round_lep() %>% 
  rename_with(ren_end)


map_climate_data <- function(map_tbl, lep_sf){
  
map_tbl_long <- map_tbl %>% 
  pivot_longer(cols = !geometry)

tmap_mode("view") # OSM doesn't appear if you use plot mode
tm_basemap("OpenStreetMap") +
  tm_tiles("OpenStreetMap") +
  tm_shape(map_tbl_long) + # add shape, then config that element in following 
  tm_fill(col = "value", # tm_.. calls
          alpha = 0.5,
          palette = "Accent") +
  tm_facets(by = "name",
            nrow = 2,
            free.scales.fill = TRUE) +
  tm_shape(lep_sf) + # add lep layer
  tm_fill("name", alpha = 0, legend.show = FALSE ) +
  tm_borders(lwd = 3, alpha = 1)
}

# run the function on each dataset, producing faceted, interactive maps
map_climate_data(temp_subset_sf, lep_sf)
map_climate_data(drought_subset_sf, lep_sf)
