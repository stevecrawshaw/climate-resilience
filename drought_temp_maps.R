pacman::p_load(tidyverse,
               sf,
               fs,
               glue,
               janitor)

drought_sf <- st_read('data/Drought_Severity_Index%2C_12-Month_Accumulations_-_Projections.geojson') %>% 
  st_transform(crs = 27700)

temp_sf <- st_read('data/Summer_Maximum_Temperature_Change_-_Projections_(12km).geojson')%>% 
  st_transform(crs = 27700)

lep_sf <- st_read("../lnrs/data/lep_boundary.geojson")

rename_drought <- function(drought_col){
  stripped <- str_remove(drought_col, "DSI12_|tasmax_")
  stripp = if_else(str_starts(stripped, "[0-9]"),
    glue("H_{stripped}"),
    if_else(str_starts(stripped, "geometry"),
            stripped,
            str_to_sentence(stripped)
    ))
  stripp
}

lep_bbox <- st_bbox(lep_sf) %>% 
  st_as_sfc(crs = 27700) %>% 
  st_buffer(20000, endCapStyle = "SQUARE")


rename_round_lep <- function(sf_obj){

  sf_obj %>% 
    rename_with(rename_drought) %>% 
    mutate(across(where(is.numeric), ~round(.x, 1))) %>%
    st_intersection(lep_bbox)

}

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
  


glimpse(drought_subset_sf)

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
  rename_round_lep()

plot(drought_subset_sf, )
plot(temp_subset_sf)


drought_subset_sf %>% 
  select(Baseline_81_00_median) %>% 
  ggplot() +
  geom_sf(aes(fill = Baseline_81_00_median)) +
  geom_sf_label(aes(label = Baseline_81_00_median)) +
  geom_sf(data = lep_sf, fill = NA, linewidth = 2)
