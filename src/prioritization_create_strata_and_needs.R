# ---
#   title: "Plot Prioritization"
# author: "Shive Lab (B. Baker)"
# date: "`r Sys.Date()`"
# output: html_document
# ---
library(tidyverse)
library(here)
library(sf)
library(terra)
library(janitor)

groves <- vect(here("data/spatial_data/SEKI_groves/SEKI_groves_list.shp")) %>% 
  st_as_sf()

existing_plots <-read_csv(here("data/spatial_data/all_plots_groves_env.csv"))

aspect <- vect(here("data/spatial_data/aspect_polys.shp")) %>% 
  st_as_sf() %>% 
  st_transform(crs(groves))

trt <- vect(here("data/spatial_data/trt/SEGI_Trtdata_05feb25.shp")) %>% 
  project(crs(groves))

fire <- vect(here("data/spatial_data/cbi_all/cbi_all.shp")) %>% 
  project(crs(groves))

no_fire <- read_sf(here("data/spatial_data/no_fire/no_fire.shp")) %>% 
  st_transform(crs(groves))

plots_sf <- st_read(here("data/spatial_data/all_plots_groves.shp")) 

access <- read.csv(here("data/grove_access.csv"))

#get all spatial files of same type
fire <- fire %>% 
  st_as_sf()

trt <- trt %>% 
  st_as_sf()

#pull fire related treatments to count as low sev fire
rx_seki <- trt %>% 
  # st_as_sf() %>% 
  filter(treatment == "Fire-related treatment") %>%  
  # vect() %>% 
  st_intersection(groves) %>% 
  # st_as_sf() %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% 
  mutate(burnsev = 2,
         id = paste(grov_nm, year, rowid, sep = "_"))%>% 
  rename(fire_yr = year)%>% 
  select(id, fire_yr, burnsev)

no_burn <- no_fire %>% 
  mutate(
    burnsev = 0,
    fire_yr = 0000,
    id = "unburned"
  )%>% 
  select(id, fire_yr, burnsev)


# aspect_groves <- intersect(aspect_vect, groves_vect) %>% 
#   st_as_sf()%>% 
aspect_groves <- aspect %>%
  st_intersection(groves) %>% 
  #ks: changed names for aspect to make briefer strata names later
  mutate(aspect = case_when(
    Id == 1 ~ "NW_E",
    Id == 2 ~ "SE_W"
  ))


# burn <- intersect(fire, groves_vect) %>% 
#   st_as_sf()%>%
burn <- fire %>%
  st_intersection(groves) %>%
  mutate(fire_yr = as.numeric(fire_yr)) %>% 
  select(id, fire_yr, burnsev) %>% 
  bind_rows(rx_seki) %>% 
  bind_rows(no_burn)


grove_area <- sum(groves$grovr_h)

##get plot totals
plots_rx <- st_intersection(rx_seki, plots_sf)

plots_rx_count <- plots_rx %>% 
  st_drop_geometry() %>% 
  summarise(.by = plot_id, trt_ct = n())

plots_rx_yr <- plots_rx %>% 
  group_by(plot_id) %>% 
  slice_max(fire_yr) %>% 
  rename(trt_yr = fire_yr) %>% 
  left_join(plots_rx_count) %>% 
  mutate(sev = "Low") %>% 
  select(plot_id, trt_yr, trt_ct, sev) %>% 
  st_drop_geometry()


totals_summary <- existing_plots %>% 
  summarise(.by = c(grove_name), total = n())

plot_total <-  sum(totals_summary$total)

plots_all <- existing_plots %>% 
  left_join(plots_rx_yr) %>% 
  replace_na(list(trt_yr = 0000, trt_ct = 0, sev = "Unburned")) %>% 
  mutate(
    fire_yr = as.numeric(fire_yr),
    burnsev = case_when(fire_yr<trt_yr ~ sev, T ~ burnsev),
    fire_yr = case_when(fire_yr<trt_yr ~ trt_yr, T ~ fire_yr),
    fire_count = fire_count+trt_ct
  )

plots_classified <- plots_all %>% 
  mutate(
    burnsev = case_when(burnsev == "Undetected change" ~ "Unburned",
                        T ~ burnsev),
    #ks: changed names for aspect and time_since to make briefer strata names later
    aspect = case_when(
      aspect <= 90 | aspect > 315~ "NW_E",
      T ~ "SE_W"),
    time_since = case_when(
      fire_yr == 0000 ~ "over5",
      fire_yr  >= 2020 ~ "under5",
      T ~ "over5"
    ),
    count = case_when(fire_count >= 2 ~ "many",
                      T ~ "one"),
    count_v2 = case_when(burnsev == "Unburned" ~ "none", T ~ count))
# 
# pc = plots_classified %>%
#   filter(aspect == "SE_W" & time_since == "under5" & burnsev == "Low" &
#            count_v2 == "many")
# nrow(pc)

plot_sets <- plots_classified %>% 
  mutate(count_v2 = case_when(burnsev == "Unburned" ~ "none", T ~ count)) %>%
  summarize(.by = c(aspect, burnsev, time_since, count_v2), count_plot = n()) %>% 
  #ks: changed names for count_v2 to make briefer strata names later
  mutate(strata_nm = paste(aspect,"-",time_since,"-",burnsev,"-",count_v2, sep = "")) %>%
  group_by(aspect,burnsev,time_since,count_v2,strata_nm) %>%
  summarise(existing_plots = sum(count_plot))
# %>%
#   mutate(prop_plot = round(count_plot/plot_total, 2))


#get burn counts and sev
burn_count <- burn %>% 
  summarize(.by = c(id, fire_yr, burnsev), geometry = st_combine(geometry)) %>% 
  st_make_valid() %>% 
  arrange(-fire_yr)

burn_id <- burn_count %>% 
  st_drop_geometry() %>% 
  rowid_to_column("rid")

fire_count <- st_intersection(burn_count) %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE)

burn_classed <- fire_count %>%
  mutate(time_since = case_when(
    fire_yr == 0000 ~ "over5",
    fire_yr  >= 2020 ~ "under5",
    T ~ "over5"
  ),
  burnsev = case_when(
    burnsev == 0 ~ "Unburned",
    burnsev == 1 ~ "Unburned",
    burnsev == 2 ~ "Low",
    burnsev == 3 ~ "Moderate",
    burnsev == 4 ~ "High",
    T ~ "Unburned"
  ),
  #ks: changed names for count to make briefer strata names later
  count = case_when(n.overlaps >= 2 ~ "many",
                    T ~ "one")) %>%
  summarize(.by = c(time_since, burnsev, count), geometry = st_combine(geometry)) %>% 
  st_make_valid() 

##there are 28 possible combos in our actual grove data when all unburned are lumped,
##and then i am excluding any that are represented by <5 acres TOTAL, leaving 25 strata
groves_set <- burn_classed %>% 
  st_intersection(aspect_groves) %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% 
  mutate(count_v2 = case_when(
    burnsev == "Unburned" ~ "none", T ~ count)) %>%
  summarize(.by = c(aspect, time_since, burnsev, count_v2), geometry = st_combine(geometry)) %>% 
  st_make_valid() %>% 
  mutate(area_ha = as.numeric(st_area(.))*0.0001) %>%
  filter(area_ha > 2.02343) %>%
  mutate(prop_area = round(area_ha/grove_area, 2)) %>%
  mutate(strata_nm = 
           paste(aspect,"-", time_since, "-",burnsev, "-",count_v2, sep = "")) %>%
  group_by(strata_nm) 
# st_drop_geometry() >%>
View(groves_set)
grove_area_of22strata = sum(groves_set$area_ha)
sum(groves_set$prop_area)
#convert multipolygons to polygons so i can exclude slivers,
# this results 22 strata (3 more strata getting excluded), cuz tho they totalled >5 acres, 
#they were comprised of slivers that individually were smaller
groves_classified_poly_explode = st_collection_extract(groves_set, type = c("POLYGON"), warn = FALSE)
groves_classified_poly_explode$area_ha = as.numeric(st_area(groves_classified_poly_explode)*0.0001)

groves_classified_poly = groves_classified_poly_explode %>%
  # mutate(area_ha = (st_area(groves_classified_poly_explode))*0.0001) %>%
  #filter out individual silvers <5 acres
  filter(area_ha > 2.02343) %>%
  #regroup by strata
  # mutate(count_v2 = case_when(burnsev == "Unburned" ~ "none", T ~ count)) %>%
  group_by(aspect, time_since, burnsev, count_v2,prop_area) %>%
  summarise(area_ha = round(sum(area_ha),0)) %>%
  # create unique strada ID
  mutate(strata_nm = paste(aspect,"-",time_since,"-",burnsev,"-",count_v2, sep = "")) %>%
  left_join(plot_sets)
View(groves_classified_poly)  
sum(groves_classified_poly$prop_area)

# write_sf(groves_classified_poly, here("data/spatial_data/outputs/groveset_wGroveNames_over5acres.shp"))
#write_sf(groves_set, here("data/spatial_data/debug_groveset.shp"))


##get needed number of plots based on area of strata
final.strata.that.need.new.plots <- groves_classified_poly %>% 
  mutate(
    # area_ha = round(area_ha, 2),
    # diff.prop = prop_area - prop_plot,
    total.plots.needed = ceiling(501*prop_area),
    useful.plots = case_when(existing_plots>total.plots.needed ~ total.plots.needed,
                             T ~ existing_plots),
    NEW.plots.needed = total.plots.needed - useful.plots) %>%
  filter(NEW.plots.needed > 0)
head(final.strata.that.need.new.plots)
nrow(final.strata.that.need.new.plots)

#Since some strata needed none, leaves 14 strata that need sampling
write_sf(final.strata.that.need.new.plots, here("data/spatial_data/outputs/final.strata.that.need.new.plots.shp"))

##Create new shapefile that removes "Very Difficult" access groves, and dissolve into one big shapefile
access.min = access %>% 
  mutate(grov_nm = grove) %>%
  select(grov_nm,Ease.of.Access)
#get easier groves and dissolve to one multipolygon
groves_easier = groves %>% 
  # mutate(grove = grove_nm) %>%
  left_join(access.min) %>%
  filter(Ease.of.Access != "Very Difficult") %>%
  # st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(keep = T) %>%
  # st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON")
nrow(groves_easier)
head(groves_easier)

write_sf(groves_easier, here("data/spatial_data/outputs/groves_easier.shp"))


final.strata.that.need.new.plots_easier.1 = final.strata.that.need.new.plots %>%
  st_intersection(groves_easier) %>%
  st_cast("MULTIPOLYGON") %>%
  select(aspect:strata_nm) 
# write_sf(final.strata.that.need.new.plots_easier.1, here("data/spatial_data/outputs/final.strata.that.need.new.plots_easier_diss_no_cast.shp"))
final.strata.that.need.new.plots_easier.1$area_ha = as.numeric(st_area(final.strata.that.need.new.plots_easier.1)*0.0001)
head(final.strata.that.need.new.plots_easier.1)

#create a table to join the details on plots needed per strata back
final.strata.that.need.new.plots_table = final.strata.that.need.new.plots %>%
  st_drop_geometry() %>%
  select(strata_nm, existing_plots, total.plots.needed, useful.plots, NEW.plots.needed)

final.strata.that.need.new.plots_easier_table = final.strata.that.need.new.plots_easier.1 %>%
  st_drop_geometry() %>%
  left_join(final.strata.that.need.new.plots_table)
head(final.strata.that.need.new.plots_easier_table)
names(final.strata.that.need.new.plots_easier_table)
View(final.strata.that.need.new.plots_easier_table)

#join the table to the reduced strata shapefile
final.strata.that.need.new.plots_easier = final.strata.that.need.new.plots_easier.1 %>%
  left_join(final.strata.that.need.new.plots_easier_table) %>%
  st_cast("MULTIPOLYGON")
final.strata.that.need.new.plots_easier$area_ha = st_area(final.strata.that.need.new.plots_easier)*0.0001

write_sf(final.strata.that.need.new.plots_easier, here("data/spatial_data/outputs/final.strata.that.need.new.plots_easier.shp"))

#create table version
write.csv(final.strata.that.need.new.plots_easier_table, here("outputs/plot_needs_by_strata_2May2025_kls.csv"))

##split by attributes to get individual shapefiles for the GRTS
#select the column of the attribute table for the split
unique <- unique(final.strata.that.need.new.plots_easier$strata_nm)

#create new polygons based on the unique column ID
for (i in 1:length(unique)) {
  tmp <- final.strata.that.need.new.plots_easier[final.strata.that.need.new.plots_easier$strata_nm == unique[i], ]
  write_sf(tmp, here("data/spatial_data/ind_strata"), unique[i], driver="ESRI Shapefile",
           overwrite_layer=TRUE)
}



```


#Summarize plots by grove and rank

```{r}
##this table made in "existing_plots_assessment.Rmd"
groves_seki_summary = read.csv(here("outputs/groves_seki_summary.csv"))
plots_sf$grove_name = plots_sf$grov_nm

#make grove summary plot, nonspatial
groves_summary <- plots_sf %>% 
  st_drop_geometry() %>% #remove polygon reference
  summarize(.by = c(grove_name, project), plot_count = n()) %>% #count per type
  pivot_wider(names_from = project, # make wider dataframe for viz
              values_from = plot_count, 
              values_fill = 0) %>% 
  adorn_totals("col") %>% #calc total plots per grove
  # adorn_totals() %>%  #calc total plots per type
  left_join(groves_seki_summary) %>% # add grove info
  mutate(plots_peracre_buf = round(Total/groveacres,2),
         acres_perplot_buf = round(groveacres/Total, 0),
         groveacres = round(groveacres,0),
         total.existing.plots = Total) %>%
  select(-c(2:7))
# View(groves_summary)

hist(groves_summary$total.existing.plots)
hist(groves_summary$acres_perplot_buf, breaks = 100)

##rough ranking based on total plots and plot density
groves_summary_rank = groves_summary %>%
  mutate(total.plots.rank = 
           case_when(total.existing.plots <10 ~ "very few plots",
                     total.existing.plots >=10 & 
                       total.existing.plots <30 ~ "moderate plots",
                     total.existing.plots >=30 ~ "lotsa plots",
                     .default = "oopsie"),
         plot.density.rank = 
           case_when(acres_perplot_buf <10 ~ "high density",
                     acres_perplot_buf >=10 & 
                       acres_perplot_buf <50 ~ "moderate density",
                     acres_perplot_buf >=50 ~ "low density",
                     .default = "oopsie"),
         combo.rank = paste(total.plots.rank,"-",plot.density.rank))

# View(groves_summary_rank)  
write.csv(groves_summary_rank,here("outputs/groves_summary_rank.csv"))



```






