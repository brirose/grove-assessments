############################################################################
################# Grove Fuels Assessment - SEKI ############################

#This code takes the full strata spatial set and refines it to exclude
#slivers and very difficult to access groves

library(tidyverse)
library(here)
library(sf)
library(terra)
library(janitor)
library(gdalUtilities)
library(scales)

options(scipen = 999)

groves <- vect(here("data/spatial_data/SEKI_groves/SEKI_groves_list.shp")) %>%
  st_as_sf()

existing_plots <-read_csv(here("data/spatial_data/all_plots_groves_env.csv"))

# aspect <- vect(here("data/spatial_data/aspect_polys.shp")) %>%
#   st_as_sf() %>%
#   st_transform(crs(groves))
# 
# trt <- vect(here("data/spatial_data/trt/SEGI_Trtdata_05feb25.shp")) %>%
#   project(crs(groves))
# 
# fire <- vect(here("data/spatial_data/cbi_all/cbi_all.shp")) %>%
#   project(crs(groves))
# 
# no_fire <- read_sf(here("data/spatial_data/no_fire/no_fire.shp")) %>%
#   st_transform(crs(groves))

plots_sf <- st_read(here("data/spatial_data/all_plots_groves.shp"))

access <- read.csv(here("data/grove_access.csv"))
# access = access_prep %>%
#   mutate(Ease.Of.Access.revised = case_when(
#     grove %in% c("Cahoon","Horse Creek") ~ "Difficult",
#     .default = Ease.of.Access))

#################################################
#################################################
#this has to be repaired in arcpro first - ogr2ogr is supposed to do it but I cannot make it work
groves_set1a.test = st_read(here("data/spatial_data/groves_set_full_28strata.shp"))

###########################################################################################
##get stats on strata percentages across groves and total hectares

#first total hectares per strata

#make list of strata that are less than 2 ha

#get groves with their proportion of strata 

#are any less tan 2 ha that are more than 10%


# Refining strata to exclude tiny strata 
##and then i am excluding any that are represented by <5 acres TOTAL, leaving 25 strata
groves_set1 <- groves_set1a %>%
  mutate(area_ha = round(as.numeric(st_area(.))*0.0001,3)) %>%
  filter(area_ha > 2.02343)
# %>%
#   mutate(prop_area_grove = round(area_ha/grovr_h, 2))
groves_set1

#get a table of strata proportions (excluding groves)
groves_set_seki_prop = groves_set1 %>%
  group_by(strata_nm) %>%
  summarise(total.strata.area.ha = sum(area_ha),
            strata.prop.area.seki = round(total.strata.area.ha/grove_area, 4))
groves_set_seki_prop

#join the strata proportions to the shapefile with all strata >5 acres
groves_set = groves_set1 %>%
  left_join(groves_set_seki_prop) %>%
  mutate(strata_grv_area_ha = area_ha, 
         perc.of.grv.in.strata = strata_grv_area_ha/grovr_h,
         perc.of.strata.in.grv = strata_grv_area_ha/total.strata.area.ha,
         )
groves_set
# View(groves_set)

write.csv(groves_set, here("outputs/strata_by_grove.csv"))


#######################################################################
#######################################################################
## EXISTING PLOTS
#get number of existing plots per strata - not by grove
existing.plots.area.per.strata1 = st_intersection(plots_sf,groves_set) %>%
  group_by(aspect,time_since,burnsev,
           count_v2,strata_nm) %>%
  summarise(existing_plots_by_strata = length(plot_id)) %>%
  st_drop_geometry()

existing.plots.area.per.strata = groves_set %>%
  left_join(existing.plots.area.per.strata1) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  group_by(strata_nm) %>%
  summarise(existing_plots_by_strata = mean(existing_plots_by_strata))
head(existing.plots.area.per.strata)

existing.plots.area.per.strata_lookup = existing.plots.area.per.strata %>%
  st_drop_geometry() %>%
  select(strata_nm, existing_plots_by_strata)
existing.plots.area.per.strata_lookup

write.csv(existing.plots.area.per.strata_lookup, here("outputs/existing_plots_and_area_per_strata.csv"))

#get existing plots by grove, plot density and access difficulty
existing.plots.grove = st_intersection(plots_sf,groves_set) %>%
  st_drop_geometry() %>%
  group_by(grov_nm) %>%
  summarise(existing_plots_grove = length(plot_id),
            grove_area = mean(grovr_h), grove_plot_density = round(existing_plots_grove/grove_area,2)) %>%
  mutate(grove = grov_nm) %>%
  left_join(access) %>%
  st_drop_geometry() %>%
  select(grove, existing_plots_grove, grove_area, grove_plot_density, grove_area)
existing.plots.grove

write.csv(existing.plots.grove, here("outputs/existing_plots_density_per_grove.csv"))


########################################################################
########################################################################
##excluding slivers and getting final plots needs

###Below convert multipolygons to polygons so i can exclude slivers,
# this results 22 strata (3 more strata getting excluded), cuz tho they totalled >5 acres, 
#they were comprised of slivers that individually were smaller

##explode to also remove individual slivers
groves_classified_poly_explode = st_cast(groves_set, "MULTIPOLYGON") %>% st_cast("POLYGON")
groves_classified_poly_explode$area_ha = as.numeric(st_area(groves_classified_poly_explode)*0.0001)

#remove slivers and regroup to strata_nm and grove
groves_classified_poly = groves_classified_poly_explode %>%
  #filter out individual silvers <5 acres
  filter(area_ha > 2.02343) %>%
  #regroup by strata
  group_by(strata_nm) %>%
  summarise(strata_area_ha = round(sum(area_ha),1),
            strata.prop.area.seki = mean(strata.prop.area.seki)) %>%
  left_join(existing.plots.area.per.strata_lookup)
head(groves_classified_poly)
View(groves_classified_poly)
unique(groves_classified_poly$strata_nm)
# write_sf(groves_classified_poly, here("data/spatial_data/outputs/groveset_over5acres.shp"))

###########################################################################
###########################################################################
#get needed number of plots based on area of strata
### this is broken!

#total plots needed is the existing number of plots that meet our needs (elsewhere calculated at 261)
#plus our estimate of new plots this year (200)
final.strata.that.need.new.plots <- groves_classified_poly %>% 
  group_by(strata_nm) %>%
  summarise(
    # strata_area_ha = round(area_ha, 2),
    total.plots.needed = ceiling(461*strata.prop.area.seki),
    useful.plots = case_when(existing_plots_by_strata>total.plots.needed ~ total.plots.needed,
                             T ~ existing_plots_by_strata),
    NEW.plots.needed = total.plots.needed - useful.plots) %>%
  filter(NEW.plots.needed > 0)
View(final.strata.that.need.new.plots)
head(final.strata.that.need.new.plots)
unique(final.strata.that.need.new.plots$strata_nm)
plot(final.strata.that.need.new.plots)
# View(final.strata.that.need.new.plots)
sum(final.strata.that.need.new.plots$NEW.plots.needed)+sum(final.strata.that.need.new.plots$existing_plots)

#Since some strata needed none, leaves 14 strata that need sampling
# write_sf(final.strata.that.need.new.plots, here("data/spatial_data/outputs/final.strata.that.need.new.plots.shp"))

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
  # st_union() %>% # unite to a geometry object
  # st_sf() 
  # %>% # make the geometry a data frame object
  # mutate(keep = T) 
  # %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON")
groves_easier
plot(groves_easier)
# write_sf(groves_easier, here("data/spatial_data/outputs/groves_easier.shp"))

final.strata.that.need.new.plots_easier.1 = final.strata.that.need.new.plots %>%
  st_intersection(groves_easier) %>%
  st_collection_extract("POLYGON") %>%
  mutate(area_ha_strata_grv = round(as.numeric(st_area(.)*0.0001),2)) %>%
  select(grov_nm, strata_nm, area_ha_strata_grv)
final.strata.that.need.new.plots_easier.1
# View(final.strata.that.need.new.plots_easier.1)

write_sf(final.strata.that.need.new.plots_easier.1, here("data/spatial_data/outputs/final_strata_for_plot_layout_2.shp"))


# # 
# # final.strata.that.need.new.plots_easier.1.grp = final.strata.that.need.new.plots_easier.1 %>%
# #   group_by(strata_nm) %>%
# #   mutate(area_ha_strata = sum(area_ha_strata))
# # View(final.strata.that.need.new.plots_easier.1.grp)
# # # write_sf(final.strata.that.need.new.plots_easier.1, here("data/spatial_data/outputs/final.strata.that.need.new.plots_easier_diss.shp"))
# # 
# # #create a table to join the details on plots needed per strata back
# # final.strata.that.need.new.plots_table = final.strata.that.need.new.plots %>%
# #   st_drop_geometry() %>%
# #   select(strata_nm, existing_plots, total.plots.needed, useful.plots, NEW.plots.needed)
# # 
# final.strata.that.need.new.plots_easier_table = final.strata.that.need.new.plots_easier.1 %>%
#   st_drop_geometry()
# # View(final.strata.that.need.new.plots_easier_table)
# 
# # #join the table to the reduced strata shapefile
# final.strata.that.need.new.plots_easier = final.strata.that.need.new.plots_easier.1  %>%
#   # st_collection_extract(type = c("POLYGON")) %>%
#   left_join(final.strata.that.need.new.plots_easier_table)
# # %>%
# #   group_by(strata_nm) %>%
# #   summarise(across(existing_plots:NEW.plots.needed,mean))
# final.strata.that.need.new.plots_easier$area_ha = st_area(final.strata.that.need.new.plots_easier)*0.0001
# 
# strat_only_needs = final.strata.that.need.new.plots_easier %>%
#   group_by(strata_nm) %>%
#   mutate(area)
# write_sf(final.strata.that.need.new.plots_easier, here("data/spatial_data/outputs/final.strata.that.need.new.plots_easier.shp"))
# 
# #create table version
# write.csv(final.strata.that.need.new.plots_easier_table, here("outputs/plot_needs_by_strata_2June2025_kls.csv"))
# 
# ##split by attributes to get individual shapefiles for the GRTS
# #select the column of the attribute table for the split
# unique <- unique(final.strata.that.need.new.plots_easier$strata_nm)
# 
# #create new polygons based on the unique column ID
# for (i in 1:length(unique)) {
#   tmp <- final.strata.that.need.new.plots_easier[final.strata.that.need.new.plots_easier$strata_nm == unique[i], ]
#   write_sf(tmp, here("data/spatial_data/ind_strata"), unique[i], driver="ESRI Shapefile",
#            overwrite_layer=TRUE)
# }
# 
# 
# ##this table made in "existing_plots_assessment.Rmd"
# groves_seki_summary = read.csv(here("outputs/groves_seki_summary.csv"))
# plots_sf$grove_name = plots_sf$grov_nm
# 
# #make grove summary plot, nonspatial
# groves_summary <- plots_sf %>% 
#   st_drop_geometry() %>% #remove polygon reference
#   summarize(.by = c(grove_name, project), plot_count = n()) %>% #count per type
#   pivot_wider(names_from = project, # make wider dataframe for viz
#               values_from = plot_count, 
#               values_fill = 0) %>% 
#   adorn_totals("col") %>% #calc total plots per grove
#   # adorn_totals() %>%  #calc total plots per type
#   left_join(groves_seki_summary) %>% # add grove info
#   mutate(plots_peracre_buf = round(Total/groveacres,2),
#          acres_perplot_buf = round(groveacres/Total, 0),
#          groveacres = round(groveacres,0),
#          total.existing.plots = Total) %>%
#   select(-c(2:7))
# # View(groves_summary)
# 
# hist(groves_summary$total.existing.plots)
# hist(groves_summary$acres_perplot_buf, breaks = 100)
# 
# ##rough ranking based on total plots and plot density
# groves_summary_rank = groves_summary %>%
#   mutate(total.plots.rank = 
#            case_when(total.existing.plots <10 ~ "very few plots",
#                      total.existing.plots >=10 & 
#                        total.existing.plots <30 ~ "moderate plots",
#                      total.existing.plots >=30 ~ "lotsa plots",
#                      .default = "oopsie"),
#          plot.density.rank = 
#            case_when(acres_perplot_buf <10 ~ "high density",
#                      acres_perplot_buf >=10 & 
#                        acres_perplot_buf <50 ~ "moderate density",
#                      acres_perplot_buf >=50 ~ "low density",
#                      .default = "oopsie"),
#          combo.rank = paste(total.plots.rank,"-",plot.density.rank))
# 
# # View(groves_summary_rank)  
# write.csv(groves_summary_rank,here("outputs/groves_summary_rank.csv"))