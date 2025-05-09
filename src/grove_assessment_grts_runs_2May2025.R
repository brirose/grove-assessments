library(tidyverse)
library(here)
library(sf)
library(terra)
library(janitor)
library(spsurvey)
library(mapedit)

#import access
access <- read.csv(here("data/grove_access.csv"))
plots_sf <- st_read(here("data/spatial_data/all_plots_groves.shp")) 
final.strata.shape = st_read(here("data/spatial_data/outputs/final.strata.that.need.new.plots_easier.shp"))

#import strata set needs
strata.needs1 = read.csv(here("data/plot_needs_by_strata_2May2025_kls.csv"))

strata.needs = strata.needs1 %>%
  select(strata_nm,NEW.plots.needed)
  
#setwd to load in the individual strata
setwd(here("data/spatial_data/ind_strata"))

#read in all files
shapes.dir <- (here("data/spatial_data/ind_strata"))
shapes.list <- list.files(shapes.dir, pattern="\\.shp$", full.names=FALSE)

#load in each shapefile and its necessary plots

for(i in 1:length(shapes.list)) {
 
  shape.is = paste(as.character(shapes.list[i]))
  plots.needed = strata.needs %>%
       filter(strata_nm == gsub('.{4}$', '', as.character(shape.is)))
  strat.shp = st_read(shapes.list[i]) 
  
    {
       set.seed(66625) # another draw
  
       draw_grts <- grts(strat.shp,
                  n_base = plots.needed$NEW.plots.needed, # number of 'base' sites 
                  n_over = (plots.needed$NEW.plots.needed)*3,          # nuber of 'oversample' sites
                  mindis = 100, # minimum distance between sites. Look at package details to see how it calculates distance unit
                  maxtry = 100 # sometimes, grts can't fit the draw on it's first attempt. 
            )
  
  #make separate object for base and oversample draws
  draw_grts_base <- draw_grts$sites_base
  draw_grts_over <- draw_grts$sites_over
  
  #add numeric field for site
    #Note: this was in David's original code, in the individual (ie not looped) grove runs,
    #not sure why we need it. if we do, can extract from siteID column

    # draw_grts_base$site_num <- as.numeric(paste("1:",plots.needed$NEW.plots.needed,sep = ''))
    # draw_grts_over$site_num <- paste(plots.needed$NEW.plots.needed+plots.needed$NEW.plots.needed,":",
    #                                  (plots.needed$NEW.plots.needed*10)+1, sep = '')
  draw_grts_base_and_over <- rbind(draw_grts_base, draw_grts_over)

  #write grts draw shapefile
  write_sf(draw_grts_base, here(paste("data/spatial_data/outputs/grts/grts_",gsub('.{4}$', '', as.character(shape.is)),"_BASE.shp",sep = '')))
  write_sf(draw_grts_over, here(paste("data/spatial_data/outputs/grts/grts_",gsub('.{4}$', '', as.character(shape.is)),"_OVER.shp",sep = '')))
  
  
  #write .csv of grts draw details, eg. lat/long, site number
  # setwd("C:/Users/dsoderberg/OneDrive - DOI/Desktop/Shive_SEGIpretreatment_grts/csv")
  write.csv(draw_grts_base_and_over, here(paste("data/spatial_data/outputs/grts/grts_base_and_over_",gsub('.{4}$', '', as.character(shape.is)),".csv",sep="")))
  
    }
}


##merge all bo and all over to one shapefile each
bo.dir <- (here("data/spatial_data/outputs/grts"))
bo.list <- list.files(bo.dir, pattern="\\.shp$", full.names=TRUE)
bo.shape_list <- lapply(bo.list, st_read)
bo <- mapedit:::combine_list_of_sf(bo.shape_list)

#reduce access file
access.min = access %>% select(grove,Ease.of.Access)

##add grove location information
grts.groves = bo %>%
  st_intersection(groves) %>%
  mutate(grove = grove_name) %>%
  left_join(access.min)

# write_sf(grts.groves,(here("data/spatial_data/outputs")),"grts_points_easier_groves_4May2025.shp",
#          driver="ESRI Shapefile", overwrite_layer=TRUE)

grts.groves.table = grts.groves %>%
  st_drop_geometry() %>%
  select(-c(replsite,stratum,wgt,ip,caty,grove,unit_name)) %>% 
  # left_join(strata.needs) %>%
  group_by(strata_nm,grove_name,siteuse) %>%
  summarise(num.grts.plots = length(strata_nm)) %>%
  st_drop_geometry() %>%
  mutate(grov_nm = grove_name, study_design = "grts") %>%
  # left_join(plots_sf_summary) %>%
  select(-grov_nm)
View(grts.groves.table)
# #write out csv
# write.csv(grts.groves,(here("outputs/grts_points_easier_groves_table_summary.csv")))

grts.groves = read.csv(here("outputs/grts_points_easier_groves_table_summary.csv"))


###########################################################
##explore grid approach

xgrid200 <- groves %>% 
  st_make_grid(cellsize = c(200,200), what = "centers") %>% # grid of points
  st_as_sf() %>%
  st_intersection(groves_easier) %>%
  st_intersection(final.strata.shape) %>%
  mutate(study.design = "grid")
head(xgrid200)
nrow(xgrid200)

write_sf(xgrid200,(here("data/spatial_data/outputs/grid200m_in_needed_strata_easier.shp")),
         driver="ESRI Shapefile", overwrite_layer=TRUE)

plots_sf_summary = plots_sf %>%
  group_by(grov_nm) %>%
  summarise(existing.plots = length(grov_nm)) %>%
  st_drop_geometry()

# grid200 = st_read(here("data/spatial_data/outputs/grid200m_points_easier_groves_4May2025_wstrata.shp"))
grid.plot.summary = xgrid200 %>%
  st_intersection(groves) %>%
  group_by(strata_nm,grove_name) %>%
  summarise(num.grid.plots = length(strata_nm)) %>%
  st_drop_geometry() %>%
  mutate(grov_nm = grove_name, study_design = "grid") #%>%
  # left_join(plots_sf_summary) %>%
  # select(-grov_nm)
View(grid.plot.summary)

write.csv(grid.plot.summary,(here("outputs/grid_points_easier_groves_table_summary.csv")))

