library(tidyverse)
library(here)
library(sf)
library(terra)
library(janitor)
library(spsurvey)
library(mapedit)

#import access
groves <- st_read(here("data/spatial_data/SEKI_groves/SEKI_groves_list.shp"))
access <- read.csv(here("data/grove_access.csv"))
plots_sf <- st_read(here("data/spatial_data/all_plots_groves.shp")) %>%
  st_transform(crs(groves))
final.strata.shape = st_read(here("data/spatial_data/outputs/final_strata_need_plots_6June2025.shp")) %>%
  st_transform(crs(groves))

#import strata set needs
strata.needs1 = read.csv(here("outputs/plot_needs_by_strata_2June2025_kls.csv"))
head(strata.needs1)

strata.needs = strata.needs1 %>%
  select(strata_nm,NEW.plots.needed)

plot.grv.strt.needs = read.csv(here("outputs/final_num_plots_needed_by_strata_grv_from_Bri_06Jun.csv"))

##this is the grts run which we are not using
#setwd to load in the individual strata
# setwd(here("data/spatial_data/ind_strata"))
# 
# #read in all files
# shapes.dir <- (here("data/spatial_data/ind_strata"))
# shapes.list <- list.files(shapes.dir, pattern="\\.shp$", full.names=FALSE)
# 
# for(i in 1:length(shapes.list)) {
#  
#   shape.is = paste(as.character(shapes.list[i]))
#   plots.needed = strata.needs %>%
#        filter(strata_nm == gsub('.{4}$', '', as.character(shape.is)))
#   strat.shp = st_read(shapes.list[i]) 
#   
#     {
#        set.seed(66625) # another draw
#   
#        draw_grts <- grts(strat.shp,
#                   n_base = plots.needed$NEW.plots.needed, # number of 'base' sites 
#                   n_over = (plots.needed$NEW.plots.needed)*3,          # nuber of 'oversample' sites
#                   mindis = 100, # minimum distance between sites. Look at package details to see how it calculates distance unit
#                   maxtry = 100 # sometimes, grts can't fit the draw on it's first attempt. 
#             )
#   
#   #make separate object for base and oversample draws
#   draw_grts_base <- draw_grts$sites_base
#   draw_grts_over <- draw_grts$sites_over
#   
#   #add numeric field for site
#     #Note: this was in David's original code, in the individual (ie not looped) grove runs,
#     #not sure why we need it. if we do, can extract from siteID column
# 
#     # draw_grts_base$site_num <- as.numeric(paste("1:",plots.needed$NEW.plots.needed,sep = ''))
#     # draw_grts_over$site_num <- paste(plots.needed$NEW.plots.needed+plots.needed$NEW.plots.needed,":",
#     #                                  (plots.needed$NEW.plots.needed*10)+1, sep = '')
#   draw_grts_base_and_over <- rbind(draw_grts_base, draw_grts_over)
# 
#   #write grts draw shapefile
#   write_sf(draw_grts_base, here(paste("data/spatial_data/outputs/grts/grts_",gsub('.{4}$', '', as.character(shape.is)),"_BASE.shp",sep = '')))
#   write_sf(draw_grts_over, here(paste("data/spatial_data/outputs/grts/grts_",gsub('.{4}$', '', as.character(shape.is)),"_OVER.shp",sep = '')))
#   
#   
#   #write .csv of grts draw details, eg. lat/long, site number
#   # setwd("C:/Users/dsoderberg/OneDrive - DOI/Desktop/Shive_SEGIpretreatment_grts/csv")
#   write.csv(draw_grts_base_and_over, here(paste("data/spatial_data/outputs/grts/grts_base_and_over_",gsub('.{4}$', '', as.character(shape.is)),".csv",sep="")))
#   
#     }
# }
# 
# 
# ##merge all bo and all over to one shapefile each
# bo.dir <- (here("data/spatial_data/outputs/grts"))
# bo.list <- list.files(bo.dir, pattern="\\.shp$", full.names=TRUE)
# bo.shape_list <- lapply(bo.list, st_read)
# bo <- mapedit:::combine_list_of_sf(bo.shape_list)
# 
# #reduce access file
# access.min = access %>% select(grove,Ease.of.Access)
# 
# ##add grove location information
# grts.groves = bo %>%
#   st_intersection(groves) %>%
#   mutate(grove = grove_name) %>%
#   left_join(access.min)
# 
# # write_sf(grts.groves,(here("data/spatial_data/outputs")),"grts_points_easier_groves_4May2025.shp",
# #          driver="ESRI Shapefile", overwrite_layer=TRUE)
# 
# grts.groves.table = grts.groves %>%
#   st_drop_geometry() %>%
#   select(-c(replsite,stratum,wgt,ip,caty,grove,unit_name)) %>% 
#   # left_join(strata.needs) %>%
#   group_by(strata_nm,grove_name,siteuse) %>%
#   summarise(num.grts.plots = length(strata_nm)) %>%
#   st_drop_geometry() %>%
#   mutate(grov_nm = grove_name, study_design = "grts") %>%
#   # left_join(plots_sf_summary) %>%
#   select(-grov_nm)
# View(grts.groves.table)
# # #write out csv
# # write.csv(grts.groves,(here("outputs/grts_points_easier_groves_table_summary.csv")))
# 
# grts.groves = read.csv(here("outputs/grts_points_easier_groves_table_summary.csv"))


###########################################################
##explore grid approach
##Create new shapefile that removes "Very Difficult" access groves, and dissolve into one big shapefile
access.min = access %>% 
  mutate(grov_nm = grove) %>%
  select(grov_nm,Ease.of.Access)

#get easier groves and dissolve to one multipolygon
groves_easier = groves %>% 
  # mutate(grove = grove_nm) %>%
  left_join(access.min) %>%
  filter(Ease.of.Access != "Very Difficult")
# %>%
#   # st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
#   st_union() %>% # unite to a geometry object
#   st_sf() 

xgrid200.a <- groves_easier %>%
  st_make_grid(cellsize = c(200,200), what = "centers") %>% # grid of points
  st_as_sf() %>%
  st_intersection(groves_easier) %>%
  select(grov_nm)
head(xgrid200.a)

xgrid200 = xgrid200.a %>%
  st_intersection(final.strata.shape) %>%
  select(grov_nm,strt_nm)
# View(xgrid200)
nrow(xgrid200)

#check number available by strata and grove
p.need = plot.grv.strt.needs %>%
  # filter(to_est>0) %>%
  mutate(strt_grv = paste(grov_nm, strata_nm,sep="-")) %>%
  select(strt_grv, to_est)

num.avail = xgrid200 %>%
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  group_by(strt_grv) %>%
  summarise(poss.plots = length(strt_grv)) %>%
  left_join(p.need) %>%
  filter(to_est>0) %>%
  mutate(unavail.plots = to_est-poss.plots) %>%
  filter(unavail.plots>0)
num.avail
sum(num.avail$unavail.plots)

write_sf(xgrid200,(here("data/spatial_data/outputs/grid200m_points_easier_groves_4May2025_wstrata.shp")),
         driver="ESRI Shapefile", overwrite_layer=TRUE)
grid200 = st_read(here("data/spatial_data/outputs/grid200m_points_easier_groves_4May2025_wstrata.shp"))

# xgrid200 = st_read(here("data/spatial_data/outputs/grid200m_in_needed_strata_easier.shp"))

# plots_sf_summary = plots_sf %>%
#   group_by(grov_nm) %>%
#   summarise(existing.plots = length(grov_nm)) %>%
#   st_drop_geometry()
# 
# grid.plot.summary = xgrid200 %>%
#   st_intersection(groves) %>%
#   group_by(strt_nm,grov_nm) %>%
#   summarise(num.grid.plots = length(strt_nm)) %>%
#   # st_drop_geometry() %>%
#   mutate(study_design = "grid") #%>%
#   # left_join(plots_sf_summary) %>%
#   # select(-grov_nm)
# View(grid.plot.summary)

##create buffer around existing plots
plots_sf_buff100 = st_buffer(plots_sf,100)

#erase grid points that are in here
plot.grv.strt.needs.redu = plot.grv.strt.needs %>%
  filter(to_est>0) %>%
  mutate(strt_grv = paste(grov_nm, strata_nm,sep="-")) %>%
  select(strt_grv, to_est)
plot.grv.strt.needs.redu
sum(plot.grv.strt.needs.redu$to_est)

good.pts <- st_difference(xgrid200, st_union(plots_sf_buff100)) %>%
  mutate(point_id = ave(grov_nm, strt_nm, FUN = seq_along)) %>%
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  select(strt_grv, point_id) %>%
  left_join(plot.grv.strt.needs.redu) %>%
  filter(!is.na(to_est))
nrow(good.pts)

###check again for plots not avail
#check number available by strata and grove
p.need = plot.grv.strt.needs %>%
  # filter(to_est>0) %>%
  mutate(strt_grv = paste(grov_nm, strata_nm,sep="-")) %>%
  select(strt_grv, to_est)

num.avail = good.pts %>%
  group_by(strt_grv) %>%
  summarise(poss.plots = length(strt_grv)) %>%
  left_join(p.need) %>%
  filter(to_est>0) %>%
  mutate(unavail.plots = to_est-poss.plots) %>%
  filter(unavail.plots>0)
num.avail
sum(num.avail$unavail.plots)

# write_sf(good.pts,(here("data/spatial_data/outputs/good_points.shp")),
#          driver="ESRI Shapefile", overwrite_layer=TRUE)
# 
good.pts_table = st_drop_geometry(good.pts)

# View(good.pts)
nrow(good.pts)
#remove from here
plots.avail <- good.pts_table %>% 
  summarise(.by = strt_grv, avail = n()) 

final.strata.list <- final.strata.shape %>% 
  st_drop_geometry() %>% 
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>% 
  select(strt_grv) %>% 
  summarise(.by = strt_grv)

nrow(plot.grv.strt.needs.redu)
nrow(plots.wanted.have)

xgridStrata <- xgrid200 %>% 
  st_drop_geometry() %>% 
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  summarize(.by = strt_grv, possible=n())

#show where buffer or grid eliminates all plots
plots.byStrata <- final.strata.list %>% 
  left_join(xgridStrata) %>% 
  left_join(plots.avail) %>% 
  left_join(plot.grv.strt.needs.redu) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(unavail_buf = avail-to_est,
         unavail_grid = possible - to_est,
         plots_elim = case_when(possible>avail&unavail_buf< 0 ~T, T ~F), #currently catches unavail grid too
         smaller_grid = case_when(unavail_grid>= 0 ~F, T ~T)
  )

  

#check where assigned plots are in silvers or removed
setdiff(plot.grv.strt.needs.redu$strt_grv, final.strata.list$strt_grv)
sum(xgridStrata$to_est)
sum(plot.grv.strt.needs.redu$to_est)
  
#tohere

strt_grv_list = unique(good.pts$strt_grv)
datalist = list()

#set up for loop to get needed plot number for each strata
for(i in 1:length(strt_grv_list)) {
  #set up to get number of plots needed
  plt.need = plot.grv.strt.needs.redu %>%
    filter(strt_grv == as.character(paste(strt_grv_list[i])))
  #pull the subset (may be fewer than called for if tehre weren't enough in the subset)
  each.strt.grv = good.pts %>%
    st_drop_geometry() %>%
    filter(strt_grv == as.character(paste(strt_grv_list[i]))) %>%
    slice_sample(n = plt.need$to_est)

  datalist[[i]] <- each.strt.grv # add it to your list
  
}

selected.plots = do.call(rbind, datalist)
nrow(selected.plots)
# View(selected.plots)


write.csv(grid.plot.summary,(here("outputs/grid_points_easier_groves_table_summary.csv")))

