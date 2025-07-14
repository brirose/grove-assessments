library(tidyverse)
library(here)
library(sf)
library(terra)
library(janitor)
library(spsurvey)
# library(mapedit)

#import access
groves <- st_read(here("data/spatial_data/SEKI_groves/SEKI_groves_list.shp"))
access <- read.csv(here("data/grove_access.csv"))
plots_sf <- st_read(here("data/spatial_data/all_plots_groves.shp")) %>%
  st_transform(crs(groves))
final.strata.shape = st_read(here("data/spatial_data/outputs/final_strata_need_plots_6June2025.shp")) %>%
  st_transform(crs(groves)) %>%
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  filter(ar_h_s_>0)

slope = rast(here("data/spatial_data/So_Sierra_10mDEM_slopePercent.tif")) %>%
  project(crs(groves))
# 
# #classify slope
# reclass_matrix <- tibble(
#     from = c(0, 50),
#     to = c(50, 1295),
#     becomes = c(0, 50)
#   )
# 
# ##first make classified rasters
# slp = slope %>%
#   classify(reclass_matrix,
#            right = T) # include "from" value in category
# # writeRaster(slp, here(paste("data/spatial_data/So_Sierra_10mDEM_slopePercent_50class.tif")), overwrite = T)
# 
# slp.cls.p <- slp %>%
#   as.polygons() %>%
#   st_as_sf() %>%
#   mutate(slp.perc = So_Sierra_10mDEM_slopePercent,
#          slp.perc.cls = case_when(slp.perc == 0 ~ "Under 50% slope",
#                                   slp.perc == 50 ~ "Over 50% slope",
#                                   .default = "oopsy")
#   ) %>%
#   st_transform(crs(groves))
# 
# slp.cls.p2 = st_intersection(slp.cls.p,groves)
# # plot(slp.cls.p2)
# 
# write_sf(slp.cls.p2, here(paste("data/spatial_data/slope_perc_polys_clp.shp")))
# slp = slp.cls.p2
slp = read_sf(here(paste("data/spatial_data/slope_perc_polys_clp.shp")))
slp.0 = slp %>%
  filter(slp_prc == 0)

# #import strata set needs
# strata.needs1 = read.csv(here("outputs/.csv"))
# head(strata.needs1)
# 
# strata.needs = strata.needs1 %>%
#   mutate(to_est = replace_na(to_est, 0)) %>%
#   select(strata_nm,to_est)

plot.grv.strt.needs = read.csv(here("outputs/final_num_plots_needed_by_strata_grv_from_Bri_09Jun.csv")) %>%
    mutate(to_est = replace_na(to_est, 0)) %>%
    select(strata_nm,grov_nm, to_est)
sum(plot.grv.strt.needs$to_est)


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

xgrid200.a <- groves_easier %>%
  st_make_grid(cellsize = c(200,200), what = "centers") %>% # grid of points
  st_as_sf() %>%
  st_intersection(groves_easier) %>%
  select(grov_nm)
head(xgrid200.a)

xgrid200 = xgrid200.a %>%
  st_intersection(final.strata.shape) %>%
  select(grov_nm,strt_nm) %>%
  mutate(strt_grv = paste(grov_nm,strt_nm,sep="-"))
# View(xgrid200)
head(xgrid200)
nrow(xgrid200)

xgrid200.s = xgrid200 %>%
  st_intersection(slp.0)
head(xgrid200.s)
nrow(xgrid200.s)

#check number available by strata and grove
p.need = plot.grv.strt.needs %>%
  # filter(to_est>0) %>%
  mutate(strt_grv = paste(grov_nm, strata_nm,sep="-")) %>%
  select(strt_grv, to_est)
sum(p.need$to_est)

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

# write_sf(xgrid200,(here("data/spatial_data/outputs/grid200m_points_easier_groves_4May2025_wstrata.shp")),
#          driver="ESRI Shapefile", overwrite_layer=TRUE)
# grid200 = st_read(here("data/spatial_data/outputs/grid200m_points_easier_groves_4May2025_wstrata.shp"))

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
# View(plot.grv.strt.needs.redu)
sum(plot.grv.strt.needs.redu$to_est)

good.pts <- 
  st_difference(xgrid200, st_union(plots_sf_buff100)) %>%
  mutate(point_id = ave(grov_nm, strt_nm, FUN = seq_along)) %>%
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  select(strt_grv, point_id) %>%
  left_join(plot.grv.strt.needs.redu) %>%
  filter(!is.na(to_est))
nrow(good.pts)

good.pts.slp = good.pts %>%
  st_intersection(slp.0) %>%
  select(-S_S_10D, -slp_prc,-slp_pr_,-grov_nm,-grovr_h)
nrow(good.pts.slp)

# write_sf(good.pts,(here("data/spatial_data/outputs/good_points_09June2025.shp")),
#          driver="ESRI Shapefile", overwrite_layer=TRUE)

###check again for plots not avail
#check number available by strata and grove
p.need = plot.grv.strt.needs %>%
  # filter(to_est>0) %>%
  mutate(strt_grv = paste(grov_nm, strata_nm,sep="-")) %>%
  select(strt_grv, to_est)
sum(p.need$to_est)

num.avail = good.pts %>%
  group_by(strt_grv) %>%
  summarise(poss.plots = length(strt_grv)) %>%
  left_join(p.need) %>%
  filter(to_est>0) %>%
  mutate(unavail.plots = to_est-poss.plots) %>%
  filter(unavail.plots>0)
num.avail
sum(num.avail$unavail.plots)
# 
# # 
# good.pts_table = st_drop_geometry(good.pts)
# 
# # View(good.pts)
# nrow(good.pts)


# #in the needs list form Bri but not in the 200 grid with buffer exclusion - 17
strata.not.in.good.pts = as.data.frame(setdiff(plot.grv.strt.needs.redu$strt_grv,good.pts.slp$strt_grv))
strata.not.in.good.pts = strata.not.in.good.pts %>% mutate(strt_grv = setdiff(plot.grv.strt.needs.redu$strt_grv, good.pts.slp$strt_grv)) %>%
  select(strt_grv)

#create list to select plots
strt_grv_list = unique(good.pts.slp$strt_grv)
datalist = list()

#set up for loop to get needed plot number for each strata from 200m grid
for(i in 1:length(strt_grv_list)) {
  #set up to get number of plots needed
  plt.need = plot.grv.strt.needs.redu %>%
    filter(strt_grv == as.character(paste(strt_grv_list[i])))
  plts.est = plt.need$to_est
  #pull the subset (may be fewer than called for if tehre weren't enough in the subset)
  each.strt.grv = good.pts.slp %>%
    # st_drop_geometry() %>%
    filter(strt_grv == as.character(paste(strt_grv_list[i]))) %>%
    slice_sample(n = plts.est)

  datalist[[i]] <- each.strt.grv # add it to your list
  
}

selected.plots = do.call(rbind, datalist)
nrow(selected.plots)

sp.slp =  slope %>%
  extract(vect(selected.plots))
# View(sp.slp)
max(sp.slp$So_Sierra_10mDEM_slopePercent)

# write_sf(selected.plots, here("data/spatial_data/outputs/selected_plots_164_11July2025_slp50.shp"))

#################################################################
#################################################################
#get plots using smaller grid for missed 

#first get strata and number of plots in current selection
summary.new = selected.plots %>%
  st_drop_geometry() %>%
  # right_join(plot.grv.strt.needs.redu) %>%
  group_by(strt_grv) %>%
  summarise(new.plts.selected = length(strt_grv))

#Then see what strata are totally left out - 23
strata.not.in.200 = as.data.frame(setdiff(plot.grv.strt.needs.redu$strt_grv,summary.new$strt_grv))
new.plots.all.strata = strata.not.in.200a %>% mutate(strt_grv = setdiff(plot.grv.strt.needs.redu$strt_grv,summary.new$strt_grv)) %>%
  select(strt_grv) %>%
  mutate(new.plts.selected = 0) %>%
  rbind(summary.new)
new.plots.all.strata 

#then see how many you need
missed.plots = new.plots.all.strata %>%
  inner_join(plot.grv.strt.needs.redu, by = "strt_grv") %>%
  mutate(still.needed = abs(new.plts.selected-to_est)) %>%
  filter(still.needed>0)
missed.plots
sum(missed.plots$still.needed)


#create 50m grid
xgrid50 <- groves_easier %>%
  st_make_grid(cellsize = c(50,50), what = "centers") %>% # grid of points
  st_as_sf() %>%
  st_intersection(groves_easier) %>%
  select(grov_nm) %>%
  st_intersection(final.strata.shape) %>%
  select(grov_nm,strt_nm) %>%
  mutate(strt_grv = paste(grov_nm,strt_nm,sep="-")) %>%
  st_intersection(slp)
# View(xgrid200)
head(xgrid50)

# write_sf(xgrid50, here("data/spatial_data/xgrid50_base_grid.shp"))

good.pts50 <- 
  # xgrid200 %>%
  st_difference(xgrid50, st_union(plots_sf_buff100)) %>%
  mutate(point_id = ave(grov_nm, strt_nm, FUN = seq_along)) %>%
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  select(strt_grv, point_id) %>%
  left_join(missed.plts) %>%
  filter(!is.na(still.needed))
# View(good.pts50)
nrow(good.pts50)

good.pts50_slp = good.pts50 %>%
  st_intersection(slp.0) %>%
  select(-S_S_10D, -slp_prc,-slp_pr_,-grov_nm,-grovr_h)
nrow(good.pts50_slp)

sp50.slp =  slope %>%
  extract(vect(good.pts50_slp)) 
max(sp50.slp$So_Sierra_10mDEM_slopePercent)
# 

strt_grv_list50 = unique(good.pts50_slp$strt_grv)
datalist50 = list()

#set up for loop to get needed plot number for each strata
for(i in 1:length(strt_grv_list50)) {
  #set up to get number of plots needed
  plt.need50 = missed.plots %>%
    filter(strt_grv == as.character(paste(strt_grv_list50[i])))
  plts.est50 = plt.need50$still.needed
  #pull the subset (may be fewer than called for if tehre weren't enough in the subset)
  each.strt.grv50 = good.pts50_slp %>%
    # st_drop_geometry() %>%
    filter(strt_grv == as.character(paste(strt_grv_list50[i]))) %>%
    slice_sample(n = plts.est50)
  
  datalist50[[i]] <- each.strt.grv50 # add it to your list
  
}

selected.plots50 = do.call(rbind, datalist50)
nrow(selected.plots50)
# View(selected.plots50)

sp50.slp =  slope %>%
  extract(vect(selected.plots50))
max(sp50.slp$So_Sierra_10mDEM_slopePercent)

#merge the main with the 50
most2025.plots = selected.plots %>%
  bind_rows(selected.plots50)
most2025.plots
nrow(most2025.plots)

most2025.plots.slp =  slope %>%
  extract(vect(most2025.plots))
max(most2025.plots.slp$So_Sierra_10mDEM_slopePercent)

##strata plots STILL missing
#check that there are any plots missing for strata that were included 
#first get strata and number of plots in current selection
summary.new50 = most2025.plots %>%
  st_drop_geometry() %>%
  # right_join(plot.grv.strt.needs.redu) %>%
  group_by(strt_grv) %>%
  summarise(new.plts.selected = length(strt_grv))
summary.new50
#get 184 plots selected (now need 13)
sum(summary.new50$new.plts.selected)

#Then see what strata are totally left out - 7
strata.not.in.200.50a = as.data.frame(setdiff(plot.grv.strt.needs.redu$strt_grv,summary.new50$strt_grv))
new.plots.all.strata50 = strata.not.in.200.50a %>% mutate(strt_grv = setdiff(plot.grv.strt.needs.redu$strt_grv,summary.new50$strt_grv)) %>%
  select(strt_grv) %>%
  mutate(new.plts.selected = 0) %>%
  rbind(summary.new50)
new.plots.all.strata50
sum(new.plots.all.strata50$new.plts.selected)

#then see how many you need
missed.plots50 = new.plots.all.strata50 %>%
  inner_join(plot.grv.strt.needs.redu, by = "strt_grv") %>%
  mutate(still.needed = abs(new.plts.selected-to_est)) %>%
  filter(still.needed>0)
missed.plots50
sum(missed.plots50$still.needed)

##now get a grid of 20m for picking the last 3
#create 50m grid
xgrid20 <- groves_easier %>%
  st_make_grid(cellsize = c(20,20), what = "centers") %>% # grid of points
  st_as_sf() %>%
  st_intersection(groves_easier) %>%
  select(grov_nm) %>%
  st_intersection(final.strata.shape) %>%
  select(grov_nm,strt_nm) %>%
  mutate(strt_grv = paste(grov_nm,strt_nm,sep="-")) %>%
  st_intersection(slp)
# View(xgrid200)
head(xgrid20)
nrow(xgrid20)

#remove points within 100m of pre-existing plots
good.pts20 <- 
  # xgrid200 %>%
  st_difference(xgrid20, st_union(plots_sf_buff100)) %>%
  mutate(point_id = ave(grov_nm, strt_nm, FUN = seq_along)) %>%
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  select(strt_grv, point_id) %>%
  left_join(missed.plots50) %>%
  filter(!is.na(still.needed))
# View(good.pts20)
nrow(good.pts20)

#also remove those within 100m of this years new plots assigned so far
new.plots_sf_buff100 = st_buffer(most2025.plots,100)

good.pts20.b <- 
  # xgrid200 %>%
  st_difference(good.pts20, st_union(new.plots_sf_buff100)) %>%
  mutate(point_id = ave(strt_grv, FUN = seq_along)) %>%
  # mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  select(strt_grv, point_id) %>%
  left_join(missed.plots50) %>%
  filter(!is.na(still.needed))
# View(good.pts20)
nrow(good.pts20.b)

good.pts20_slp = good.pts20.b %>%
  st_intersection(slp.0)
nrow(good.pts20_slp)

strt_grv_list20 = unique(good.pts20_slp$strt_grv)
datalist20 = list()

#set up for loop to get needed plot number for each strata
for(i in 1:length(strt_grv_list20)) {
  #set up to get number of plots needed
  plt.need20 = missed.plots50 %>%
    filter(strt_grv == as.character(paste(strt_grv_list20[i])))
  plts.est20 = plt.need20$still.needed
  #pull the subset (may be fewer than called for if tehre weren't enough in the subset)
  each.strt.grv20 = good.pts20_slp %>%
    # st_drop_geometry() %>%
    filter(strt_grv == as.character(paste(strt_grv_list20[i]))) %>%
    slice_sample(n = plts.est20)
  
  datalist20[[i]] <- each.strt.grv20 # add it to your list
  
}

selected.plots20 = do.call(rbind, datalist20)
nrow(selected.plots20)
# View(selected.plots20)

all2025.plots = most2025.plots %>%
  bind_rows(selected.plots20) %>%
  select(strt_grv, point_id)
all2025.plots
nrow(all2025.plots)

all2025.plots.slp =  slope %>%
  extract(vect(all2025.plots))
View(all2025.plots.slp)

write_sf(all2025.plots, here("outputs/selected_grid_plots_2025sampling_14July2025_slope50.shp"), overwrite = T)

all2025.plots_table = st_drop_geometry(all2025.plots)

write.csv(all2025.plots_table,(here("outputs/selected_grid_plots_2025sampling_14July2025_slope50.csv")))

