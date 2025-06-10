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
sum(good.pts$to_est)

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

  
#tohere

# #in the needs list form Bri but not in the 200 grid with buffer exclusion - 17
strata.not.in.good.pts = as.data.frame(setdiff(plot.grv.strt.needs.redu$strt_grv,good.pts$strt_grv))
strata.not.in.good.pts = strata.not.in.good.pts %>% mutate(strt_grv = setdiff(plot.grv.strt.needs.redu$strt_grv, good.pts$strt_grv)) %>%
  select(strt_grv)

#create list to select plots
strt_grv_list = unique(good.pts$strt_grv)
datalist = list()

#set up for loop to get needed plot number for each strata from 200m grid
for(i in 1:length(strt_grv_list)) {
  #set up to get number of plots needed
  plt.need = plot.grv.strt.needs.redu %>%
    filter(strt_grv == as.character(paste(strt_grv_list[i])))
  plts.est = plt.need$to_est
  #pull the subset (may be fewer than called for if tehre weren't enough in the subset)
  each.strt.grv = good.pts %>%
    # st_drop_geometry() %>%
    filter(strt_grv == as.character(paste(strt_grv_list[i]))) %>%
    slice_sample(n = plts.est)

  datalist[[i]] <- each.strt.grv # add it to your list
  
}

selected.plots = do.call(rbind, datalist)
nrow(selected.plots)

# write_sf(selected.plots, here("data/spatial_data/outputs/selected_plots_176_"))

#################################################################
#################################################################
#get plots using smaller grid for missed 

#plots per strata missing 
sp = selected.plots %>%
  group_by(strt_grv) %>%
  summarise(new.plts.selected = length(strt_grv)) %>%
  right_join(plot.grv.strt.needs.redu) %>%
  mutate(still.needed = abs(new.plts.selected-to_est)) %>%
  filter(still.needed > 0)
sp
sum(sp$new.plts.selected)

unique(plot.grv.strt.needs.redu$strt_grv)
setdiff(plot.grv.strt.needs.redu$strt_grv,selected.plots$strt_grv)
# View(selected.plots)
setdiff(selected.plots$strt_grv, final.strata.list$strt_grv)

#bind back needed plots from Bri's list to those not in grid
missed.plts = strata.not.in.good.pts %>%
  left_join(plot.grv.strt.needs.redu) %>%
  mutate(still.needed = to_est) %>%
  bind_rows(sp) %>%
  select(strt_grv, still.needed)
missed.plts
sum(missed.plts$still.needed)

#create 50m grid
xgrid50 <- groves_easier %>%
  st_make_grid(cellsize = c(50,50), what = "centers") %>% # grid of points
  st_as_sf() %>%
  st_intersection(groves_easier) %>%
  select(grov_nm) %>%
  st_intersection(final.strata.shape) %>%
  select(grov_nm,strt_nm) %>%
  mutate(strt_grv = paste(grov_nm,strt_nm,sep="-"))
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

#also remove plots within 20m of the edge of the strata they are in
redu.strata.shape50 = final.strata.shape %>%
  left_join(missed.plts) %>%
  filter(!is.na(still.needed)) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),2)) %>%
  select(strt_grv,still.needed) %>%
  st_buffer(-13)
# View(redu.strata.shape50)


good.pts50_buff = good.pts50 %>%
  st_intersection(redu.strata.shape50) %>%
  mutate(to_est = still.needed) %>%
  select(strt_grv, point_id, to_est)
nrow(good.pts50_buff)
# View(good.pts50_buff)
# 
# write_sf(good.pts50_buff,(here("data/spatial_data/outputs/good_points50_10June2025_w13m_buff.shp")),
#          driver="ESRI Shapefile", overwrite_layer=TRUE)

strt_grv_list50 = unique(good.pts50_buff$strt_grv)
datalist50 = list()

#set up for loop to get needed plot number for each strata
for(i in 1:length(strt_grv_list50)) {
  #set up to get number of plots needed
  plt.need50 = missed.plts %>%
    filter(strt_grv == as.character(paste(strt_grv_list50[i])))
  plts.est50 = plt.need50$still.needed
  #pull the subset (may be fewer than called for if tehre weren't enough in the subset)
  each.strt.grv50 = good.pts50_buff %>%
    # st_drop_geometry() %>%
    filter(strt_grv == as.character(paste(strt_grv_list50[i]))) %>%
    slice_sample(n = plts.est50)
  
  datalist50[[i]] <- each.strt.grv50 # add it to your list
  
}

selected.plots50 = do.call(rbind, datalist50)
nrow(selected.plots50)
# View(selected.plots50)

#merge the main with the 50
all2025.plots = selected.plots %>%
  bind_rows(selected.plots50)
all2025.plots
nrow(all2025.plots)

write_sf(all2025.plots, here("outputs/selected_grid_plots_2025sampling_10June2025.shp"), overwrite = T)

all2025.plots_table = st_drop_geometry(all2025.plots)

write.csv(all2025.plots_table,(here("outputs/selected_grid_plots_2025sampling_10June2025.csv")))


























##find the strata and groves that were not placed
#remove from here
good.pts_table = st_drop_geometry(good.pts)

plots.avail <- good.pts_table %>%
  summarise(.by = strt_grv, avail = n())

final.strata.list <- good.pts %>%
  st_drop_geometry() %>%
  # mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  select(strt_grv) %>%
  summarise(.by = strt_grv)

xgridStrata <- xgrid200 %>%
  st_drop_geometry() %>%
  mutate(strt_grv = paste(grov_nm, strt_nm,sep="-")) %>%
  summarize(.by = strt_grv, possible=n())

#show where buffer or grid eliminates all plots
plots.byStrata <- 
  # strt_grv_list %>%
  # xgridStrata %>%
  final.strata.list %>%
  # left_join(xgridStrata) %>%
  left_join(plots.avail) %>%
  left_join(plot.grv.strt.needs.redu) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(diff = possible-avail)
plots.byStrata

%>%
  mutate(unavail_buf = avail-to_est,
         unavail_grid = possible - to_est,
         plots_elim = case_when(possible>avail&unavail_buf< 0 ~T, T ~F), #currently catches unavail grid too
         smaller_grid = case_when(unavail_grid>= 0 ~F, T ~T)
  ) 
# View(plots.byStrata)

plots.manual.assign = plots.byStrata %>%
  filter(smaller_grid==TRUE)
nrow(plots.manual.assign)

#check where assigned plots are in silvers
setdiff(plot.grv.strt.needs.redu$strt_grv, final.strata.list$strt_grv)
sum(xgridStrata$to_est)
sum(plot.grv.strt.needs.redu$to_est)

