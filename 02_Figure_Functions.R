# 03_Ch1_Figure_Functions
# Figures for Ch. 1 manuscript

#Explanation:
# --Load from local disk if you have previously loaded from server and saved an .RData of layers.
# --Load from the server and save the workspace if you do not have a local RData of the layers saved.
# -- If you want to update an existing .RData file of layers, simply delete or rename the old one.
# -- Save workspace option provided in case of not wanting to overwrite existing .RData file



# setup -------------------------------------------------------------------

res_dpi = 800
figure_units = "metric"
cfs_to_TAF_per_day = 1/43560 * 60*60*24 / 1000 # 1cfs / sq ft_per_acre * seconds_per_day / 1000
cfs_to_m3sec = 1 / 35.3147 # 1 cfs / cubic ft per cubic m
cfs_to_m3day = 1 / 35.3147 * 60*60*24 # 1 cfs / cubic ft per cubic m * seconds per day
m3_to_TAF = 1 * 35.5 * 1/43560 * 1/1000 # 1 m3 * ft^3/m^3 * acre/ft^2 * TAF/AF

month_1s = c(1,32,60,91,121,152,182,213,244,274,305,335) # non-leap year Julian day
month_1_labs = paste(month.abb[1:12], "1")
water_year_months = c(10:12, 1:9)

# Set colors for all figures

color_qvir = "mediumorchid"
color_state_buffer = "gray"
color_klamath = "khaki"
color_confluence = "deepskyblue"
color_states = "darkolivegreen3"
color_county = "gray40"
color_watershed = "black"
color_watershed_fill = "dodgerblue"
color_interstate = "red"
color_state_road = "brown"
color_river = "blue"
color_cities= "#d7922d"
# color_basin = "black"
color_tribs = "dodgerblue1"
color_gauges = "yellow1"
color_rst = "green3"
color_fcf = "red2"
fall_col = "darkgoldenrod"
spring_col = "green4"
wet_sn_col = "royalblue3"
dry_sn_col = "orangered"
peak_col = "black"
lam_color = "brown"

season_color_tab = data.frame(season = c("fall", "wet", "spring", "dry"),
                              abbrev = c("f", "w","s","d"),
                              color = c("darkgoldenrod", "royalblue3", "green4", "orangered"))

# subset data -------------------------------------------------------------

# Isolate river
riv = named_streams[named_streams$gnis_name == "Scott River", ]

# Isolate Scott confluence with Klamath (northernmost vertex of northernmost reach)
rchs = sort(unique(riv$reachcode))
rch1 = riv[riv$reachcode == rchs[length(rchs)],]
rch1_coords = st_coordinates(rch1)
conf_x = rch1_coords[nrow(rch1_coords), "X"]
conf_y = rch1_coords[nrow(rch1_coords), "Y"]
conf_coords = data.frame(x = conf_x, y = conf_y, name = "Scott-Klamath \n Confluence")
scott_klamath_confluence = sf::st_as_sf(x = conf_coords, coords = c("x","y"), crs = crs(riv))

# isolate main roads for setting figure
rt3 = roads_all[roads_all$FULLNAME %in% "State Rte 3", ]
i5 = roads_all[roads_all$FULLNAME == "I- 5",]

#Hillshade black-white palette
hill_wsh = terra::rast(file.path(data_dir,"hillshade_cropped_raster.tif"))
# hill_wsh = raster::raster(file.path(data_dir,"hillshade_cropped_raster.tif"))
hillshade_palette_faded = colorRampPalette(c("gray30", "white")) # This washed-out hillshade palette allows legends to be plotted on top

# scott tribal areas
tribal_all = tribal
tribal = tribal_all[watershed,]
qvir = st_intersection(tribal, watershed) #exclude trust land outside watershed

# Reduce number of cities. Calculate centroids for plotting and assign label positions
cities_all = cities # store copy of all siskiyou county cities
cities = cities[cities$NAME %in% c("Etna", "Fort Jones","Greenview"), ] #Greenview is also in the database but they are a census-designated place, not a municipality
# Transform to WGS84 and extract coordinates
cities=sf::st_transform(x=cities, crs=st_crs(4326))
cities_centroid_df = as.data.frame(st_coordinates(st_centroid(cities, byid=T)))
cities_centroid_df$NAME = cities$NAME
# add Callahan
cal_df = data.frame(X=-122.80142144655451, Y=41.309662937183475,NAME="Callahan")
cities_centroid_df=rbind(cities_centroid_df, cal_df)
# convert back to spatial and 3310 projection
cities_centroid = sf::st_as_sf(x=cities_centroid_df, coords=c("X","Y"), crs =st_crs(4326))
cities_centroid = st_transform(cities_centroid, crs=st_crs(3310))

# add label location numbers
# cities_centroid$label_xmod = c(-1.2, 2.2, 1.5)
# cities_centroid$label_ymod = c(0, -.2, -.5)
### cities_centroid$name = Etna, Fort Jones, Greenview, Callahan
# cities_centroid$label_xmod = c(-0.5,  0,    0,  0)
# cities_centroid$label_ymod = c(  0, 0.3, -0.3, -0.4)
cities_centroid$label_xmod = c(-1.2, 0,    0,  0)
cities_centroid$label_ymod = c(  0, 0.7, -0.7, -0.7)

# Reduce number of streams to major tributaries.
mapped_stream_names = c("Shackleford Creek","Mill Creek", "Oro Fino Creek",
                        "Moffett Creek", "Kidder Creek", "Patterson Creek",
                        "Crystal Creek","Johnson Creek", "Etna Creek",
                        "Clark Creek", "French Creek", "Miners Creek", "Sugar Creek",
                        "Scott River", "East Fork Scott River",
                        "South Fork Scott River",
                        "Wildcat Creek")
mapped_streams = named_streams[named_streams$gnis_name %in% mapped_stream_names,]
#clean up mill and patterson creeks - there are multiple creeks with that name and we just want the major tributaries
patterson_reaches = 18010208000000 + c(203, 204, 205, 1691)
patterson_reaches_exclude = 18010208000000 + c(237, 583, 2579, 2622, 2639)
mill_reaches = 18010208000000 + c(580, 1888, 7481, 58, 59, 60)
mill_exclude_reaches = 18010208000000 + c(157, 158, 159, 160, 161, 162, 163, 164, 165, 349, 350, 351, 352)
eliminate_these_reaches = (mapped_streams$gnis_name == "Mill Creek" & !(mapped_streams$reachcode %in% mill_reaches)) |
  (mapped_streams$gnis_name == "Patterson Creek"
   & !(mapped_streams$reachcode %in% patterson_reaches))
mapped_streams = mapped_streams[!eliminate_these_reaches,]

# Isolate fort jones gauge
pick_this_one = usgs_gauges$station_nm=="SCOTT R NR FORT JONES CA" &
  usgs_gauges$data_type_cd == "dv"
fj_gauge = usgs_gauges[pick_this_one,]
fj_gauge$station_nm = "FJ Gauge"

# clean data types
smolt_per_fem$Smolt_Point_Estimate = as.numeric(as.character(smolt_per_fem$Smolt_Point_Estimate))
smolt_per_fem$Smolts_Produced_Per_Female = as.numeric(as.character(smolt_per_fem$Smolts_Produced_Per_Female))
outmigs$Smolt.point.Estimate = as.numeric(as.character(outmigs$Smolt.point.Estimate))


# subfunctions ------------------------------------------------------------

# Read functional flow metrics -------------------------------------------------------------------

read_fflows_FJ = function(calculator = "Regular"){
  # fflows = read.csv(file.path(data_dir, "ScottR_FJ_wy1942_2024.05.06_annual_flow_result.csv")) # old metrics

  if(tolower(calculator) == "regular"){
    fflows = read.csv(file.path(data_dir, "ScottR_FJ_wy1942_2025.01.21_annual_Reg_Calc.csv"))
  }
  if(tolower(calculator) == "flashy"){
    fflows = read.csv(file.path(data_dir, "ScottR_FJ_wy1942_2025.01.21_annual_Flashy_Calc.csv"))
  }
  # clean table
  if(colnames(fflows)[1]=="X"){fflows[1]=NULL} # remove column of row numbers
  colnames(fflows)[1] = "Water_Year" # name it Water Year instead of just Year for clarity.
  # all the columns in this df should be numeric.
  if(class(fflows[,2])=="character"){
    fflows[colnames(fflows[2:ncol(fflows)])] = sapply(fflows[colnames(fflows[2:ncol(fflows)])],as.numeric)
  }
  return(fflows)
}

read_fflows_csv = function(scen_id){
  fflows_scen = read.csv(file.path(data_dir, "SVIHM Model Results", "tables for func flows",
                                   paste(scen_id, "func flow metrics.csv")))
  fflows = data.frame(t(as.matrix(fflows_scen)))
  colnames(fflows)=fflows_scen[,1]
  fflows = fflows[row.names(fflows)!="Year",]
  for(i in 1:ncol(fflows)){
    fflows[,i]=as.numeric(as.character(fflows[,i]))
  }

  years = as.numeric(substr(x=rownames(fflows), start = 2, stop = 5)) # convert rownames to years
  max_yr = max(years); min_yr = min(years)
  fflows$Water_Year = min_yr:max_yr

  new_col_order = c("Water_Year", colnames(fflows)[colnames(fflows)!="Water_Year"])
  fflows = fflows[,new_col_order]
  row.names(fflows) = NULL

  return(fflows)
}

# Intro, Case Study functions ---------------------------------------------------------------

ca_or_figure = function(include_legend = T){
  buf = st_buffer(ca_or, dist = 3000000)

  # plot(ca_or)
  # plot(buf, add=T, col = "gray")
  # plot(ca_or, add=T, col = "darkolivegreen3")
  # plot(klamath, add=T, col = "khaki", border = "gray10")
  # plot(county, add=T, col = NA, border = "brown", lwd = 2)
  # plot(watershed, add=T, col = "deepskyblue", border = "gray10")
  #
  # legend(x = "bottomleft", pch = 22, col = c("black", "brown","gray10","gray10"),
  #        pt.bg = c("darkolivegreen3",NA,"khaki","deepskyblue"),
  #        pt.lwd = c(1,2,1,1), pt.cex = 2,
  #        legend = c("State Boundaries", "Siskiyou County", "Klamath Basin", "Scott River Watershed"))

  if(include_legend ==TRUE){
    ca_or_fig =
      tm_shape(ca_or) + tm_polygons()+
      tm_shape(buf) + tm_polygons(col = color_state_buffer) +
      tm_shape(ca_or) + tm_polygons(col = color_states, border.col = NA)+
      tm_shape(klamath) + tm_polygons(col = color_klamath, border.col = "gray10") +
      tm_shape(county) + tm_borders(col = color_county, lwd = 2) +
      tm_shape(watershed) + tm_polygons(col = color_watershed_fill, border.col = "gray10") +
      tm_add_legend(type="fill", col = color_watershed_fill, border.col = "gray10", labels = "Scott R. Watershed") +
      tm_add_legend(type="fill", col = color_klamath, border.col = "gray10", labels = "Klamath Basin")+
      tm_add_legend(type="fill", col = color_states, border.col = "gray10", labels = "U.S. States")+
      tm_add_legend(type="line", col = color_county, labels = "Siskiyou County", lwd=2)+
      tm_layout(legend.bg.color = "white", legend.frame = T,
                legend.width = .7, legend.position = c("RIGHT", "BOTTOM"))
  }
  if(include_legend ==FALSE){
    ca_or_fig = tm_shape(buf) + tm_polygons(col = color_state_buffer) +
      tm_shape(ca_or, is.master = T) +
      tm_polygons(col = color_states, border.col = NA)+
      tm_shape(klamath) + tm_polygons(col = color_klamath, border.col = "gray10") +
      tm_shape(county) + tm_borders(col = color_county, lwd = 2) +
      tm_shape(watershed) + tm_polygons(col = color_watershed_fill, border.col = "gray10")
    }


  # maybe add the klamath river if you get around to it
  # maybe also labels for California and Oregon

  return(ca_or_fig)
}


save_setting_figure = function(){

  # tmap_mode("plot")

  main_map =
  tm_shape(hill_wsh) +
    tm_raster(palette = hillshade_palette_faded(20), legend.show = F) +
    tm_shape(watershed, name= "Watershed Boundary", is.master=T, unit=figure_units) + tm_borders (color_watershed, lwd = 2) +
    # tm_shape(basin, name = "Groundwater Basin") + tm_borders(color_basin, lwd = 2) +
    # tm_shape(adj, name = "Adjudicated Area") + tm_polygons(col = color_adju, border.col = color_adju) +
    tm_shape(mapped_streams, name = "Tributaries") + tm_lines(color_tribs, lwd = 2) +
    tm_shape(riv, name = "Scott River") + tm_lines(color_river, lwd = 3) +
    tm_shape(scott_klamath_confluence) +
    tm_symbols(col = color_confluence, shape = 25) +
    # tm_text("name", size = 1, xmod = -3.5, ymod = -.5, fontface = "bold") +
    tm_shape(i5, name = "Interstate 5") + tm_lines(color_interstate, lwd = 2) +
    tm_shape(rt3, name = "State Route 3") + tm_lines(color_state_road, lwd = 1.5) +
    tm_shape(fj_gauge, name = "FJ Gauge") + tm_symbols(color_gauges, size = 1) +
    # tm_text("station_nm", size = 1, xmod = 1, ymod = 1, fontface = "bold") +
    tm_shape(srfcf, name = "Fish Counting Facility") + tm_symbols(col=color_fcf, size = 1, shape = 23) +
    tm_shape(srrst, name = "Rotary Screw Trap") + tm_symbols(col=color_rst, size = 1, shape = 22) +
    tm_shape(qvir) + tm_polygons(color_qvir , border.lwd = 1)+
    tm_shape(cities_centroid, name = "Town or Community") +
    tm_symbols(color_cities, border.lwd=1, size = 0.5) +
    tm_text("NAME", size = 0.8, xmod = cities_centroid$label_xmod, ymod = cities_centroid$label_ymod) +
    tm_scale_bar(position = c("RIGHT", "BOTTOM"))+
    tm_compass(type = "4star", #size=0.9, #position = c("RIGHT", "BOTTOM"), ) +
               position = c(.82,.07)) +

    # legend for main figure
    tm_add_legend(type = "title", title = "Scott R Watershed Features") +
    tm_add_legend( type = "line", lwd = c(2, 3, 2), col =  c(color_watershed, color_river, color_tribs),
                   labels = c("Watershed Boundary", "Scott River", "Major Tributaries"))+
    tm_add_legend(type="fill", col = color_qvir, border.lwd = 1, labels = "QVIR", size=1) +
    tm_add_legend(type="symbol", border.lwd = .5,
                  col = c(color_cities, color_gauges, color_confluence, color_fcf, color_rst),
                  labels = c("Town or Place", "Fort Jones Gauge", "Confluence", "FCF", "RST"),
                  shape = c(21, 21, 25, 23, 22)) + #size = 0.7,
    tm_add_legend(type = "line", lwd = c(2, 1.5), col = c(color_interstate, color_state_road),
                  labels = c("Interstate 5", "State Route 3")) +
    # legend for inset map
    tm_add_legend(type = "title", title = "Inset Map") +
    tm_add_legend(type="fill", col = color_watershed_fill, border.col = "gray10", labels = "Scott River Watershed") +
    tm_add_legend(type="fill", col = color_klamath, border.col = "gray10", labels = "Klamath Basin")+
    tm_add_legend(type="fill", col = color_states, border.col = "gray10", labels = "Ore. and Calif.")+
    tm_add_legend(type="line", col = color_county, labels = "Siskiyou County", lwd=2)+
    # finish legend
    # tm_layout(legend.bg.color = "white", legend.frame = T, legend.text.size = 1, legend.only = T)
    tm_layout(legend.bg.color = "white", legend.frame = T, legend.width=.28)

  ca_or_inset = ca_or_figure(include_legend = F)
  # print(inset_map, vp=viewport(x= 0.15, y= 0.15, width= 0.3, height= 0.3))

  graphic_filename = file.path(ms_dir,"Graphics and Supplements",
                               "Graphics source","scott valley setting_tmsave.png")
  tmap_save(tm = main_map, filename = graphic_filename,
            insets_tm = ca_or_inset,
            insets_vp = viewport(x= 0.8, y= 0.75, width= 0.4, height= 0.4),
            width=7, height = 8.3, units = "in", dpi=400)

  # return(result)
}




save_setting_figure_old = function(){

  # ADD LOCATIONS OF VIDEO COUNTING FACILITY AND ROTARY SCREW TRAP (Scott JSO 2022)

  main_map =
    tm_shape(hill_wsh) +
    tm_raster(palette = hillshade_palette_faded(20), legend.show = F) +
    tm_shape(watershed, name= "Watershed Boundary", is.master=T, unit=figure_units) + tm_borders (color_watershed, lwd = 2) +
    # tm_shape(basin, name = "Groundwater Basin") + tm_borders(color_basin, lwd = 2) +
    # tm_shape(adj, name = "Adjudicated Area") + tm_polygons(col = color_adju, border.col = color_adju) +
    tm_shape(mapped_streams, name = "Tributaries") + tm_lines(color_tribs, lwd = 2) +
    tm_shape(riv, name = "Scott River") + tm_lines(color_river, lwd = 3) +
    tm_shape(scott_klamath_confluence) +
      tm_symbols(col = color_confluence, shape = 25) +
      tm_text("name", size = 1, xmod = -3.5, ymod = -.5, fontface = "bold") +
    tm_shape(i5, name = "Interstate 5") + tm_lines(color_interstate, lwd = 2) +
    tm_shape(rt3, name = "State Route 3") + tm_lines(color_state_road, lwd = 1.5) +
    tm_shape(fj_gauge, name = "FJ Gauge") + tm_symbols(color_gauges, size = 1) +
      tm_text("station_nm", size = 1, xmod = 1, ymod = 1, fontface = "bold") +
    tm_shape(qvir) + tm_polygons(color_qvir , border.lwd = 1)+
    tm_shape(cities_centroid, name = "Town or Community") +
    tm_symbols(color_cities, border.lwd=1, size = 0.5) +
      tm_text("NAME", size = 0.8, xmod = cities_centroid$label_xmod, ymod = cities_centroid$label_ymod) +
    tm_scale_bar(position = c("right", "bottom"))+
    tm_compass(type = "4star",#position = c("left", "top"), ) +
               position = c(.82,.07))+
    # legend for main figure
    tm_add_legend(type = "title", title = "Watershed Features") +
    tm_add_legend( type = "line", lwd = c(2, 3, 2), col =  c(color_watershed, color_river, color_tribs),
                   labels = c("Watershed Boundary", "Scott River", "Major Tributaries")) +
    tm_add_legend(type="fill", col = color_qvir, border.lwd = 1, labels = "QVIR")+
    tm_add_legend(type="symbol", col = color_cities, border.lwd = NA, labels = "Town or Place")+ #size = 0.7,
    tm_add_legend(type="symbol", col = color_gauges, labels = "Fort Jones Gauge")+ #size = 0.8,
    tm_add_legend(type="symbol", col = color_confluence, shape = 25, size = 1, labels = "Scott-Klamath \n Confluence")+
    tm_add_legend(type = "line", lwd = c(2, 1.5), col = c(color_interstate, color_state_road),
                  labels = c("Interstate 5", "State Route 3")) +
    # legend for inset map
    tm_add_legend(type = "title", title = "Inset Map") +
    tm_add_legend(type="fill", col = color_watershed_fill, border.col = "gray10", labels = "Scott River Watershed") +
    tm_add_legend(type="fill", col = color_klamath, border.col = "gray10", labels = "Klamath Basin")+
    tm_add_legend(type="fill", col = color_states, border.col = "gray10", labels = "Ore. and Calif.")+
    tm_add_legend(type="line", col = color_county, labels = "Siskiyou County", lwd=2)+
    # finish legend
    # tm_layout(legend.bg.color = "white", legend.frame = T, legend.text.size = .7)
    tm_layout(legend.bg.color = "white", legend.frame = T, legend.width=.28)

  inset_fig = ca_or_figure( include_legend = F)

  # convert to grobs
  main_grob = tmap_grob(main_map)
  inset_grob = tmap_grob(inset_fig)

  # Arrange plots
  result = ggdraw() +
    draw_plot(main_grob) +
    # draw_plot(inset_grob, width = .4, height = .6, x = .62, y = -.07)
    draw_plot(inset_grob, width = .35, height = .6, x = .67, y = .49)

  # produce map
  graphic_filename = file.path(ms_dir,"Graphics and Supplements",
                               "Graphics source","scott valley setting.png")
  file.remove(graphic_filename) # remove old version to save new one
  png(filename = graphic_filename,
      width = 7, height = 8.3, units = "in", res = res_dpi)
      # width = 9, height = 10, units = "in", res = 400)
  result
  dev.off()


  # return(result)
}


fj_flow_figure=function(last_wy = 2021, roll_window = 15){

  # Convert flow to daily volume
  fj_flow$Flow_m3day = fj_flow$Flow * cfs_to_m3day
  # Aggregate flow by WY to assign 25% water year types by total flow
  fj_flow_wy = aggregate(fj_flow$Flow_m3day[fj_flow$wy<=last_wy],
                         by = list(fj_flow$wy[fj_flow$wy<=last_wy]), FUN = sum)
  colnames(fj_flow_wy) = c("wy","tot_flow")
  wy_tot_quartiles = quantile(fj_flow_wy$tot_flow, c(0, .25,.5, .75,1))
  fj_flow_wy$quartile = cut(fj_flow_wy$tot_flow, breaks = wy_tot_quartiles)

  wy_type_tab = data.frame(quartile = sort(unique(fj_flow_wy$quartile)),
                           color = c("firebrick", "darkgoldenrod1","chartreuse2","deepskyblue3"),
                           type = c("Dry", "Below Avg.", "Above Avg.", "Wet"))
  # plot(fj_flow_wy$wy, fj_flow_wy$tot_flow,
  #      col = wy_type_tab$color[fj_flow_wy$quartile], pch = 19)

  # Make a daily vector, fj_flow_wy_type, indicating which days are in dry and wet years etc
  fj_flow_days_quartile = fj_flow_wy$quartile[match(fj_flow$wy, fj_flow_wy$wy)]
  fj_flow_wy_type = wy_type_tab$type[match(fj_flow_days_quartile, wy_type_tab$quartile)]


  # Set up to make WYs plot
  fj_flow$jday_from_oct1 = NA

  #Plot setup
  water_years = unique(fj_flow$wy) #Isolate water years for plot
  n_wy = length(water_years)
  wy_text = paste(min(water_years), last_wy, sep = "-")
  plot_colors = rep(rgb(0,0,.5,0.1), n_wy)
  par(mar = c(5,5,4,5) + 0.1) # add more space for 2nd axis
  # plot_colors = gray((n_wy:1)/n_wy, alpha = 0.5) # light colors in the past
  # plot_colors = rev(topo.colors(n_wy)) # light colors in the past

  # make plot
  for(i in 1:length(water_years)){
    wy = water_years[i]
    wy_indices = which(fj_flow$wy==wy)

    if(i == 1){
      # Initialize plot
      plot(x = 1:366,
           y = c(fj_flow$Flow[wy_indices],NA), # add extra day for leap days in later years
           main=paste("Scott River annual hydrographs, Fort Jones gauge,", wy_text),
           xaxt="n", yaxt = "n",
           log = "y",
           xlab = "Date in Water Year (starting Oct. 1)",
           ylab = "Daily Average Flow (cfs)",
           ylim = c(3, max(fj_flow$Flow)),
           type = "l", col = plot_colors[i])
    } else {
      lines(x=1:length(wy_indices),
            y=fj_flow$Flow[wy_indices],
            col = plot_colors[i])
    }

    # While we're in this for loop, assign the julian day (starting Oct. 1)
    #  for median calcs later
    fj_flow$jday_from_oct1[wy_indices] = 1:length(wy_indices)
  }
  quarter_day1s = c(0,92,182,273,365)
  axis(side=1,at=quarter_day1s, tick=TRUE,
       labels=c("Oct 1","Jan 1", "Apr 1","Jul 1","Oct 1"))
  flow_labels = c("0.1", "1", "10", "100","1,000", "10,000")
  axis(side=2, at=10^(-1:4), labels = flow_labels, las = 1, cex.axis=.8)
  axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA, tck = -0.01)
  abline(col="gray", lty=3, v= quarter_day1s, h= 10^(-1:4))

  # Plot daily median flow
  # daily_med = aggregate(fj_flow$Flow, by = list(fj_flow$jday_from_oct1), FUN = median)
  # lines(daily_med$Group.1, daily_med$x,lwd = 2, col = "brown")

  # Aggregate daily flow by WY type
  daily_type = aggregate(fj_flow$Flow, by = list(fj_flow$jday_from_oct1, fj_flow_wy_type), FUN = median)
  colnames(daily_type)=c("jday_from_oct_1", "wy_type","Flow_cfs")
  for(i in 1:4){
    type = wy_type_tab$type[i]
    record = daily_type[daily_type$wy_type==type,]
    lines(x = record$jday_from_oct_1[1:365],
          y = rollmean(x = record$Flow_cfs[1:365],
                       k = roll_window, align = "center", fill=NA),
          col = wy_type_tab$color[i], lwd = 2)
  }

  # Add 2nd cms axis
  par(new=TRUE)
  plot(x=c(1,1), y = range(fj_flow$Flow) * cfs_to_m3sec,
       ylim = range(fj_flow$Flow) * cfs_to_m3sec, col = NA,
       xaxt = "n", yaxt = "n", log = "y", ylab = "", xlab = "")
  axis(side=4, at=10^(-1:4), labels = flow_labels, las = 1)
  axis(side=4, at=rep(1:9,7) * 10 ^ (rep(-1:5, each = 9)), labels = NA, tck = -0.01)
  mtext(expression(Daily~Average~Flow~(m^3~"/"~sec)), side = 4, line = 3)


  legend(x="topright", lwd=c(1,2), cex = 0.9,
         col = c(rgb(0,0,.5,0.3), wy_type_tab$color),
         legend = c("Annual hydrograph",
                    paste(wy_type_tab$type, "years")))

}

total_flow_over_time = function(fj_flow, period = "annual",
                                start_date = as.Date("1940-10-01"),
                                end_date = as.Date("2021-10-01"),
                                rolling_avg_line = F){
  # par(mar = c(5,5,4,2))
  # create new version of FJ flow record for this function
  fjd = fj_flow # fjd = Fort Jones Daily
  fjd_for_wy = fjd[fjd$Date>=start_date & fjd$Date<end_date,] # can switch to full record if desired
  # Convert to daily volume
  fjd_for_wy$m3_per_day = fjd_for_wy$Flow*cfs_to_m3day
  #subset by period
  if(period == "annual"){
    plot_title = "Total Annual Flow at Fort Jones Gauge"
    ylab_text1 = "Total Annual Flow (TAF)"
    ylab_text2 = expression(Total~annual~flow~(million~m^3))
  }
  if(period == "sep-dec"){ # subset to just sep-dec days
    plot_title = "Total Flow at Fort Jones Gauge, Sep-Dec"
    ylab_text = expression(Total~flow~Sep~through~Dec~(million~m^3))
    # assign each september to the following water year
    september_days = month(fjd_for_wy$Date)==9
    fjd_for_wy$wy[september_days] = year(fjd_for_wy$Date[september_days]) + 1
    fjd_for_wy = fjd_for_wy[month(fjd_for_wy$Date) %in% 9:12,]}

  m3_per_wy = aggregate(fjd_for_wy$m3_per_day, by=list(fjd_for_wy$wy), FUN=sum)
  colnames(m3_per_wy)=c("wy","tot_flow_m3")

  par(mar = c(4,5,1,5) ) # add more space for 2nd axis

  plot(m3_per_wy$wy, m3_per_wy$tot_flow_m3 * m3_to_TAF,#m3_per_wy$tot_flow_m3 / 1E6,
       pch =19, col = "darkgray",# type = "o",
       ylab = ylab_text1, xlab= "Water Year"#, main = plot_title
       )
  bestfit =lm(m3_per_wy$tot_flow_m3/1E6 ~ m3_per_wy$wy)
  abline(bestfit, lty = 2)
  if(rolling_avg_line == T){
    # rolling avg
    roll_n_yrs = 10
    lines(x = m3_per_wy$wy[roll_n_yrs:nrow(m3_per_wy)],
          y = rollmean(x = m3_per_wy$tot_flow_m3/1E6, k = roll_n_yrs, align = "left"),
          col = "black", lwd=2)

  }

  grid()
  legend(x="topleft", legend = "A",pch=NULL, col = NULL, bty="n")  # add panel label

  # Add 2nd axis
  par(new=TRUE)
  plot(x=c(1,1), y = range(m3_per_wy$tot_flow_m3 / 1E6),
       ylim = range(m3_per_wy$tot_flow_m3 / 1E6), col = NA,
       xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  axis(side=4, at=pretty(range(m3_per_wy$tot_flow_m3 / 1E6)),
       labels = pretty(range(m3_per_wy$tot_flow_m3 / 1E6)), las = 1)

  # axis(side=4, at=10^(-1:4), labels = flow_labels, las = 1)
  # axis(side=4, at=rep(1:9,7) * 10 ^ (rep(-1:5, each = 9)), labels = NA, tck = -0.01)
  mtext(ylab_text2, side = 4, line = 3.5, cex = .75)


  # fjd_for_wy$TAF_per_day = fjd_for_wy$Flow*cfs_to_TAF_per_day
  # TAF_per_wy = aggregate(fjd_for_wy$TAF_per_day, by=list(fjd_for_wy$wy), FUN=sum)
  # colnames(TAF_per_wy)=c("wy","tot_flow_TAF")
  #
  # plot(TAF_per_wy$wy, TAF_per_wy$tot_flow_TAF, pch =19, col = "darkgray",# type = "o",
  #      ylab = "Total annual flow (thousand acre-feet)", xlab= "Water Year")
  # bestfit =lm(TAF_per_wy$tot_flow_TAF ~ TAF_per_wy$wy)
  # abline(bestfit, lty = 2)
  # # rolling avg
  # roll_n_yrs = 10
  # lines(x = TAF_per_wy$wy[roll_n_yrs:nrow(TAF_per_wy)],
  #       y = rollmean(x = TAF_per_wy$tot_flow_TAF, k = roll_n_yrs, align = "left"),
  #       col = "black", lwd=2)

  # legend(x = "topright", lty = c(2,1), lwd = c(1,2),
  #        legend = c("Line of best fit", "10-year trailing average"))
}

func_flow_timeseries_fig = function(fj_flow, fflows){
  par(mfrow = c(4,2), mar = c(4,5,1,2))
  total_flow_over_time(fj_flow = fj_flow, period = "annual")

  ff_timeseries_figs = c("FA_Mag", "FA_Tim",
                         "Wet_Tim","Wet_BFL_Mag_50",
                         "SP_ROC","DS_Mag_50","DS_Dur_WS")
  ff_labels = c("Fall Pulse Mag. (cfs)", "Fall Pulse Onset \n (days since Oct. 1)",
                "Wet Season Onset \n (days since Oct. 1)",
                "Wet Season Median \n Baseflow (cfs)",
                "Spring Recession \n rate of change (%/day)",
                "Dry Season Median \n Flow (cfs)","Dry Season Duration (days)")
  ff_cols = c(rep(fall_col,2), rep(wet_sn_col,2), spring_col, rep(dry_sn_col, 2))

  for(i in 1:length(ff_timeseries_figs)){
    par(mar = c(4,5,1,2))
    ff = ff_timeseries_figs[i]
    plot(fflows$Water_Year, fflows[,ff], pch = 19, col = ff_cols[i],
         ylab = ff_labels[i], xlab = "Water Year", )
    grid()
    abline(lm(formula = paste(ff ,"~ Water_Year"), data = fflows), lty = 2)
    legend(x="topleft", legend = LETTERS[i+1], pch=NULL, col = NULL, bty="n")  # add panel label

    if(i==1){legend(x = "topright", lty = 2, legend = "Line of best fit")}
    if(i==6){legend(x = "topright", pch = 19, bg="white", cex =.9,
                    col = c("darkgray",fall_col, wet_sn_col, spring_col, dry_sn_col),
                    legend = paste(c("Annual","Fall", "Wet Season", "Spring", "Dry Season"),"metric"))}
  }

}

re_and_disconnect_date_tab=function(thresholds = c(10,20,30,40,60,100), fj_flow, last_wy = 2022){

  wat_years = unique(fj_flow$wy)
  wat_years = wat_years[wat_years<=last_wy]
  # output_tab = data.frame(year = wat_years, min_flow = NA,
  #                         recon_date_10 = NA, recon_date_20 = NA,
  #                         recon_date_30 = NA, recon_date_40 = NA,
  #                         recon_date_60 = NA, recon_date_100 = NA,
  #                         discon_date_10 = NA, discon_date_20 = NA,
  #                         discon_date_30 = NA, discon_date_40 = NA,
  #                         discon_date_60 = NA, discon_date_100 = NA)
  output_tab = data.frame(matrix(data = NA, nrow = length(wat_years),
                                 ncol = length(thresholds) * 2 + 3))
  colnames(output_tab) = c('water_year', "min_flow", "tot_flow",paste0("recon_date_",thresholds),
                          paste0("discon_date_",thresholds))
  output_tab$water_year = wat_years


  for(i in 1:length(wat_years)){
    year = wat_years[i]

    date1_discon = as.Date(paste0(year,"-03-01"))
    date2_discon = as.Date(paste0(year,"-08-31"))
    dates_discon = seq.Date(from=date1_discon, to = date2_discon, by="day")

    date1_recon = as.Date(paste0(year-1,"-09-01"))
    date2_recon = as.Date(paste0(year,"-02-28"))
    dates_recon = seq.Date(from=date1_recon, to = date2_recon, by="day")

    discon_flows = fj_flow$Flow[fj_flow$Date %in% dates_discon]
    recon_flows = fj_flow$Flow[fj_flow$Date %in% dates_recon]

    output_tab$min_flow[i] = min(fj_flow$Flow[fj_flow$wy==year])
    output_tab$tot_flow[i] = sum(fj_flow$Flow[fj_flow$wy==year] * cfs_to_m3day) / (10^6) #million cubic m


    for(thresh in thresholds){
      output_tab[i, paste0("recon_date_", thresh)] = calc_recon_days_since_aug_31(dates=dates_recon,
                                                                                  flow = recon_flows,
                                                                                  recon_threshold = thresh)
      output_tab[i, paste0("discon_date_", thresh)] = calc_discon_days_since_aug_31(dates=dates_discon,
                                                                                   flow = discon_flows,
                                                                                   discon_threshold = thresh)
    }
      #
      # output_tab$recon_date_10[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 10)
      # output_tab$discon_date_10[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 10)
      #
      # output_tab$recon_date_20[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 20)
      # output_tab$discon_date_20[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 20)
      #
      # output_tab$recon_date_30[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 30)
      # output_tab$discon_date_30[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 30)
      #
      # output_tab$recon_date_40[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 40)
      # output_tab$discon_date_40[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 40)
      #
      # output_tab$recon_date_60[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 60)
      # output_tab$discon_date_60[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 60)
      #
      # output_tab$recon_date_100[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 100)
      # output_tab$discon_date_100[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 100)


  }

  return(output_tab)

}


recon_and_discon_explainer_hydrograph= function(water_year = 2016,
                                                 tot_flow_annotate = T,
                                                 connection_date_annotate="all_6"){
  fj_wy = fj_flow[fj_flow$wy==water_year |
                    fj_flow$wy==water_year - 1 & month(fj_flow$Date) == 9,]

  # Initialize Plot
  par(mar = c(5,5,4,5) + 0.1) # add more space for 2nd axis
  plot(fj_wy$Date, fj_wy$Flow, type = "l", log = "y",
       lwd = 2, col = "dodgerblue", yaxt = "n", xaxt="n",
       ylim = range(fj_wy$Flow),
       main = "Fort Jones Annual Hydrograph with Selected Functional Flow Metrics",
       # ylab = "Average Daily Flow at FJ Gauge (cfs)",
       ylab = expression(Average~Daily~Flow~at~FJ~Gauge~(ft^3~"/"~sec)),
       xlab = paste("Date in water year", water_year))

  # Plot fine-tuning
  flow_labels = c("0.1","1","10","100","1,000","10,000","100,000")
  # date_lines = as.Date(paste0(water_year,"-01-01")) + (month_1s - 1)
  date_lines = seq.Date(from=min(fj_wy$Date), to = max(fj_wy$Date)+32, by = "month")
  label_these_months = date_lines[c(2,4,6,8,10,12,14)]
  axis(side=1, at=label_these_months,
       labels = month.abb[c(month(label_these_months))], las = 1)
  axis(side=2, at=10^(-1:5), labels = flow_labels, las = 1)
  axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA, tck = -0.01)
  abline(h=10^(0:5), v = date_lines, col = "gray", lty = 3)

  if(tot_flow_annotate == TRUE){
    # Annotations
    # Total annual flow annotations
    poly_xmin = min(fj_wy$Date)
    poly_xmax = as.Date(paste0(water_year,"-01-01"))
    poly_x = c(poly_xmin,
               fj_wy$Date[fj_wy$Date <= poly_xmax],
               poly_xmax)
    poly_y = c(1,
               fj_wy$Flow[fj_wy$Date <= poly_xmax],
               1)
    polygon(x = poly_x, y = poly_y, col = rgb(.5,.5,.8,.3),
            border = NA)
    # text(x = poly_xmin + 30, y = 4.5, labels = "Total Flow \n Sep-Dec")
  }

  if(connection_date_annotate == "all_6"){
    # Connection dates
    threshold_color_tab = data.frame(thresholds_cfs = c(10,20,30,40,60,100),
                                     color = c("firebrick", "orangered", "goldenrod",
                                               "green4","dodgerblue", "darkblue"))
    day_buffer = 5
    for(i in 1:nrow(threshold_color_tab)){

      threshold = threshold_color_tab$thresholds_cfs[i]

      # Recon annotations
      recon_date = min(fj_wy$Date[fj_wy$Flow > threshold])
      arrows(x0=recon_date - day_buffer, x1=recon_date + day_buffer,
             y0=threshold, y1=threshold, lwd = 2,
             length = 0, col = threshold_color_tab$color[i])
      arrows(x0=recon_date, x1=recon_date,
             y0=1, y1=threshold, lwd = 1.5, lty = 2,
             length = 0, col = threshold_color_tab$color[i])
      text(x = recon_date - day_buffer - 25, y = threshold,
           labels = paste(threshold, "cfs"), col = threshold_color_tab$color[i])

      # Discon annotations
      discon_date = min(fj_wy$Date[fj_wy$Date > as.Date(paste0(water_year,"-02-01")) &
                                     fj_wy$Flow < threshold], na.rm=T)
      arrows(x0=discon_date - day_buffer, x1=discon_date + day_buffer,
             y0=threshold, y1=threshold, lwd = 2,
             length = 0, col = threshold_color_tab$color[i])
      text(x = discon_date + day_buffer + 20, y = threshold,
           labels = paste(threshold, "cfs"), col = threshold_color_tab$color[i])
      arrows(x0=discon_date, x1=discon_date,
             y0=1, y1=threshold, lwd = 1.5, lty = 2,
             length = 0, col = threshold_color_tab$color[i])
    }
  }

  if(connection_date_annotate == "20_and_120_only"){
    # Connection dates
    threshold_color_tab = data.frame(thresholds_cfs = c(10,20,30,40,60,120),
                                     color_fall = c(NA, "chocolate4", NA, NA,NA, "chocolate4"),
                                     color_spring = c(NA, "darkolivegreen4", NA, NA,NA, "darkolivegreen4"))
    day_buffer = 8
    for(i in c(2,6)){

      threshold = threshold_color_tab$thresholds_cfs[i]

      # Recon annotations
      recon_date = min(fj_wy$Date[fj_wy$Flow > threshold])
      arrows(x0=recon_date - day_buffer, x1=recon_date + day_buffer,
             y0=threshold, y1=threshold, lwd = 4,
             length = 0, col = threshold_color_tab$color_fall[i])
      arrows(x0=recon_date, x1=recon_date,
             y0=1, y1=threshold, lwd = 3, lty = 2,
             length = 0, col = threshold_color_tab$color_fall[i])
      text(x = recon_date - day_buffer - 40, y = threshold, cex = 1.5,
           labels = paste(threshold, "cfs"), col = threshold_color_tab$color_fall[i])

      # Discon annotations
      discon_date = min(fj_wy$Date[fj_wy$Date > as.Date(paste0(water_year,"-02-01")) &
                                     fj_wy$Flow < threshold], na.rm=T)
      arrows(x0=discon_date - day_buffer, x1=discon_date + day_buffer,
             y0=threshold, y1=threshold, lwd = 4,
             length = 0, col = threshold_color_tab$color_spring[i])
      text(x = discon_date + day_buffer + 35, y = threshold, cex = 1.5,
           labels = paste(threshold, "cfs"), col = threshold_color_tab$color_spring[i])
      arrows(x0=discon_date, x1=discon_date,
             y0=1, y1=threshold, lwd = 3, lty = 2,
             length = 0, col = threshold_color_tab$color_spring[i])
    }
  }

  if(connection_date_annotate == "10_and_100_only"){
    # Connection dates
    threshold_color_tab = data.frame(thresholds_cfs = c(10,20,30,40,60,100),
                                     color_fall = c(fall_col, NA, NA, NA,NA, fall_col),
                                     color_spring = c(spring_col,NA, NA, NA,NA, spring_col))
    day_buffer = 8
    for(i in c(1,6)){

      threshold = threshold_color_tab$thresholds_cfs[i]

      # Recon annotations
      recon_date = min(fj_wy$Date[fj_wy$Flow > threshold])
      arrows(x0=recon_date - day_buffer, x1=recon_date + day_buffer,
             y0=threshold, y1=threshold, lwd = 4,
             length = 0, col = threshold_color_tab$color_fall[i])
      arrows(x0=recon_date, x1=recon_date,
             y0=1, y1=threshold, lwd = 3, lty = 2,
             length = 0, col = threshold_color_tab$color_fall[i])
      text(x = recon_date - day_buffer - 40, y = threshold, cex = 1.5,
           labels = paste(threshold, "cfs"), col = threshold_color_tab$color_fall[i])

      # Discon annotations
      discon_date = min(fj_wy$Date[fj_wy$Date > as.Date(paste0(water_year,"-03-01")) &
                                     fj_wy$Flow < threshold], na.rm=T)
      arrows(x0=discon_date - day_buffer, x1=discon_date + day_buffer,
             y0=threshold, y1=threshold, lwd = 4,
             length = 0, col = threshold_color_tab$color_spring[i])
      if(i ==1){
        text(x = discon_date + 20, y = threshold + 12, cex = 1.5,
             labels = paste(threshold, "cfs"), col = threshold_color_tab$color_spring[i])
      }
      if(i ==6){
        text(x = discon_date + day_buffer + 40, y = threshold, cex = 1.5,
             labels = paste(threshold, "cfs"), col = threshold_color_tab$color_spring[i])
      }

      arrows(x0=discon_date, x1=discon_date,
             y0=1, y1=threshold, lwd = 3, lty = 2,
             length = 0, col = threshold_color_tab$color_spring[i])
    }
  }


  # Add 2nd cms axis
  par(new=TRUE)
  plot(x=c(1,1), y = range(fj_wy$Flow) * cfs_to_m3sec,
       ylim = range(fj_wy$Flow) * cfs_to_m3sec, col = NA,
       xaxt = "n", yaxt = "n", log = "y", ylab = "", xlab = "")
  axis(side=4, at=10^(-1:5), labels = flow_labels, las = 1)
  axis(side=4, at=rep(1:9,7) * 10 ^ (rep(-1:5, each = 9)), labels = NA, tck = -0.01)
  mtext(expression(Average~Daily~Flow~at~FJ~Gauge~(m^3~"/"~sec)), side = 4, line = 3)


}


re_and_discon_timeseries_figure = function(thresh = 120){
  connect_tab = re_and_disconnect_date_tab(fj_flow = fj_flow, last_wy = 2023, thresholds = thresh)
  # clean - turn 0s to NAs
  col_name_recon = paste0("recon_date_",thresh)
  col_name_discon = paste0("discon_date_",thresh)
  connect_tab[connect_tab[,col_name_recon]==0,col_name_recon] = NA
  connect_tab[connect_tab[,col_name_discon]==1,col_name_discon] = NA

  # par(mar = c(5,4,3,1))
  plot(x = connect_tab$water_year, y = connect_tab[,col_name_recon], col = fall_col,
       main = paste0("Fall reconnection and spring disconnection dates, \n",
                    thresh," cfs (", round(thresh*cfs_to_m3sec,1)," cms) threshold"),
       type = "o", pch=19, ylim = c(-70,366),
       ylab = "Days since Aug. 31", xlab = "Water Year")
  grid()
  abline(lm(connect_tab[,col_name_recon] ~ connect_tab$water_year), lty = 2)

  points(x = connect_tab$water_year, y = connect_tab[,col_name_discon],
         col = spring_col, pch=19)
  lines(x = connect_tab$water_year, y = connect_tab[,col_name_discon], col = spring_col)
  abline(lm(connect_tab[,col_name_discon] ~ connect_tab$water_year), lty = 2)


  legend(x="bottomright",lty = c(1,1,2), pch = c(19,19,NA), col = c(fall_col, spring_col, "black"),
         inset = .02,
         legend = c(paste("First fall day when flow exceeded",thresh,"cfs"),
                    paste("First spring day when flow fell below",thresh,"cfs"),
                    "Line of best fit"))
}


eco_vs_pred_figs = function(save_pdf=T,
                            y_val_id = "chinook_spawner_abundance"){

  if(save_pdf==T){
    fig_path = file.path(save_figs_here, paste0(y_val_id," vs hydro.pdf"))
    pdf(file = fig_path, width = 7, height = 8)
    par(mfrow = c(3,2))
  }

  y_vals = metrics_tab[,y_val_id]
  preds_for_fig = colnames(metrics_tab)[!(colnames(metrics_tab) %in% non_preds)]

  # plot chinook abundance vs all predictors
  for(i in 1:length(preds_for_fig)){
    pred = preds_for_fig[i]
    x_pred = metrics_tab[,pred]
    season_abbrev = substr(x=pred, start = 1, stop = 1)
    szn_col = season_color_tab$color[season_color_tab$abbrev==season_abbrev]
    season_id = substr(x=pred, start = 1, stop = 2)
    is_relevant = grepl(pattern = season_id,
                        x = yvlt$influencing_seasons[yvlt$y_val==y_val_id])

    if(is_relevant & sum(!is.na(x_pred)) >= 10 & length(unique(x_pred))>1){

      plot(x_pred, y_vals, pch=19, col = szn_col,
           xlab = pred, ylab = y_val_id)
      abline(lm(y_vals ~ x_pred), lty = 2)
      grid()
      corr_val = round(cor(x=x_pred, y=y_vals, use = "pairwise.complete.obs"),2)
      legend(x="topright", legend = paste0("R: ", corr_val),
             lty = 2, box.col = NULL)

    }
  }
  if(save_pdf==T){dev.off()}

}

eco_vs_time_figs = function(save_pdf = T, y_val_id = "chinook_spawner_abundance"){
  if(save_pdf==T){
    fig_path = file.path(save_figs_here, paste0(y_val_id," vs hydro vs time.pdf"))
    pdf(file = fig_path, width = 7, height = 8)
    par(mfrow = c(3,2))
  }

  dates = metrics_tab$brood_year
  y_vals = metrics_tab[,y_val_id]
  preds_for_fig = colnames(metrics_tab)[!(colnames(metrics_tab) %in% non_preds)]

  # plot chinook abundance vs all predictors
  for(i in 1:length(preds_for_fig)){
    pred = preds_for_fig[i]
    x_pred = metrics_tab[,pred]
    season_abbrev = substr(x=pred, start = 1, stop = 1)
    szn_col = season_color_tab$color[season_color_tab$abbrev==season_abbrev]
    season_id = substr(x=pred, start = 1, stop = 2)
    is_relevant = grepl(pattern = season_id,
                        x = yvlt$influencing_seasons[yvlt$y_val==y_val_id])

    if(is_relevant & sum(!is.na(x_pred)) >= 10 & length(unique(x_pred))>1){

      par(mar=c(5,4,4,4))
      plot(dates, y_vals, pch=19, col = szn_col,
           xlab = "Brood Year", ylab = y_val_id)
      # abline(lm(y_vals ~ x_pred), lty = 2)
      grid()
      corr_val = round(cor(x=x_pred, y=y_vals, use = "pairwise.complete.obs"),2)
      legend(x="topright", legend = paste0("R: ", corr_val),
             lty = 2, box.col = NULL)

      par(new=T)
      plot(dates, x_pred, pch = 2, xlab = NA, ylab = NA, axes=F)
      axis(side = 4)
      mtext(text = pred, side = 4, line = 2)

    }
  }
  if(save_pdf==T){dev.off()}

}


# Metrics Calculations -------------------------------------------


y_val_label_tab = function(){
  # calculate number of predictors for text below

  all_szns = paste(c("d1", "f1", "w1", "s1", "d2", "f2", "w2", "s2"), collapse =", ")
  y1_szns = paste(c("d1", "f1", "w1", "s1"), collapse = ", ")
  spawn_szns = paste(c("d1", "f1", "w1"), collapse = ", ")
  # spawn_szns = paste(c("d1", "f1"), collapse = ", ")

  y_val_label_tab = data.frame(y_val = c("chinook_spawner_abundance",
                                         "chinook_juvenile_abundance",
                                         "chinook_juv_per_adult",
                                         "coho_spawner_abundance",
                                         "coho_smolt_abun_est",
                                         "coho_smolt_per_fem",
                                         "coho_redds_in_brood"

  ),
  y_val_title = c("Chinook spawners",
                  "Chinook juv. abundance",
                  "Chinook jpa",
                  "coho spawners",
                  "coho smolt abundance",
                  "coho spf",
                  "coho redd abundace"

  ),
  y_val_label = c("Num. Chinook spawners",# (escapement)",
                  "Num. Chinook juveniles",
                  "Chinook juv. per adult",
                  "Num. coho spawners",# (escapement)",
                  "Est. num. coho smolt",
                  "Coho smolt per fem. spawner",
                  "Num. obs. coho redds"),
  influencing_seasons = c(spawn_szns,
                          y1_szns,
                          y1_szns,
                          spawn_szns,
                          all_szns,
                          all_szns,
                          spawn_szns))
  return(y_val_label_tab)
}


calc_recon_days_since_aug_31 = function(dates, flow, recon_threshold){
  dates_since_aug_31 = as.numeric(dates - as.Date(paste0(year(min(dates)), "-08-31")))
  recon_day = min(dates_since_aug_31[ flow > recon_threshold], na.rm=T)
  return(recon_day)
}

calc_discon_days_since_aug_31 = function(dates, flow, discon_threshold){
  dates_since_aug_31 = as.numeric(dates - as.Date(paste0(year(min(dates))-1, "-08-31")))
  if(sum(flow < discon_threshold) > 0){ #if it does disconnect, calculate discon day
    discon_day = min(dates_since_aug_31[ flow < discon_threshold], na.rm=T)
  }

  if(sum(flow < discon_threshold) < 1){ # if it does not disconnect, return the end of the analysis period
    return(max(dates_since_aug_31))
  } else {
    return(discon_day)
  }
}



tabulate_hydro_by_affected_brood_year = function(smolt_years = smolt_per_fem$Smolt_Year,
                                                 brood_years = smolt_per_fem$Adult_Year_Brood_Year){
  hydro_by_brood_year = fj_flow[0:0,] # Get table structure with 0 rows
  hydro_by_brood_year$smolt_year = vector(mode="integer",length=0)
  hydro_by_brood_year$smolt_year_cum_flow = vector(mode="numeric",length=0)

  #if no m3 per day units, create it
  if(!is.element(el = "Flow_m3day",colnames(fj_flow))){fj_flow$Flow_m3day = fj_flow$Flow*cfs_to_m3day}
  for(smolt_year in smolt_years){
    date1 = as.Date(paste0(smolt_year - 2,"-09-01"))
    date2 = as.Date(paste0(smolt_year,"-07-01"))
    affecting_hydro_dates = seq.Date(from = date1, to = date2, by="day")

    affecting_hydro = fj_flow[fj_flow$Date %in% affecting_hydro_dates,]
    affecting_hydro$smolt_year = smolt_year
    affecting_hydro$smolt_year_cum_flow = cumsum(affecting_hydro$Flow_m3day)
    hydro_by_brood_year = rbind(hydro_by_brood_year, affecting_hydro)

  }

  return(hydro_by_brood_year)

}

calc_metrics_hydro_by_affected_brood_year = function(hydro_by_brood_year,
                                                     thresholds = c(10, 20, 30, 40, 60, 100),
                                                     reduce_to_eco_data_years_only = T){

  hbbm = hydro_by_brood_year

  # 1. enumerate FF column names
  ff_names_df = read.csv(file.path(data_dir, "Func_Flow_Flashy_Names.csv"), header = T)
  ff_names = ff_names_df$Functional_Flow_Names
  # identify abbreviations - e.g. f1 for first fall rewetting event
  period_abbrev = rep("", length(ff_names))
  period_abbrev[grepl(pattern = "DS", x = ff_names)] = "d1"
  period_abbrev[grepl(pattern = "FA", x = ff_names)] = "f1"
  period_abbrev[grepl(pattern = "Wet", x = ff_names)] = "w1"
  period_abbrev[grepl(pattern = "Peak", x = ff_names)] = "w1"
  period_abbrev[grepl(pattern = "SP", x = ff_names)] = "s1"
  period_abbrev[grepl(pattern = "Ann", x = ff_names)] = "wy1"
  period_abbrev[grepl(pattern = "WY", x = ff_names)] = "wy1"

  ff_names_wy1 = paste0(period_abbrev, "_", ff_names)
  ff_names_wy2 = gsub(pattern = "1_", replacement = "2_", x = ff_names_wy1)
  ff_names_cflp = c(ff_names_wy1, ff_names_wy2)

  # 2. enumerate output table column names
  # Brood year index, Eco responses, connectivity timing, FFs, and # days of scouring flows
  output_colnames = c("brood_year", "smolt_year",
                      "coho_smolt_per_fem",
                      "chinook_juv_per_adult",
                      "coho_spawner_abundance",
                      "coho_redds_in_brood",
                      "coho_smolt_abun_est",
                      "chinook_spawner_abundance",
                      # "chinook_spawner_abundance_long",
                      "chinook_juvenile_abundance",
                      # "percent_coho_smolt_survival",
                      # more advanced outcomes?
                      paste0("f1_recon_",thresholds),
                      paste0("s1_discon_",thresholds),
                      paste0("f2_recon_",thresholds),
                      paste0("s2_discon_",thresholds),
                      # hoping total flow can get captured by dry season metrics?
                      ff_names_cflp,
                      "w1_num_days_gt_90_pctile","w2_num_days_gt_90_pctile"
  )


  #3. Initialize table
  ## Identify brood year coverage to find number of rows
  if(reduce_to_eco_data_years_only==T){
    brood_years_min = min(c(smolt_per_fem$Adult_Year_Brood_Year,
                            outmigs$Brood.Year, coho_abun$Return_Year,
                            spawners$year, chinook_abun$Year, chinook_spawn_and_juv$Brood_Year))
    brood_years_max = max(c(smolt_per_fem$Adult_Year_Brood_Year,
                            outmigs$Brood.Year,coho_abun$Return_Year,
                            spawners$year, chinook_abun$Year, chinook_spawn_and_juv$Brood_Year))
  } else {
    brood_years_min=min(year(hbbm$Date[month(hbbm$Date)>8]))
    brood_years_max=max(year(hbbm$Date[month(hbbm$Date)>8]))
  }
  brood_years = brood_years_min:brood_years_max

  # structure initial output dataframe and label columns
  output_tab = data.frame(matrix(data = NA,
                                 nrow = length(brood_years),
                                 ncol = length(output_colnames)))
  colnames(output_tab)=output_colnames
  output_tab$brood_year = brood_years
  output_tab$smolt_year = brood_years + 2

  # eliminate incomplete smolt years. Filter by number of days available in flow record.
  flow_days_avail = aggregate(hbbm$smolt_year, by = list(hbbm$smolt_year), FUN = length)
  keep_these_smolt_years = flow_days_avail$Group.1[flow_days_avail$x >= 669]
  output_tab = output_tab[output_tab$smolt_year %in% keep_these_smolt_years,]

  #2. Assign output metrics
  output_tab$chinook_spawner_abundance = chinook_spawn_and_juv$Spawning_Adult_Chinook_est[match(output_tab$brood_year,
                                                                                                chinook_spawn_and_juv$Brood_Year)] #chinook_abun$Total_Basin_estimate[match(output_tab$brood_year, chinook_abun$Year)]
  # output_tab$chinook_spawner_abundance_long = spawners$chinook_scott[match(output_tab$brood_year, spawners$year)]
  output_tab$chinook_juvenile_abundance = chinook_spawn_and_juv$Juvenile_Chinook_produced_BY_plus_1[match(output_tab$brood_year,
                                                                                                          chinook_spawn_and_juv$Brood_Year)]
  output_tab$chinook_juv_per_adult = chinook_spawn_and_juv$Chinook_juv_per_adult[match(output_tab$brood_year,
                                                                                       chinook_spawn_and_juv$Brood_Year)]
  output_tab$coho_spawner_abundance = coho_abun$Number_of_Coho[match(output_tab$brood_year, coho_abun$Return_Year)]
  output_tab$coho_smolt_abun_est = outmigs$Smolt.point.Estimate[match(output_tab$smolt_year, outmigs$Smolt.Year)]
  output_tab$coho_smolt_per_fem = smolt_per_fem$Smolts_Produced_Per_Female[match(output_tab$smolt_year, smolt_per_fem$Smolt_Year)]
  # output_tab$percent_coho_smolt_survival = outmigs$Percent.smolt.survival[match(output_tab$smolt_year, outmigs$Smolt.Year)]
  output_tab$coho_redds_in_brood = redds$total_redds[match(output_tab$brood_year, redds$water_year-1)]

  #3. Assign disconnection and reconnection dates
  # 4. Functional Flow metrics and 5. Scouring flow metrics assigned inside for loop
  for(i in 1:nrow(output_tab)){

    brood_yr = output_tab$brood_year[i]; smolt_yr = brood_yr+2

    # Subset dates for metric calcs
    # modified water year starts on Sep 01 (instead of Oct 01, which is standard)
    # to capture rare Sep storms
    wy1_date1 = as.Date(paste0(brood_yr,"-09-01"))
    wy1_date2 = as.Date(paste0(brood_yr+1, "-08-31"))
    wy1_dates = seq.Date(from=wy1_date1, to = wy1_date2, by="day")

    wy2_date1 = as.Date(paste0(brood_yr+1,"-09-01"))
    wy2_date2 = as.Date(paste0(brood_yr+2, "-08-31"))
    wy2_dates = seq.Date(from=wy2_date1, to = wy2_date2, by="day")

    # Define date ranges to look for reconnection and disconnection timing

    # reconnection in the first fall rewetting, during parents' spawning
    f1_date2_recon = as.Date(paste0(brood_yr+1,"-02-28"))
    f1_dates_recon = seq.Date(from=wy1_date1, to = f1_date2_recon, by="day")

    # disconnection in the first spring recession
    s1_date1_discon = as.Date(paste0(brood_yr+1,"-03-01"))
    s1_date2_discon = as.Date(paste0(brood_yr+1,"-08-31"))
    s1_dates_discon = seq.Date(from=s1_date1_discon, to = s1_date2_discon, by="day")

    # reconnection in the fall rewetting experienced as juvenile fish
    f2_date2_recon = as.Date(paste0(brood_yr+2,"-02-28"))
    f2_dates_recon = seq.Date(from=wy2_date1, to = f2_date2_recon, by="day")

    # disconnection in the second spring recession as outmigrating smolt
    s2_date1_discon = as.Date(paste0(brood_yr+2,"-03-01"))
    s2_date2_discon = as.Date(paste0(brood_yr+2,"-08-31"))
    s2_dates_discon = seq.Date(from=s2_date1_discon, to = s2_date2_discon, by="day")

    # Subset date range for seeking scouring flows
    w1_dates = seq.Date(from = wy1_date1, to = s1_date2_discon, by = "day")
    w2_dates = seq.Date(from = wy2_date1, to = s2_date2_discon, by = "day")

    # subset flow within date ranges for connectivity timing (fall and spring) and scouring flows (full wet season)
    f1_flow_recon = hbbm$Flow[hbbm$Date %in% f1_dates_recon & hbbm$smolt_year == smolt_yr]
    w1_flow = hbbm$Flow[hbbm$Date %in% w1_dates & hbbm$smolt_year == smolt_yr]
    s1_flow_discon = hbbm$Flow[hbbm$Date %in% s1_dates_discon & hbbm$smolt_year == smolt_yr]

    f2_flow_recon = hbbm$Flow[hbbm$Date %in% f2_dates_recon & hbbm$smolt_year == smolt_yr]
    w2_flow = hbbm$Flow[hbbm$Date %in% w2_dates & hbbm$smolt_year == smolt_yr]
    s2_flow_discon = hbbm$Flow[hbbm$Date %in% s2_dates_discon & hbbm$smolt_year == smolt_yr]

    # Calculate reconnection and disconnection timing
    for(j in 1:length(thresholds)){
      thresh = thresholds[j]
      f1_recon_day_since_aug31  = calc_recon_days_since_aug_31(dates = f1_dates_recon, flow = f1_flow_recon, recon_threshold=thresh)
      s1_discon_day_since_aug31 = calc_discon_days_since_aug_31(dates = s1_dates_discon, flow = s1_flow_discon, discon_threshold=thresh)
      f2_recon_day_since_aug31 = calc_recon_days_since_aug_31(dates = f2_dates_recon, flow = f2_flow_recon, recon_threshold=thresh)
      s2_discon_day_since_aug31 = calc_discon_days_since_aug_31(dates = s2_dates_discon, flow = s2_flow_discon, discon_threshold=thresh)

      output_tab[i, paste0("f1_recon_",  thresh)] = f1_recon_day_since_aug31
      output_tab[i, paste0("s1_discon_", thresh)] = s1_discon_day_since_aug31
      output_tab[i, paste0("f2_recon_",  thresh)] = f2_recon_day_since_aug31
      output_tab[i, paste0("s2_discon_", thresh)] = s2_discon_day_since_aug31

      # Convert infinite reconnection dates (i.e., the flow never rose above that
      # threshold in that time period) to NA values
      conn_cols = paste0(c("f1_recon_","s1_discon_",
                           "f2_recon_","s2_discon_"),  thresh)
      output_tab[i, conn_cols][!is.finite(as.numeric(output_tab[i, conn_cols]))] = NA
    }

    # 4. Assign Total Flow metric predictors (if data's availble)
    # Subset flow for TAF calcs
    # BY_flow_m3d = hbbm$Flow_m3day[hbbm$Date %in% f1_dates & hbbm$smolt_year == smolt_yr]
    # RY_flow_m3d = hbbm$Flow_m3day[hbbm$Date %in% RY_dates & hbbm$smolt_year == smolt_yr]
    # SY_flow_m3d = hbbm$Flow_m3day[hbbm$Date %in% SY_dates & hbbm$smolt_year == smolt_yr]

    # n_BY = length(BY_flow_m3d); n_RY = length(RY_flow_m3d); n_SY = length(SY_flow_m3d)
    # n_CFLP = sum(hbbm$smolt_year == smolt_yr)
    # if(n_BY==122){
    #   output_tab[i,"BY_min_flow_sepdec"] =  min(BY_flow_m3d)
    #   output_tab[i, "BY_tot_flow_sepdec"] = sum(BY_flow_m3d) / (10^6)
    #   output_tab[i, "log_BY_tot_flow_sepdec"] = log10(sum(BY_flow_m3d))
    # }
    # if(n_RY>=365){
    #   output_tab[i,"RY_min_flow"] =  min(RY_flow_m3d)
    #   output_tab[i, "RY_tot_flow"] = sum(RY_flow_m3d) / (10^6)
    #   output_tab[i, "log_RY_tot_flow"] = log10(sum(RY_flow_m3d))
    # }
    # if(n_SY>=182){
    #   output_tab[i,"SY_min_flow_janjul"] =  min(SY_flow_m3d)
    #   output_tab[i, "SY_tot_flow_janjul"] = sum(SY_flow_m3d) / (10^6)
    #   output_tab[i, "log_SY_tot_flow_janjul"] = log10(sum(SY_flow_m3d))
    # }
    # if(n_CFLP>=669){
    #   output_tab[i, "tot_flow_CFLP"] = sum(c(BY_flow_m3d, RY_flow_m3d, SY_flow_m3d)) / (10^6)
    #   output_tab[i, "log_tot_flow_CFLP"] = log10(sum(c(BY_flow_m3d, RY_flow_m3d, SY_flow_m3d)))
    # }

    #5. Assign Functional Flow metric predictors
    ## d1 - FFs for dry season preceding parents' spawning
    output_tab[i, "d1_DS_Dur_WS"] = fflows$DS_Dur_WS[fflows$Water_Year == brood_yr] # match up brood year to water year
    output_tab[i, "d1_DS_Tim"] = fflows$DS_Tim[fflows$Water_Year == brood_yr] # match up brood year to water year
    output_tab[i, "d1_DS_Mag_50"] = fflows$DS_Mag_50[fflows$Water_Year == brood_yr] # match up brood year to water year
    output_tab[i, "d1_DS_Mag_90"] = fflows$DS_Mag_90[fflows$Water_Year == brood_yr] # match up brood year to water year

    ## f1 - FFs for fall rewetting transition during parents' spawning
    # if(n_BY==122){
    output_tab[i, "f1_FA_Mag"] = fflows$FA_Mag[fflows$Water_Year == brood_yr + 1] # match up brood year to water year
    output_tab[i, "f1_FA_Tim"] = fflows$FA_Tim[fflows$Water_Year == brood_yr + 1] # match up brood year to water year
    output_tab[i, "f1_FA_Dur"] = fflows$FA_Dur[fflows$Water_Year == brood_yr + 1] # match up brood year to water year
    output_tab[i, "f1_FA_Dif_num"] = fflows$FA_Dif_num[fflows$Water_Year == brood_yr + 1] # match up brood year to water year
    # }
    ## w1 - FFs for first wet season as egg and fry
    # if(n_RY>=365){
    output_tab[i, "w1_Wet_BFL_Dur"] = fflows$Wet_BFL_Dur[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Wet_BFL_Mag_10"] = fflows$Wet_BFL_Mag_10[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Wet_BFL_Mag_50"] = fflows$Wet_BFL_Mag_50[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Wet_Tim"] = fflows$Wet_Tim[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_10"] = fflows$Peak_10[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_5"] = fflows$Peak_5[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_2"] = fflows$Peak_2[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Dur_10"] = fflows$Peak_Dur_10[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Dur_5"] = fflows$Peak_Dur_5[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Dur_2"] = fflows$Peak_Dur_2[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Fre_10"] = fflows$Peak_Fre_10[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Fre_5"] = fflows$Peak_Fre_5[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Fre_2"] = fflows$Peak_Fre_2[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Tim_10"] = fflows$Peak_Tim_10[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Tim_5"] = fflows$Peak_Tim_5[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Tim_2"] = fflows$Peak_Tim_2[fflows$Water_Year == brood_yr + 1]

    ## s1 - FFs for first spring season as juvenile fish
    output_tab[i, "s1_SP_Dur"] = fflows$SP_Dur[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "s1_SP_Mag"] = fflows$SP_Mag[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "s1_SP_Tim"] = fflows$SP_Tim[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "s1_SP_ROC"] = fflows$SP_ROC[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "s1_SP_ROC_Max"] = fflows$SP_ROC_Max[fflows$Water_Year == brood_yr + 1]

    # if(is.element(el=brood_yr+2, set = fflows$Water_Year)){
    ## d2 - FFs for dry season as juvenile fish
    output_tab[i, "d2_DS_Dur_WS"] = fflows$DS_Dur_WS[fflows$Water_Year == brood_yr +1] # match up brood year to water year
    output_tab[i, "d2_DS_Tim"] = fflows$DS_Tim[fflows$Water_Year == brood_yr +1] # match up brood year to water year
    output_tab[i, "d2_DS_Mag_50"] = fflows$DS_Mag_50[fflows$Water_Year == brood_yr +1] # match up brood year to water year
    output_tab[i, "d2_DS_Mag_90"] = fflows$DS_Mag_90[fflows$Water_Year == brood_yr +1] # match up brood year to water year
    # }
    # FFs for fall pulse as juvenile fish
    output_tab[i, "f2_FA_Mag"] = fflows$FA_Mag[fflows$Water_Year == brood_yr + 2] # match up brood year to water year
    output_tab[i, "f2_FA_Tim"] = fflows$FA_Tim[fflows$Water_Year == brood_yr + 2] # match up brood year to water year
    output_tab[i, "f2_FA_Dur"] = fflows$FA_Dur[fflows$Water_Year == brood_yr + 2] # match up brood year to water year
    output_tab[i, "f2_FA_Dif_num"] = fflows$FA_Dif_num[fflows$Water_Year == brood_yr + 2] # match up brood year to water year

    # }
    # w2 - FFs during second winter as overwintering juveniles
    output_tab[i, "w2_Wet_BFL_Dur"] = fflows$Wet_BFL_Dur[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Wet_BFL_Mag_10"] = fflows$Wet_BFL_Mag_10[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Wet_BFL_Mag_50"] = fflows$Wet_BFL_Mag_50[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Wet_Tim"] = fflows$Wet_Tim[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_10"] = fflows$Peak_10[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_5"] = fflows$Peak_5[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_2"] = fflows$Peak_2[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Dur_10"] = fflows$Peak_Dur_10[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Dur_5"] = fflows$Peak_Dur_5[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Dur_2"] = fflows$Peak_Dur_2[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Fre_10"] = fflows$Peak_Fre_10[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Fre_5"] = fflows$Peak_Fre_5[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Fre_2"] = fflows$Peak_Fre_2[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Tim_10"] = fflows$Peak_Tim_10[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Tim_5"] = fflows$Peak_Tim_5[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Tim_2"] = fflows$Peak_Tim_2[fflows$Water_Year == brood_yr + 2]

    ## s2 - FFs for second spring season as outmigrating smolt
    output_tab[i, "s2_SP_Dur"] = fflows$SP_Dur[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "s2_SP_Mag"] = fflows$SP_Mag[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "s2_SP_Tim"] = fflows$SP_Tim[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "s2_SP_ROC"] = fflows$SP_ROC[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "s2_SP_ROC_Max"] = fflows$SP_ROC_Max[fflows$Water_Year == brood_yr + 2]

    output_tab[i, "wy1_Mean_Ann_Flow"] = fflows$Mean_Ann_Flow[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "wy2_Mean_Ann_Flow"] = fflows$Mean_Ann_Flow[fflows$Water_Year == brood_yr + 2]
    output_tab[i, "wy1_WY_Cat"] = fflows$WY_Cat[fflows$Water_Year == brood_yr + 1]
    output_tab[i, "wy2_WY_Cat"] = fflows$WY_Cat[fflows$Water_Year == brood_yr + 2]

    # 6. Storm days
    cfs_90th_pctile = quantile(fj_flow$Flow, 0.9)
    output_tab[i, "w1_num_days_gt_90_pctile"] = sum(w1_flow > cfs_90th_pctile)
    output_tab[i, "w2_num_days_gt_90_pctile"] = sum(w2_flow > cfs_90th_pctile)

  }

  #remove SY discon 20, since that usually happens after the smolt leave the watershed
  problem_discons = paste("s2_discon", thresholds[thresholds < 60], sep = "_")
  output_tab = output_tab[,!(colnames(output_tab) %in% problem_discons)]

  return(output_tab)
}

zscore_column_na_rm = function(x){
  mean_x = mean(x, na.rm=T)
  sd_x = sd(x, na.rm=T)
  return((x - mean_x) / sd_x)
}


# Correlations ------------------------------------------------------------

calc_corr_matrix=function(metrics_tab,
                          preds_all,
                          corr_method = "pearson",
                          min_pairs = 10,
                          fish_outcome_cols = NA){

  if(sum(is.na(fish_outcome_cols)) >0){
    # Reorder fish outcome columns
    fish_outcome_cols = c("chinook_juvenile_abundance", "chinook_spawner_abundance", "chinook_juv_per_adult",
                          "coho_smolt_abun_est", "coho_spawner_abundance", "coho_smolt_per_fem", "coho_redds_in_brood")
  } #else if(fish_outcome_cols == "coho" & fish_outcome_cols != "chinook"){
  #   fish_outcome_cols = c("coho_spawner_abundance", "coho_redds_in_brood",
  #                         "coho_smolt_abun_est", "coho_smolt_per_fem")
  # } else if(fish_outcome_cols == "chinook" & fish_outcome_cols != "coho"){
  #   fish_outcome_cols = colnames(metrics_tab)[grepl(pattern = "chinook", x = colnames(metrics_tab))]
  # }

  predictor_cols = colnames(metrics_tab)[colnames(metrics_tab) %in% preds_all]

  #Initialize output matrix
  output_tab = data.frame(matrix(data=NA, nrow = length(predictor_cols),
                                 ncol = length(fish_outcome_cols)))
  for(j in 1:length(fish_outcome_cols)){
    outcome_col = fish_outcome_cols[j]
    y = metrics_tab[,outcome_col]
    for(i in 1:length(predictor_cols)){
      predictor_col = predictor_cols[i]
      x = metrics_tab[,predictor_col]

      if(sum(!is.na(x*y)) >= min_pairs){
        output_tab[i, j] = cor(x = x, y = y, use = "pairwise.complete.obs", method = corr_method)
      }
    }
  }
  # assign row and column names
  colnames(output_tab) = fish_outcome_cols
  rownames(output_tab) = predictor_cols

  # remove NA values (currently just peak flood metrics. They have too few obs. to make a corr calculation)
  output_tab = output_tab[!is.na(output_tab[,1]),]

  # clear out metrics in RY and SY that don't affect the number of spawners (in BY)
  # spawning occurs affected by preceding dry season and first fall rewetting
  d1_f1_rows = grepl(pattern = "d1", x = row.names(output_tab)) |
    grepl(pattern = "f1", x = row.names(output_tab))
  # coho redd observations probably affected by d1 and f1 plus first wet season conditions
  w1_rows = grepl(pattern = "w1", x = row.names(output_tab))
  # outmigrating Chinook smolt not affected by year 2, starting with dry season 2
  wy2_rows = grepl(pattern = "d2", x = row.names(output_tab)) |
    grepl(pattern = "f2", x = row.names(output_tab))|
    grepl(pattern = "w2", x = row.names(output_tab)) |
    grepl(pattern = "s2", x = row.names(output_tab)) |
    grepl(pattern = "wy2", x = row.names(output_tab))


  # ID rows with spawner abundance. keep all columns of corr. for spawners except for with itself
  spawn_co = rownames(output_tab) == "coho_spawners_zscored"
  spawn_ch = rownames(output_tab) == "chinook_spawners_zscored"
  spawn_rows = spawn_co | spawn_ch

  output_tab$chinook_spawner_abundance[!(d1_f1_rows | spawn_co)] = NA # keep only d1 and f1
  output_tab$chinook_juvenile_abundance[wy2_rows] = NA # exclude wy2
  output_tab$chinook_juv_per_adult[wy2_rows] = NA # exclude wy2
  output_tab$coho_spawner_abundance[!(d1_f1_rows | spawn_ch)] = NA # keep only d1 and f1
  output_tab$coho_redds_in_brood[
    ! (d1_f1_rows | w1_rows | spawn_rows)] = NA # keep only d1, f1,  w1, and spawners

  return(output_tab)
}



corr_matrix_fig_2 = function(corr_matrix, pred_subset, preds_in_order){

  pred_subset_fac = factor(pred_subset, levels = preds_in_order)
  pred_subset_in_order = sort(pred_subset_fac)
  ## Prepare for plotting
  #Prettify column names
  colname_matching_df = data.frame(data_name = c("coho_smolt_per_fem",
                                                 "chinook_juv_per_adult",
                                                 "coho_spawner_abundance",
                                                 "coho_redds_in_brood",
                                                 "coho_smolt_abun_est",
                                                 "chinook_spawner_abundance",
                                                 "chinook_juvenile_abundance"
                                                 # "percent_coho_smolt_survival",
  ),
  fig_name = c("Coho spf",
               "Chinook jpa",
               "Num. Co. Spawners",
               "Num. Co. Redds",
               "Co. Juv. Abun. Est.",
               "Num. Ch. Spawners",
               "Num. Ch. Juveniles"
               # "% Co. Smolt Survival"
  ))

  # corr_matrix1 = corr_matrix[row.names(corr_matrix) %in% pred_subset_in_order, ]
  corr_matrix1 = corr_matrix[match(pred_subset_in_order, rownames(corr_matrix)),]
  colnames(corr_matrix1) = colname_matching_df$fig_name[match(colnames(corr_matrix1),
                                                             colname_matching_df$data_name)]

    rownames(corr_matrix1)[rownames(corr_matrix1) %in% colname_matching_df$data_name] =
      colname_matching_df$fig_name[match(rownames(corr_matrix1),
                                         colname_matching_df$data_name)]


  if(length(pred_subset)>10){
    corrplot(as.matrix(corr_matrix1), addCoef.col = "black",
             tl.offset = .8, cl.offset = 3, cl.ratio = .3,
             number.cex = .7, tl.cex = .8, cl.cex = 1, number.font = .8,
             xlab = "", ylab = "", main = "", na.label = "--",
             # axis.row = list(side = 2, las = 1), axis.col = list(side = 1, las = 2),
             # col = c("orangered3", "lightpink", "lightskyblue","deepskyblue4"),
             col = c(rep("orangered3"), rep("lightpink",2),rep("mistyrose",2),
                     # rep("white",2),
                     rep("lightcyan",2),
                     rep("lightskyblue",2),rep("deepskyblue4")),
             cl.pos = "r")
  } else {
    if(length(pred_subset)>10){
      corrplot(as.matrix(corr_matrix1), addCoef.col = "black",
               xlab = "", ylab = "", main = "", na.label = "--",
               axis.row = list(side = 2, las = 1), axis.col = list(side = 1, las = 2),
               # col = c("orangered3", "lightpink", "lightskyblue","deepskyblue4"),
               col = c(rep("orangered3"), rep("lightpink",2),rep("mistyrose",2),
                       # rep("white",2),
                       rep("lightcyan",2),
                       rep("lightskyblue",2),rep("deepskyblue4")),
               cl.pos = "b")
    }
  }

    vert_line_y1 = length(pred_subset)+0.5
    # arrows(x0=3.5, x1=3.5, y0=0.5, y1=vert_line_y1,
    # separate two normalized metrics from other metrics
    arrows(x0=3.5, x1=3.5, y0=0.5, y1=vert_line_y1, length=0, lwd=2)
    #separate chinook from coho metrics
    # arrows(x0=5.5, x1=5.5, y0=0.5, y1=vert_line_y1, length=0, lwd=2)
    # if(predict_eco == "juvenile abundance"){
    h_y = nrow(corr_matrix)-2+0.5 # horiz. line under the 2 spawner rows
    x1_preds = ncol(corr_matrix1) + .5
    arrows(x0 = 0.5, x1 = x1_preds, y0 = h_y, y1 = h_y, length = 0, lwd = 2)
    # }

}

# corr_matrix_fig_spawners_as_x = function(corr_matrix, pred_subset, preds_in_order){
#   pred_subset_fac = factor(pred_subset, levels = preds_in_order)
#   pred_subset_in_order = sort(pred_subset_fac)
#   ## Prepare for plotting
#   #Prettify column names
#   colname_matching_df = data.frame(data_name = c("coho_smolt_per_fem",
#                                                  "chinook_juv_per_adult",
#                                                  "coho_spawner_abundance",
#                                                  "coho_redds_in_brood",
#                                                  "coho_smolt_abun_est",
#                                                  "chinook_spawner_abundance",
#                                                  "chinook_juvenile_abundance"
#                                                  # "percent_coho_smolt_survival",
#   ),
#   fig_name = c("Coho spf",
#                "Chinook jpa",
#                "Num. Co. Spawners",
#                "Num. Co. Redds",
#                "Co. Juv. Abun. Est.",
#                "Num. Ch. Spawners",
#                "Num. Ch. Juveniles"
#                # "% Co. Smolt Survival"
#   ))
#
#   # corr_matrix1 = corr_matrix[row.names(corr_matrix) %in% pred_subset_in_order, ]
#   corr_matrix1 = corr_matrix[match(pred_subset_in_order, rownames(corr_matrix)),]
#   colnames(corr_matrix1) = colname_matching_df$fig_name[match(colnames(corr_matrix1),
#                                                               colname_matching_df$data_name)]
#
#
#   if(length(pred_subset)>10){
#     corrplot(as.matrix(corr_matrix1), addCoef.col = "black",
#              tl.offset = .8, cl.offset = 3, cl.ratio = .3,
#              number.cex = .8, tl.cex = .8, cl.cex = 1, number.font = .8,
#              xlab = "", ylab = "", main = "", na.label = "--",
#              # axis.row = list(side = 2, las = 1), axis.col = list(side = 1, las = 2),
#              # col = c("orangered3", "lightpink", "lightskyblue","deepskyblue4"),
#              col = c(rep("orangered3"), rep("lightpink",2),rep("mistyrose",2),
#                      # rep("white",2),
#                      rep("lightcyan",2),
#                      rep("lightskyblue",2),rep("deepskyblue4")),
#              cl.pos = "r")
#   } else {
#     if(length(pred_subset)>10){
#       corrplot(as.matrix(corr_matrix1), addCoef.col = "black",
#                xlab = "", ylab = "", main = "", na.label = "--",
#                axis.row = list(side = 2, las = 1), axis.col = list(side = 1, las = 2),
#                # col = c("orangered3", "lightpink", "lightskyblue","deepskyblue4"),
#                col = c(rep("orangered3"), rep("lightpink",2),rep("mistyrose",2),
#                        # rep("white",2),
#                        rep("lightcyan",2),
#                        rep("lightskyblue",2),rep("deepskyblue4")),
#                cl.pos = "b")
#     }
#   }
#
#   vert_line_y1 = length(pred_subset)+0.5
#   # arrows(x0=3.5, x1=3.5, y0=0.5, y1=vert_line_y1,
#   # separate two normalized metrics from other metrics
#   arrows(x0=2.5, x1=2.5, y0=0.5, y1=vert_line_y1, length=0, lwd=2)
#   #separate chinook from coho metrics
#   arrows(x0=5.5, x1=5.5, y0=0.5, y1=vert_line_y1, length=0, lwd=2)
#
# }

collinear_screening_exercise = function(metrics_tab, corr_tab_all_metrics,
                                        minimum_years = 10,
                                        high_corr_threshold = 0.7,
                                        zscore_flow_metrics = T,
                                        return_tab = "elim_pred_tab"){


  # Exclude predictors with too small a sample size
  mt_years_count = apply(X=metrics_tab, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  # minimum_years = 10
  too_few_years = names(mt_years_count)[mt_years_count < minimum_years]

  # Select high correlation threshold (0.7 after Baruch et al 2024)
  high_corr_threshold = 0.7

  # Make new table of corr. coeffs, excluding index years, eco responses,
  # and predictors with too-small sample size
  corr_tab_screener = corr_tab_all_metrics
  diag(corr_tab_screener) = NA # exclude 1.0 R values along diagonal

  exclude_these = colnames(corr_tab_screener) %in% too_few_years +
    colnames(corr_tab_screener) %in% c("brood_year","smolt_year") +
    grepl(pattern = "coho", x = colnames(corr_tab_screener)) |
    grepl(pattern = "chinook", x = colnames(corr_tab_screener))

  corr_tab_screener = corr_tab_screener[!exclude_these, ]
  corr_tab_screener = corr_tab_screener[ ,!exclude_these]

  # Convert remaining corr. coefs into a long-form table
  corr_tab_long = melt(corr_tab_screener)
  ctl = corr_tab_long
  ctl = ctl[!(is.na(ctl$value)),] # remove NA values

  # explore the predictors correlated with the highest number of other predictors

  # To find the most parsimonious explainer, pick one predictor from each group of
  # collinears, and eliminate the remaining ones until no abs(R)> threshold remain

  # Sort predictors according to highest number of collinears > threshold
  preds_sort = sort(table(ctl$Var1[abs(ctl$value) > high_corr_threshold]), decreasing = T)
  # identify the predictors collinear with the top-correlated predictor
  collin_preds = preds_sort[preds_sort > 0]
  # Initialize the while loop and structures to record the collinear groups
  loop = 1; collinear_group = list(); selected_pred = list()

  while(length(collin_preds)>0){
    most_collin_pred = names(collin_preds[1]) # pick predicter with highest number of collinears
    collinear_with = unique(ctl$Var2[ctl$Var1 == most_collin_pred &
                                       abs(ctl$value) > high_corr_threshold])

    # Record the groups of collinear predictors and the ultimate selected one
    if(zscore_flow_metrics == F){
      collinear_group[[loop]] = c(most_collin_pred, as.character(collinear_with))
      if(loop==1){selected_pred[[loop]] = "w1_Wet_BFL_Mag_50"} # How wet the wet season (year 1)
      if(loop==2){selected_pred[[loop]] = "w2_Wet_BFL_Mag_50"} # How wet the wet season (year 2)
      if(loop==3){selected_pred[[loop]] = "d1_DS_Mag_50"} # How dry the dry season (pre-spawning)
      if(loop==4){selected_pred[[loop]] = "w1_Wet_BFL_Dur"} # How long the wet season (rearing juv)
      if(loop==5){selected_pred[[loop]] = "w2_Wet_BFL_Dur"} # Dry to wet transition timing (rearing juv)
      if(loop==6){selected_pred[[loop]] = "d1_DS_Dur_WS"} # Fall pulse magnitude during spawning (diff has higher sample size)
      if(loop==7){selected_pred[[loop]] = "f1_FA_Mag"} # Fall pulse magnitude (rearing juv) (diff has higher sample size)
      if(loop==8){selected_pred[[loop]] = "f2_FA_Mag"} # Fall pulse magnitude (rearing juv) (diff has higher sample size)
    }

    if( zscore_flow_metrics == T){
      collinear_group[[loop]] = c(most_collin_pred, as.character(collinear_with))
      collinear_group[[loop]]
      loop
      if(loop==1){selected_pred[[loop]] = "w1_Wet_BFL_Mag_50"} # How wet the wet season (year 1)
      if(loop==2){selected_pred[[loop]] = "w2_Wet_BFL_Mag_50"} # How wet the wet season (year 2)
      if(loop==3){selected_pred[[loop]] = "d1_DS_Mag_50"} # How dry the dry season (pre-spawning)
      if(loop==4){selected_pred[[loop]] = "w2_Wet_Tim"} # Dry to wet transition timing (rearing juv)
      if(loop==5){selected_pred[[loop]] = "w1_Wet_BFL_Dur"} # How long the first wet season
      if(loop==6){selected_pred[[loop]] = "f1_FA_Dif_num"}#"f1_FA_Mag"} # Fall pulse magnitude (rearing juv) (diff has higher sample size)
      if(loop==7){selected_pred[[loop]] = "f2_FA_Dif_num"}#"f2_FA_Mag"} # Fall pulse magnitude (rearing juv) (diff has higher sample size)
      selected_pred[[loop]]
    }

    # Remove the other collinears
    keep = selected_pred[[loop]] # Keep this one
    dont_keep = collinear_group[[loop]][collinear_group[[loop]] != keep] # exclude these ones
    ctl = ctl[!(ctl$Var1 %in% dont_keep |
                  ctl$Var2 %in% dont_keep),]
    # Recalculate the number of remaining collinear predictors with R > threshold
    preds_sort = sort(table(ctl$Var1[abs(ctl$value) > high_corr_threshold]), decreasing = T)
    collin_preds = preds_sort[preds_sort>0]
    loop = loop + 1
    loop
  }

  # Restructure the groups and eliminated predictors in a table
  elim_pred_tab_col1 = unlist(lapply(X=collinear_group, FUN = function(x){paste(x, collapse = ", ")}))
  if(zscore_flow_metrics == F){
    elim_pred_tab_col2 = c("How wet was the wet season? (year 1, as eggs and fry)",
                           "How wet was the wet season? (year 2, as rearing juv.)",
                           "How dry was the dry season? (pre-spawning)",
                           "How long was the wet season? (year 1, as eggs and fry)",
                           "How long was the wet season? (year 2, as rearing juv.)",
                           "How long was the dry season? (pre-spawning)",
                           "Fall pulse magnitude (year 1, during parents' spawning)",
                           "Fall pulse magnitude (year 2, as rearing juv.)")
  }
  if(zscore_flow_metrics == T){
    elim_pred_tab_col2 = c("How wet was the wet season? (year 1, as eggs and fry)",
                           "How wet was the wet season? (year 2, as rearing juv.)",
                           "How dry was the dry season? (pre-spawning",
                           "Dry to wet season transition timing (as rearing juv.)",
                           "How long was the wet season (as eggs and fry)",
                           "Fall pulse magnitude (parents' spawning)",
                           "Fall pulse magnitude (rearing juv.)")
  }


  elim_pred_tab_col3 = unlist(selected_pred)

  elim_pred_tab = data.frame(collin_group=elim_pred_tab_col1,
                             interp = elim_pred_tab_col2,
                             selected_pred = elim_pred_tab_col3)

  if(return_tab == "elim_pred_tab"){return(elim_pred_tab)}
  if(return_tab == "corr_tab_long_screened"){return(ctl)}

}

# Lasso and Ridge Regression ----------------------------------------------

kfold_cv = function(mt, y_val = "coho_smolt_per_fem",
                    alpha = 0,
                    return_mod = T, return_cv = T){
  x_and_y = get_refined_x_and_y_for_lasso_mod(mt = mt, y_val = y_val)
  x = x_and_y[[1]]; y = x_and_y[[2]]
  # n_folds_for_groups = floor(nrow(x)/3) # make sure at least 3 in each fold
  n_folds_for_groups = nrow(x) # leave one out cross validation
  # n_folds_for_groups = 10
  set.seed(1)
  mod1 = glmnet(x, y, alpha = alpha)
  cv1 = cv.glmnet(x = x, y = y, nfolds = n_folds_for_groups)
  if(return_mod == T & return_cv ==T){return(list(mod = mod1, cv = cv1))}
}


kfold_cv_plus_spawn = function(mt,
                               y_val = "coho_smolt_per_fem",
                               alpha = 0,
                               return_mod = T, return_cv = T){
  x_and_y = get_refined_x_and_y_for_lasso_mod_plus_spawn(mt = mt, y_val = y_val)
  x = x_and_y[[1]]; y = x_and_y[[2]]
  # n_folds_for_groups = floor(nrow(x)/3) # make sure at least 3 in each fold
  n_folds_for_groups = nrow(x) # leave one out cross validation
  # n_folds_for_groups = 10
  set.seed(1)
  mod1 = glmnet(x, y, alpha = alpha)
  cv1 = cv.glmnet(x = x, y = y, nfolds = n_folds_for_groups)
  if(return_mod == T & return_cv ==T){return(list(mod = mod1, cv = cv1))}
}

get_pred_coefs = function(mod, cv, coef_digits = 2, alt_lambda = NA){
  if(is.na(alt_lambda)){s_for_coef = cv$lambda.min} else {s_for_coef = alt_lambda}
  pred = as.matrix(predict.glmnet(mod, s = s_for_coef,
                                  type = "coefficients"))
  coef_all = data.frame(pred = rownames(pred)[order(abs(pred), decreasing = T)],
                    coef = round(as.numeric(pred[order(abs(pred), decreasing = T)]), coef_digits))
  int = coef_all$coef[coef_all$pred=="(Intercept)"]
  int_and_non0 = coef_all[abs(coef_all$coef)>0,]
  coef_non0_only = int_and_non0$coef[int_and_non0$pred !="(Intercept)"]
  names(coef_non0_only) = int_and_non0$pred[abs(int_and_non0$coef)>0  &
                                              int_and_non0$pred !="(Intercept)"]
  return(list(coef_all = coef_all,
              coef_non0_only = coef_non0_only,
              intercept = int,
              int_and_coefs = int_and_non0))
}


lasso_results = function(cv_co, cv_ch, mod_co, mod_ch, alt_lambda_co=NA){
  par(mfrow = c(3,2))
  # Row 1: CV error and min lambda
  plot(cv_co); grid()
  title(main = yvlt$y_val_label[yvlt$y_val==y_val_coho], line = 3)
  abline(v=log(cv_co$lambda.min), lty = 2, lwd = 2, col = "brown")
  abline(v = log(alt_lambda_co), lty = 3, lwd = 3, col = "dodgerblue")
  legend(x = "topright", legend = "A", bty = "n") # panel label

  plot(cv_ch); grid()
  title(main = yvlt$y_val_label[yvlt$y_val==y_val_chinook], line = 3)
  abline(v=log(cv_ch$lambda.min), lty = 2, lwd = 2, col = "brown")
  legend(x = "topright", legend = "B", bty = "n") # panel label


  #Row 2: variance explained
  plot(log(mod_co$lambda), mod_co$dev.ratio, type = "l", ylim = c(0,1),
       xlab = expression(Log(lambda)), ylab = "Fraction of null deviance explained")
  grid()
  abline(v=log(cv_co$lambda.min), lty = 2, lwd = 2, col = "brown")
  abline(v = log(alt_lambda_co), lty = 3, lwd = 3, col = "dodgerblue")
  if(!is.na(alt_lambda_co)){
    legend(x="bottomleft", lwd = 2, lty = c(2,3), col= c("brown", "dodgerblue"),
           legend = c(expression(Min.~err.~lambda~from~CV), expression(Alt.~low~err.~lambda)))
  } else {
    legend(x="bottomleft", lwd = 2, lty = 2, col= c("brown"),
           legend = expression(Min.~err.~lambda~from~CV))
  }

  legend(x = "topright", legend = "C", bty = "n") # panel label

  plot(log(mod_ch$lambda), mod_ch$dev.ratio, type = "l", ylim = c(0,1),
       xlab = expression(Log(lambda)), ylab = "Fraction of null deviance explained")
  grid()
  abline(v=log(cv_ch$lambda.min), lty = 2, lwd = 2, col = "brown")
  legend(x = "topright", legend = "D", bty = "n") # panel label


  #Row 3: coefficients
  plot(mod_co, xvar = "lambda", xlab = expression(Log(lambda))); grid()
  abline(v=log(cv_co$lambda.min), lty = 2, lwd = 2, col = "brown")
  abline(v = log(alt_lambda_co), lty = 3, lwd = 3, col = "dodgerblue")
  legend(x = "topright", legend = "E", bty = "n") # panel label

  plot(mod_ch, xvar = "lambda", xlab = expression(Log(lambda))); grid()
  abline(v=log(cv_ch$lambda.min), lty = 2, lwd = 2, col = "brown")
  legend(x = "topright", legend = "F", bty = "n") # panel label
}


# # Lasso Regression old -----------------------------------------------
#
#
# generate_pred_appear_tab = function(lasso_mod, best_lam_range){
#   coefs = as.data.frame((as.matrix(coef(lasso_mod))))
#   non0_coef_lambda_index = apply(X = coefs, MARGIN = 1, function(x){min(which(abs(x)>0))})
#   coefs$lambda_val_coef_appears = best_lam_range[non0_coef_lambda_index]
#   coef_lambda_non0_vals = as.data.frame(cbind(rownames(coefs), coefs$lambda_val_coef_appears))
#   colnames(coef_lambda_non0_vals) = c("predictor","lambda_non0_val_appears")
#   coef_lambda_non0_vals$lambda_non0_val_appears = as.numeric(coef_lambda_non0_vals$lambda_non0_val_appears)
#   # View predictors in order of when they enter the regression by increasing lambda value
#   pred_appear_tab = coef_lambda_non0_vals[order(coef_lambda_non0_vals$lambda_non0_val_appears,
#                                                 decreasing=T),]
#   pred_appear_tab = pred_appear_tab[!pred_appear_tab$predictor=="(Intercept)",]
#
#   # # CURRENTLY HERE.
#   # degfree_diff = diff(lasso_mod$df)
#   # # CASE FOR DIFF = 1
#   # coef_appear_index = which(degfree_diff==1)
#   # lasso_mod$dev.ratio[coef_appear_index]
#   # deg_free_for_tab = lasso_mod$df[c(coef_appear_index[-1], length(lasso_mod$df))]
#   # dev_for_tab = lasso_mod$dev.ratio[c(coef_appear_index[-1], length(lasso_mod$df))]
#   # dev_tab = data.frame(deg_free = deg_free_for_tab, max_dev_expl = dev_for_tab)
#   # dev_tab$incremental_dev = NA
#   # dev_tab$incremental_dev[1] = dev_tab$max_dev_expl[1]
#   # dev_tab$incremental_dev[-1] = dev_tab$max_dev_expl[-1] - dev_tab$max_dev_expl[-nrow(dev_tab)]
#   # dev_tab$max_dev_expl=round(dev_tab$max_dev_expl, 2)
#   # dev_tab$incremental_dev=round(dev_tab$incremental_dev, 2)
#
#   # if(sum(diff(deg_free_for_tab)>1) >0){
#   #   # CASE FOR DIFF = 2 or more (split difference in deviation between 2 predictors)
#   #   need_these = setdiff(seq(from=min(dev_tab$deg_free), to = max(dev_tab$deg_free)),
#   #                        dev_tab$deg_free)
#   #   dev_tab_2 = data.frame(deg_free = need_these, max_dev_expl = NA)
#   #   dev_diff = diff()
#   # }
#
#   # lasso_mod$df[which(degfree_diff==2)]
#
#
#   return(pred_appear_tab)
# }
#
# find_all_best_lambda_vals = function(com_tab, x, y,
#                                      lam_vals = 10^seq(-2, 5,length=100),
#                                      output = "lambda_and_rmse_tab",
#                                      outer_best_lam_range = NA){
#   #Initialize output tab
#   output_tab = as.data.frame(com_tab)
#   if(output == "lambda_and_rmse_tab"){output_tab$bestlam = NA; output_tab$rmse = NA; output_tab$bestlam_numpred = NA}
#   # columns for top predictors
#   if(output == "predictor_rank"){
#     output_tab$pred1=NA; output_tab$pred2=NA;output_tab$pred3=NA; output_tab$pred4=NA; output_tab$pred5=NA
#   }
#
#   #Iterate through test-train sets
#   for(i in 1:nrow(com_tab)){
#     test = com_tab[i,]
#     train = -com_tab[i,]
#     y.test = y[test]
#
#     if(output == "lambda_and_rmse_tab"){
#       lasso_i = glmnet(x[train,], y[train], alpha = 1, lambda = lam_vals)
#       set.seed(1)
#       cv.out=cv.glmnet(x[train,], y[train], alpha = 1)
#       output_tab$bestlam[i] = cv.out$lambda.min
#       # calculate an RMSE for this test set
#       lasso.pred=predict(lasso_i, s=output_tab$bestlam[i], newx=x[test,])
#       output_tab$rmse[i] = sqrt(mean((lasso.pred-y.test)^2))
#       # ID number of non0 coefs for this optimal lambda val:
#       # calculate regression model for the single optimal lambda value
#       lasso_i_opt = glmnet(x[train,], y[train], alpha = 1, lambda = cv.out$lambda.min)
#       output_tab$bestlam_numpred[i] = lasso_i_opt$df # assign degrees of freedom
#     }
#
#     if(output == "predictor_rank"){
#       lasso_i = glmnet(x[train,], y[train], alpha = 1, lambda = outer_best_lam_range)
#       pred_appear_tab = generate_pred_appear_tab(lasso_mod = lasso_i, best_lam_range = outer_best_lam_range)
#
#       for(j in 1:5){
#         output_tab[i,paste0("pred",j)] = pred_appear_tab$predictor[j]
#       }
#     }
#   }
#
#   return(output_tab)
# }
#
# plot_lasso_diagnostics = function(x, y, best_lam_range, lambdas_and_rmse,
#                                   selected_lam, alpha_val=0.05){  # Plot lasso diagnostics.
#   # A) Plot lambda vs test error - setup
#   nbreaks = 25
#   bin_centers = seq(from=min(lambdas_and_rmse$bestlam, na.rm=T),
#                     to = max(lambdas_and_rmse$bestlam, na.rm=T),
#                     length.out=nbreaks-1)
#   categs = cut(x = lambdas_and_rmse$bestlam, breaks = length(bin_centers))
#   lambdas_and_rmse$rmse[is.infinite(lambdas_and_rmse$rmse)]= NA # remove inf values
#   # avg_by_bin = aggregate(lambdas_and_rmse$rmse, by = list(categs), FUN=mean, na.rm=T) # arithmetic mean
#   avg_by_bin = aggregate(x=lambdas_and_rmse$rmse, by = list(categs),
#                          function(x){exp(mean(log(x)))}) # geometric mean
#   # plot
#   plot(lambdas_and_rmse$bestlam,
#        lambdas_and_rmse$rmse/mean(y),
#        pch = 19, col = rgb(0.5,0.5,0.5,alpha_val*3),
#        main = "Test error in models for 50% test-train subsets",
#        xlab = "Lambda value (shrinkage penalty)",
#        # log="y",
#        ylab = paste("Rel. test error (rel. RMSE) of models made \n from different combinations of data points"))
#   # summarize by binning
#   grid()
#   abline(v=selected_lam, lty = 2, col = lam_color)
#   points(bin_centers, avg_by_bin$x/mean(y), pch=23, cex = 1.2, bg = "firebrick", type = "o")
#   legend(x="topright", col = c("gray","black"),pt.bg=c(NA,"firebrick"),
#          pch = c(19,23),
#          legend = c("Model RMSE at opt. lambda", "Binned geom. mean RMSE"))
#
#   #B: deviance and non-0 coefficients
#   lasso_mod = glmnet(x, y, alpha = 1, lambda = best_lam_range)
#   # par(mar=c(5,5,3,5))
#   plot(lasso_mod$lambda, lasso_mod$dev.ratio,  type = "l",
#        ylab = "% of null deviation explained by model",
#        xlab = "Lambda value (shrinkage penalty)",
#        main = "Percent varition (full dataset) and degrees of \n freedom (full dataset and test-train sets)")
#   grid()
#   par(new=T)
#   plot(lasso_mod$lambda, lasso_mod$df, col = "dodgerblue", lwd = 2,
#        xlab="",ylab="", axes=F, type = "l")
#   num_pred_col = rgb(.3,.3,.7,alpha_val); num_pred_col_leg = rgb(.3,.3,.7,alpha_val*10)
#   points(lambdas_and_rmse$bestlam,
#        lambdas_and_rmse$bestlam_numpred, pch=19, col = num_pred_col)
#   axis(side=4,at=pretty(range(lasso_mod$df)))
#   mtext("Degrees of freedom (number of non-0 coef.)", side = 4, line = 3, cex=0.7)
#   abline(v=selected_lam, lty = 2, col = lam_color)
#   legend(x = "topright", col=c(num_pred_col_leg,"black","dodgerblue",lam_color),
#          lwd = c(NA,1,2,1), lty = c(NA,1,1,2), pch=c(19,NA,NA,NA),
#          legend = c("Deg. of freedom (test sets, opt. lambda)",
#                     "% Deviance (full dataset)", "Deg. of freedom (full dataset)",
#                     "Selected lambda (full dataset)"))
#
#
#   }
#
# plot_lasso_coefs = function(lasso_mod, pred_appear_tab, best_lam_range,
#                             y_val_label, mt_nrow,
#                             selected_lam = selected_lam){
#   coefs = as.data.frame((as.matrix(coef(lasso_mod))))
#
#   # Plot lambda vs highlighted coefs
#   # standardize coefficients
#   plot_tab = coefs / apply(X = coefs, MARGIN = 1, FUN = sd)
#   plot_tab = as.data.frame(t(plot_tab))
#   plot_tab[plot_tab==0] = NA # remove 0-values for plotting
#   # add in lambda values for plotting
#   plot_tab$lambda_val = best_lam_range
#
#   # setup for plot
#   tot_preds = sum(!is.na(pred_appear_tab$lambda_non0_val_appears))
#   n_high = 5
#   pred_names = pred_appear_tab$predictor
#   pred_pal = colorblind_pal()(n_high)
#   # Initialize plot
#   plot(x=range(plot_tab$lambda_val), col=NA,
#        # main = "Higher shrinkage penalties produce models with fewer and smaller coefficients",
#        main = paste0("Regression coefficients - predicting ",y_val_label,
#                      "\n with hydrologic metrics (n=",mt_nrow,")"),
#        y = range(plot_tab[,colnames(plot_tab)!="lambda_val"],na.rm=T),# ylim = c(-2,2),
#        xlab = "Lambda value (shrinkage penalty)", ylab = "Standardized predictor coefficients")
#   for(i in tot_preds:1){
#     if(i>n_high){
#       pred_col = "gray70"; pred_width = 1
#     } else {
#       pred_col = pred_pal[i]; pred_width = 2
#     }
#     pred_name = pred_names[i]
#     lines(x=plot_tab$lambda_val, y = plot_tab[,pred_name], lwd=pred_width, col = pred_col)
#   }
#   grid()
#   abline(v=selected_lam, lty = 2, col = lam_color)
#   legend(x = "bottomright", legend = c(pred_names[1:n_high],"Other non-0 coef."),
#          col = c(pred_pal[1:n_high],"gray70"), lwd = c(rep(2,n_high),1),
#          ncol=2)
# }
#
get_refined_x_and_y_for_lasso_mod = function(mt,
                                             y_val = "coho_smolt_per_fem"){
  #step 1. Prep x matrix and y array
  # (Dev: run manuscript .Rmd through line 395)
  # 1a. remove rows with no response var
  non_pred_vals = c("brood_year","smolt_year","coho_smolt_per_fem",
                    "chinook_juv_per_adult",
                    "coho_spawner_abundance",
                    "coho_redds_in_brood",
                    "coho_smolt_abun_est",
                    "chinook_spawner_abundance",
                    "chinook_juvenile_abundance"
                    # "percent_coho_smolt_survival"
                    )
  # if(y_val=="coho_smolt_per_fem"){non_pred_vals = c(non_pred_vals,"chinook_juv_per_adult")}
  # if(y_val=="chinook_juv_per_adult"){non_pred_vals = c(non_pred_vals,"coho_smolt_per_fem")}
  non_y_vals = non_pred_vals[non_pred_vals != y_val]
  mt = mt[,!(colnames(mt) %in% non_y_vals)]
  mt = mt[!is.na(mt[,y_val]),]
  na_col_detector = apply(X = mt, MARGIN = 2, FUN = sum)
  mt = mt[,!is.na(na_col_detector) ]

  # 1b. Optional. Remove the other thresholds or Smolt Year metrics
  # if(remove_extra_recon_thresholds==T){
  #   remove_these = c("f1_recon_15", "f1_recon_20", "f1_recon_50", "f1_recon_80",
  #                    "s1_discon_15", "s1_discon_20", "s1_discon_50", "s1_discon_80",
  #                    "f2_recon_15", "f2_recon_20", "f2_recon_50", "f2_recon_80",
  #                    "s2_discon_80")
  #   mt = mt[,!(colnames(mt) %in% remove_these)]
  # }
  # if(remove_SY_metrics == T){
  #   remove_these = grepl(pattern = "SY", x=colnames(mt)) |
  #     grepl(pattern = "tot_flow_CFLP", x=colnames(mt))
  #   mt = mt[,!remove_these]
  # }
  # if(remove_RY_metrics == T){
  #   remove_these = grepl(pattern = "RY", x=colnames(mt)) |
  #     grepl(pattern = "tot_flow_CFLP", x=colnames(mt))
  #   mt = mt[,!remove_these]
  # }

  # 2. Lasso Regression

  # Set up x and y, and retrieve table of best lambda and rmse values
  y = mt[,y_val]

  if(y_val=="chinook_spawner_abundance"){x = model.matrix(object = chinook_spawner_abundance~., data = mt)[,-1]}
  if(y_val=="chinook_juvenile_abundance"){x = model.matrix(object = chinook_juvenile_abundance~., data = mt)[,-1]}
  if(y_val=="chinook_juv_per_adult"){x = model.matrix(object = chinook_juv_per_adult~., data = mt)[,-1]}
  if(y_val=="coho_spawner_abundance"){x = model.matrix(object = coho_spawner_abundance~., data = mt)[,-1]}
  if(y_val=="coho_smolt_per_fem"){x = model.matrix(object = coho_smolt_per_fem~., data = mt)[,-1]}
  if(y_val=="coho_smolt_abun_est"){x = model.matrix(object = coho_smolt_abun_est~., data = mt)[,-1]}
  # if(y_val=="percent_coho_smolt_survival"){x = model.matrix(object = percent_coho_smolt_survival~., data = mt)[,-1]}
  if(y_val=="coho_redds_in_brood"){x = model.matrix(object = coho_redds_in_brood~., data = mt)[,-1]}


  return(list(x=x,y=y))
}

get_refined_x_and_y_for_lasso_mod_plus_spawn = function(mt,
                                             y_val = "coho_smolt_per_fem"){
  #step 1. Prep x matrix and y array
  # 1a. remove rows with no response var
  non_pred_vals = c("brood_year","smolt_year","coho_smolt_per_fem",
                    "chinook_juv_per_adult",
                    "coho_spawner_abundance",
                    "coho_redds_in_brood",
                    "coho_smolt_abun_est",
                    "chinook_spawner_abundance",
                    "chinook_juvenile_abundance"
                    # "percent_coho_smolt_survival"
  )



  if(grepl(pattern="coho",x = y_val) == T){
    x_spawn = "coho_spawners_zscored"
    non_pred_vals = c(non_pred_vals, "chinook_spawners_zscored")}
  if(grepl(pattern="chinook",x = y_val) == T){
    x_spawn = "chinook_spawners_zscored"
    non_pred_vals = c(non_pred_vals, "coho_spawners_zscored")
    }


  non_y_vals = non_pred_vals[non_pred_vals != y_val]
  mt = mt[,!(colnames(mt) %in% non_y_vals)]
  mt = mt[!(is.na(mt[,y_val]) | is.na(mt[, x_spawn])),]
  na_col_detector = apply(X = mt, MARGIN = 2, FUN = sum)
  mt = mt[,!is.na(na_col_detector) ]

  # Set up x and y, and retrieve table of best lambda and rmse values
  y = mt[,y_val]

  if(y_val=="chinook_spawner_abundance"){x = model.matrix(object = chinook_spawner_abundance~., data = mt)[,-1]}
  if(y_val=="chinook_juvenile_abundance"){x = model.matrix(object = chinook_juvenile_abundance~., data = mt)[,-1]}
  if(y_val=="chinook_juv_per_adult"){x = model.matrix(object = chinook_juv_per_adult~., data = mt)[,-1]}
  if(y_val=="coho_spawner_abundance"){x = model.matrix(object = coho_spawner_abundance~., data = mt)[,-1]}
  if(y_val=="coho_smolt_per_fem"){x = model.matrix(object = coho_smolt_per_fem~., data = mt)[,-1]}
  if(y_val=="coho_smolt_abun_est"){x = model.matrix(object = coho_smolt_abun_est~., data = mt)[,-1]}
  # if(y_val=="percent_coho_smolt_survival"){x = model.matrix(object = percent_coho_smolt_survival~., data = mt)[,-1]}
  if(y_val=="coho_redds_in_brood"){x = model.matrix(object = coho_redds_in_brood~., data = mt)[,-1]}


  return(list(x=x,y=y))
}

#
# lasso_regression_plots_and_tabs = function(metrics_tab,
#                                   y_val = "coho_smolt_per_fem",
#                                   remove_extra_recon_thresholds = F,
#                                   # remove_SY_metrics = T,
#                                   # remove_RY_metrics = F,
#                                   return_pred_appear_tab = T,
#                                   yvlt,
#                                   selected_lam,
#                                   show_plot = T,
#                                   log_transform_eco_metrics, zscore_flow_metrics){
#
#   # Lasso regression. Informed by lab from ISLR 7th printing
#   x_and_y = get_refined_x_and_y_for_lasso_mod(mt = metrics_tab,
#                                               y_val = y_val)
#                                               # remove_RY_metrics = remove_RY_metrics,
#                                               # remove_SY_metrics = remove_SY_metrics)
#   x = x_and_y[[1]]; y = x_and_y[[2]]
#
#   # name rmse file
#   # fname = paste( y_val,"- lambdas_and_rmse")
#   if(log_transform_eco_metrics==F & zscore_flow_metrics ==F){fname = paste( y_val,"- lambdas_and_rmse, no transform fmet or ecomet" )}
#   if(log_transform_eco_metrics==T & zscore_flow_metrics ==T){fname = paste( y_val,"- lambdas_and_rmse, zscore fmet T, log ecomet T" )}
#   lambda_tab_path = file.path(data_dir,paste0(fname,".csv" ))
#   if(log_transform_eco_metrics==F & zscore_flow_metrics ==F){fname2 = paste( y_val,"- predictor rank, no transform fmet or ecomet" )}
#   if(log_transform_eco_metrics==T & zscore_flow_metrics ==T){fname2 = paste( y_val,"- predictor rank, zscore fmet T, log ecomet T" )}
#   pred_rank_path = file.path(data_dir,paste0(fname2,".csv" ))
#
#   # Find the range of "best" lambda values, based on cross-validation, and associated RMSE errors
#   if(!file.exists(lambda_tab_path) | !file.exists(pred_rank_path)){
#     save_pred_and_rmse_files(metrics_tab = metrics_tab,
#                              y_val = y_val,
#                              lambda_tab_path = lambda_tab_path, pred_rank_path = pred_rank_path)
#   }
#   lambdas_and_rmse = read.csv(lambda_tab_path)
#   pred_rank_tab = read.csv(pred_rank_path)
#
#   min_lam = min(lambdas_and_rmse$bestlam, na.rm=T)
#   max_lam = max(lambdas_and_rmse$bestlam, na.rm=T)
#   by_val = diff(range(lambdas_and_rmse$bestlam, na.rm=T))/99
#   best_lam_range = rev(seq(min_lam, max_lam, by_val)) #lambdas in reverse order to match coefs output
#
#   # Calculate lasso models over range of lambda values
#   lasso_mod = glmnet(x, y, alpha = 1, lambda = best_lam_range)
#   # find lambda values at which each coefficient becomes non-0
#   pred_appear_tab = generate_pred_appear_tab(lasso_mod, best_lam_range = best_lam_range)
#
#   if(show_plot==T){
#     # Plots
#     par(mfrow=c(3,1), mar=c(5,5,5,2))
#     y_val_label = yvlt$y_val_title[yvlt$y_val==y_val]
#     if(grepl(x=y_val,pattern ="coho")){alpha_val=0.05}
#     if(grepl(x=y_val,pattern="chinook")){alpha_val=0.01}
#     #Panel 1 and 2
#     plot_lasso_diagnostics(x=x, y=y, best_lam_range, lambdas_and_rmse = lambdas_and_rmse,
#                            selected_lam = selected_lam, alpha_val=alpha_val)
#     # Panel 3
#     plot_lasso_coefs(lasso_mod = lasso_mod,
#                      pred_appear_tab = pred_appear_tab,
#                      best_lam_range = best_lam_range,
#                      y_val_label = y_val_label,
#                      mt_nrow = nrow(x),
#                      selected_lam = selected_lam)
#
#   }
#   if(return_pred_appear_tab==T){return(list(pred_appear_tab = pred_appear_tab,
#                                             pred_rank_tab = pred_rank_tab,
#                                             lasso_mod_range = lasso_mod))}
# }
#
#
# get_lasso_mod = function(metrics_tab,
#                          y_val = "coho_smolt_per_fem",
#                          lambda_val#,
#                          # remove_extra_recon_thresholds = F,
#                          # remove_RY_metrics = F,
#                          # remove_SY_metrics = T
# ){
#
#   #step 1. Prep x matrix and y array
#   # Lasso regression. Informed by lab from ISLR 7th printing
#   x_and_y = get_refined_x_and_y_for_lasso_mod(mt = metrics_tab,
#                                               y_val = y_val)
#   x = x_and_y[[1]]; y = x_and_y[[2]]
#
#   lasso_mod = glmnet(x, y, alpha = 1, lambda = lambda_val)
#
#   return(lasso_mod)
# }
#
#
# save_multiple_rmse_and_pred_rank_files=function(metrics_tab, yvlt,
#                                                 log_transform_eco_metrics, zscore_flow_metrics){
#   # to use: run through outer main script until arrive at lasso function. use
#   # metrics_tab that is the input to the lasso plotting function (i.e.,
#   # metrics_tab = metrics_tab_screened)
#
#   for(y_val in yvlt$y_val){
#
#     if(log_transform_eco_metrics==F & zscore_flow_metrics ==F){fname = paste( y_val,"- lambdas_and_rmse, no transform fmet or ecomet" )}
#     if(log_transform_eco_metrics==T & zscore_flow_metrics ==T){fname = paste( y_val,"- lambdas_and_rmse, zscore fmet T, log ecomet T" )}
#     if(log_transform_eco_metrics==F & zscore_flow_metrics ==F){fname2 = paste( y_val,"- predictor rank, no transform fmet or ecomet" )}
#     if(log_transform_eco_metrics==T & zscore_flow_metrics ==T){fname2 = paste( y_val,"- predictor rank, zscore fmet T, log ecomet T" )}
#     lambda_tab_path = file.path(data_dir,paste0(fname,".csv" ))
#     pred_rank_path = file.path(data_dir,paste0(fname2,".csv" ))
#
#     save_pred_and_rmse_files(metrics_tab = metrics_tab,
#                              y_val = y_val,
#                              lambda_tab_path = lambda_tab_path,
#                              pred_rank_path = pred_rank_path)
#   }
#
#
#
#
#   # pred_rank_tab = find_all_best_lambda_vals(com_tab = com, x = x, y = y,
#   #                                           output = "predictor_rank",
#   #                                           outer_best_lam_range = best_lam_range)
#   # write.csv(pred_rank_tab, file = pred_rank_path, quote = F, row.names=F)
#
# }
#
# save_pred_and_rmse_files = function(metrics_tab, y_val, lambda_tab_path, pred_rank_path){
#   x_and_y = get_refined_x_and_y_for_lasso_mod(mt = metrics_tab,
#                                               y_val = y_val)
#   x = x_and_y[[1]]; y = x_and_y[[2]]
#
#   com = t(combn(x = 1:length(y), m = floor(length(y)/2)))     # Split dataset into halves
#   # com = t(combn(x = 1:length(y), m = ceiling(length(y)*.25))) # leave out 25%
#   # split data set into all possible test and train sets.
#   # if total number of samples is more than 16, randomly sample only 10k of the possible test-train combos
#   if(nrow(com)>10000){
#     set.seed(1)
#     com = com[sample(x = 1:nrow(com), size = 10000),]
#   }
#
#   lambdas_and_rmse = find_all_best_lambda_vals(com_tab = com, x = x, y = y)
#   write.csv(lambdas_and_rmse, lambda_tab_path, quote=F, row.names = F)
#
#   min_lam = min(lambdas_and_rmse$bestlam, na.rm=T)
#   max_lam = max(lambdas_and_rmse$bestlam, na.rm=T)
#   by_val = diff(range(lambdas_and_rmse$bestlam, na.rm=T))/99
#   best_lam_range = rev(seq(min_lam, max_lam, by_val)) #lambdas in reverse order to match coefs output
#
#
#   pred_rank_tab = find_all_best_lambda_vals(com_tab = com, x = x, y = y,
#                                             output = "predictor_rank",
#                                             outer_best_lam_range = best_lam_range)
#   write.csv(pred_rank_tab, file = pred_rank_path, quote = F, row.names=F)
# }
#
#

# MARSS  ------------------------------------------------------------------

## The code below follows the R script Baruch_2023_Putah_Creek_MARSS.R, published
## in support of:
## Baruch et al., 2024. "Mimicking Functional Elements of the Natural Flow Regime
## Promotes Native Fish Recovery in a Regulated River." https://doi.org/10.1002/eap.3013.


get_longest_cont_ts = function(y, y_years){
  n_y = length(y)
  changes = data.frame( missing = is.na(y), present = !is.na(y))
  changes$first_cont_present_vals = changes$present & c(T, changes$missing[1:(n_y-1)])
  changes$last_cont_present_vals = c(changes$missing[2:n_y],T) & changes$present
  cont_lengths = which(changes$last_cont_present_vals) - which(changes$first_cont_present_vals) + 1
  longest_j = which.max(cont_lengths)
  first_i = which(changes$first_cont_present_vals)[longest_j]
  last_i = which(changes$last_cont_present_vals)[longest_j]

  out_tab = data.frame(years = y_years[first_i:last_i],
                       y_cont = y[first_i:last_i])
  return(out_tab)
}


# Manipulate observation data

get_obs_data_for_MARSS = function(metrics_tab,
                                  y_val_names = c("coho_smolt_abun_est","chinook_juvenile_abundance")){

  y_obs = t(metrics_tab[,y_val_names])
  colnames(y_obs) = metrics_tab$brood_year
  rownames(y_obs) = y_val_names

  return(y_obs)
}



get_single_cov_model_fit_for_MARSS = function(y_obs_tab, pred, method = "kem"){
  species = rownames(y_obs_tab)
  nspp = length(species)

  ## Data structure: the "site" for each species is the entire watershed
  ## (as monitored at the counting weir for adults and the rotary screw trap
  ## for juveniles).
  ##
  ## So, we need a x-column Z matrix. One col per species, one row per species-site
  ## combination. We have x species all at the same site.

  n_sites = 1 # scott River
  Z = matrix(1, nspp * n_sites, 1)
  # diag(Z) = 1
  # B2024: R matrix: species-specific observation error -> rep each species name for number of observation streams
  # R=matrix(list(0),nspp,nspp)
  # diag(R) = species

  mod = list()

  mod$Q = "diagonal and equal" #matrix("q") # single process error across species and replicates
  mod$U = matrix(0) # No drift - 0 intercept for hidden state series
  mod$B = matrix(1) # No B - no coefficients for hidden state series
  mod$Z = Z
  mod$R = matrix("r") # One obs error (i.e., for the selected ecological obs. series)
  mod$A = matrix(0) # 0 intercept for observation data series

  # mod_ct.fit = MARSS(y = y_obs, model=mod, #method = method,
  #                    control=list(maxit=10000), method="BFGS")

  # basically: https://atsa-es.github.io/atsa-labs/sec-uss-fitting-a-state-space-model-with-marss.html

  #### DS_Dur_WS

  flow_metric_ct = pred
  covar_ct = t(metrics_tab[,flow_metric_ct])
  rownames(covar_ct) = flow_metric_ct
  colnames(covar_ct) = metrics_tab$brood_year
  ncov = nrow(covar_ct)

  # # reduce observation data to years with observations in the selected metrics
  # how_many_na = apply(X = y_obs_tab, MARGIN = 2, function(x){sum(is.na(x))})
  # # currently assumes 4 missing values only for end-years outside a continuous string of years with < 4 NAs
  # y_obs = y_obs_tab[ , how_many_na < nspp]

  # reduce observation data to years with observations in the selected metrics
  how_many_vals_y = apply(X = y_obs_tab, MARGIN = 2, function(x){length(x) - sum(is.na(x))})
  how_many_vals_cov = apply(X = covar_ct, MARGIN = 2, function(x){length(x) - sum(is.na(x))})
  first_i_y = min(which(how_many_vals_y > 0)); last_i_y = max(which(how_many_vals_y > 0)) # cover any data for all species
  first_i_cov = min(which(how_many_vals_cov == ncov)); last_i_cov = max(which(how_many_vals_cov == ncov))# can't have missing covariates.
  # assumes continuous covariance coverage between min and max

  keep_cols = max(c(first_i_y, first_i_cov)):  min(c(last_i_y, last_i_cov))
  # currently assumes 4 missing values only for end-years outside a continuous string of years with < 4 NAs
  y_obs = y_obs_tab[ , keep_cols]
  cov_ct = matrix(covar_ct[,keep_cols], nrow = 1)



  #Create C matrix: 1 column b/c one covariate, specify if species abundance or normalized
  C_ct <- matrix("ScottR")#matrix(c("abundance","abundance","normalized","normalized"), ncol = 1, nrow = nspp)

  ## Now fit a MARSS model
  mod_ct = mod
  mod_ct$C = C_ct # Abundance and normalized data be affected by the covariate differently
  mod_ct$c = cov_ct # Covariate data


  #### U = zero, B = identity, Q = equal
  # Fit the MARSS model
  # control$trace = 1
  mod_ct.fit = MARSS(y = y_obs, model=mod_ct, #method = method,
                     control=list(maxit=10000))#, method="BFGS")

  return(mod_ct.fit)
  # mod_ct.CI <- MARSSparamCIs(mod_ct.fit)
  #
  # # what if we just start with the basics?
  #
  # kemfit2 = MARSS(y_obs, model = list(
  #   Z = matrix(1, 4, 1),
  #   R = "diagonal and equal"))
}

get_flow_and_spawn_cov_model_fit_for_MARSS = function(y_obs_tab, pred, y_spawn_colname, method = "kem"){

  species = rownames(y_obs_tab)

  nspp = length(species)

  ## Data structure: the "site" for each species is the entire watershed
  ## (as monitored at the counting weir for adults and the rotary screw trap
  ## for juveniles).
  ##
  ## So, we need a x-column Z matrix. One col per species, one row per species-site
  ## combination. We have x species all at the same site.

  n_sites = 1 # scott River
  Z = matrix(1, nspp * n_sites, 1)
  # diag(Z) = 1
  # B2024: R matrix: species-specific observation error -> rep each species name for number of observation streams
  # R=matrix(list(0),nspp,nspp)
  # diag(R) = species

  mod = list()

  mod$Q = "diagonal and equal" #matrix("q") # single process error across species and replicates
  mod$U = matrix(0) # No drift - 0 intercept for hidden state series
  mod$B = matrix(1) # No B - no coefficients for hidden state series
  mod$Z = Z
  mod$R = matrix("r") # One obs error (i.e., for the selected ecological obs. series)
  mod$A = matrix(0) # 0 intercept for observation data series

  # basically: https://atsa-es.github.io/atsa-labs/sec-uss-fitting-a-state-space-model-with-marss.html

  #### DS_Dur_WS

  flow_metric_ct = pred
  covar_ct = t(metrics_tab[, c(flow_metric_ct, y_spawn_colname)])
  ncov = 2

  # reduce observation data to years with observations in the selected metrics
  how_many_vals_y = apply(X = y_obs_tab, MARGIN = 2, function(x){length(x) - sum(is.na(x))})
  how_many_vals_cov = apply(X = covar_ct, MARGIN = 2, function(x){length(x) - sum(is.na(x))})
  first_i_y = min(which(how_many_vals_y > 0)); last_i_y = max(which(how_many_vals_y > 0))
  first_i_cov = min(which(how_many_vals_cov == ncov)); last_i_cov = max(which(how_many_vals_cov == ncov))# can't have missing covariates
  # assumes continuous coverage between min and max for covariate data

  keep_cols = max(c(first_i_y, first_i_cov)):  min(c(last_i_y, last_i_cov))
  # currently assumes 4 missing values only for end-years outside a continuous string of years with < 4 NAs
  y_obs = y_obs_tab[ , keep_cols]
  cov_ct = covar_ct[,keep_cols]


  #Create C matrix: 1 column b/c one variable, specify if species abundance or normalized
  C_ct <- matrix(c("ScottR","Spawners"), ncol = 1)#matrix(c("abundance","abundance","normalized","normalized"), ncol = 1, nrow = nspp)
  # C_ct <- matrix(rep("ScottR"), ncol = 1, nrow = nspp)

  ## Now fit a MARSS model
  mod_ct = mod
  mod_ct$C = C_ct # Abundance and normalized data be affected by the covariate differently
  mod_ct$c = cov_ct # Covariate data


  #### U = zero, B = identity, Q = equal
  # Fit the MARSS model
  # control$trace = 1
  mod_ct.fit = MARSS(y = y_obs, model=mod_ct, #method = method,
                     control=list(maxit=10000))#, method="BFGS")

  return(mod_ct.fit)
  # mod_ct.CI <- MARSSparamCIs(mod_ct.fit)
  #
  # # what if we just start with the basics?
  #
  # kemfit2 = MARSS(y_obs, model = list(
  #   Z = matrix(1, 4, 1),
  #   R = "diagonal and equal"))
}

marss_predict_plot_single_covars = function(metrics_tab, pred_yrs_i, y_val,
                                            top_cov, top_models){
  plot(metrics_tab$brood_year[pred_yrs_i],
       metrics_tab[pred_yrs_i, y_val],
       main = paste0("MARSS models of ", yvlt$y_val_title[yvlt$y_val==y_val],
                     ", single hydrologic covariates"), pch = 19, #type = "o",
       xlab = "Brood Year",
       ylab = paste0("Log10 of ", yvlt$y_val_title[yvlt == y_val]))
  grid()

  for(j in 1:length(top_cov)){
    pred_j = top_cov[j]
    mod_name = paste0(y_val, "__", pred_j)
    mod_j = top_models[[mod_name]]
    prediction = predict(mod_j, type = "ytt")
    lines(x = metrics_tab$brood_year[pred_yrs_i],
          y = prediction$pred$estimate, col = j+1,
          lty = 2, pch = 18)
  }
  legend(x = "bottomright", legend = top_cov,
         col = 1+(1:length(top_cov)), lwd = 1)

}

marss_predict_plot_flow_and_spawn = function(metrics_tab, pred_yrs_i, y_val,
                                            top_cov, top_models){
  plot(metrics_tab$brood_year[pred_yrs_i],
       metrics_tab[pred_yrs_i, y_val],
       main = paste0("MARSS models of ", yvlt$y_val_title[yvlt$y_val==y_val],
                     ", with two cov. \n (spawners and one hydro. metric)"), pch = 19, #type = "o",
       xlab = "Brood Year",
       ylab = paste0("Log10 of ", yvlt$y_val_title[yvlt == y_val]))
  grid()

  for(j in 1:length(top_cov)){
    pred_j = top_cov[j]
    mod_name = paste0(y_val, "__", pred_j)
    mod_j = top_models[[mod_name]]
    prediction = predict(mod_j, type = "ytt")
    lines(x = metrics_tab$brood_year[pred_yrs_i],
          y = prediction$pred$estimate, col = j+1,
          lty = 2, pch = 18)
  }
  legend(x = "bottomright", legend = top_cov,
         col = 1+(1:length(top_cov)), lwd = 1)

}

# Hydrologic Benefit Function Results -------------------------------------


get_hbf_tab = function(mt, coefs, int){
  # coefs = as.data.frame(t(as.matrix(coef(lasso_mod))))
  # coef_names = row.names(coef(hbf_coho))
  #removing the intercept will make the row names unusable

  # int_picker = 1
  intercept = int

  # store relevant metric values in an intermediate table
  intermed_tab=mt
  intermed_tab=intermed_tab[,colnames(intermed_tab) %in%
                              c(names(coefs),"brood_year")]
  # match metrics tab to coefficients
  term_matcher = match(names(coefs), colnames(intermed_tab))

  # record HB contribution values in an output tab. initialize all values to NA
  output_tab = intermed_tab
  output_tab[,term_matcher] = NA


  for(i in 1:nrow(intermed_tab)){
    by_vals = intermed_tab[i,]
    hb_components = by_vals [term_matcher] * coefs
    output_tab[i,term_matcher] = hb_components
  }

  # sum all components for annual HB values
  brood_year_col = which(colnames(output_tab)=="brood_year")
  output_tab$hbf_total = as.numeric(intercept) + apply(X=as.data.frame(output_tab[,-brood_year_col]),
                                                  MARGIN = 1, FUN=sum,na.rm=T)

  return(output_tab)
}

hbf_over_time_fig = function(mt, hbf_tab, y_val,
                             y_val_axis,
                             write_hist_HB_vals=F,
                             plot_yrs = NA,
                             show_legend = T){
  # plot details
  neg_values = hbf_tab$hbf_total < 0
  if(sum(neg_values,na.rm=T)>0){
    neg_value_col = "red"; neg_val_pch = 5
  }
  obs_col = "goldenrod"

  if(sum(is.na(plot_yrs))>0){plot_yrs = hbf_tab$brood_year}

  if(write_hist_HB_vals == T){
    file_name = paste0("hist_HBF_vals_",y_val,"_",Sys.Date(),".csv")
    write.csv(hbf_tab[,c("water_year","hbf_total")],
              file = file_name, quote = F, row.names = F)}

  # Plot predicted values
  y_lims_for_plot = c(min(c(hbf_tab$hbf_total,mt[,y_val]),na.rm=T),
                      max(c(hbf_tab$hbf_total,mt[,y_val]),na.rm=T))
  par(mar=c(5,5,2,5))
  plot(x=plot_yrs, y=rep(NA,length(plot_yrs)),#hbf_tab$hbf_total,
       col = NA,
       ylim = y_lims_for_plot, #yaxt = "n",
       xlab = "Brood Year",
       ylab = paste0("Hydrologic Benefit value (predicted \n log10 of ",y_val_axis, ")"))
  # axis(side = 2, at = seq(from = -100, to = 200, by = 20))
  abline(v = seq(from = 1940, to = 2140, by = 10),
         h = pretty(range(c(hbf_tab$hbf_total,mt[,y_val]),na.rm=T)),
         lty = 3, col = "gray")
  abline(h=0, col = "darkgray")
  points(hbf_tab$brood_year, hbf_tab$hbf_total, pch = 19)
  lines(hbf_tab$brood_year, hbf_tab$hbf_total)
  # Add observed values
  points(mt$brood_year, mt[,y_val],
         pch = 24, bg= obs_col, cex = 1.1)
  # Add arrows connecting predicted values with observed
  arrows_x = intersect(mt$brood_year[!is.na(mt[,y_val])], hbf_tab$brood_year)
  arrows_y0 = mt[mt$brood_year %in% arrows_x, y_val]
  arrows_y1 = hbf_tab$hbf_total[hbf_tab$brood_year %in% arrows_x]
  arrows(x0 = arrows_x, y0 = arrows_y0, x1 = arrows_x, y1 = arrows_y1,
         length = 0, lty = 1, col = obs_col, lwd = 2)

  if(sum(neg_values,na.rm=T)>0){
    points(hbf_tab$brood_year[neg_values], hbf_tab$hbf_total[neg_values],
           pch = neg_val_pch, col = neg_value_col, lwd=2)
  }

  abline(h=0, col = "darkgray")
  if(y_val=="coho_smolt_per_fem"){
    legend_place = "bottomleft"
  } else if (predict_eco == "juvenile abundance, hydro and spawners"){
    legend_place = "topright"
  }  else {
    legend_place="topleft"
  }

  if(show_legend==T){
    if(sum(neg_values,na.rm=T)>0){
      legend(legend_place, pch = c(19,neg_val_pch,24, NA),
             pt.lwd = c(NA,2,1, NA), pt.cex = c(1,1,1.2, NA), bg="white",
             col = c("black", neg_value_col,"black", obs_col),
             pt.bg=c(NA,NA,obs_col, NA), lwd = c(NA,NA,NA,2), lty = c(NA,NA,NA,1),
             legend = c("Predicted", "Predicted (neg. value)",
                        "Observed", "Diff., pred.-obs."))
    } else {
      legend(legend_place, pch = c(19,24, NA),
             pt.lwd = c(NA,2, NA), pt.cex = c(1,1.2, NA), bg="white",
             col = c("black", "black", obs_col),
             pt.bg=c(NA,obs_col, NA), lwd = c(NA,NA,2), lty = c(NA,NA,1),
             legend = c("Predicted",
                        "Observed", "Diff., pred.-obs."))

    }

  }

  par(new = T)
  plot(1000,NA, ylim = 10^y_lims_for_plot,
  log = "y", xaxt = "n", yaxt = "n", ylab = "", xlab = "",
  )
  axis_oom_range = range(c(ceiling(y_lims_for_plot), floor(y_lims_for_plot)))
  axis_oom = axis_oom_range[1]:axis_oom_range[2]

  axis(side = 4, at = 10^axis_oom, tck=-.04)
  axis(side=4, at = (1:9) * 10^(sort(rep(axis_oom, 9))), labels = F, tck = -0.02)
  mtext(paste0("Corresponding predicted \n ",y_val_axis), side = 4, line = 3)
}

