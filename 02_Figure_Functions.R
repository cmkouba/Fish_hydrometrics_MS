# 03_Ch1_Figure_Functions
# Figures for Ch. 1 manuscript

#Explanation:
# --Load from local disk if you have previously loaded from server and saved an .RData of layers.
# --Load from the server and save the workspace if you do not have a local RData of the layers saved.
# -- If you want to update an existing .RData file of layers, simply delete or rename the old one.
# -- Save workspace option provided in case of not wanting to overwrite existing .RData file



# setup -------------------------------------------------------------------


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
fall_col = "darkgoldenrod"
spring_col = "green4"
wet_sn_col = "royalblue3"
dry_sn_col = "orangered"

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
cities_centroid$label_xmod = c(-1.2, 2.2,    0,  0)
cities_centroid$label_ymod = c(  0, -0.2, -0.5, -0.7)

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
  graphic_filename = file.path(ms_dir,"Graphics and Supplements","scott valley setting.png")
  file.remove(graphic_filename) # remove old version to save new one
  png(filename = graphic_filename,
      width = 7, height = 8.3, units = "in", res = 400)
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

  if(connection_date_annotate == "20_and_100_only"){
    # Connection dates
    threshold_color_tab = data.frame(thresholds_cfs = c(10,20,30,40,60,100),
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


re_and_discon_timeseries_figure = function(){
  connect_tab = re_and_disconnect_date_tab(fj_flow = fj_flow, last_wy = 2023)
  # clean - turn 0s to NAs
  connect_tab$recon_date_100[connect_tab$recon_date_100==0] = NA
  connect_tab$discon_date_100[connect_tab$discon_date_100==1] = NA

  # par(mar = c(5,4,3,1))
  plot(x = connect_tab$water_year, y = connect_tab$recon_date_100, col = fall_col,
       main = "Fall reconnection and spring disconnection dates, \n 100 cfs (2.8 cms) threshold",type = "o",
       ylab = "Days since Aug. 31", xlab = "Water Year", pch=19, ylim = c(-70,366))
  grid()
  abline(lm(connect_tab$recon_date_100 ~ connect_tab$water_year), lty = 2)

  points(x = connect_tab$water_year, y = connect_tab$discon_date_100,
         col = spring_col, pch=19)
  lines(x = connect_tab$water_year, y = connect_tab$discon_date_100, col = spring_col)
  abline(lm(connect_tab$discon_date_100 ~ connect_tab$water_year), lty = 2)


  legend(x="bottomright",lty = c(1,1,2), pch = c(19,19,NA), col = c(fall_col, spring_col, "black"),
         inset = .02,
         legend = c("First fall day when flow exceeded 100 cfs",
                    "First spring day when flow fell below 100 cfs",
                    "Line of best fit"))
}


# Methods and Metrics Functions -------------------------------------------

calc_recon_days_since_aug_31 = function(dates, flow, recon_threshold){
  dates_since_aug_31 = as.numeric(dates - as.Date(paste0(year(min(dates)), "-08-31")))
  recon_day = min(dates_since_aug_31[ flow > recon_threshold], na.rm=T)
  return(recon_day)
}

calc_discon_days_since_aug_31 = function(dates, flow, discon_threshold){
  dates_since_aug_31 = as.numeric(dates - as.Date(paste0(year(min(dates))-1, "-08-31")))
  discon_day = min(dates_since_aug_31[ flow < discon_threshold], na.rm=T)

  if(sum(flow < discon_threshold) < 1){
    return(max(dates_since_aug_31))
  } else {
    return(discon_day)
  }
}



tabulate_hydro_by_affected_smolt_year = function(smolt_years = smolt_per_fem$Smolt_Year,
                                                 brood_years = smolt_per_fem$Adult_Year_Brood_Year){
  hydro_by_smolt_year = fj_flow[0:0,] # Get table structure with 0 rows
  hydro_by_smolt_year$smolt_year = vector(mode="integer",length=0)
  hydro_by_smolt_year$smolt_year_cum_flow = vector(mode="numeric",length=0)

  #if no m3 per day units, create it
  if(!is.element(el = "Flow_m3day",colnames(fj_flow))){fj_flow$Flow_m3day = fj_flow$Flow*cfs_to_m3day}
  for(smolt_year in smolt_years){
    date1 = as.Date(paste0(smolt_year - 2,"-09-01"))
    date2 = as.Date(paste0(smolt_year,"-07-01"))
    affecting_hydro_dates = seq.Date(from = date1, to = date2, by="day")

    affecting_hydro = fj_flow[fj_flow$Date %in% affecting_hydro_dates,]
    affecting_hydro$smolt_year = smolt_year
    affecting_hydro$smolt_year_cum_flow = cumsum(affecting_hydro$Flow_m3day)
    hydro_by_smolt_year = rbind(hydro_by_smolt_year, affecting_hydro)

  }

  return(hydro_by_smolt_year)

}

calc_metrics_hydro_by_affected_smolt_year = function(hydro_by_smolt_year,
                                                     thresholds = c(10, 20, 30, 40, 60, 100)){

  hbsm = hydro_by_smolt_year

  # metric table column names

  #Outcomes
  ## coho_abundance, chinook_abundance, smolt_per_fem, percent_smolt_survival
  # Possibly add advanced outcomes:
  ## percent increase over previous cohort year (for coho)

  # predictors
  # 24 columns of recon/discon dates. 4 x 6
  ## reconnection - brood, rear years
  ## disconnection - rear, smolt years
  ## 10, 20, 30, 40, 60, 100 cfs.
  # total flow for the whole period
  # total flow for brood months, rear year, smolt months
  # Number of days of flow above the 90th percentile
  # Precalc metrics :
  ## Brood year (BY): FA_Mag, FA_Tim
  ## Rear year (RY): Wet_BFL_Mag_50, Wet_Tim, Wet_BFL_Dur, SP_Tim, SP_ROC, DS_Mag_50, FA_Mag, FA_Tim
  ## Smolt year (SY): Wet_BFL_Mag_50, Wet_Tim, Wet_BFL_Dur, SP_Tim, SP_ROC


  output_colnames_extended = c("brood_year", "smolt_year",
                               "chinook_spawner_abundance","chinook_juvenile_abundance",
                               "chinook_juv_per_adult",
                               "coho_spawner_abundance", "coho_smolt_per_fem",
                               "coho_smolt_abun_est",
                               "percent_coho_smolt_survival","coho_redds_in_brood",
                               # more advanced outcomes?
                               paste0("BY_recon_",thresholds),
                               paste0("RY_discon_",thresholds),
                               paste0("RY_recon_",thresholds),
                               paste0("SY_discon_",thresholds),
                               "BY_min_flow_sepdec","RY_min_flow","SY_min_flow_janjul",
                               "tot_flow_CFLP" ,"BY_tot_flow_sepdec" ,"RY_tot_flow" ,"SY_tot_flow_janjul",
                               "BY_FA_Mag" , "BY_FA_Tim", "BY_FA_Dur", "BY_num_days_gt_90_pctile",
                               "RY_Wet_BFL_Mag_50","RY_Wet_Tim" ,"RY_Wet_BFL_Dur", "RY_num_days_gt_90_pctile","RY_SP_Tim" ,"RY_SP_ROC",  "RY_DS_Tim","RY_DS_Mag_50", "RY_DS_Mag_90", "RY_DS_Dur_WS","RY_FA_Mag", "RY_FA_Tim", "RY_FA_Dur",
                               "SY_Wet_BFL_Mag_50" ,"SY_Wet_Tim" ,"SY_Wet_BFL_Dur", "SY_num_days_gt_90_pctile", "SY_SP_Tim" ,"SY_SP_ROC")

  output_colnames = c("brood_year", "smolt_year",
                      "chinook_spawner_abundance","chinook_juvenile_abundance",
                      "chinook_juv_per_adult",
                      "coho_spawner_abundance", "coho_smolt_per_fem",
                      "coho_smolt_abun_est",
                      "percent_coho_smolt_survival","coho_redds_in_brood",
                      # more advanced outcomes?
                      paste0("BY_recon_",thresholds),
                      paste0("RY_discon_",thresholds),
                      paste0("RY_recon_",thresholds),
                      paste0("SY_discon_",thresholds),
                      "BY_min_flow_sepdec","RY_min_flow","SY_min_flow_janjul",
                      "tot_flow_CFLP" ,"BY_tot_flow_sepdec" ,"RY_tot_flow" ,"SY_tot_flow_janjul",
                      "log_tot_flow_CFLP" ,"log_BY_tot_flow_sepdec" ,"log_RY_tot_flow" ,"log_SY_tot_flow_janjul",
                      "BY_FA_Mag" , "BY_FA_Tim", "BY_FA_Dur", #"BY_num_days_gt_90_pctile",
                      "RY_Wet_Tim" ,"RY_Wet_BFL_Dur", "RY_SP_ROC", "RY_DS_Mag_50", "RY_DS_Mag_90", "RY_FA_Mag",
                      "RY_FA_Tim", "RY_FA_Dur", "RY_Wet_BFL_Mag_50",#"RY_num_days_gt_90_pctile",
                      "RY_DS_Tim","RY_SP_Tim", "RY_DS_Dur_WS",
                      "SY_Wet_Tim" ,"SY_Wet_BFL_Dur", "SY_SP_ROC" #"SY_Wet_BFL_Mag_50" ,"SY_num_days_gt_90_pctile", "SY_SP_Tim" ,
  )

  #1. Initialize table
  ## Identify brood year coverage to find number of rows
  brood_years_min = min(c(smolt_per_fem$Adult_Year_Brood_Year,
                          outmigs$Brood.Year, coho_abun$Return_Year,
                          chinook_abun$Year, chinook_spawn_and_juv$Brood_Year))
  brood_years_max = max(c(smolt_per_fem$Adult_Year_Brood_Year,
                          outmigs$Brood.Year,coho_abun$Return_Year,
                          chinook_abun$Year, chinook_spawn_and_juv$Brood_Year))
  brood_years = brood_years_min:brood_years_max

  # structure initial output dataframe and label columns
  output_tab = data.frame(matrix(data = NA,
                                 nrow = length(brood_years),
                                 ncol = length(output_colnames)))
  colnames(output_tab)=output_colnames
  output_tab$brood_year = brood_years
  output_tab$smolt_year = brood_years + 2

  # eliminate incomplete smolt years. Filter by number of days available in flow record.
  flow_days_avail = aggregate(hbsm$smolt_year, by = list(hbsm$smolt_year), FUN = length)
  keep_these_smolt_years = flow_days_avail$Group.1[flow_days_avail$x >= 669]
  output_tab = output_tab[output_tab$smolt_year %in% keep_these_smolt_years,]

  #2. Assign output metrics
  output_tab$chinook_spawner_abundance = chinook_spawn_and_juv$Spawning_Adult_Chinook_est[match(output_tab$brood_year,
                                                                       chinook_spawn_and_juv$Brood_Year)] #chinook_abun$Total_Basin_estimate[match(output_tab$brood_year, chinook_abun$Year)]
  output_tab$chinook_juvenile_abundance = chinook_spawn_and_juv$Juvenile_Chinook_produced_BY_plus_1[match(output_tab$brood_year,
                                                                                                chinook_spawn_and_juv$Brood_Year)]
  output_tab$chinook_juv_per_adult = chinook_spawn_and_juv$Chinook_juv_per_adult[match(output_tab$brood_year,
                                                                                                          chinook_spawn_and_juv$Brood_Year)]
  output_tab$coho_spawner_abundance = coho_abun$Number_of_Coho[match(output_tab$brood_year, coho_abun$Return_Year)]
  output_tab$coho_smolt_abun_est = outmigs$Smolt.point.Estimate[match(output_tab$smolt_year, outmigs$Smolt.Year)]
  output_tab$coho_smolt_per_fem = smolt_per_fem$Smolts_Produced_Per_Female[match(output_tab$smolt_year, smolt_per_fem$Smolt_Year)]
  output_tab$percent_coho_smolt_survival = outmigs$Percent.smolt.survival[match(output_tab$smolt_year, outmigs$Smolt.Year)]
  output_tab$coho_redds_in_brood = redds$total_redds[match(output_tab$brood_year, redds$water_year-1)]

  #3. Assign disconnection and reconnection date predictors
  # 4. Total Flow metrics and 5. Functional Flow metrics assigned inside for loop
  for(i in 1:nrow(output_tab)){
    brood_yr = output_tab$brood_year[i]; smolt_yr = brood_yr+2

    # Subset dates for metric calcs
    BY_date1 = as.Date(paste0(brood_yr,"-09-01"))
    BY_date2 = as.Date(paste0(brood_yr,"-12-31"))
    BY_dates = seq.Date(from=BY_date1, to = BY_date2, by="day")

    BY_date2_recon = as.Date(paste0(brood_yr+1,"-02-28"))
    BY_dates_recon = seq.Date(from=BY_date1, to = BY_date2_recon, by="day")

    RY_date1 = as.Date(paste0(brood_yr+1,"-01-01"))
    RY_date2 = as.Date(paste0(brood_yr+1,"-12-31"))
    RY_dates = seq.Date(from=RY_date1, to = RY_date2, by="day")

    RY_date1_discon = as.Date(paste0(brood_yr+1,"-03-01"))
    RY_date2_discon = as.Date(paste0(brood_yr+1,"-08-31"))
    RY_dates_discon = seq.Date(from=RY_date1_discon, to = RY_date2_discon, by="day")

    RY_date1_recon = as.Date(paste0(brood_yr+1,"-09-01"))
    RY_date2_recon = as.Date(paste0(brood_yr+2,"-02-28"))
    RY_dates_recon = seq.Date(from=RY_date1_recon, to = RY_date2_recon, by="day")

    SY_date1 = as.Date(paste0(brood_yr+2,"-01-01"))
    SY_date2 = as.Date(paste0(brood_yr+2,"-07-31"))
    SY_dates = seq.Date(from=SY_date1, to = SY_date2, by="day")

    SY_date1_discon = as.Date(paste0(brood_yr+2,"-03-01"))
    SY_dates_discon = seq.Date(from=SY_date1_discon, to = SY_date2, by="day")

    # Subset flow for metric calcs
    BY_flow = hbsm$Flow[hbsm$Date %in% BY_dates & hbsm$smolt_year == smolt_yr]
    RY_flow = hbsm$Flow[hbsm$Date %in% RY_dates & hbsm$smolt_year == smolt_yr]
    SY_flow = hbsm$Flow[hbsm$Date %in% SY_dates & hbsm$smolt_year == smolt_yr]

    BY_flow_recon = hbsm$Flow[hbsm$Date %in% BY_dates_recon & hbsm$smolt_year == smolt_yr]
    RY_flow_discon = hbsm$Flow[hbsm$Date %in% RY_dates_discon & hbsm$smolt_year == smolt_yr]
    RY_flow_recon = hbsm$Flow[hbsm$Date %in% RY_dates_recon & hbsm$smolt_year == smolt_yr]
    SY_flow_discon = hbsm$Flow[hbsm$Date %in% SY_dates_discon & hbsm$smolt_year == smolt_yr]

    for(j in 1:length(thresholds)){
      thresh = thresholds[j]
      BY_recon_day_since_aug31  = calc_recon_days_since_aug_31(dates = BY_dates_recon, flow = BY_flow_recon, recon_threshold=thresh)
      RY_discon_day_since_aug31 = calc_discon_days_since_aug_31(dates = RY_dates_discon, flow = RY_flow_discon, discon_threshold=thresh)
      RY_recon_day_since_aug31 = calc_recon_days_since_aug_31(dates = RY_dates_recon, flow = RY_flow_recon, recon_threshold=thresh)
      SY_discon_day_since_aug31 = calc_discon_days_since_aug_31(dates = SY_dates_discon, flow = SY_flow_discon, discon_threshold=thresh)

      output_tab[i, paste0("BY_recon_",  thresh)] = BY_recon_day_since_aug31
      output_tab[i, paste0("RY_discon_", thresh)] = RY_discon_day_since_aug31
      output_tab[i, paste0("RY_recon_",  thresh)] = RY_recon_day_since_aug31
      output_tab[i, paste0("SY_discon_", thresh)] = SY_discon_day_since_aug31
    }

    # 4. Assign Total Flow metric predictors (if data's availble)
    # Subset flow for TAF calcs
    BY_flow_m3d = hbsm$Flow_m3day[hbsm$Date %in% BY_dates & hbsm$smolt_year == smolt_yr]
    RY_flow_m3d = hbsm$Flow_m3day[hbsm$Date %in% RY_dates & hbsm$smolt_year == smolt_yr]
    SY_flow_m3d = hbsm$Flow_m3day[hbsm$Date %in% SY_dates & hbsm$smolt_year == smolt_yr]

    n_BY = length(BY_flow_m3d); n_RY = length(RY_flow_m3d); n_SY = length(SY_flow_m3d)
    n_CFLP = sum(hbsm$smolt_year == smolt_yr)
    if(n_BY==122){
      output_tab[i,"BY_min_flow_sepdec"] =  min(BY_flow_m3d)
      output_tab[i, "BY_tot_flow_sepdec"] = sum(BY_flow_m3d) / (10^6)
      output_tab[i, "log_BY_tot_flow_sepdec"] = log10(sum(BY_flow_m3d))
    }
    if(n_RY>=365){
      output_tab[i,"RY_min_flow"] =  min(RY_flow_m3d)
      output_tab[i, "RY_tot_flow"] = sum(RY_flow_m3d) / (10^6)
      output_tab[i, "log_RY_tot_flow"] = log10(sum(RY_flow_m3d))
    }
    if(n_SY>=182){
      output_tab[i,"SY_min_flow_janjul"] =  min(SY_flow_m3d)
      output_tab[i, "SY_tot_flow_janjul"] = sum(SY_flow_m3d) / (10^6)
      output_tab[i, "log_SY_tot_flow_janjul"] = log10(sum(SY_flow_m3d))
    }
    if(n_CFLP>=669){
      output_tab[i, "tot_flow_CFLP"] = sum(c(BY_flow_m3d, RY_flow_m3d, SY_flow_m3d)) / (10^6)
      output_tab[i, "log_tot_flow_CFLP"] = log10(sum(c(BY_flow_m3d, RY_flow_m3d, SY_flow_m3d)))
    }

    #5. Assign Functional Flow metric predictors
    ## Brood Year FFs
    if(n_BY==122){
      output_tab[i, "BY_FA_Mag"] = fflows$FA_Mag[fflows$Water_Year == brood_yr + 1] # match up brood year to water year
      output_tab[i, "BY_FA_Tim"] = fflows$FA_Tim[fflows$Water_Year == brood_yr + 1] # match up brood year to water year
      output_tab[i, "BY_FA_Dur"] = fflows$FA_Dur[fflows$Water_Year == brood_yr + 1] # match up brood year to water year
    }
    ## Rearing Year FFs
    if(n_RY>=365){
      output_tab[i, "RY_Wet_BFL_Mag_50"] = fflows$Wet_BFL_Mag_50[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_Wet_Tim"] = fflows$Wet_Tim[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_Wet_BFL_Dur"] = fflows$Wet_BFL_Dur[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_SP_Tim"] = fflows$SP_Tim[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_SP_ROC"] = fflows$SP_ROC[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_DS_Tim"] = fflows$DS_Tim[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_DS_Mag_50"] = fflows$DS_Mag_50[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_DS_Mag_90"] = fflows$DS_Mag_90[fflows$Water_Year == brood_yr + 1]
      output_tab[i, "RY_DS_Dur_WS"] = fflows$DS_Dur_WS[fflows$Water_Year == brood_yr + 1]
      if(is.element(el=brood_yr+2, set = fflows$Water_Year)){
        output_tab[i, "RY_FA_Mag"] = fflows$FA_Mag[fflows$Water_Year == brood_yr + 2]
        output_tab[i, "RY_FA_Tim"] = fflows$FA_Tim[fflows$Water_Year == brood_yr + 2]
        output_tab[i, "RY_FA_Dur"] = fflows$FA_Dur[fflows$Water_Year == brood_yr + 2]
      }
    }
    ## Smolt Year FFs
    if(n_SY>=182 & is.element(el=brood_yr+2, set = fflows$Water_Year)){
      # output_tab[i, "SY_Wet_BFL_Mag_50"] = fflows$Wet_BFL_Mag_50[fflows$Water_Year == brood_yr + 2]
      output_tab[i, "SY_Wet_Tim"] = fflows$Wet_Tim[fflows$Water_Year == brood_yr + 2]
      output_tab[i, "SY_Wet_BFL_Dur"] = fflows$Wet_BFL_Dur[fflows$Water_Year == brood_yr + 2]
      # output_tab[i, "SY_SP_Tim"] = fflows$SP_Tim[fflows$Water_Year == brood_yr + 2]
      output_tab[i, "SY_SP_ROC"] = fflows$SP_ROC[fflows$Water_Year == brood_yr + 2]
    }

    # 6. Storm days
    cfs_90th_pctile = quantile(fj_flow$Flow, 0.9)
    # output_tab[i, "BY_num_days_gt_90_pctile"] = sum(BY_flow > cfs_90th_pctile)
    # output_tab[i, "RY_num_days_gt_90_pctile"] = sum(RY_flow > cfs_90th_pctile)
    # output_tab[i, "SY_num_days_gt_90_pctile"] = sum(SY_flow > cfs_90th_pctile)

  }

  #remove SY discon 20, since that usually happens after the smolt leave the watershed
  problem_discons = paste("SY_discon", thresholds[thresholds < 60], sep = "_")
  output_tab = output_tab[,!(colnames(output_tab) %in% problem_discons)]

  return(output_tab)
}


# V1 Linear Model Selection functions ----------------------------------------




calc_corr_matrix=function(metrics_tab, corr_method = "pearson", min_pairs = 4,
                          fish_outcome_cols = NA){

  if(sum(is.na(fish_outcome_cols)) >0){
    fish_outcome_cols = colnames(metrics_tab)[grepl(pattern = "coho", x = colnames(metrics_tab)) |
                                                grepl(pattern = "chinook", x = colnames(metrics_tab))]
  }
  if(fish_outcome_cols == "coho" & fish_outcome_cols != "chinook"){
    # fish_outcome_cols = colnames(metrics_tab)[grepl(pattern = "coho", x = colnames(metrics_tab)) &
    #                                             colnames(metrics_tab) != "percent_coho_smolt_survival"]
    fish_outcome_cols = c("coho_spawner_abundance", "coho_redds_in_brood",
                          "coho_smolt_abun_est", "coho_smolt_per_fem")
  } else if(fish_outcome_cols == "chinook" & fish_outcome_cols != "coho"){
    fish_outcome_cols = colnames(metrics_tab)[grepl(pattern = "chinook", x = colnames(metrics_tab))]
  }

  predictor_cols = colnames(metrics_tab)[grepl(pattern = "BY", x = colnames(metrics_tab)) |
                                           grepl(pattern = "RY", x = colnames(metrics_tab))|
                                           grepl(pattern = "SY", x = colnames(metrics_tab)) |
                                           grepl(pattern = "tot", x = colnames(metrics_tab)) |
                                           grepl(pattern = "min", x = colnames(metrics_tab)) ]

  #Initialize output matrix
  output_tab = data.frame(matrix(data=NA, nrow = length(predictor_cols),ncol = length(fish_outcome_cols)))
  for(j in 1:length(fish_outcome_cols)){
    outcome_col = fish_outcome_cols[j]
    y = metrics_tab[,outcome_col]
    for(i in 1:length(predictor_cols)){
      predictor_col = predictor_cols[i]
      x = metrics_tab[,predictor_col]

      if(sum(!is.na(x) & !is.na(y)) >= min_pairs){
        output_tab[i, j] = cor(x = x, y = y, use = "pairwise.complete.obs", method = corr_method)
      }
    }
  }
  colnames(output_tab) = fish_outcome_cols
  rownames(output_tab) = predictor_cols
  return(output_tab)
}



corr_matrix_fig = function(corr_matrix, thresholds = c(10,20,30,40,60,100)){

  # clear out metrics in RY and SY that don't affect the number of spawners (in BY)
  ry_and_sy_rows = grepl(pattern = "RY", x = row.names(corr_matrix)) |
    grepl(pattern = "SY", x = row.names(corr_matrix))
  corr_matrix$coho_spawner_abundance[ry_and_sy_rows] = NA
  corr_matrix$coho_redds_in_brood[ry_and_sy_rows] = NA


  ## Prepare for plotting
  #Prettify column names
  colnames(corr_matrix) = c("Num. Spawners","Num. Redds", "Est. Num. Smolt", "Smolt per Female")

  # Subset into 2 correlation matrices for 2 side by side plots

  # Matrix 1, Subset rows for recon, disconnection dates
  conn_date_rows = paste(sort(rep(c("BY_recon", "RY_discon","RY_recon","SY_discon"),length(thresholds))),
                         thresholds,  sep = "_")
  corr_matrix1 = corr_matrix[row.names(corr_matrix) %in% conn_date_rows,]
  # Prettify row names for corr matrix 1
  conn_date_rownames =  paste(sort(rep(c("BY recon.", "RY discon.",
                                         "RY recon.", "SY discon."),
                                       length(thresholds))),
                              "day,", thresholds, "cfs", sep = " ")
  # extract the SY problem recon dates
  problem_rownames = paste(rep(c("SY discon."), 4),
                           "day,", thresholds[thresholds<60],
                           "cfs", sep = " ")
  conn_date_rownames = conn_date_rownames[!(conn_date_rownames %in% problem_rownames)]
  row.names(corr_matrix1) = conn_date_rownames

  # Matrix 2, Subset for total flow and Functional Flow metrics
  corr_matrix2 = corr_matrix[!(row.names(corr_matrix) %in% conn_date_rows),]

  # Prettify row names for corr matrix 2
  other_rownames = c("BY min. flow Sep-Dec","RY min. flow","SY min. flow Jan-Jul",
                     "Total Flow CFLP" ,"BY Tot. Flow Sep-Dec",
                     "RY Tot. Flow" ,"SY Tot. Flow Jan-Jul",
                     "log of Tot. Flow CFLP" ,"log of BY Tot. Flow Sep-Dec",
                     "log of RY Tot. Flow" ,"log of SY Tot. Flow Jan-Jul",
                     "BY FA_Mag", "BY FA_Tim", "BY FA_Dur",
                     "RY Wet_Tim","RY Wet_BFL_Dur", "RY SP_ROC",
                     "RY DS_Mag_50", "RY DS_Mag_90", "RY FA_Mag",
                     "RY FA_Tim", "RY FA_Dur", "RY Wet_BFL_Mag_50",
                     "RY DS_Tim","RY SP_Tim", "RY DS_Dur_WS",
                     "SY Wet_Tim" ,"SY Wet_BFL_Dur", "SY SP_ROC")
  rownames(corr_matrix2) = other_rownames

  #make plots, side by side
  par(mfrow = c(1,2))

  corrplot(as.matrix(corr_matrix1), #cex.axis = .5,
           xlab = "", ylab = "", main = "", na.label = "--",
           axis.row = list(side = 2, las = 1), axis.col = list(side = 1, las = 2),
           col = c("orangered3", "lightpink", "lightskyblue","deepskyblue4"),
           cl.pos = "b")

  corrplot(as.matrix(corr_matrix2), #cex.axis = .5,
           xlab = "", ylab = "", main = "", na.label = "--",
           axis.row = list(side = 2, las = 1), axis.col = list(side = 1, las = 2),
           col = c("orangered3", "lightpink", "lightskyblue","deepskyblue4"),
           cl.pos = "b")

}

correl_scatter_plots= function(metrics_tab, predicted, y_label){
  # add a log flow term
  metrics_tab$BY_tot_flow_sepdec_log = log10(metrics_tab$BY_tot_flow_sepdec)

  # Prep for plots
  # predictors = c("BY_recon_10", "BY_recon_100", "RY_discon_20",
  #                "BY_tot_flow_sepdec_log","BY_FA_Mag",
  #                "RY_Wet_Tim","RY_Wet_BFL_Dur", "RY_FA_Mag" )

  predictors = c("BY_recon_10", "BY_recon_100",
                 "BY_FA_Mag", "RY_Wet_Tim",
                 "RY_Wet_BFL_Dur", "RY_SP_ROC",#"RY_SP_ROC",
                 "RY_discon_10","RY_discon_100") # "log_BY_tot_flow_sepdec" "RY_tot_flow"

  predictors_longname = c("Brood Year Fall Reconnection, 10 cfs",
                          "Brood Year Fall Reconnection, 100 cfs",
                          "Brood Year Fall Pulse Flow Magnitude",
                          "Rearing Year Wet Season Onset",
                          "Rearing Year Wet Season Duration",
                          "Rearing Year Spring Recession rate of change",
                          "Rearing Year Spring Disconnection, 10 cfs",
                          # "Total Flow, Rearing Year"
                          "Rearing Year Spring Disconnection, 100 cfs"
                          # "Log of Brood Year Total Flow, Sep-Dec",
                          # "Rearing Year Fall Pulse Flow Magnitude"
                          )
  predictors_units = c("Days post-Aug. 31","Days post-Aug. 31",
                       "cfs", "Days post-Oct. 1",
                       "Days", "%/day", "Days post-Aug 31.", "Days post-Aug 31.")
  leg_pos = c(rep("topright", 2), "bottomright","topright","topleft","bottomright","topleft","bottomright")
  pc = data.frame(predictor = predictors,
                  colors = rainbow(n=length(predictors)))


  par(mfrow = c(4,2), mar = c(5,4,3,2))

  for(i in 1:length(predictors)){
    x_name = predictors[i]
    long = predictors_longname[i]
    p_units = predictors_units[i]

    plot(x=metrics_tab[,x_name],
         y=metrics_tab[,predicted],
         pch = 19, col = pc$colors[i], cex = 1.5,
         # main = paste(long, "vs Fish Population"),
         ylab = y_label,
         xlab = paste0(long, " (", p_units,")"))
    example_model = lm(metrics_tab[,predicted] ~ metrics_tab[,x_name])
    abline(example_model, lwd = 3, lty = 2)

    text1 = paste0("Slope: ", round(example_model$coefficients[2], digits = 2))
    text2 = paste0("R square: ", round(summary(example_model)$r.square, digits = 2))
    legend(x= leg_pos[i], col = NA,legend = c(text1, text2),
           bg=NA, border.col=NA)
  }
}

all_lms_tab_and_plot = function(data_tab,
                                        y_name = "coho_smolt_per_fem",
                                        num_xs = 1, graph_p_adjR=T, alpha_val = .5,
                                xlab_text = NA, ylab_text = NA){

  # Initialize output tab as a combination of
  non_pred_columns = c("brood_year","smolt_year", "chinook_spawner_abundance",
                       "chinook_juvenile_abundance", "chinook_juv_per_adult",
                       "coho_spawner_abundance", "coho_smolt_per_fem",
                       "coho_smolt_abun_est","percent_coho_smolt_survival",
                       "coho_redds_in_brood")
  data_tab_pred = data_tab[,!(colnames(data_tab) %in% non_pred_columns)]
  cdtb = colnames(data_tab_pred)
  if(num_xs==1){model_combos = cdtb}
  if(num_xs==2){model_combos = expand.grid(cdtb, cdtb)}
  if(num_xs==3){model_combos = expand.grid(cdtb, cdtb, cdtb)}
  if(num_xs==4){model_combos = expand.grid(cdtb, cdtb, cdtb, cdtb)}
  if(num_xs==5){model_combos = expand.grid(cdtb, cdtb, cdtb, cdtb, cdtb)}
  if(num_xs==6){model_combos = expand.grid(cdtb, cdtb, cdtb, cdtb, cdtb, cdtb)}

  output_tab = as.data.frame(as.matrix(model_combos))
  pred_colnames = paste0("x", 1:num_xs)
  colnames(output_tab) = pred_colnames
  # eliminate models with repeat predictors. ugh. hard to do this without knowing the # of predictors
  # dup_rows = (output_tab$x1==output_tab$x2) | (output_tab$x2==output_tab$x3) | (output_tab$x1==output_tab$x3)
  # output_tab = output_tab[!dup_rows,]

  # add model
  output_tab$Fstat = NA; output_tab$pval = NA
  output_tab$Rsquare = NA; output_tab$adjRsquare = NA
  output_tab$AICc = NA

  for(i in 1:nrow(output_tab)){
    predictors = as.character(output_tab[i, 1:num_xs])

    if(length(unique(predictors)) == num_xs){ # weed out combos with redundant factors (again)
      f1 = paste(y_name,
                 paste(predictors, collapse = " + "), sep = " ~ ")
      lm1 = lm(data = data_tab, formula = f1)
      coeffs = summary(lm1)$coefficients

      if(sum(is.na(coeffs))<1){  # Don't populate the output tab if there's insufficient pairwise complete data
        #assign output tab vars
        output_tab$Fstat[i] = summary(lm1)$fstatistic["value"]
        output_tab$pval[i] =  coeffs[1,ncol(coeffs)]
        output_tab$Rsquare[i] = summary(lm1)$r.squared
        output_tab$adjRsquare[i] = summary(lm1)$adj.r.squared
        output_tab$AICc[i] = AICc(lm1)
      }
    }
  }

  if(graph_p_adjR==T){
    y_name_long = y_name
    if(y_name=="coho_spawner_abundance"){y_name_long = "Coho Spawner Abundance"}
    if(y_name=="coho_smolt_abun_est"){y_name_long = "Coho Smolt Abundance"}
    if(y_name=="coho_smolt_per_fem"){y_name_long = "Coho Smolt per Female Spawner"}
    if(y_name=="coho_redds_in_brood"){y_name_long = "Coho Redds"}
    if(y_name=="chinook_juv_per_adult"){y_name_long = "Juvenile Chinook per Adult"}
    if(y_name=="chinook_juvenile_abundance"){y_name_long = "Chinook Juvenile Abundance"}
    if(y_name=="chinook_spawner_abundance"){y_name_long = "Chinook Spawner Abundance"}
    # if(num_xs == 1){title_text = paste("All models of", y_name_long,"\n with", num_xs, "predictor")}
    # if(num_xs > 1){title_text = paste("All models of", y_name_long,"\n with", num_xs, "predictors")}
    if(num_xs == 1){title_text = paste0(y_name_long,", \n ", num_xs, " predictor")}
    if(num_xs > 1){title_text = paste0(y_name_long,", \n ", num_xs, " predictors")}

    #plot color

    rgb_val = col2rgb("violet")
    if(y_name=="coho_smolt_per_fem"){rgb_val = c(1,0,0)}
    if(y_name=="chinook_juv_per_adult"){rgb_val = c(0,0,1)}


    plot(output_tab$pval ,output_tab$adjRsquare, pch = 21,
         bg = rgb(rgb_val[1],rgb_val[2],rgb_val[3],alpha_val),
         col = NULL, # border color
         ylim = c(-1,1),#c(min(output_tab$adjRsquare, na.rm=T), 1),
         xlab = xlab_text, ylab = ylab_text,
         # xlab = "linear model P value", ylab = "Adj. R square",
         main = title_text)
    grid()
    abline(h=0)
  }

  return(output_tab)


}

threshold_explainer_trendlines = function(data_tab, y_name = "coho_smolt_per_fem",
                                          pred_type = "BY_recon",
                                          thresholds = c(8,10,15,20,40,100),
                                          print_r2=F){

  par(mfrow = c(3,2), mar = c(4,4,1,2))
  # thresholds = thresh_for_fig #c(8,10,15,20,40,100)
  ylimits = range(data_tab[,y_name],na.rm = T)
  if(y_name=="coho_smolt_per_fem"){ylab_text = "Coho smolt per female spawner (spf)"}
  else if(y_name=="chinook_juv_per_adult"){ylab_text = "Chinook juveniles per adult (jpa)"}
  else {ylab_text = y_name}
  ylab_6 = c(ylab_text, "", ylab_text,"", ylab_text,"" )
  if(pred_type == "BY_recon"){
    xlimits = c(0,165)
    xlab_text = "Recon. in BY: Days after Aug. 31 of BY with flow > threshold"
    text_x = c(rep(130,6))
    text_y = c(rep(95,6))
    point_col = "darkgoldenrod"
  }
  if(pred_type == "RY_discon"){
    xlab_text = "Discon. in RY: First day after wet season with flow < threshold (days after Aug 31 of BY)"
    xlimits = c(300,370)
    text_x = c(rep(315,2),320,315,315,360)
    text_y = c(rep(95,2),rep(25,4))
    point_col = "olivedrab4"
  }
  xlab_6 = c("", "","","",xlab_text,xlab_text)


  # assign colors based on year? meh
  for(i in 1:length(thresholds)){
    thresh=thresholds[i]
    tx=text_x[i]; ty=text_y[i]
    plot(x = data_tab[,paste0(pred_type,"_",thresh)],
         y = data_tab[,y_name],
         ylim = ylimits, xlim = xlimits,
         pch = 19, col = point_col, cex = 1.5,
         xlab = paste(xlab_6[i]),
         ylab = ylab_6[i])
    grid()
    lm1 = lm(data = data_tab, formula = paste0(y_name, " ~ ", paste0(pred_type,"_",thresh)))
    abline(lm1, lty = 2, lwd = 2)
    abline(v = 244, lty =2, lwd = 1.5, col = "orange")
    text(x=tx, y = ty, paste(thresh,"cfs"), cex = 1.5)
    text(x = tx, y = ty-10, paste("Slope:", round(summary(lm1)$coefficients[2,1], 2), "spf/day"))
    R2_lab  = bquote(italic(R)^2 == .(format(summary(lm1)$r.squared, digits = 3)))
    text(x = tx, y = ty-20, R2_lab)
    if(print_r2==T){print(paste(thresh,R2_lab))}
  }
}


make_lm_table_for_rmd = function(best_tab, y_name = "coho_smolt_per_fem", data_tab,
                                 include_pluses=F){
  # make output table
  output_tab = as.data.frame(matrix(data = NA, nrow = nrow(best_tab), ncol = 7))
  colnames(output_tab) = c("lm_id","preds","fstat", "pval", "Rsquare","adjRsquare","AICc")
  output_tab$lm_id = best_tab$model_id

  x_cols = grepl(pattern = "x", x = colnames(best_tab))
  for(i in 1:nrow(best_tab)){
    preds = best_tab[i, x_cols][!(is.na(best_tab[i, x_cols]) | best_tab[i, x_cols]=="") ]

    pred_string = paste(preds, collapse = " + ")
    output_tab$preds[i] = pred_string

    if(include_pluses==F){
      pred_string_noplus = paste(preds, collapse = ", ")
      output_tab$preds[i] = pred_string_noplus
      }

    f1 = paste(y_name, pred_string, sep = " ~ ")
    lm_i = lm(data = data_tab, formula = f1)
    coeffs = summary(lm_i)$coefficients

    output_tab$fstat[i] = summary(lm_i)$fstatistic[1]
    output_tab$pval[i] = coeffs[1,ncol(coeffs)]
    output_tab$Rsquare[i] = summary(lm_i)$r.squared
    output_tab$adjRsquare[i] = summary(lm_i)$adj.r.squared
    output_tab$AICc[i] = AICc(lm_i)
  }
  # round to reasonable digits
  output_tab$fstat = output_tab$fstat #round(output_tab$fstat, 1)
  output_tab$pval = output_tab$pval # round(output_tab$pval, 3)
  output_tab$Rsquare = output_tab$Rsquare # round(output_tab$Rsquare, 3)
  output_tab$adjRsquare = output_tab$adjRsquare# round(output_tab$adjRsquare, 3)
  output_tab$AICc = output_tab$AICc# round(output_tab$AICc, 1)

  return(output_tab)
}

best_lms_slopes_table = function(best_tab, data_tab,
                                 y_name = "coho_smolt_per_fem"){

  rownames(best_tab)=best_tab$model_id; best_tab=best_tab[,colnames(best_tab)!="model_id"]
  n_lms = nrow(best_tab)
  preds = unique(unlist(best_tab))
  preds = preds[preds!="" & !is.na(preds)] # take out the blanks
  n_preds = length(preds)
  output_tab = as.data.frame(matrix(data =NA, nrow = n_lms, ncol = n_preds+1))
  rownames(output_tab)=as.character(rownames(best_tab))
  colnames(output_tab) = c("Intercept",preds)

  for(i in 1:n_lms){
    preds_lm = best_tab[i, !is.na(best_tab[i,]) & best_tab[i,]!=""]
    n_preds_lm = length(preds_lm)
    f1 = paste(y_name,
               paste(preds_lm, collapse = " + "), sep = " ~ ")
    lm_i = lm(data = data_tab, formula = f1)
    coeffs = data.frame(summary(lm_i)$coefficients)

    output_tab[i,"Intercept"] =coeffs$Estimate[row.names(coeffs)=="(Intercept)"]
    for(j in 1:n_preds_lm){
      pred = as.character(preds_lm[j])
      slope_val = coeffs$Estimate[row.names(coeffs)==pred]
      output_tab[i, colnames(output_tab)==pred] = slope_val
    }
  }
  return(output_tab)

}


homebrew_loocv = function(data_tab, lm_name = "lm1",
                          y_name = "coho_smolt_per_fem",
                          preds_lm ="BY_recon_10",
                          point_col = "salmon", point_pch=19,
                          output_type = "LOOCV"){

  f1 = paste(y_name, sep = " ~ ",
             paste(preds_lm, collapse = " + "))

  output_tab = data_tab
  output_tab$loocv_obs_minus_predicted = NA

  for(i in 1:nrow(data_tab)){
    obs_i = data_tab[i,y_name]
    predictor_vals_i = as.numeric(data_tab[i, preds_lm])

    # build lm from subset
    data_sub = data_tab[-i,] # leave this one out
    lm_sub = lm(data = data_sub, f1)

    coeffs = data.frame(summary(lm_sub)$coefficients)

    lm_int = coeffs$Estimate[row.names(coeffs)=="(Intercept)"]
    slopes = as.numeric(coeffs$Estimate[row.names(coeffs)!="(Intercept)"])

    predicted_val_i = lm_int + sum(slopes * predictor_vals_i)

    output_tab$loocv_obs_minus_predicted[i] = obs_i - predicted_val_i
  }
  output_tab$MSE = (output_tab$loocv_obs_minus_predicted)^2
  CV_n = 1/nrow(output_tab) * sum(output_tab$MSE)

  if(output_type == "table"){return(output_tab)}
  if(output_type == "LOOCV"){return(CV_n)}

  if(output_type == "observed vs predicted"){
    lm_all_data = lm(data = data_tab, formula = f1)

    predicted = predict.lm(object = lm_all_data, newdata = data_tab)

    plot(data_tab[,y_name], predicted, # observed on x and predicted on y
         col = point_col, cex= 2, pch = point_pch,
         ylim = c(-5,110), xlim = c(-5,110),
         ylab = "Predicted (coho spf)", xlab = "Observed (coho spf)",
         # main = "Linear model of coho smolt per female",
         # sub = paste("Predictor(s):",paste(preds_lm, collapse = " + "))
         )
    grid()
    legend("topleft",bg=NA,legend = lm_name, cex=1.5, border.col = NA)
  }


}

lm_pred_v_obs_fig = function(best_tab, metrics_tab, plot_rowscols = c(4,2)){

  lm_cols = c("indianred1","indianred3", # 1-predictor models
              "khaki3", "darkgoldenrod",        # 2-predictor models
              "olivedrab2","green4", # 3-pred models
              "lightblue3","dodgerblue") # 4-pred models
  lm_pch = c(15,18,17,16,2,6,3,4)

  par(mfrow = plot_rowscols, mar = c(4, 5,1,1))

  # Plot each model's predicted v observed

  for(i in 1:nrow(best_tab)){
    preds = best_tab[i,colnames(best_tab)!="model_id"]
    preds_i = preds[preds!="" & !is.na(preds)]

    # make plots
    homebrew_loocv(data_tab = metrics_tab, preds_lm = preds_i,
                   output_type = "observed vs predicted",
                   point_col = lm_cols[i], point_pch = 19,#point_pch = lm_pch[i],
                   lm_name = best_tab$model_id[i])
    abline(1,1,lty=2)
  }
}


# V1 HBF Results -----------------------------------------------------------------

calc_hbf_tab_feb2022 = function(thresholds_hbf = c(10,100), last_wy = 2021,
                                flow_tab_for_hbf, ch1_hbftab = F, weights,
                                scen_id = "hist_obs"){

  hbf_tab = re_and_disconnect_date_tab(thresholds = thresholds_hbf,
                                       fj_flow = flow_tab_for_hbf)
  hbf_tab = hbf_tab[hbf_tab$water_year <= last_wy,]
  hbf_tab = hbf_tab[,c("water_year","recon_date_10","recon_date_100")]

  fflows = read_fflows_csv(scen_id = scen_id)

  hbf_tab$Wet_Tim = fflows$Wet_Tim[match(hbf_tab$water_year,fflows$Water_Year)]
  hbf_tab$Wet_BFL_Dur = fflows$Wet_BFL_Dur[match(hbf_tab$water_year,fflows$Water_Year)]
  hbf_tab$SP_ROC = fflows$SP_ROC[match(hbf_tab$water_year,fflows$Water_Year)]


  if(ch1_hbftab ==T){
    write.csv(x = hbf_tab, quote = F, row.names = F,
              file = file.path(ms_dir, "Graphics and Supplements",
                               "Supplemental Table 2 - Flow Metrics by Water Year, 1942-2021.csv"))
  }

  # Calculate HBF component parts and add together
  hbf_comp = weights

  hbf_tab$Int = hbf_comp[1]
  hbf_tab$comp1 = hbf_comp[2] * hbf_tab$recon_date_10
  hbf_tab$comp2 = hbf_comp[3] * hbf_tab$recon_date_100
  hbf_tab$comp3 = hbf_comp[4] * hbf_tab$Wet_Tim
  hbf_tab$comp4 = hbf_comp[5] * hbf_tab$Wet_BFL_Dur
  hbf_tab$comp5 = hbf_comp[6] * hbf_tab$SP_ROC

  hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
    hbf_tab$comp3 + hbf_tab$comp4 + hbf_tab$comp5

  return(hbf_tab)
}


calc_hbf_tab_mar2022 = function(thresholds_hbf = c(10,100), last_wy = 2021,
                                flow_tab_for_hbf, ch1_hbftab = F, weights,
                                scen_id = "hist_obs"){

  hbf_tab = re_and_disconnect_date_tab(thresholds = thresholds_hbf,
                                       fj_flow = flow_tab_for_hbf)
  hbf_tab = hbf_tab[hbf_tab$water_year <= last_wy,]
  hbf_tab = hbf_tab[,c("water_year","recon_date_10","recon_date_100")]

  fflows = read_fflows_csv(scen_id = scen_id)

  hbf_tab$Wet_Tim = fflows$Wet_Tim[match(hbf_tab$water_year,fflows$Water_Year)]
  hbf_tab$Wet_BFL_Dur = fflows$Wet_BFL_Dur[match(hbf_tab$water_year,fflows$Water_Year)]


  if(ch1_hbftab ==T){
    write.csv(x = hbf_tab, quote = F, row.names = T,
              file = file.path(ms_dir, "Graphics and Supplements",
                               "Supplemental Table 2 - Flow Metrics by Water Year, 1942-2021.csv"))
  }

  # Calculate HBF component parts and add together
  hbf_comp = weights

  # hbf_tab$Int = hbf_comp[rownames(hbf_comp) == "Intercept" | names(hbf_comp) == "Intercept" ]
  # hbf_tab$comp1 = hbf_comp[rownames(hbf_comp) == "BY_recon_10"] * hbf_tab$recon_date_10
  # hbf_tab$comp2 = hbf_comp[rownames(hbf_comp) == "BY_recon_100"] * hbf_tab$recon_date_100
  # hbf_tab$comp3 = hbf_comp[rownames(hbf_comp) == "RY_Wet_Tim"] * hbf_tab$Wet_Tim
  # hbf_tab$comp4 = hbf_comp[rownames(hbf_comp) == "RY_Wet_BFL_Dur"] * hbf_tab$Wet_BFL_Dur

  hbf_tab$Int = hbf_comp[1]
  hbf_tab$comp1 = hbf_comp[2] * hbf_tab$recon_date_10
  hbf_tab$comp2 = hbf_comp[3] * hbf_tab$recon_date_100
  hbf_tab$comp3 = hbf_comp[4] * hbf_tab$Wet_Tim
  hbf_tab$comp4 = hbf_comp[5] * hbf_tab$Wet_BFL_Dur

  hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
    hbf_tab$comp3 + hbf_tab$comp4

  return(hbf_tab)
}


hbf_over_time_fig = function(hbf_tab, write_hist_HB_vals=F){
  # plot details
  neg_values = hbf_tab$hbf_total < 0
  neg_value_col = "red"; neg_val_pch = 5
  obs_col = "goldenrod"

  if(write_hist_HB_vals == T){
    write.csv(hbf_tab[,c("water_year","hbf_total")],
              file = "hist_HBV_2022.04.29.csv", quote = F, row.names = F)}

  # Plot predicted values
  plot(hbf_tab$water_year, hbf_tab$hbf_total, col = NA, yaxt = "n",
       xlab = "Water Year", ylab = "Hydrologic Benefit value (predicted coho spf-equiv.)")
  axis(side = 2, at = seq(from = -100, to = 200, by = 20))
  abline(v = seq(from = 1940, to = 2140, by = 10), h = seq(from = -100, to = 200, by = 20),
         lty = 3, col = "gray")
  abline(h=0, col = "darkgray")
  points(hbf_tab$water_year, hbf_tab$hbf_total, pch = 19)
  lines(hbf_tab$water_year, hbf_tab$hbf_total)
  # Add observed values
  points(metrics_tab$brood_year+1, metrics_tab$coho_smolt_per_fem,
         pch = 24, bg= obs_col, cex = 1.1)
  # Add arrows connecting predicted values with observed
  arrows_x = metrics_tab$brood_year+1
  arrows_y0 = metrics_tab$coho_smolt_per_fem
  arrows_y1 = hbf_tab$hbf_total[hbf_tab$water_year %in% arrows_x]
  arrows(x0 = arrows_x, y0 = arrows_y0, x1 = arrows_x, y1 = arrows_y1,
         length = 0, lty = 1, col = obs_col, lwd = 2)

  points(hbf_tab$water_year[neg_values], hbf_tab$hbf_total[neg_values],
         pch = neg_val_pch, col = neg_value_col, lwd=2)


  abline(h=0, col = "darkgray")
  legend("bottomleft", pch = c(19,neg_val_pch,24, NA),
         pt.lwd = c(NA,2,1, NA), pt.cex = c(1,1,1.2, NA), bg="white",
         col = c("black", neg_value_col,"black", obs_col),
         pt.bg=c(NA,NA,obs_col, NA), lwd = c(NA,NA,NA,2), lty = c(NA,NA,NA,1),
         legend = c("Predicted coho spf", "Predicted coho spf (neg. value)",
                    "Observed coho spf", "Pred. - Obs. difference"))
}





# V2 Linear Model Selection -----------------------------------------------

generate_pred_appear_tab = function(lasso_mod){
  coefs = as.data.frame((as.matrix(coef(lasso_mod))))
  non0_coef_lambda_index = apply(X = coefs, MARGIN = 1, function(x){min(which(abs(x)>0))})
  coefs$lambda_val_coef_appears = best_lam_range[non0_coef_lambda_index]
  coef_lambda_non0_vals = as.data.frame(cbind(rownames(coefs), coefs$lambda_val_coef_appears))
  colnames(coef_lambda_non0_vals) = c("predictor","lambda_non0_val_appears")
  coef_lambda_non0_vals$lambda_non0_val_appears = as.numeric(coef_lambda_non0_vals$lambda_non0_val_appears)
  # View predictors in order of when they enter the regression by increasing lambda value
  pred_appear_tab = coef_lambda_non0_vals[order(coef_lambda_non0_vals$lambda_non0_val_appears,
                                                decreasing=T),]
  pred_appear_tab = pred_appear_tab[!pred_appear_tab$predictor=="(Intercept)",]
  return(pred_appear_tab)
}

find_all_best_lambda_vals = function(com_tab, x, y,
                                     lam_vals = 10^seq(-2, 5,length=100)){
  output_tab = as.data.frame(com_tab)
  output_tab$bestlam = NA
  output_tab$rmse = NA
  for(i in 1:nrow(com)){
    train = com[i,]
    test = -com[i,]
    y.test = y[test]
    lasso_1 = glmnet(x[train,], y[train], alpha = 1, lambda = lam_vals)
    set.seed(1)
    cv.out=cv.glmnet(x[train,], y[train], alpha = 1)
    output_tab$bestlam[i] = cv.out$lambda.min
    # calculate an RMSE for this test set
    lasso_i=glmnet(x=x[test,], y=y.test, alpha = 1, lambda = output_tab$bestlam[i])
    lasso.pred=predict(lasso_i, s=output_tab$bestlam[i], newx=x)
    output_tab$rmse[i] = sqrt(mean((lasso.pred-y.test)^2))

  }

  return(output_tab)
}

plot_lasso_diagnostics = function(x, y, best_lam_range, lambdas_and_rmse){  # Plot lasso diagnostics.
  # par(mfrow=(c(2,1)))
  #A: deviance and non-0 coefficients
  lasso_3 = glmnet(x, y, alpha = 1, lambda = best_lam_range)
  deg_free_tab = print(lasso_3)
  par(mar=c(5,5,3,5))
  plot(deg_free_tab$Lambda, deg_free_tab$`%Dev`,  type = "l",
       ylab = "% of null deviation explained by model",
       xlab = "Lambda value (shrinkage penalty)",
       main = "Adding more coefficients explains more variation...")
  grid()
  par(new=T)
  plot(deg_free_tab$Lambda, deg_free_tab$Df, col = "dodgerblue", lwd = 2,
       xlab="",ylab="", axes=F, type = "l")
  axis(side=4,at=pretty(range(deg_free_tab$Df)))
  mtext("Degrees of freedom (number of non-0 coef.)", side = 4, line = 3)
  legend(x = "topright", col=c("black","dodgerblue"), lwd = c(1,2),
         legend = c("% Deviance", "Degrees of freedom"))

  # B) Plot lambda vs test error - setup
  nbins = 30
  bin_centers = seq(from=min(lambdas_and_rmse$bestlam), to = max(lambdas_and_rmse$bestlam),
                    length.out=nbins)
  categs = cut(x = lambdas_and_rmse$bestlam, breaks = nbins)
  avg_by_bin = aggregate(lambdas_and_rmse$rmse, by = list(categs), FUN=mean)
  # plot
  plot(lambdas_and_rmse$bestlam, lambdas_and_rmse$rmse, pch = 19, col = rgb(0.5,0.5,0.5,0.5),
       main = "... but test error is larger with more coefficients",
       xlab = "Lambda value (shrinkage penalty)",
       ylab = "Test error (RMSE) of models made \n from each set of 5 data points")
  # summarize by binning
  grid()
  points(bin_centers, avg_by_bin$x, pch=23, cex = 1.2, bg = "firebrick", type = "o")
  legend(x="topright", col = c("gray","firebrick"),bg=c(NA,"firebrick"),
         pch = c(19,23),
         legend = c("Individual model RMSE", "Binned average RMSE"))}

plot_lasso_coefs = function(lasso_mod, pred_appear_tab, best_lam_range, y_val_label){
  coefs = as.data.frame((as.matrix(coef(lasso_mod))))

  # Plot lambda vs highlighted coefs
  # standardize coefficients
  plot_tab = coefs / apply(X = coefs, MARGIN = 1, FUN = sd)
  plot_tab = as.data.frame(t(plot_tab))
  plot_tab[plot_tab==0] = NA # remove 0-values for plotting
  # add in lambda values for plotting
  plot_tab$lambda_val = best_lam_range

  # setup for plot
  tot_preds = sum(!is.na(pred_appear_tab$lambda_non0_val_appears))
  n_high = 7
  pred_names = pred_appear_tab$predictor
  pred_pal = colorblind_pal()(n_high)
  # Initialize plot
  plot(x=range(plot_tab$lambda_val), col=NA,
       # main = "Higher shrinkage penalties produce models with fewer and smaller coefficients",
       main = paste("Regression coefficients - predicting",y_val_label, "with hydrologic metrics"),
       y = range(plot_tab[,colnames(plot_tab)!="lambda_val"],na.rm=T),# ylim = c(-2,2),
       xlab = "Lambda value (shrinkage penalty)", ylab = "Standardized predictor coefficients")
  for(i in tot_preds:1){
    if(i>n_high){
      pred_col = "gray70"; pred_width = 1
    } else {
      pred_col = pred_pal[i]; pred_width = 2
    }
    pred_name = pred_names[i]
    lines(x=plot_tab$lambda_val, y = plot_tab[,pred_name], lwd=pred_width, col = pred_col)
  }
  grid()
  legend(x = "bottomright", legend = c(pred_names[1:n_high],"Other non-0 coef."),
         col = c(pred_pal[1:n_high],"gray70"), lwd = c(rep(2,n_high),1),
         ncol=2)
}


lasso_regression_plots = function(metrics_tab,
                                  y_val = "coho_smolt_per_fem",
                                  remove_extra_recon_thresholds = F,
                                  remove_SY_metrics = F){

  # Lasso regression. Informed by lab from ISLR 7th printing

  #step 1. Prep x matrix and y array
  # (Dev: run manuscript .Rmd through line 395)
  # 1a. remove rows with no response var
  mt = metrics_tab
  non_pred_vals = c("brood_year","smolt_year","chinook_spawner_abundance", "chinook_juvenile_abundance",
                    "chinook_juv_per_adult", "coho_smolt_per_fem",
                    "coho_spawner_abundance", "coho_smolt_per_fem", "coho_smolt_abun_est",
                    "percent_coho_smolt_survival", "coho_redds_in_brood")
  # if(y_val=="coho_smolt_per_fem"){non_pred_vals = c(non_pred_vals,"chinook_juv_per_adult")}
  # if(y_val=="chinook_juv_per_adult"){non_pred_vals = c(non_pred_vals,"coho_smolt_per_fem")}
  non_y_vals = non_pred_vals[non_pred_vals != y_val]
  mt = mt[,!(colnames(mt) %in% non_y_vals)]
  mt = mt[!is.na(mt[,y_val]),]
  na_col_detector = apply(X = mt, MARGIN = 2, FUN = sum)
  mt = mt[,!is.na(na_col_detector) ]

  # 1b. Optional. Remove the other thresholds or Smolt Year metrics
  if(remove_extra_recon_thresholds==T){
    remove_these = c("BY_recon_15", "BY_recon_20", "BY_recon_50", "BY_recon_80",
                     "RY_discon_15", "RY_discon_20", "RY_discon_50", "RY_discon_80",
                     "RY_recon_15", "RY_recon_20", "RY_recon_50", "RY_recon_80",
                     "SY_discon_80")
    mt = mt[,!(colnames(mt) %in% remove_these)]
  }
  if(remove_SY_metrics == T){
    remove_these = grepl(pattern = "SY", x=colnames(mt))
    mt = mt[,!remove_these]

  }


  # 2. Lasso Regression

  # Set up x and y, and retrieve table of best lambda and rmse values
  y = mt[,y_val]
  if(y_val=="coho_smolt_per_fem"){
    x = model.matrix(object = coho_smolt_per_fem~., data = mt)[,-1]
    lambda_tab_path = file.path(data_dir,paste( y_val,"- lambdas_and_rmse.csv" ))
  }
  if(y_val=="chinook_juv_per_adult"){
    x = model.matrix(object = chinook_juv_per_adult~., data = mt)[,-1]
    if(remove_SY_metrics==T){lambda_tab_path = file.path(data_dir, paste( y_val,"- no_SY lambdas_and_rmse.csv" ))}
    if(remove_SY_metrics==F){lambda_tab_path = file.path(data_dir,paste( y_val,"- lambdas_and_rmse.csv" ))}
  }

  # Find the range of "best" lambda values, based on cross-validation, and associated RMSE errors
  if(file.exists(lambda_tab_path)){lambdas_and_rmse = read.csv(lambda_tab_path)}
  if(!file.exists(lambda_tab_path)){
    # find all combinations of test and train data points for lambda values
    com = t(combn(x = 1:length(y), m = floor(length(y)/2)))
    # split data set into all possible test and train sets.
    # if total number of samples is more than 16, randomly sample only 10k of the possible test-train combos
    if(length(y)>15){
      set.seed(1)
      com = com[sample(x = 1:nrow(com), size = 10000),]
    }
    # calculate models across range of lambdas.
    # use cross-validation to find best lambda value for each set. Save table
    lambdas_and_rmse = find_all_best_lambda_vals(com_tab = com, x = x, y = y)
    write.csv(lambdas_and_rmse, lambda_tab_path, quote=F, row.names = F)
  }
  min_lam = min(lambdas_and_rmse$bestlam)
  max_lam = max(lambdas_and_rmse$bestlam)
  by_val = diff(range(lambdas_and_rmse$bestlam))/99
  # } else { #stored values for best lambdas
  #   # if(y_val=="coho_smolt_per_fem"){min_lam = 0.1; max_lam = 40; by_val = 0.1}
  #   # if(y_val=="chinook_juv_per_adult"){min_lam = 0.5; max_lam = 195; by_val = 2}
  # }

  best_lam_range = rev(seq(min_lam, max_lam, by_val)) #lambdas in reverse order to match coefs output

  # Calculate lasso models over range of lambda values
  lasso_3 = glmnet(x, y, alpha = 1, lambda = best_lam_range)
  # find lambda values at which each coefficient becomes non-0
  pred_appear_tab = generate_pred_appear_tab(lasso_3)
  # Plots
  par(mfrow=c(3,1))
  if(y_val=="coho_smolt_per_fem"){y_val_label = "coho spf"}
  if(y_val=="chinook_juv_per_adult"){y_val_label = "Chinook jpa"}
  plot_lasso_coefs(lasso_mod = lasso_3, pred_appear_tab = pred_appear_tab,
                   best_lam_range = best_lam_range, y_val_label = y_val_label)
  plot_lasso_diagnostics(x=x, y=y, best_lam_range, lambdas_and_rmse = lambdas_and_rmse)

}

# lasso_regression_plots(metrics_tab = metrics_tab)
# lasso_regression_plots(metrics_tab = metrics_tab, y_val = "chinook_juv_per_adult")
# lasso_regression_plots(metrics_tab = metrics_tab, y_val = "chinook_juv_per_adult",
#                        remove_SY_metrics=T)

# Supplemental lm tables --------------------------------------------------


make_table_of_all_lms = function(data_tab, y_name = "coho_smolt_per_fem",
                                 num_xs = 1, graph_p_adjR=F){

  # Initialize output tab as a combination of
  non_pred_columns = c("brood_year","smolt_year", "chinook_spawner_abundance",
                       "chinook_juvenile_abundance", "chinook_juv_per_adult",
                       "coho_spawner_abundance", "coho_smolt_per_fem",
                       "coho_smolt_abun_est","percent_coho_smolt_survival",
                       "coho_redds_in_brood")
  data_tab_pred = data_tab[,!(colnames(data_tab) %in% non_pred_columns)]
  cdtb = colnames(data_tab_pred)
  if(num_xs==1){model_combos = cdtb}
  if(num_xs==2){model_combos = t(combn(x = cdtb, m = 2))}
  if(num_xs==3){model_combos = t(combn(x = cdtb, m = 3))}
  if(num_xs==4){model_combos = t(combn(x = cdtb, m = 4))}
  if(num_xs==5){model_combos = t(combn(x = cdtb, m = 5))}
  if(num_xs==6){model_combos = t(combn(x = cdtb, m = 6))}

  output_tab = as.data.frame(model_combos)
  pred_colnames = paste0("x", 1:num_xs)
  colnames(output_tab) = pred_colnames
  # eliminate models with repeat predictors. ugh. hard to do this without knowing the # of predictors
  # dup_rows = (output_tab$x1==output_tab$x2) | (output_tab$x2==output_tab$x3) | (output_tab$x1==output_tab$x3)
  # output_tab = output_tab[!dup_rows,]

  # add model
  output_tab$Fstat = NA; output_tab$pval = NA
  output_tab$Rsquare = NA; output_tab$adjRsquare = NA
  output_tab$AICc = NA

  for(i in 1:nrow(output_tab)){
    predictors = as.character(output_tab[i, 1:num_xs])

    if(length(unique(predictors)) == num_xs){ # weed out combos with redundant factors (again)
      f1 = paste(y_name,
                 paste(predictors, collapse = " + "), sep = " ~ ")
      lm1 = lm(data = data_tab, formula = f1)
      coeffs = summary(lm1)$coefficients

      if(sum(is.na(coeffs))<1){  # Don't populate the output tab if there's insufficient pairwise complete data
        #assign output tab vars
        output_tab$Fstat[i] = summary(lm1)$fstatistic["value"]
        output_tab$pval[i] =  coeffs[1,ncol(coeffs)]
        output_tab$Rsquare[i] = summary(lm1)$r.squared
        output_tab$adjRsquare[i] = summary(lm1)$adj.r.squared
        output_tab$AICc[i] = AICc(lm1)
      }
    }
  }

  if(graph_p_adjR==T){
    plot(output_tab$pval ,output_tab$adjRsquare,
         main = paste("All Models with", num_xs, "predictors"))
  }

  return(output_tab)
}

show_lm_summary_from_table = function(pred_tab, data_tab, y_name = "coho_smolt_per_fem"){
  # assumes each predictor is in a column with an "x1", "x2" name, etc
  x_cols_selector = grepl(pattern = "x", x = colnames(pred_tab)) | grepl(pattern="pred",x=colnames(pred_tab))
  num_xs = sum(x_cols_selector)

  f1 = paste(y_name,
             paste(pred_tab[1,x_cols_selector], collapse = " + "), sep = " ~ ")
  lm1 = lm(data = data_tab, formula = f1)
  print(summary(lm1))

  # str(lm1)
}

add_loocv_column_to_lm_diagnostics_tab = function(data_tab, lm_tab,
                                                  y_name = "coho_smolt_per_fem"){
  lm_tab$loocv = NA
  lm_tab$n = NA
  data_tab = data_tab[!is.na(data_tab[,y_name]),] # reduce data tab to rows with response
  for(i in 1:nrow(lm_tab)){
    preds_i = as.character(lm_tab[i, grepl(pattern = "pred", x=colnames(lm_tab))])
    if(length(preds_i)>1){preds_na_detector = is.na(apply(X = data_tab[,preds_i], MARGIN = 1, FUN = sum))}
    if(length(preds_i)==1){preds_na_detector = is.na(data_tab[,preds_i])}
    # reduce data tab to rows with predictors
    data_tab_i = data_tab[!preds_na_detector,]
    n_val = nrow(data_tab_i)
    lm_tab$n[i] = n_val
    loocv_val = homebrew_loocv(data_tab = data_tab_i,
                           y_name = y_name,
                           preds_lm = preds_i)
    if(loocv_val >100){lm_tab$loocv[i] = round(loocv_val)}
  }
#
#   if(num_xs == 2){
#     homebrew_loocv(data_tab = data_tab, preds_lm = preds_i)
#   }
  return(lm_tab)
}

save_lm_diagnostics_tables = function(metrics_tab, show_best_analysis = F, show_best_analysis_2=F){
  # March 2024: retain all metrics for these tables
  save_dir = graphics_dir

  ### 1a. Selection of best 1 predictor model: coho
  pred1 = make_table_of_all_lms(data_tab = metrics_tab, y_name = "coho_smolt_per_fem",
                                num_xs = 1, graph_p_adjR=F)
  colnames(pred1)[1] = "pred1"
  pred1 = add_loocv_column_to_lm_diagnostics_tab(data_tab = metrics_tab, lm_tab = pred1)   # add LOOCV column
  write.csv(pred1, quote=F, row.names = F,
            file=file.path(save_dir, "Supplemental Table 3 - One Predictor Models of Coho spf.csv"))

  ### 1b. Selection of best 1 predictor model: Chinook
  pred1_ch = make_table_of_all_lms(data_tab = metrics_tab, graph_p_adjR=F, y_name = "chinook_juv_per_adult", num_xs = 1)
  colnames(pred1_ch)[1] = "pred1"
  pred1_ch = add_loocv_column_to_lm_diagnostics_tab(data_tab = metrics_tab, y_name = "chinook_juv_per_adult",
                                                    lm_tab = pred1_ch)   # add LOOCV column
  write.csv(pred1_ch, file=file.path(save_dir,"Supplemental Table 4 - One Predictor Models of Chinook jpa.csv"),
            quote=F, row.names = F)

  ### 2a. Selection of best 2 predictor model: coho
  pred2 = make_table_of_all_lms(data_tab = metrics_tab, graph_p_adjR=F,
                                y_name = "coho_smolt_per_fem", num_xs = 2)
  pred2 = pred2[!is.na(pred2$pval),] # get rid of NA rows
  colnames(pred2)[1:2] = c("pred1", "pred2")
  pred2 = add_loocv_column_to_lm_diagnostics_tab(data_tab = metrics_tab, y_name = "coho_smolt_per_fem",
                                                 lm_tab = pred2)   # add LOOCV column
  write.csv(pred2, quote=F, row.names = F,
            file=file.path(save_dir,"Supplemental Table 5 - Two Predictor Models of Coho spf.csv"))

  ### 2b. Selection of best 2 predictor model: Chinook
  pred2_ch = make_table_of_all_lms(data_tab = metrics_tab, graph_p_adjR=F,
                                   y_name = "chinook_juv_per_adult", num_xs = 2)
  pred2_ch = pred2_ch[!is.na(pred2_ch$pval),] # get rid of NA rows
  pred2_ch = pred2_ch[!duplicated(pred2_ch$Fstat,4),] # get rid of duplicates
  colnames(pred2_ch)[1:2] = c("pred1", "pred2")
  pred2_ch = add_loocv_column_to_lm_diagnostics_tab(data_tab = metrics_tab, y_name = "chinook_juv_per_adult",
                                                    lm_tab = pred2_ch)   # add LOOCV column
  write.csv(pred2_ch, quote=F, row.names = F,
            file=file.path(save_dir, "Supplemental Table 6 - Two Predictor Models of Chinook jpa.csv"))


  if(show_best_analysis==T){

    # 1a. Selection of best 1 predictor model - coho
    # Which are the best? Want a high Rsquare and a low pval
    plot(pred1$pval, pred1$Rsquare) # choose a quadrant
    AICc_threshold = quantile(pred1$AICc, 0.5)
    best_1 = pred1[pred1$Rsquare>.3 & pred1$pval < .1 & pred1$AICc < AICc_threshold,]
    best_1
    # Best is BY_recon_10, based on pval and Rsquare, but AICc is high (107).
    # RY_Wet_Tim is similar. (and BY_recon_15)
    # Alternate best is RY_FA_Mag. Much better AICc of 73. But tiny sample size.

    best1_sub = best_1[best_1$pred1=="BY_recon_10",]
    show_lm_summary_from_table(pred_tab = best1_sub, data_tab = metrics_tab_redu)



    ### 1b. Selection of best 1 predictor model - Chinook

    # Which are the best? Want a high Rsquare and a low pval
    plot(pred1_ch$pval, pred1_ch$Rsquare) # choose a quadrant
    AICc_threshold = quantile(pred1_ch$AICc, .5)
    best_1_ch = pred1_ch[pred1_ch$Rsquare>.2 & pred1_ch$pval < .1 &
                        pred1_ch$AICc < AICc_threshold,]
    best_1_ch
    # Best is log_BY_tot_flow_sepdec, based on pval and Rsquare, but AICc is high (107).
    # SY_min_flow_janjul is better but mechanistically, SY flows should not affect Chinook since they're already migrated.
    # BY_FA_Dur is better but too small a sample size.

    show_lm_summary_from_table(pred_tab = best_1_ch[best_1_ch$pred1=="log_BY_tot_flow_sepdec",],
                               data_tab = metrics_tab_redu)

    # 2a. best 2-predictor models - coho
    plot(pred2$pval, pred2$Rsquare) # choose a quadrant
    AICc_threshold = quantile(pred2$AICc, 0.5)
    # Apply selection criteria and exclude FA metrics
    best_2 = pred2[pred2$Rsquare>.6 & pred2$pval < .2 &
                     # pred2$AICc < AICc_threshold & # AIC selector not needed actually
                     pred2$Fstat > 10 &
                     !grepl(pattern = "FA", x = pred2$pred1) &
                     !grepl(pattern = "FA", x = pred2$pred2),]
    best2_sub = best_2[!duplicated(round(best_2$Fstat,4)),] # remove duplicates
    # Best 3 are:
    # BY_recon_10 + BY_recon_100,
    # BY_recon_10 + RY_Wet_Tim,
    # BY_recon_10 + RY_Wet_BFL_Dur

    show_lm_summary_from_table(pred_tab = best2_sub[1,], data_tab = metrics_tab)
    show_lm_summary_from_table(pred_tab = best2_sub[2,], data_tab = metrics_tab)
    show_lm_summary_from_table(pred_tab = best2_sub[3,], data_tab = metrics_tab)

    # 2b. best 2-predictor models - Chinook
    plot(pred2_ch$pval, pred2_ch$Rsquare) # choose a quadrant
    AICc_threshold = quantile(pred2_ch$AICc, 0.5)
    best_2_ch = pred2_ch[pred2_ch$Rsquare>.3 & pred2_ch$pval < .2 &
                     # pred2$AICc_ch < AICc_threshold & #pred2$Fstat > 10 &
                     !grepl(pattern = "FA", x = pred2_ch$pred1) &
                     !grepl(pattern = "FA", x = pred2_ch$pred2) &
                       !grepl(pattern = "SY", x = pred2_ch$pred1) &
                       !grepl(pattern = "SY", x = pred2_ch$pred2),]
    best2_sub_ch = best_2_ch[!duplicated(round(best_2_ch$Fstat,4)),] # remove duplicates
    # Best 3 are:
    # BY_recon_10 + BY_recon_100,
    # BY_recon_10 + RY_Wet_Tim,
    # BY_recon_10 + RY_Wet_BFL_Dur

    show_lm_summary_from_table(pred_tab = best2_sub[1,], data_tab = metrics_tab)
    show_lm_summary_from_table(pred_tab = best2_sub[2,], data_tab = metrics_tab)
    show_lm_summary_from_table(pred_tab = best2_sub[3,], data_tab = metrics_tab)

  }
  if(show_best_analysis_2==T){

    dim(pred2)
    pred2_orig = pred2
    pred2=pred2[pred2$n>10,] # include only models with 11

    # plots
    # lesson 1: for a set of models with the same number of predictors, adjRsquare is just
    # a straight penalty so it's the same data for inter model comparison purposes. Might as well
    # just use R2.
    par(mfrow = c(1,2))
    # pval vs Rsquare
    plot(pred2$pval, pred2$Rsquare, pch = 19, col = rgb(.5,.5,.5,.5)); grid(); abline(h=0)
    # pval vs adj-Rsquare
    plot(pred2$pval, pred2$adjRsquare, pch = 19, col = rgb(.5,.5,.5,.5)); grid(); abline(h=0)

    # Lesson 2: Fstat seems to carry similar info to AICc. and Rsquare!
    # probably don't need Fstat included
    # in the selection exercise if AIC is included.
    # Fstat vs AICc #par(mfrow = c(1,1))
    plot(pred2$Fstat, pred2$AICc, pch = 19, col = rgb(.5,.5,.5,.5)); grid()
    # rsquare vs aic. makes a line! Maybe we should stick with AIC instead of Rsquare.
    plot(pred2$AICc, pred2$Rsquare, pch = 19, col = rgb(.5,.5,.5,.5)); grid()

    # so we can ignore adjRsquare and Fstat.
    # Leaving us with 4 selection criteria: pval, Rsquare, AIC and loocv.

    # AICc vs error (loocv) - very related but not strictly linearly.
    # A group of 4 or 6 models, then a break, highlighted on this plot. AIC < 108?
    plot(pred2$AICc, pred2$loocv, pch = 19, col = rgb(.5,.5,.5,.5)); grid()
    # There's two enormous outliers. Control for that.
    # plot(pred2$AICc, pred2$loocv, pch = 19, col = rgb(.5,.5,.5,.5),ylim = c(0,8000)); grid()
    # or exclude them:     pred2$loocv[pred2$loocv>10000] = NA

    #AICc vs pval
    plot(pred2$pval, pred2$AICc, pch = 19, col = rgb(.5,.5,.5,.5)); grid()

    #pval vs error. A slight positive trend (less error means more significance)
    # but huge range at low p values.
    plot(pred2$pval, pred2$loocv, pch = 19, col = rgb(.5,.5,.5,.5)); grid()

    #rsquare vs error. same 8 highlighted I assume as AIC vs error
    plot(pred2$Rsquare, pred2$loocv, pch = 19, col = rgb(.5,.5,.5,.5)); grid()
    plot(pred2$AICc, pred2$loocv, pch = 19, col = rgb(.5,.5,.5,.5)); grid()


    # if we group Rsqaure, AICc, and Fstat then we only have 3. so we can use the plot3d
    plot3d(x=pred2$pval, y=pred2$Rsquare, z=pred2$loocv)
    # things associated with pval: scatter and slight trends. can ID quadrants
    # rsquare with error: swoosh. clearly IDs 4 or 6 good models.
    # thresholds: loocv < 1000, Rsquare > 0.65

  }

}










# ...................-------------------------------------------------------

# One-time analyses -------------------------------------------------------



linear_model_forward_selection = function(){


  ### 0. Prepare reduced table of metrics
  #reduced metrics tab for the 4-predictor models
 # for chinook  metrics_tab = metrics_tab_ch

  # reduced_pred = c("BY_FA_Mag", "BY_recon_10", "BY_recon_100", "BY_tot_flow_sepdec",
  #                  "RY_discon_10", "RY_discon_100", "RY_DS_Mag_50", "RY_DS_Mag_90", "RY_FA_Mag",
  #                  "RY_recon_10", "RY_recon_100", "RY_SP_ROC", "RY_tot_flow", "RY_Wet_BFL_Dur", "RY_Wet_Tim",
  #                  "SY_discon_100", "SY_SP_ROC", "SY_tot_flow_janjul", "SY_Wet_BFL_Dur",
  #                  "SY_Wet_Tim", "tot_flow_CFLP")
  reduced_pred = c("BY_FA_Mag", "BY_recon_10", #"BY_recon_15", "BY_recon_20",
                   "BY_recon_100", "BY_tot_flow_sepdec",
                   "RY_discon_10", "RY_discon_15", "RY_discon_20", "RY_discon_100",
                   "RY_DS_Mag_50", "RY_DS_Mag_90", "RY_FA_Mag",
                   "RY_recon_10", "RY_recon_15", "RY_recon_20", "RY_recon_100",
                   "RY_SP_ROC", "RY_tot_flow", "RY_Wet_BFL_Dur", "RY_Wet_Tim",
                   "SY_discon_100", "SY_SP_ROC", "SY_tot_flow_janjul", "SY_Wet_BFL_Dur",
                   "SY_Wet_Tim", "tot_flow_CFLP")
  keep_cols = c(colnames(metrics_tab)[1:7],reduced_pred)
  metrics_tab_redu = metrics_tab[,keep_cols]

  # March 2024: retain all metrics for these tables
  metrics_tab_redu = metrics_tab

  # For Chinook
# remove_pred = c("SY_min_flow_janjul","SY_discon_100", "SY_SP_ROC",
#                 "SY_tot_flow_janjul", "SY_min_flow_janjul", "SY_Wet_BFL_Dur",
#                 "SY_Wet_Tim", "tot_flow_CFLP", "RY_FA_Mag", "RY_FA_Tim", "RY_FA_Dur")
# metrics_tab_redu = metrics_tab_ch[,!(colnames(metrics_tab_ch) %in% remove_pred)]

  ### 1. Selection of best 1 predictor model
  pred1 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 1)
  colnames(pred1)[1] = "pred1"
  write.csv(pred1, file="pred1 diagnostics.csv", quote=F, row.names = F)
  # pred1 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "chinook_juv_per_adult", num_xs = 1)

  # Which are the best? Want a high Rsquare and a low pval
  plot(pred1$pval, pred1$Rsquare) # choose a quadrant
  hist(pred1$AICc)#, xlim = c(0,140))
  AICc_threshold = quantile(pred1$AICc, 0.5)
  best_1 = pred1[pred1$Rsquare>.3 & pred1$pval < .1 & pred1$AICc < AICc_threshold,]
  best_1
  # Best is BY_recon_10, based on pval and Rsquare, but AICc is high (107).
  # RY_Wet_Tim is similar. (and BY_recon_15)
  # Alternate best is RY_FA_Mag. Much better AICc of 73.

  best1_sub = best_1[best_1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1_sub, data_tab = metrics_tab_redu)



  ### 2. Selection of best 2 predictor model
  pred2 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 2)
  pred2 = pred2[!is.na(pred2$pval),] # get rid of NA rows
  colnames(pred2)[1] = "pred1"; colnames(pred2)[2] = "pred2"
  write.csv(pred2, file="pred2 diagnostics.csv", quote = F, row.names =F)
  # pred2 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "chinook_juv_per_adult", num_xs = 2)

  # # get rid of redundant combos
  # pred2_alpha_order = pred2$x1 < pred2$x2 # find the ones in alphabetical order
  # pred2_pred_combo = apply(X= as.character(pred2[,]), MARGIN = 1, FUN = paste)
  # pred2_pred_combo[!pred2_alpha_order] = paste(pred2$x2, pred2$x1, collapse = "_")
  # pred2 = duplicated(pred2_pred_combo)

  pred2$Fstat = round(pred2$Fstat, 1); pred2$pval = round(pred2$pval, 3)
  pred2$Rsquare = round(pred2$Rsquare, 3); pred2$adjRsquare = round(pred2$adjRsquare, 3)
  pred2$AICc = round(pred2$AICc, 1)
  # Which are best?
  # pred2_nofamag = pred2[!(grepl(pattern = "FA_Mag", x = pred2$x1) |
  #                           grepl(pattern = "FA_Mag", x = pred2$x2)),]
  plot(pred2$pval, pred2$adjRsquare)
  hist(pred2$AICc)#, xlim = c(0,140))
  AICc_threshold = quantile(pred2$AICc, 0.5)
  fstat_threshold = quantile(pred2$Fstat, 0.75)
  best_2 = pred2[pred2$adjRsquare > .4 & pred2$pval < 0.2 & pred2$Fstat>fstat_threshold,]#pred2$AICc<AICc_threshold,]
  dim(best_2)
  # which ones show up the most in the best models?
  x1_freq = aggregate(best_2$x1, by = list(best_2$x1), FUN = length)
  x1_freq[order(x1_freq$x, decreasing = T),]

  # Definition of best: R2 > 0.7, pval <0.1, and AICc < 112 (50th %ile). 12 models.
  # In 3 best models: BY_recon_10
  # In 2 best models: BY_FA_Mag, RY_Wet_Tim, BY_recon_100, RY_DS_Mag_90s

  best_2_sub = best_2[best_2$x1=="BY_recon_10" &
                             best_2$x2 %in% c("BY_recon_100",
                                              "RY_Wet_Tim"),]
  show_lm_summary_from_table(pred_tab = best_2_sub[1,], data_tab = metrics_tab_redu)
  show_lm_summary_from_table(pred_tab = best_2_sub[2,], data_tab = metrics_tab_redu)
  show_lm_summary_from_table(pred_tab = pred2[pred2$x1=="BY_recon_10" & pred2$x2=="SY_SP_ROC",], data_tab = metrics_tab_redu)

  show_lm_summary_from_table(pred_tab = pred2[pred2$x1=="BY_FA_Mag" & pred2$x2=="RY_discon_15",], data_tab = metrics_tab_redu)
  show_lm_summary_from_table(pred_tab = pred2[pred2$x1=="RY_Wet_BFL_Mag_50" & pred2$x2=="BY_FA_Dur",], data_tab = metrics_tab_redu)

  best_2_sub = best_2[best_2$x1=="BY_recon_10",]

  ### 3. Selection of best 3-predictor model

  pred3 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 3)

  pred3 = pred3[!is.na(pred3$pval),] # get rid of NA rows
  pred3$Fstat = round(pred3$Fstat, 1); pred3$pval = round(pred3$pval, 3)
  pred3$Rsquare = round(pred3$Rsquare, 3); pred3$adjRsquare = round(pred3$adjRsquare, 3)
  pred3$AICc = round(pred3$AICc, 1)
  # Which are best?
  plot(pred3$pval, pred3$Rsquare)
  # plot(pred3$pval, pred3$AICc)   # saturation relat
  # plot(pred3$Rsquare, pred3$AICc) # negative saturation relat.
  hist(pred3$AICc, xlim = c(0,140))
  AICc_threshold = quantile(pred3$AICc, 0.3)
  best_3 = pred3[pred3$Rsquare > .7 & pred3$pval < 0.1 & pred3$AICc<AICc_threshold,]
  dim(best_3)
  # which ones show up the most in the best models?
  x1_freq = aggregate(best_3$x1, by = list(best_3$x1), FUN = length)
  x1_freq[order(x1_freq$x, decreasing = T),]
  # Best definition, 3 predictors round: R2 > 0.7, pval < 0.1, AICc < 103.7 (30th %ile). 228 models of 48,714.

  # # FA_Mags are involved in a lot of the best models but have 5 missing values, so they reduce the sample size a lot.
  famags = c("BY_FA_Mag", "RY_FA_Mag", "RY_DS_Mag_90","RY_DS_Mag_50")
  best_3_nofamag = best_3[!(best_3$x1 %in% famags | best_3$x2 %in% famags | best_3$x3 %in% famags),]
  # View(best_3_nofamag)
  # x1_freq_nof = aggregate(best_3_nofamag$x1, by = list(best_3_nofamag$x1), FUN = length)
  # x1_freq_nof[order(x1_freq_nof$x, decreasing = T),]

  #In the best models excluding the suspicious predictors,
  # 26 models: BY_recon_10
  # 18 models: BY_recon_100
  # 10 models: RY_Wet_Tim
  # 4 models: RY_recon_10, RY_Wet_BFL_Dur, and SY_Wet_BFL_Dur.

  best3_suba = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & !(best_3$x3 %in% famags),]
  best3_subb = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="RY_Wet_Tim" & !(best_3$x3 %in% famags),]
  # # View(best3_subb)


  ### 4. Selection of best 4-predictor model

  pred4 = make_table_of_all_lms(data_tab = metrics_tab_redu, y_name = "coho_smolt_per_fem", num_xs = 4)

  pred4 = pred4[!is.na(pred4$pval),] # get rid of NA rows
  pred4$Fstat = round(pred4$Fstat, 1); pred4$pval = round(pred4$pval, 3)
  pred4$Rsquare = round(pred4$Rsquare, 3); pred4$adjRsquare = round(pred4$adjRsquare, 3)
  pred4$AICc = round(pred4$AICc, 1)
  # Which are best?
  plot(pred4$pval, pred4$Rsquare)
  # plot(pred4$pval, pred4$AICc)   # saturation relat
  # plot(pred4$Rsquare, pred4$AICc) # negative saturation relat.
  AICc_threshold = quantile(pred4$AICc, 0.5)
  best_4 = pred4[pred4$Rsquare > .7 & pred4$pval < 0.1 & pred4$AICc<AICc_threshold,]
  dim(best_4)
  # which ones show up the most in the best models?
  x1_freq = aggregate(best_4$x1, by = list(best_4$x1), FUN = length)
  x1_freq[order(x1_freq$x, decreasing = T),]

  #take out suspicious predictors
  famags = c("BY_FA_Mag", "RY_FA_Mag", "RY_DS_Mag_90","RY_DS_Mag_50")
  best_4_nofamag = best_4[!(best_4$x1 %in% famags | best_4$x2 %in% famags |
                              best_4$x3 %in% famags | best_4$x4 %in% famags),]
  x1_freq_nof = aggregate(best_4_nofamag$x1, by = list(best_4_nofamag$x1), FUN = length)
  x1_freq_nof[order(x1_freq_nof$x, decreasing = T),]

  #In the best models excluding the suspicious predictors,
  # 540 models: BY_recon_10
  # 324 models: BY_recon_100
  # 252 models: RY_Wet_Tim

  # 138: SY_tot_flow_janjul
  # 132: SY_Wet_BFL_Dur
  # 126: RY_recon_10
  # 120: RY_SP_ROC
  # 102-114 models: BY_tot_flow_sepdec, RY_recon_100, tot_flow_CFLP, RY_tot_flow


  # # Best model subsetting
  best4_sub1 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="RY_Wet_Tim" & !(pred4$x4 %in% famags),]
  best4_sub2 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="SY_tot_flow_janjul" & !(pred4$x4 %in% famags),]
  best4_sub3 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="SY_Wet_BFL_Dur" & !(pred4$x4 %in% famags),]
  best4_sub4 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="RY_recon_10" & !(pred4$x4 %in% famags),]
  best4_sub5 = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                       pred4$x3=="RY_SP_ROC" & !(pred4$x4 %in% famags),]

  View(best4_sub3)
  # dim(best4_sub)

  # # what about these FAMags. Damn, they all have an FA_Mag.
  # famags = c("BY_FA_Mag", "RY_FA_Mag")
  # best_4_nofamag = best_3[!(best_4$x1 %in% famags | best_4$x2 %in% famags |
  #                             best_4$x3 %in% famags | best_4$x4 %in% famags),]



  ### 5. Best Models and lm Summaries - 1/28/2022

  #1 pred comp
  best1 = pred1[pred1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1, data_tab = metrics_tab_redu)
  # 2 pred comp
  best2_a = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="BY_recon_100",]
  show_lm_summary_from_table(pred_tab = best2_a, data_tab = metrics_tab_redu)
  best2_b = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="RY_Wet_Tim",]
  show_lm_summary_from_table(pred_tab = best2_b, data_tab = metrics_tab_redu)
  # 3 pred comp
  best3_a = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best3_a, data_tab = metrics_tab_redu)
  best3_b = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="RY_Wet_Tim" & best_3$x3=="SY_SP_ROC" ,]
  show_lm_summary_from_table(pred_tab = best3_b, data_tab = metrics_tab_redu)
  # 4 pred comp
  best4_a = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_BFL_Dur"& pred4$x4=="RY_SP_ROC",]
  show_lm_summary_from_table(pred_tab = best4_a, data_tab = metrics_tab_redu)
  best4_b = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="RY_Wet_Tim" &
                    pred4$x3=="SY_SP_ROC"& pred4$x4=="RY_discon_10",]
  show_lm_summary_from_table(pred_tab = best4_b, data_tab = metrics_tab_redu)


  ### 5. Best Models and lm Summaries - 1/31/2022

  #1 pred comp
  best1 = pred1[pred1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1, data_tab = metrics_tab_redu)
  # 2 pred comp
  best2_a = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="BY_recon_100",]
  show_lm_summary_from_table(pred_tab = best2_a, data_tab = metrics_tab_redu)
  best2_b = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="RY_Wet_Tim",]
  show_lm_summary_from_table(pred_tab = best2_b, data_tab = metrics_tab_redu)
  # 3 pred comp
  best3_a = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best3_a, data_tab = metrics_tab_redu)
  best3_b = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_Tim" ,]
  show_lm_summary_from_table(pred_tab = best3_b, data_tab = metrics_tab_redu)
  # 4 pred comp
  best4_a = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_BFL_Dur"& pred4$x4=="RY_SP_ROC",]
  show_lm_summary_from_table(pred_tab = best4_a, data_tab = metrics_tab_redu)
  best4_b = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_Tim"& pred4$x4=="SY_SP_ROC",]
  show_lm_summary_from_table(pred_tab = best4_b, data_tab = metrics_tab_redu)

  ### 5. Best Models and lm Summaries - 2/06/2022

  #1 pred comp
  best1 = pred1[pred1$x1=="BY_recon_10",]
  show_lm_summary_from_table(pred_tab = best1, data_tab = metrics_tab_redu)
  # 2 pred comp
  best2_a = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="BY_recon_100",]
  show_lm_summary_from_table(pred_tab = best2_a, data_tab = metrics_tab_redu)
  best2_b = best_2[best_2$x1=="BY_recon_10" & best_2$x2=="RY_Wet_Tim",]
  show_lm_summary_from_table(pred_tab = best2_b, data_tab = metrics_tab_redu)
  # 3 pred comp
  best3_a = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="BY_recon_100" & best_3$x3=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best3_a, data_tab = metrics_tab_redu)
  best3_b = best_3[best_3$x1=="BY_recon_10" & best_3$x2=="RY_Wet_Tim" & best_3$x3=="SY_SP_ROC" ,]
  show_lm_summary_from_table(pred_tab = best3_b, data_tab = metrics_tab_redu)
  # 4 pred comp
  best4_a = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_Wet_Tim"& pred4$x4=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best4_a, data_tab = metrics_tab_redu)
  best4_b = pred4[pred4$x1=="BY_recon_10" & pred4$x2=="BY_recon_100" &
                    pred4$x3=="RY_SP_ROC"& pred4$x4=="RY_Wet_BFL_Dur",]
  show_lm_summary_from_table(pred_tab = best4_b, data_tab = metrics_tab_redu)


  ### 6. AICc comparisons
  par(mfrow = c(2,2))
  AICc_breaks = seq(from=-20, to = 150, by = 5)
  hist(pred1$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F, ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 1-predictor models")
  grid()
  hist(pred2$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F,ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 2-predictor models")
  grid()
  hist(pred3$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F,ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 3-predictor models")
  grid()
  hist(pred4$AICc, xlim = c(-20,140), breaks = AICc_breaks, freq=F,ylim = c(0,.08),
       xlab = "AICc of linear models", main = "Histogram of AICc for 4-predictor models")
  grid()



}



make_connection_date_hydrographs_pdf = function(){
  pdf(file = "connection date hydrographs_full record.pdf", height = 11/2, width = 8.5)
  for(wy in 1942:2021){
    recon_and_discon_explainer_hydrograph(water_year = wy)
  }
  dev.off()
}

watershed_population_calc = function(){
  # 1. Read in data and assign population data to spatial blocks data
  # read in census blocks shapefile (660 blocks)
  blocks_path = "C:/Users/Claire/Box/CKouba_Dissertation_DMS"
  blocks = readOGR(dsn = blocks_path, layer = "tiger_census_blocks_scott_watershed")

  # read in census blocks population table for the 4 tracts that overlap the watershed (>2000 blocks)
  block_pop = read.csv(file.path(ms_dir, "Data","ScottCensusBlocksPopulation2020_DECENNIALPL2020.P1_2022-01-04T150447",
                                 "DECENNIALPL2020.P1_data_with_overlays_2022-01-04T150435.csv"),
                       header = T)
  block_pop=block_pop[-1,] # get rid of 2nd "translated col name" row
  # associate pop with shapefile
  blocks$GEOID = paste0("1000000US",blocks$GEOID20)
  block_pop$P1_001N = as.numeric(as.character(block_pop$P1_001N))
  blocks$tot_pop = block_pop$P1_001N[match(blocks$GEOID, block_pop$GEO_ID)]

  # 2. plot for visual reference/gut check
  pop_breaks = c(0,1, 10, 50, 100,400)
  nclass = length(pop_breaks) - 1
  # pop_pal = rev(topo.colors(n = nclass))
  pop_pal= c("gray", rev(topo.colors(n = nclass-1)))
  blocks$pop_class = cut(blocks$tot_pop, include.lowest = T, breaks = pop_breaks)
  plot(blocks, col = pop_pal[blocks$pop_class])
  plot(watershed, add=T, border = "blue", lwd = 2)

  legend_labels = paste(pop_breaks[1:nclass], pop_breaks[2:(nclass+1)], sep = "-")
  legend_labels[1] = "0"
  legend(x = "bottomleft", legend = legend_labels, fill = pop_pal)
  # OK, need to weight this by % of block overlap in the watershed. sigh.

  # 3. Weight by % of block overlap in the watershed
  blocks$weight_fraction = 1 #  default assume it's 100% overlap
  watershed_line = as(watershed, "SpatialLinesDataFrame")
  falls_on_border = gIntersects(blocks, watershed_line, byid=T)
  for(i in 1:dim(blocks[2])){
    block = blocks[i,]
    if(falls_on_border[i]==T){
      in_watershed_area = area(gIntersection(spgeom1=block, spgeom2 = watershed))
      blocks$weight_fraction[i] = in_watershed_area / area(block)
    }
  }
  # area-weight the population estimate
  blocks$weighted_pop = blocks$tot_pop * blocks$weight_fraction

  # plot weighted population for visual reference/gut check
  pop_breaks = c(0,1, 10, 50, 100,350)
  nclass = length(pop_breaks) - 1
  # pop_pal = rev(topo.colors(n = nclass))
  pop_pal= c("gray", rev(topo.colors(n = nclass-1)))
  blocks$pop_class = cut(blocks$weighted_pop, include.lowest = T, breaks = pop_breaks)
  plot(blocks, col = pop_pal[blocks$pop_class])
  plot(watershed, add=T, border = "blue", lwd = 2)

  legend_labels = paste(pop_breaks[1:nclass], pop_breaks[2:(nclass+1)], sep = "-")
  legend_labels[1] = "0"
  legend(x = "bottomleft", legend = legend_labels, fill = pop_pal)
  # sum population
  # sum(blocks$tot_pop, na.rm=T) # by weighting, we remove about 350 people

  total_weighted_pop = sum(blocks$weighted_pop, na.rm=T)

  return(total_weighted_pop)
}

# Scratchwork & Data exploration --------------------------------------------------------



# _stream connectvity figs ------------------------------------------------


get_3310_pts_from_latlong = function(sp_pts){
  # store lat and long col names
  long_string = colnames(sp_pts)[grep(pattern = "lon", ignore.case = TRUE, x = colnames(sp_pts))]
  lat_string = colnames(sp_pts)[grep(pattern = "lat", ignore.case = TRUE, x = colnames(sp_pts))]
  # reassign column names
  colnames(sp_pts)[colnames(sp_pts) == long_string] = "longitude"
  colnames(sp_pts)[colnames(sp_pts) == lat_string] = "latitude"
  # Create spatial object, assign projection, reproject, and return
  coordinates(sp_pts) = ~longitude + latitude
  proj4string(sp_pts) <- CRS("+init=epsg:4326") #assign WGS84 projection to coordinates
  sp_pts = spTransform(sp_pts, crs("+init=epsg:3310"))
  return(sp_pts)
}


habitat_access_map = function(){
  # categories
  access_categories = c("Accessible, FJ below 20 cfs",
                        "Access gained, FJ at 20 cfs",
                        # "Uncertain - Access gained, FJ at 20 cfs",
                        # "Uncertain - Inaccessible, FJ at 20 cfs",
                        "Access gained, FJ at 20-30 cfs (?)",
                        "Access gained, FJ at 30-40 cfs (?)",
                        "Access gained 40-100 cfs",
                        "Access gained 100+ cfs")
  access_colors = c("darkorchid",
                    "dodgerblue",
                    "darkolivegreen3",
                    "gold",
                    "darkorange2",
                    "firebrick4")
  category_tab = data.frame(category = access_categories, color = access_colors)
  hab_access$acc_at_100[is.na(hab_access$acc_at_100)] = "Uncertain - No"
  hab_access$acc_at_100[hab_access$Stream == "Big Slough"] = "Yes"

  hab_access$category = NA
  hab_access$category[hab_access$acc_blw_20 == "Y"] = "Accessible, FJ below 20 cfs"
  hab_access$category[hab_access$acc_blw_20 == "No" &
                        hab_access$acc_at_20 == "Yes"] = "Access gained, FJ at 20 cfs"
  # hab_access$category[hab_access$acc_at_20 == "Uncertain - Yes"] = "Uncertain - Access gained, FJ at 20 cfs"
  # hab_access$category[hab_access$acc_at_20 == "Uncertain - No"] = "Uncertain - Inaccessible, FJ at 20 cfs"
  hab_access$category[hab_access$acc_at_20 == "Uncertain - Yes"] = "Access gained, FJ at 20-30 cfs (?)"
  hab_access$category[hab_access$acc_at_20 == "Uncertain - No"] = "Access gained, FJ at 30-40 cfs (?)"
  hab_access$category[hab_access$acc_at_100 == "Yes" &
                        hab_access$acc_at_20 == "No"] = "Access gained 40-100 cfs"
  hab_access$category[hab_access$acc_at_100 %in% c("Uncertain - No", "Uncertain - Yes")] = "Access gained 100+ cfs"

  # assign colors
  hab_access$color = category_tab$color[match(hab_access$category, category_tab$category)]
  plot(hab_access, col = hab_access$color, lwd = 2)
  # plot(hill_wsh, add=T)
  plot(hab_access, col = hab_access$color, lwd = 2, add=T)
  # add choke point observation locations
  plot(chokepts, add=T, pch =21, bg = "salmon", cex = 1.5)

  legend("topright", col = category_tab$color, cex = .9,
         # ncol = 2,
         legend = category_tab$category, lwd = 2)
  legend("bottomleft",
         pch = 21, pt.cex = 1.5, pt.bg = "salmon", box.col = "white",
         legend = "Potential fish passage chokepoints: \nDisconnected river observed, Nov. 2020, FJ flow 20-40 cfs")

  # tm_shape(hill_wsh) +
  #   tm_raster(palette = hillshade_palette_faded(10), legend.show = F) +
  #   tm_shape(hab_access) + tm_lines(hab_access$color, lwd = 1)


}

sat_connectivity_data_exp = function(){
  # plot satellite connectivity data. does this support chokepoint theory?
}

salmon_chokepoints_and_habitat = function(){

  # Connectivity data sources
  ## SRWC baseflow data
  ## connectivity @ 20 cfs map

  # watershed
  # tribs
  # mainstem
  # chokepoints, based on SRWC data
  # qgis data munging to designate reaches as accessible at 20 and 100cfs?
}



map_flowing_obs = function(obs_date = as.Date("2020-11-18")){
  dry_wet_col_tab = data.frame(status = c("Dry", "Disconnected Pools", "Hydrated", "Flowing"),
                               color = c("red", "orange", "cyan", "blue"))
  obs_on_day = ground_obs_pts[ground_obs_pts$Obs_Date == obs_date,]
  obs_on_day$dry_wet_color = dry_wet_col_tab$color[match(obs_on_day$Status, dry_wet_col_tab$status)]

  plot(mapped_streams, main = paste("Observed stream conditions on", obs_date))
  plot(obs_on_day, add=T,
       pch = 19, col = obs_on_day$dry_wet_color)
  legend(x="topright", col = dry_wet_col_tab$color,
         legend = dry_wet_col_tab$status, pch = 19)
}



connected_reaches_figure = function(){

  date_selector = fj_flow$Date >as.Date("2020-11-01") & fj_flow$Date < as.Date("2020-12-01")
  plot(x = fj_flow$Date[date_selector],
       y = fj_flow$Flow[date_selector],
       xlab = "Date in year 2020",
       ylab = "Daily Avg Flow, Ft Jones Gauge (cfs)",
       pch=19, type = "o", col="dodgerblue")
  abline(h=pretty(range(fj_flow$Flow[date_selector])),
         v=seq.Date(from=as.Date("2020-11-01"),
                    to =  as.Date("2020-12-01"),
                    by = "days"),
         col = "gray", lty = 3)

  developing_this_fig = F
  if(developing_this_fig){
    # next up: add a FJ flow value subtitle

    # reaches
    pt1s = ground_obs_rch[,!(colnames(ground_obs_rch) %in%
                               c("Latitude_pt2", "Longitude_pt2"))]
    pt2s = ground_obs_rch[,!(colnames(ground_obs_rch) %in%
                               c("Latitude_pt1", "Longitude_pt1"))]
    stream_pts = as(mapped_streams, "SpatialPointsDataFrame")
    stream_pts$Lines.ID


    for(i in 1:nrow(pt1s)){
      pt1 = get_3310_pts_from_latlong(sp_pts = pt1s[i,])
      pt2 = get_3310_pts_from_latlong(sp_pts = pt2s[i,])

      # reach_ends=rbind(pt1, pt2)
      # plot(reach_ends, col = c("red", "blue"))
      # plot(mapped_streams, add=T)

      pt1_nearest_index = nn2(data = stream_pts@coords, query = pt1@coords, k = 1)[[1]][,1]
      pt2_nearest_index = nn2(data = stream_pts@coords, query = pt2@coords, k = 1)[[1]][,1]

      reach_pts = stream_pts[pt1_nearest_index:pt2_nearest_index,]

      # build the vertices back into lines like in the original line shapefile
      reach_pts_line_ids = unique(reach_pts$Lines.ID)
      line_obj_list = vector("list", length(reach_pts_line_ids))
      take_out_these_line_ids=vector("character",0)
      for(j in 1:length(reach_pts_line_ids)){
        line_id = reach_pts_line_ids[j]
        this_lines_coords = reach_pts@coords[reach_pts$Lines.ID==line_id,]
        if(length(this_lines_coords)==2){
          take_out_these_line_ids = c(take_out_these_line_ids, line_id)
          break}
        line_obj=Line(this_lines_coords)
        line_obj_list[[j]] = line_obj
      }
      line_obj_list = line_obj_list[-which(reach_pts_line_ids==take_out_these_line_ids)]
      reach_pts_line_ids = reach_pts_line_ids[!(reach_pts_line_ids %in% take_out_these_line_ids)]

      lines_obj = Lines(line_obj_list, ID = "reaches")
      spLine_obj = SpatialLines(list(lines_obj), proj4string=crs("+init=epsg:3310"))

      plot(mapped_streams)
      plot(spLine_obj, col = "red")
      plot(mapped_streams, add=T)
      plot(spLine_obj, col = "red", add=T)

      # OK, new troubleshooting issues:
      # shit, some of the lines are not within the indices
      # goddammit
      # find the missing indices, goddammit

    }

    # calculate nearest vertex on line to each point

    # connect reaches between identified vertices

  }

}



# _total flow, func. flows, water year type -------------------------------


cum_flow_annual_cdf=function(fj_flow, year_rank_tab = NA, return_tab=F){

  # Convert flow to daily volume
  fj_flow$Flow_m3day = fj_flow$Flow * cfs_to_m3day
  # Initialize cumulative flow column
  fj_flow$annual_cum_flow_m3 = NA

  #Plot setup
  water_years = unique(fj_flow$wy) #Isolate water years for plot
  n_wy = length(water_years)
  # plot_colors = gray((n_wy:1)/n_wy, alpha = 0.5) # light colors in the past
  plot_colors = rev(topo.colors(n_wy)) # light colors in the past

  if(sum(!is.na(year_rank_tab))>1){
    unranked_years = unique( c(setdiff(water_years, year_rank_tab$water_year),
                               year_rank_tab$water_year[is.na(year_rank_tab$rank)]))
    plot_colors[water_years %in% unranked_years] = "lightgray"

    ranked_years_tab = year_rank_tab[!is.na(year_rank_tab$rank),]
    n_ranked = nrow(ranked_years_tab)
    ranked_years = ranked_years_tab$water_year
    plot_colors[water_years %in% ranked_years] = rev(topo.colors(n_ranked))[order(ranked_years_tab$rank)] # light colors in the past
  }

  # calculate cumulative daily flow for each year
  for(i in 1:length(water_years)){
    wy = water_years[i]
    wy_indices = which(fj_flow$wy==wy)
    fj_flow$annual_cum_flow_m3[wy_indices] =
      cumsum(fj_flow$Flow_m3day[wy_indices])
  }
  # make plot
  for(i in 1:length(water_years)){
    wy = water_years[i]
    wy_indices = which(fj_flow$wy==wy)

    if(i == 1){
      # Initialize plot
      plot(x = 1:366,
           y = c(fj_flow$annual_cum_flow_m3[wy_indices],NA)/1E6, # add extra day for leap days in later years
           main="Cumulative flow volume by water year",
           xaxt="n",
           xlab = "Days in water year since Oct. 1",
           ylab = "Cumulative flow at FJ Gauge (million cubic m)",
           ylim = c(0, max(fj_flow$annual_cum_flow_m3)/1E6),
           type = "l", col = plot_colors[i])
    } else {
      lines(x=1:length(wy_indices),
            y=fj_flow$annual_cum_flow_m3[wy_indices]/1E6,
            col = plot_colors[i])
    }
  }
  quarter_day1s = c(0,92,182,273,365)
  axis(side=1,at=quarter_day1s, tick=TRUE,
       labels=c("Oct 1","Jan 1", "Apr 1","Jul 1","Oct 1"))
  abline(col="gray", lty=3, v= quarter_day1s,
         h=pretty(range(fj_flow$annual_cum_flow_m3/1E6)))
  # customize legend
  legend(x="topleft", lwd=2, title = "Color Range",
         col = rev(topo.colors(5)),
         legend = round(seq(from=min(water_years),
                            to = max(water_years),
                            length.out=5)))

  if(return_tab==TRUE){
    total_flow_tab = aggregate(fj_flow$annual_cum_flow_m3, by=list(fj_flow$wy), FUN = max)
    colnames(total_flow_tab)=c("water_year","total_flow_m3")
    return(total_flow_tab)
  }

}

calc_water_year_rank=function(basis = "population growth rate",
                              basis_year = "Smolt_Year",
                              threshold = 60, metrics_tab){
  if(tolower(basis_year) %in% c("brood year", "brood_year", "brood_yr", "brood yr")){basis_year="Adult_Year_Brood_Year"}
  if(tolower(basis_year) %in% c("smolt_year", "smolt year", "smolt yr", "smolt_yr")){basis_year="Smolt_Year"}

  if(basis == "smolts produced per female"){
    #convert smolts per fem number to numeric.
    smolt_per_fem$Smolts_Produced_Per_Female = as.numeric(smolt_per_fem$Smolts_Produced_Per_Female)

    smolt_per_fem$rank = NA # make new column
    # Assign rank. 1 = highest smolt per fem = best;  higher ranks are lower numbers
    smolt_per_fem$rank = rank(smolt_per_fem$Smolts_Produced_Per_Female * -1) # negatized for 1 to go to highest number

    #clean up extra rank assigned to a no-value year
    smolt_per_fem$rank[is.na(smolt_per_fem$Smolts_Produced_Per_Female)] = NA

    rank_tab = smolt_per_fem[,c(basis_year, "rank")]
  }

  if(basis=="disconnection date"){

  }





  return(rank_tab)
}

cum_flow_CFLP=function(hydro_by_smolt_year, sy_rank_tab){
  hbsy = hydro_by_smolt_year
  if(is.na(plot_colors)){plot_colors = gray.colors(n=length(smolt_years))}

  # plot affecting hydro
  for(i in 1:length(smolt_years)){
    smolt_year = smolt_years[i]
    sy_indices = which(hbsy$smolt_year==smolt_year)

    if(i == 1){
      # Initialize plot
      plot(x = 1:(length(sy_indices)+1), # ndays in Nov 1 to next July 30, 20 months, +1 day for leap days
           y = c(hbsy$smolt_year_cum_flow[sy_indices],NA)/1E6, # add extra day for leap days in later years
           main="Cumulative flow volume by Salmon Life Period",
           xaxt="n",
           xlab = "Days since Nov. 1 of Brood Year",
           ylab = "Cumulative flow at FJ Gauge (million cubic m)",
           ylim = c(0, max(hbsy$smolt_year_cum_flow)/1E6),
           type = "l", col = plot_colors[i])
    } else {
      lines(x=1:length(sy_indices),
            y = hbsy$smolt_year_cum_flow[sy_indices]/1E6,
            col = plot_colors[i])
    }
  }
  quarter_day1s = c(0,61,152,243,335,427,517,608)
  axis(side=1,at=quarter_day1s, tick=TRUE,
       # labels=c("Brood Yr Nov 1","Rear Yr Jan 1", "Rear Yr Apr 1","Rear Yr Jul 1","Rear Yr Oct 1",
       #          "Smolt Yr Jan 1", "Smolt Yr Apr 1","Smolt Yr Jul 1")
       labels=c("BY Nov 1","RY Jan 1", "RY Apr 1","RY Jul 1","RY Oct 1",
                "SY Jan 1", "SY Apr 1","SY Jul 1"))
  abline(col="gray", lty=3, v= quarter_day1s,
         h=pretty(range(fj_flow$annual_cum_flow_m3/1E6)))
  # customize legend
  legend(x="topleft", lwd=2, title = "Color Range",
         col = rev(topo.colors(5)),
         legend = round(seq(from=min(water_years),
                            to = max(water_years),
                            length.out=5)))
}


fflow_metrics_histograms = function(){
  #Histograms - straight distributions
  # pdf("USGS FJ fflows metrics - histograms.pdf",11,8.5)
  par(mfrow = c(5,3))
  for(i in c(1:7,17:24)){ # avoids peak metrics
    metric = metrics[i]
    hist(fflows[,metric], main = metric, col = metric_colors[i],
         xlab = metrics_units[i])
  }
  # dev.off()

}


fflow_metrics_over_time = function(metrics, metrics_plot_names,
                                   pick_these=metrics[c(1:7, 17:24)],
                                   print_correlations = F){
  #Plots over time
  # pdf("USGS FJ fflows metrics - time series.pdf",11,8.5)
  # par(mfrow = c(5,3))
  # par(mfrow = c(4,2))
  par(mfrow = c(2,2))

  indices = match(pick_these, metrics)

  for(i in indices){ # avoids peak metrics
    metric = metrics[i]
    plot_name = metrics_plot_names[i]
    plot(x = fflows$Water_Year, y = fflows[,metric], pch = 19,#, type = "o"
         main = plot_name, col = metric_colors[i],
         ylab = metrics_units[i], xlab = "Water Year")
    bestfit =lm(fflows[,metric] ~ fflows$Water_Year)
    abline(bestfit, lty = 2)
    # rolling avg
    roll_n_yrs = 10
    lines(x = fflows$Water_Year[roll_n_yrs:nrow(fflows)],
          y = rollmean(x = fflows[,metric], k = roll_n_yrs, align = "left", na.rm=T),
          col = "black", lwd=2)
    grid()
    if(print_correlations){
      print(paste(metric, "slope:", round(bestfit$coefficients[2],2), metrics_units[i], "per year, Rsquare:", round(summary(bestfit)$r.squared,2)))
    }
    # legend(x = "topright", lty = c(2,1), lwd = c(1,2), col=metric_colors[i], legend = c("Line of best fit", "10-year rolling average"))

  }
  # dev.off()
}


percentile_flows_fj_gauge_figure = function(save_as_png=TRUE,
                                            percent_survival_threshold = 5,
                                            scenario_id = "basecase",
                                            plot_name = "Simulated Historical"){

  plot_yr = 1991
  # save_as_png=F

  ######### Process Data
  # 1. Historical data

  fj_flow_monthly_stats = function(fj_flow,
                                   start_date = as.Date("1941-10-01"),
                                   end_date = as.Date("1976-09-01")){

    fj_flow_sub = fj_flow[fj_flow$Date > start_date & fj_flow$Date < end_date,]
    fj_flow_sub$month = month(fj_flow_sub$Date)
    fj_month_pre_mean = aggregate(fj_flow_sub$Flow, by = list(fj_flow_sub$month),  FUN = mean)
    fj_month_pre_med = aggregate(fj_flow_sub$Flow, by = list(fj_flow_sub$month),  FUN = median)
    # fj_month_pre_sd = aggregate(fj_flow_sub$Flow, by = list(fj_flow_sub$month),  FUN = sd)
    fj_month_pre_05pctl = aggregate(fj_flow_sub$Flow, by = list(fj_flow_sub$month), function(x){quantile(x, probs = 0.05)})
    fj_month_pre_95pctl = aggregate(fj_flow_sub$Flow, by = list(fj_flow_sub$month), function(x){quantile(x, probs = 0.95)})
    fj_month_pre_25pctl = aggregate(fj_flow_sub$Flow, by = list(fj_flow_sub$month), function(x){quantile(x, probs = 0.25)})
    fj_month_pre_75pctl = aggregate(fj_flow_sub$Flow, by = list(fj_flow_sub$month), function(x){quantile(x, probs = 0.75)})

    fj_flow_month_sub = data.frame(Month = fj_month_pre_mean$Group.1,
                                   date = as.Date(paste(plot_yr,1:12,"15",sep="-")),
                                   flow_cfs_mean = fj_month_pre_mean$x,
                                   flow_cfs_med = fj_month_pre_med$x,
                                   flow_cfs_05pctl = fj_month_pre_05pctl$x,
                                   flow_cfs_95pctl = fj_month_pre_95pctl$x,
                                   flow_cfs_25pctl = fj_month_pre_25pctl$x,
                                   flow_cfs_75pctl = fj_month_pre_75pctl$x)

    return(fj_flow_month_sub)
  }

  # Avg observed flows, pre-1977 period
  fj_flow_pre = fj_flow_monthly_stats(fj_flow = fj_flow,
                                      start_date = as.Date("1941-10-01"),
                                      end_date = as.Date("1976-09-01"))
  # Avg observed flows, post-1977 period
  fj_flow_post = fj_flow_monthly_stats(fj_flow = fj_flow,
                                       start_date = as.Date("1976-10-01"),
                                       end_date = as.Date("2020-09-01"))

  # 2. CDFW recommended instream flows - convert instream flows table into a time series
  start_days = as.Date(paste(plot_yr,cdfw_tab$start_date_month, cdfw_tab$start_date_day, sep = "-"))
  end_days = as.Date(paste(plot_yr, cdfw_tab$end_date_month, cdfw_tab$end_date_day, sep = "-"))
  start_days_lastyear =  as.Date(paste(plot_yr-1,cdfw_tab$start_date_month, cdfw_tab$start_date_day, sep = "-"))
  end_days_lastyear = as.Date(paste(plot_yr-1, cdfw_tab$end_date_month, cdfw_tab$end_date_day, sep = "-"))
  cdfw_rec = data.frame(date = c(start_days_lastyear, end_days_lastyear,start_days, end_days),
                        rec_flow_cfs = rep(cdfw_tab$rec_flow_cfs, 4))
  cdfw_rec = cdfw_rec[order(cdfw_rec$date),]

  # 3. Forest Service water right - convert FS water right flows table into a time series
  start_days = as.Date(paste(plot_yr,fs_tab$start_date_month, fs_tab$start_date_day, sep = "-"))
  end_days = as.Date(paste(plot_yr, fs_tab$end_date_month, fs_tab$end_date_day, sep = "-"))
  start_days_lastyear =  as.Date(paste(plot_yr-1,fs_tab$start_date_month, fs_tab$start_date_day, sep = "-"))
  end_days_lastyear = as.Date(paste(plot_yr-1, fs_tab$end_date_month, fs_tab$end_date_day, sep = "-"))
  fs_right = data.frame(date = c(start_days_lastyear, end_days_lastyear,start_days, end_days),
                        rec_flow_cfs = rep(fs_tab$rec_flow_cfs, 4))
  fs_right = fs_right[order(fs_right$date),]

  # 4. Calculate MO.
  good_conditions_years = outmigs$conditions_year[outmigs$Percent.smolt.survival > percent_survival_threshold]
  good_conditions_years = good_conditions_years[!is.na(good_conditions_years)]

  fj_good = fj_flow[year(fj_flow$Date) %in% good_conditions_years,]
  fj_good_monthly_stats = fj_flow_monthly_stats(fj_flow = fj_good,
                                                start_date = as.Date("1941-10-01"),
                                                end_date = as.Date("2020-09-01"))
  # write.csv(x = fj_good_month, file = "FJ_good_yrs_monthly_avg.csv")


  # CURRENTLY HERE.


  # 5. Read in scenario results
  # scenario_id = "natveg_gwmixed_outside_adj"; plot_name = "No Pumping Outside Adjudication"
  scenario_directory = file.path("C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios", scenario_id)
  fj_file_name = file.path(scenario_directory,"Streamflow_FJ_SVIHM.dat")

  start_date = as.Date("1990-10-01"); end_date = as.Date("2018-09-30")
  start_wy = 1991;  end_wy = 2018
  num_stress_periods = length(seq(start_date, end_date, by="month")); nsp = num_stress_periods

  scen = data.frame(Date = seq(start_date, end_date, "days"),                         # Import Basecase flow data
                    Flow_m3day = read.table(fj_file_name, skip = 2)[,3],
                    Flow_cfs = read.table(fj_file_name, skip = 2)[,3]*0.000408734569)
  scen$month = month(scen$Date)

  scen_month_mean = aggregate(scen$Flow_cfs, by = list(scen$month),  FUN = mean)
  scen_month_med = aggregate(scen$Flow_cfs, by = list(scen$month),  FUN = median)
  scen_month_05pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.05)})
  scen_month_95pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.95)})
  scen_month_25pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.25)})
  scen_month_75pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.75)})
  scen_month = data.frame(Month = scen_month_mean$Group.1,
                          date = as.Date(paste(plot_yr,1:12,"15",sep="-")),
                          flow_cfs_mean = scen_month_mean$x,
                          flow_cfs_med = scen_month_med$x,
                          flow_cfs_05pctl = scen_month_05pctl$x,
                          flow_cfs_95pctl = scen_month_95pctl$x,
                          flow_cfs_25pctl = scen_month_25pctl$x,
                          flow_cfs_75pctl = scen_month_75pctl$x)


  ##### PLOTS

  # Plot 0. Historical flows and recs
  if(save_as_png==TRUE){png(filename = "Pre-1977 FJ Flow plus recommendeds.png",
                            width = 6, height = 6, units = "in", res = 300)}

  plot(fj_flow_month_pre$date, fj_flow_month_pre$flow_cfs_med, type = "l", log = "y",
       ylim =c(1, 100000), xlim = as.Date(c("1991-01-01","1991-12-31")),
       col = "blue", yaxt="n", ylab = "Daily Average Flow (cfs)", xlab = "",
       main = paste("Fort Jones Gauge flow, 1977-2020"),
       sub = paste("USGS Gauge", fj_num, "(Fort Jones Gauge)"))
  abline(h=10^(0:5), v = as.Date(paste(plot_yr,1:12,"01", sep ="-")),col = "darkgray")
  # fix lower bounds to account for
  polygon(x = c(fj_flow_month_pre$date,rev(fj_flow_month_pre$date)),
          y = c( fj_flow_month_pre$flow_cfs_95pctl, rev(fj_flow_month_pre$flow_cfs_05pctl)),
          col = "gray90", border = "NA")
  polygon(x = c(fj_flow_month_pre$date,rev(fj_flow_month_pre$date)),
          y = c( fj_flow_month_pre$flow_cfs_75pctl, rev(fj_flow_month_pre$flow_cfs_25pctl)),
          col = "gray75", border = "NA")
  lines(fj_flow_month_pre$date, fj_flow_month_pre$flow_cfs_med, col = "black", lwd = 2)
  points(fj_flow_month_pre$date, fj_flow_month_pre$flow_cfs_med, pch=19, col = "black", lwd = 2)

  axis(side = 2, at = 10^(0:5), labels = c("1", "10", "100", "1,000", "10,000", "100,000"),
       cex.axis = 0.8, las = 1)
  axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA)

  # Plot CDFW recommended instream flows
  lines(cdfw_rec$date, cdfw_rec$rec_flow_cfs, col = "dodgerblue",lwd=2)

  # Plot Forest Service schedule E water right
  lines(fs_right$date, fs_right$rec_flow_cfs, col = "blue",lwd=2)

  # Legend
  legend(x="topright",pt.bg = c(NA,"gray90","gray75",NA,NA),
         pch=c(NA,22,22,NA,NA), pt.cex = 2,
         lwd = c(2,NA,NA,2,2),
         col = c(rep("black",3),"dodgerblue","blue"),
         legend = c("Monthly median flow","90% of flow","50% of flow",
                    "CDFW recommended flows","USFS Water Right"))
  if(save_as_png==TRUE){dev.off()}

  # Plot 1. Historical flows and recs
  if(save_as_png==TRUE){png(filename = "FJ Flow plus recommendeds.png",
                            width = 6, height = 6, units = "in", res = 300)}

  plot(fj_flow_month$date, fj_flow_month$flow_cfs_med, type = "l", log = "y",
       ylim =c(1, 100000), xlim = as.Date(c("1991-01-01","1991-12-31")),
       col = "blue", yaxt="n", ylab = "Daily Average Flow (cfs)", xlab = "",
       main = paste("Fort Jones Gauge flow, 1977-2020"),
       sub = paste("USGS Gauge", fj_num, "(Fort Jones Gauge)"))
  abline(h=10^(0:5), v = as.Date(paste(plot_yr,1:12,"01", sep ="-")),col = "darkgray")
  # fix lower bounds to account for
  polygon(x = c(fj_flow_month$date,rev(fj_flow_month$date)),
          y = c( fj_flow_month$flow_cfs_95pctl, rev(fj_flow_month$flow_cfs_05pctl)),
          col = "gray90", border = "NA")
  polygon(x = c(fj_flow_month$date,rev(fj_flow_month$date)),
          y = c( fj_flow_month$flow_cfs_75pctl, rev(fj_flow_month$flow_cfs_25pctl)),
          col = "gray75", border = "NA")
  lines(fj_flow_month$date, fj_flow_month$flow_cfs_med, col = "black", lwd = 2)
  points(fj_flow_month$date, fj_flow_month$flow_cfs_med, pch=19, col = "black", lwd = 2)

  axis(side = 2, at = 10^(0:5), labels = c("1", "10", "100", "1,000", "10,000", "100,000"),
       cex.axis = 0.8, las = 1)
  axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA)

  # Plot CDFW recommended instream flows
  lines(cdfw_rec$date, cdfw_rec$rec_flow_cfs, col = "dodgerblue",lwd=2)

  # Plot Forest Service schedule E water right
  lines(fs_right$date, fs_right$rec_flow_cfs, col = "blue",lwd=2)

  # Legend
  legend(x="topright",pt.bg = c(NA,"gray90","gray75",NA,NA),
         pch=c(NA,22,22,NA,NA), pt.cex = 2,
         lwd = c(2,NA,NA,2,2),
         col = c(rep("black",3),"dodgerblue","blue"),
         legend = c("Monthly median flow","90% of flow","50% of flow",
                    "CDFW recommended flows","USFS Water Right"))
  if(save_as_png==TRUE){dev.off()}


  ####### Plot 2. MO proposal, based on "good years" median, and recs.

  if(save_as_png==TRUE){png(filename = "FJ Flow, good yrs, plus recommendeds.png",
                            width = 6, height = 6, units = "in", res = 300)}

  plot(fj_good_month$date, fj_good_month$flow_cfs_med, type = "l", log = "y",
       ylim =c(1, 100000), xlim = as.Date(c("1991-01-01","1991-12-31")),
       col = "blue", yaxt="n", ylab = "Daily Average Flow (cfs)", xlab = "",
       main = paste("Flow in",paste(good_conditions_years, collapse= ", ")),
       sub = paste("USGS Gauge", fj_num, "(Fort Jones Gauge)"))
  abline(h=10^(0:5), v = as.Date(paste(plot_yr,1:12,"01", sep ="-")),col = "darkgray")
  # fix lower bounds to account for
  polygon(x = c(fj_good_month$date,rev(fj_good_month$date)),
          y = c( fj_good_month$flow_cfs_95pctl, rev(fj_good_month$flow_cfs_05pctl)),
          col = "gray90", border = "NA")

  polygon(x = c(fj_good_month$date,rev(fj_good_month$date)),
          y = c( fj_good_month$flow_cfs_75pctl, rev(fj_good_month$flow_cfs_25pctl)),
          col = "gray75", border = "NA")
  lines(fj_good_month$date, fj_good_month$flow_cfs_med, col = "brown", lwd = 2)
  points(fj_good_month$date, fj_good_month$flow_cfs_med, pch=19, col = "brown", lwd = 2)

  # Plot CDFW recommended instream flows
  lines(cdfw_rec$date, cdfw_rec$rec_flow_cfs, col = "dodgerblue",lwd=2)

  # Plot Forest Service schedule E water right
  lines(fs_right$date, fs_right$rec_flow_cfs, col = "blue",lwd=2)

  legend(x="topright",pt.bg = c(NA,"gray90","gray75",NA,NA),
         pch=c(NA,22,22,NA,NA), pt.cex = 2,
         lwd = c(2,NA,NA,2,2),
         col = c("brown",rep("black",2),"dodgerblue","blue"),
         legend = c("Monthly median flow (selected yrs)","90% of flow","50% of flow",
                    "CDFW recommended flows","USFS Water Right"))

  axis(side = 2, at = 10^(0:5), labels = c("1", "10", "100", "1,000", "10,000", "100,000"),
       cex.axis = 0.8, las = 1)
  axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA)

  if(save_as_png==TRUE){dev.off()}


  ###### Plot 3. Plot scenario vs recommended flows

  if(save_as_png==TRUE){png(filename = paste(scenario_id,"sim Flow plus recommendeds.png"),
                            width = 6, height = 6, units = "in", res = 300)}
  subtitle_text = "Simulated FJ Flow, 1991-2018"
  if(scenario_id == "fj_obs"){subtitle_text = "Observed FJ Flow, 1991-2018"}

  plot(scen_month$date, scen_month$flow_cfs_med, type = "l", log = "y",
       ylim =c(1, 100000), xlim = as.Date(c("1991-01-01","1991-12-31")),
       yaxt="n", ylab = "Daily Average Flow (cfs)", xlab = "",
       main = paste(plot_name, "scenario"), sub = subtitle_text)
  abline(h=10^(0:5), v = as.Date(paste(plot_yr,1:12,"01", sep ="-")),col = "darkgray")
  # Plot 50% of flow and 90% of flow polygons
  polygon(x = c(scen_month$date,rev(scen_month$date)),
          y = c( scen_month$flow_cfs_95pctl, rev(scen_month$flow_cfs_05pctl)),
          col = "gray90", border = "NA")
  polygon(x = c(scen_month$date,rev(scen_month$date)),
          y = c( scen_month$flow_cfs_75pctl, rev(scen_month$flow_cfs_25pctl)),
          col = "gray75", border = "NA")
  lines(scen_month$date, scen_month$flow_cfs_med, col = "darkorange", lwd = 2)
  points(scen_month$date, scen_month$flow_cfs_med, pch=19, col = "darkorange", lwd = 2)

  axis(side = 2, at = 10^(0:5), labels = c("1", "10", "100", "1,000", "10,000", "100,000"),
       cex.axis = 0.8, las = 1)
  axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA)

  # Plot CDFW recommended instream flows
  lines(cdfw_rec$date, cdfw_rec$rec_flow_cfs, col = "dodgerblue",lwd=2)

  # Plot Forest Service schedule E water right
  lines(fs_right$date, fs_right$rec_flow_cfs, col = "blue",lwd=2)

  # Legend
  legend(x="topright",pt.bg = c(NA,"gray90","gray75",NA,NA),
         pch=c(NA,22,22,NA,NA), pt.cex = 2,
         lwd = c(2,NA,NA,2,2),
         col = c("darkorange",rep("black",2),"dodgerblue","blue"),
         legend = c("Scenario monthly median flow","90% of flow","50% of flow",
                    "CDFW recommended flows","USFS Water Right"))
  if(save_as_png==TRUE){dev.off()}

  ###### Plot 4. Special plot for Katie. Plot 2 scenarios and historical vs recommended flows

  # # 5. Read in scenario results
  # scenario_id2 = "natveg_gwmixed_outside_adj"; plot_name2 = "No Pumping Outside Adjudication"
  # # currently here HEEEEEEEEEEERE
  # scenario_directory = file.path("C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios", scenario_id)
  # fj_file_name = file.path(scenario_directory,"Streamflow_FJ_SVIHM.dat")
  #
  # start_date = as.Date("1990-10-01"); end_date = as.Date("2018-09-30")
  # start_wy = 1991;  end_wy = 2018
  # num_stress_periods = length(seq(start_date, end_date, by="month")); nsp = num_stress_periods
  #
  # scen = data.frame(Date = seq(start_date, end_date, "days"),                         # Import Basecase flow data
  #                   Flow_m3day = read.table(fj_file_name, skip = 2)[,3],
  #                   Flow_cfs = read.table(fj_file_name, skip = 2)[,3]*0.000408734569)
  # scen$month = month(scen$Date)
  #
  # scen_month_mean = aggregate(scen$Flow_cfs, by = list(scen$month),  FUN = mean)
  # scen_month_med = aggregate(scen$Flow_cfs, by = list(scen$month),  FUN = median)
  # scen_month_05pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.05)})
  # scen_month_95pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.95)})
  # scen_month_25pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.25)})
  # scen_month_75pctl = aggregate(scen$Flow_cfs, by = list(scen$month), function(x){quantile(x, probs = 0.75)})
  # scen_month = data.frame(Month = scen_month_mean$Group.1,
  #                         date = as.Date(paste(plot_yr,1:12,"15",sep="-")),
  #                         flow_cfs_mean = scen_month_mean$x,
  #                         flow_cfs_med = scen_month_med$x,
  #                         flow_cfs_05pctl = scen_month_05pctl$x,
  #                         flow_cfs_95pctl = scen_month_95pctl$x,
  #                         flow_cfs_25pctl = scen_month_25pctl$x,
  #                         flow_cfs_75pctl = scen_month_75pctl$x)
  #
  #
  #
  # if(save_as_png==TRUE){png(filename = paste(scenario_id,"sim Flow plus recommendeds.png"),
  #                           width = 6, height = 6, units = "in", res = 300)}
  #
  # plot(scen_month$date, scen_month$flow_cfs_med, type = "l", log = "y",
  #      ylim =c(1, 100000), xlim = as.Date(c("1991-01-01","1991-12-31")),
  #      yaxt="n", ylab = "Daily Average Flow (cfs)", xlab = "",
  #      main = paste(plot_name, "scenario"), sub = "Simulated FJ Flow, 1991-2018")
  # abline(h=10^(0:5), v = as.Date(paste(plot_yr,1:12,"01", sep ="-")),col = "darkgray")
  # # Plot 50% of flow and 90% of flow polygons
  # polygon(x = c(scen_month$date,rev(scen_month$date)),
  #         y = c( scen_month$flow_cfs_95pctl, rev(scen_month$flow_cfs_05pctl)),
  #         col = "gray90", border = "NA")
  # polygon(x = c(scen_month$date,rev(scen_month$date)),
  #         y = c( scen_month$flow_cfs_75pctl, rev(scen_month$flow_cfs_25pctl)),
  #         col = "gray75", border = "NA")
  # lines(scen_month$date, scen_month$flow_cfs_med, col = "darkorange", lwd = 2)
  # points(scen_month$date, scen_month$flow_cfs_med, pch=19, col = "darkorange", lwd = 2)
  #
  # axis(side = 2, at = 10^(0:5), labels = c("1", "10", "100", "1,000", "10,000", "100,000"),
  #      cex.axis = 0.8, las = 1)
  # axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA)
  #
  # # Plot CDFW recommended instream flows
  # lines(cdfw_rec$date, cdfw_rec$rec_flow_cfs, col = "dodgerblue",lwd=2)
  #
  # # Plot Forest Service schedule E water right
  # lines(fs_right$date, fs_right$rec_flow_cfs, col = "blue",lwd=2)
  #
  # # Legend
  # legend(x="topright",pt.bg = c(NA,"gray90","gray75",NA,NA),
  #        pch=c(NA,22,22,NA,NA), pt.cex = 2,
  #        lwd = c(2,NA,NA,2,2),
  #        col = c("darkorange",rep("black",2),"dodgerblue","blue"),
  #        legend = c("Scenario monthly median flow","90% of flow","50% of flow",
  #                   "CDFW recommended flows","USFS Water Right"))
  # if(save_as_png==TRUE){dev.off()}

}



# _fish outcomes ----------------------------------------------------------



outmigrant_abundance_figure = function(percent_survival_threshold = 5){

  # 4. Calculate MO.
  good_conditions_years = outmigs$conditions_year[outmigs$Percent.smolt.survival > percent_survival_threshold]
  good_conditions_years = good_conditions_years[!is.na(good_conditions_years)]

  # Plot outmigrant abundance
  # png(filename = "coho outmigrant abundance.png",width = 6, height = 6, units = "in", res = 300)
  barplot(names = outmigs$conditions_year,
          height = outmigs$Percent.smolt.survival, #cex.lab=0.9,
          ylab = "Percent smolt survival", main = "Coho Salmon smolt outmigrant abundance",
          xlab = "Year of freshwater residence (between Brood Year and Smolt Year)",
          sub = "Data from CDFW 2018 Scott River Salmon Studies, Table 7")
  abline(h=percent_survival_threshold, lty = 2)
  legend(x = "right", lty=2, legend = "Proposed 'good year' threshold, 5%")
  # dev.off()
}



smolt_per_female_scatterplots = function(print_correlations = F, metrics,
                                         pick_these=metrics[c(1:7, 17:24)],
                                         smolt_year_adjustment = -1){
  # pdf("smolts per female vs fflows metrics - scatterplots.pdf",11,8.5)
  # par(mfrow = c(5,3))
  par(mfrow = c(2,2))

  # Y = smolts per female
  # X = hydro metrics (winter and spring)

  # fish_data = list(spawners, smolt_per_fem, chinook_abun, coho_abun, outmigs)

  fish_dep_var = smolt_per_fem$Smolts_Produced_Per_Female
  fish_dep_var_ylab = "Smolts per female"
  year_match_tab = data.frame(juv_fish_hydro_yr = smolt_per_fem$Smolt_Year + smolt_year_adjustment,
                              flow_metric = NA,
                              fish_data = as.numeric(smolt_per_fem$Smolts_Produced_Per_Female))

  indices = match(pick_these, metrics)

  for(i in indices){ # avoids peaks
    metric = metrics[i]

    year_match_tab$flow_metric = fflows[,metric][match(year_match_tab$juv_fish_hydro_yr, fflows$Water_Year)]

    plot(x = year_match_tab$flow_metric, y = year_match_tab$fish_data, pch = 19,
         main = metric, col = metric_colors[i],
         xlab = metrics_units[i], ylab = fish_dep_var_ylab)
    bestfit =lm(year_match_tab$fish_data ~  year_match_tab$flow_metric)
    abline(bestfit)
    grid()
    if(print_correlations){
      print(paste(metric, "slope:", round(bestfit$coefficients[2],2),
                  "smolts per female per", metrics_units[i],
                  "Rsquare:", round(summary(bestfit)$r.squared,2),
                  "Pval:", round(summary(bestfit)$coefficients[4],3)))
    }
  }
  # dev.off()
}


escapement_scatterplots = function(print_correlations = F, show_table = T,
                                   metrics,
                                   water_year_adjustment = 1,
                                   pick_these = c(1:7,17:24)){
  # pdf("escapement vs fflows metrics - scatterplots.pdf",11,8.5)
  par(mfrow = c(5,3))

  # Y = Fall escapement (salmon returning to Scott R to spawn)
  # X = hydro metrics (winter and spring)

  # spawners in fall 2007 will correlate with fall-metrics that land in WY 2008. so +1

  fish_dep_var = spawners$chinook_scott
  # remove commas
  fish_dep_var = gsub(",","", fish_dep_var)
  #
  fish_dep_var_ylab = "Scott Chinook Spawner Escapement"
  year_match_tab = data.frame(water_year = spawners$year + water_year_adjustment,
                              flow_metric = NA,
                              fish_data = as.numeric(fish_dep_var))

  lm_metrics_table = data.frame(matrix(NA, nrow = length(pick_these), ncol = 4))
  colnames(lm_metrics_table) = c("fflow","slope", "Rsquare", "Pval")
  for(i in pick_these){ # avoids peaks
    metric = metrics[i]

    year_match_tab$flow_metric = fflows[,metric][match(year_match_tab$water_year, fflows$Water_Year)]

    plot(x = year_match_tab$flow_metric, y = year_match_tab$fish_data, pch = 19,
         main = metric, col = metric_colors[i],
         xlab = metrics_units[i], ylab = fish_dep_var_ylab)
    bestfit =lm(year_match_tab$fish_data ~  year_match_tab$flow_metric)
    abline(bestfit)
    if(print_correlations){
      print(paste(metric, "slope:", round(bestfit$coefficients[2],2),
                  "smolts per female per", metrics_units[i],
                  "Rsquare:", round(summary(bestfit)$r.squared,2),
                  "Pval:", round(summary(bestfit)$coefficients[4],3)))
    }
    if(round(summary(bestfit)$r.squared,2) > 0.1){
      lm_tab_row = which(i== pick_these)
      lm_metrics_table$fflow[lm_tab_row] = metric
      lm_metrics_table$slope[lm_tab_row] = round(bestfit$coefficients[2],2)
      lm_metrics_table$Rsquare[lm_tab_row]= round(summary(bestfit)$r.squared,2)
      lm_metrics_table$Pval[lm_tab_row] = round(summary(bestfit)$coefficients[4],3)

    }
  }
  if(show_table == T){lm_metrics_table}
  # dev.off()
}

escapement_lag_semivariogram=function(fish_abun = spawners$chinook_scott,
                                      title_fish = "Chinook",
                                      min_lag = 0, max_lag = 15){
  xes = min_lag:max_lag
  gammas = vector()
  for(lag_yrs in xes){
    nrow_for_calc = length(fish_abun) - lag_yrs
    escape_0 = fish_abun[1:(nrow_for_calc)]
    escape_1 = fish_abun[(1+lag_yrs):length(fish_abun)]
    gamma_fn = 0.5 * var(escape_1 - escape_0)
    gammas = append(gammas, gamma_fn)

    # plot(main = lag_yrs, escape_0, escape_1)
    # abline(0,1)
  }

  plot(x = xes, y = gammas/10^6,
       main = paste("Temporal semivariogram of",title_fish, "cohorts"),
       xlab = "years lagged",
       ylab = expression(paste(gamma, " value, millions")),
       pch = 19)
  grid()
}


# _old figure, calc versions ----------------------------------------------------


threshold_explainer_jday_means = function(data_tab = metrics_tab,
                                          thresholds = c(8,10,15,20,40,100)){
  group_cutoff = 40 # coho smolt per female. Above this is good, below is bad
  good_selector = data_tab$coho_smolt_per_fem >= group_cutoff &
    !is.na(data_tab$coho_smolt_per_fem)
  bad_selector = data_tab$coho_smolt_per_fem < group_cutoff &
    !is.na(data_tab$coho_smolt_per_fem)
  good_col = "deepskyblue"; bad_col = "brown1"

  par(mfrow = c(3,2), mar = c(4,3,1,2))
  ylab_text = "Coho smolt per female spawner (spf)"
  ylab_6 = c(ylab_text, "", ylab_text,"", ylab_text,"" )
  xlab_text = "Days after Aug. 31 of BY with flow > threshold"
  xlab_6 = c("", "","","",xlab_text,xlab_text)
  text_x = c(rep(140,6))
  text_y = c(rep(95,6))

  ylimits = range(data_tab$coho_smolt_per_fem,na.rm = T)
  # assign colors based on year? meh
  for(i in 1:length(thresholds)){
    thresh = thresholds[i]
    tx=text_x[i]; ty=text_y[i]
    plot(x = data_tab[,paste0("BY_recon_",thresh)],
         y = data_tab$coho_smolt_per_fem,
         ylim = ylimits, xlim = c(0,165), pch = 19,
         xlab = xlab_6[i],
         ylab = ylab_6[i])
    grid()
    points(x = data_tab[good_selector,paste0("BY_recon_",thresh)],
           y = data_tab$coho_smolt_per_fem[good_selector],
           pch= 19, col = good_col)
    points(x = data_tab[bad_selector,paste0("BY_recon_",thresh)],
           y = data_tab$coho_smolt_per_fem[bad_selector],
           pch= 19, col = bad_col)
    good_jday_mean = mean(data_tab[good_selector,paste0("BY_recon_",thresh)], na.rm=T)
    bad_jday_mean = mean(data_tab[bad_selector,paste0("BY_recon_",thresh)], na.rm=T)
    abline(v = good_jday_mean, lwd = 2, lty = 2, col = good_col)
    abline(v = bad_jday_mean, lwd = 2, lty = 2, col = bad_col)

    text(x=tx, y = ty, paste(thresh,"cfs"), cex = 1.5)
    text(x = tx, y = ty-10, paste("Low-spf mean:", round(bad_jday_mean)))
    text(x = tx, y = ty-20, paste("High-spf mean:", round(good_jday_mean)))
    text(x = tx, y = ty-30, paste("Diff.:",round(bad_jday_mean-good_jday_mean), "days"))
  }
}


fj_flow_figure_old=function(){

  # fj_flow_archive=fj_flow
  # The Mediterranean climate produces highly seasonal river flows
  # and groundwater elevations. The top panel shows annual hydrographs
  # of Scott River flow measured at the Fort Jones gauge and median
  # daily flow, water years 1941-2021 (top panel).
  # The bottom panel shows monthly groundwater elevation/depth to
  # groundwater (area-weighted?), MONTH YEAR-MONTH YEAR.

  # Convert flow to daily volume
  fj_flow$Flow_cms = fj_flow$Flow * cfs_to_m3sec
  fj_flow$jday_from_oct1 = NA

  #Plot setup
  water_years = unique(fj_flow$wy) #Isolate water years for plot
  n_wy = length(water_years)
  wy_text = paste(min(water_years), max(water_years), sep = "-")
  plot_colors = rep(rgb(0,0,.5,0.1), n_wy)
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
           xlab = "Days in water year since Oct. 1",
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
  daily_med = aggregate(fj_flow$Flow, by = list(fj_flow$jday_from_oct1), FUN = median)

  lines(daily_med$Group.1, daily_med$x,lwd = 2, col = "brown")

  legend(x="topright", lwd=c(1,2),
         col = c(rgb(0,0,.5,0.3), "brown"),
         legend = c("Annual hydrograph", "Daily median flow"))

}



calc_recon_julian_day = function(dates, flow, recon_threshold, for_hbf=F){
  if(for_hbf==F){
    date_selector = month(dates) %in% c(9:12)
    recon_date = min(dates[date_selector & flow > recon_threshold], na.rm=T)
  }

  if(for_hbf==T){
    recon_date = min(dates[ flow > recon_threshold], na.rm=T)
  }
  if(is.finite(recon_date)){
    return(as.numeric(strftime(recon_date, format = "%j")))
  } else {return(NA)}
}

calc_discon_julian_day= function(dates, flow, discon_threshold){
  # date_selector = month(dates) %in% c(2:10)
  # discon_date = min(dates[date_selector & flow < discon_threshold], na.rm=T)
  discon_date = min(dates[flow < discon_threshold], na.rm=T)
  if(is.finite(discon_date)){
    return(as.numeric(strftime(discon_date, format = "%j")))
  } else {
    return(max(as.numeric(strftime(dates, format = "%j"))))}

}


calc_annual_benefit_components_tab_pre_agu = function(fj_flow_fn, # temporary fj_flow designation to avoid overwriting during debugging
                                                      connect_thresholds = c(20,100)){

  # Part 1. Build component table with untransformed metrics
  main_component_list = c(paste0("fall_recon_jday_", connect_thresholds),
                          paste0("spring_discon_jday_", connect_thresholds), #"Wet_Tim",
                          "total_flow_sep_dec")
  secondary_component_list = c("FA_Mag", "Wet_BFL_Mag_50","storm_days", "SP_ROC", "DS_Mag_50")

  n_comp = length(main_component_list) + length(secondary_component_list)
  wys = unique(fj_flow_fn$wy)
  n_wy = length(wys)
  # Initialize output table
  output_tab = data.frame(matrix(data = NA, nrow = n_wy, ncol = n_comp+1))
  colnames(output_tab) = c("water_year", main_component_list, secondary_component_list)
  output_tab$water_year = unique(fj_flow_fn$wy)

  # eliminate incomplete water years
  wy_record_days = aggregate(fj_flow_fn$Date, by = list(fj_flow$wy), FUN = length)
  keep_wys = wy_record_days$Group.1[wy_record_days$x >= 365]
  output_tab = output_tab[output_tab$water_year %in% keep_wys,]

  for(i in 1:n_wy){
    wy = wys[i]

    # Spring recession dates (Feb. 1 to Oct. 31) (includes oct. of following WY)
    recession_date1 = as.Date(paste0(wy,"-02-01")); recession_date2 = as.Date(paste0(wy,"-10-31"))
    recession_dates = seq.Date(from=recession_date1, to = recession_date2, by = "day")
    recession_flow = fj_flow_fn$Flow[fj_flow_fn$Date %in% recession_dates]

    # Capture fall season, from Sept 1 of prev. water year to Dec 31 of current water year
    wy_dates_sepdec = fj_flow_fn$Date[year(fj_flow_fn$Date) == (wy - 1) & month(fj_flow_fn$Date) %in% c(9:12)]
    wy_flow_sepdec = fj_flow_fn$Flow[fj_flow_fn$Date %in% wy_dates_sepdec]

    # Calculate for each connection threshold included
    for(j in 1:length(connect_thresholds)){
      # 1. fall_recon_jday
      pick_this_col = grepl(pattern="fall_recon_jday", x = colnames(output_tab)) &
        grepl(pattern=connect_thresholds[j], x = colnames(output_tab))
      output_tab[i, pick_this_col] = calc_recon_julian_day(dates = wy_dates_sepdec,
                                                           flow = wy_flow_sepdec,
                                                           recon_threshold = connect_thresholds[j])

      # 2. spring_discon_jday
      pick_this_col = grepl(pattern="spring_discon_jday", x = colnames(output_tab)) &
        grepl(pattern=connect_thresholds[j], x = colnames(output_tab))
      output_tab[i, pick_this_col] = calc_discon_julian_day(dates = recession_dates,
                                                            flow = recession_flow,
                                                            discon_threshold = connect_thresholds[j])
    }

    # 3. total_flow_sep_dec. total flow includes sep of previous  water year.
    output_tab[i, "total_flow_sep_dec"] = sum(wy_flow_sepdec * cfs_to_m3day)
    # 4. Wet_Tim see below

    # Secondary components
    # 5. FA_Mag see below
    # 6. Wet_BFL_Mag_50 see below
    # 7. "storm_days_fraction" see below
    # 8. SP_ROC see below
    # 9. DS_Mag_50 see below
  }

  output_tab$"Wet_Tim" = fflows$Wet_Tim[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"FA_Mag" = fflows$FA_Mag[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"Wet_BFL_Mag_50" = fflows$Wet_BFL_Mag_50[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"SP_ROC" = fflows$SP_ROC[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"DS_Mag_50" = fflows$DS_Mag_50[match(output_tab$water_year, fflows$Water_Year)]

  # storm penalty should be small influence.
  # storm penalty. number of days per year of flow greater than the 90%ile threshold. currently 1500 cfs.
  pctile_90 = quantile(fj_flow_fn$Flow, 0.9)
  storm_days = aggregate(fj_flow_fn$Flow, by = list(fj_flow_fn$wy), function(x){sum(x>pctile_90)})
  colnames(storm_days) = c("water_year", "num_days_flow_gt_90_pctile")
  output_tab$"storm_days" = storm_days$num_days_flow_gt_90_pctile[match(output_tab$water_year, storm_days$water_year)]

  # Part 2. Clean up output_tab
  # Bah. going to exclude FA_Mag because it doesn't exist every year. and it was a mixed signal.

  # spring_discon: this is a hack; to do: do this more elegantly
  # add numbers to spring discon julian days that went below the threshold after the set time period
  for(j in 1:length(connect_thresholds)){
    pick_this_col = grepl(pattern="spring_discon_jday", x = colnames(output_tab)) &
      grepl(pattern=connect_thresholds[j], x = colnames(output_tab))

    max_discon_day = max(output_tab[,pick_this_col], na.rm=T)
    output_tab[,pick_this_col][is.na(output_tab[,pick_this_col])] = max_discon_day +1

    pick_this_col = grepl(pattern="fall_recon_jday", x = colnames(output_tab)) &
      grepl(pattern=connect_thresholds[j], x = colnames(output_tab))

    min_recon_day = min(output_tab[,pick_this_col], na.rm=T)
    output_tab[,pick_this_col][is.na(output_tab[,pick_this_col])] = min_recon_day - 1

  }


  # Part 3. Translate components into fractional things to be added
  # Initialize new columns
  new_col_names = c(paste0("fall_recon_add_",connect_thresholds),
                    paste0("spring_discon_add_",connect_thresholds),#, "Wet_Tim"
                    "tot_flow_sep_dec_add")
  output_tab[,new_col_names]=NA
  # secondary_component_list = c("FA_Mag", "Wet_BFL_Mag_50","storm_days", "SP_ROC", "DS_Mag_50")

  # Connectivity 0-dates and day-to-benefit scalers
  # Fall
  fall_recon_conversion_tab = data.frame(threshold_cfs=c(10, 20, 30, 40, 60,100),
                                         days_scaler = c(12, 14, 16, 18, 20, 22))
  # I guess the argument is that 2 weeks later reconnection, at 20 cfs, is 1 benefit unit worse for fish.
  sep1 = 244 #oct1 = 273 #sep15 = 257

  spring_discon_conversion_tab = data.frame(threshold_cfs=c(10, 20, 30, 40, 60,100),
                                            days_scaler = c(18, 21, 24, 27, 30, 33))
  # Later disconnection is better, but not as critical as reconnection, so number of days per benefit is bigger


  for(j in 1:length(connect_thresholds)){
    # A) Fall recon days. Later recon is a more negative number because it is worse. A reconnect on Sep 1 would be perfect, 0 penalty.
    days_scaler = fall_recon_conversion_tab$days_scaler[fall_recon_conversion_tab$threshold_cfs ==
                                                          connect_thresholds[j]]

    jday_col = grepl(pattern="fall_recon_jday", x = colnames(output_tab)) &
      grepl(pattern=connect_thresholds[j], x = colnames(output_tab))
    add_col = grepl(pattern="fall_recon_add", x = colnames(output_tab)) &
      grepl(pattern=connect_thresholds[j], x = colnames(output_tab))

    output_tab[,add_col] = (output_tab[,jday_col] - sep1) / days_scaler * -1 # negatize
    # summary((output_tab[,add_col] - sep1))
    # hist(output_tab[,add_col])


    # B) "spring_discon_jday"

    # Bigger is better. But maybe the scaler should be different. Yeah, generally less strong correlation.
    # hist(output_tab$spring_discon_jday[output_tab$water_year > 1990])
    # summary(output_tab$spring_discon_jday)
    days_scaler = spring_discon_conversion_tab$days_scaler[spring_discon_conversion_tab$threshold_cfs==connect_thresholds[j]]

    jday_col = grepl(pattern="spring_discon_jday", x = colnames(output_tab)) &
      grepl(pattern=connect_thresholds[j], x = colnames(output_tab))
    add_col = grepl(pattern="spring_discon_add", x = colnames(output_tab)) &
      grepl(pattern=connect_thresholds[j], x = colnames(output_tab))

    output_tab[,add_col] = (output_tab[,jday_col]) / days_scaler

    # summary((output_tab$fall_recon_jday - sep1))
    # hist(output_tab$fall_recon_add)

  }

  # C) total sep-dec flow. Includes september flow from the previous water year, I guess.
  # CLEAN THIS UP. put analysis in the .Rmd.


  # this quantity is roughly log-normal. Going to just go for it and log-transform it
  # lower than average is bad. Negative number. Above average is good. 1 st. dev is about 1 benefit unit.
  # Alternative: set a threshold. Above that threshold is good; below it is bad.
  # threshold: 40 cfs for all 122 days. or, uh, CDFW flow?
  # desired_tot_flow = 122 * 40 * cfs_to_m3day
  desired_tot_flow = (30*62 + 15*134 + 16*139 + 30* 266 + 31*337) * cfs_to_m3day # cdfw
  # desired_tot_flow = (30*30 + 31*40 + 31*200) * cfs_to_m3day # usfs
  # tot_mean = mean(output_tab$total_flow_sep_dec)
  threshold = desired_tot_flow
  # threshold = 1
  # summary((output_tab$total_flow_sep_dec - threshold) / threshold)
  # output_tab$tot_flow_sep_dec_add = log((output_tab$total_flow_sep_dec - threshold) / threshold)

  # Gotta log transform this to emphasize benefits at the lower end of the scale.
  # avg_log_tot_flow = mean(log(output_tab$total_flow_sep_dec))
  log_desired_flow = log(desired_tot_flow)
  output_tab$tot_flow_sep_dec_add = log(output_tab$total_flow_sep_dec) - log_desired_flow#avg_log_tot_flow
  # hist(output_tab$tot_flow_sep_dec_add)
  #cap this component at 5. Gotta figure out a defense for this step.

  # C) "Wet_Tim",
  # Earlier onset is better.
  # (ugh, this is kind of redundant with fall reconnection.) So,
  # hist(output_tab$Wet_Tim)




  # Minor components

  # E) Wet_BFL_Mag_50,
  # F) storm_days
  # Divide by average to calculate fraction of average number of storm days
  mean_storm_days = mean(storm_days$num_days_flow_gt_90_pctile)
  output_tab$frac_of_avg_storm_days = round(storm_days$num_days_flow_gt_90_pctile / mean_storm_days, digits = 2)
  # hist(storm_days$frac_of_avg_storm_days)
  # plot(storm_days$water_year, storm_days$num_days_flow_gt_90_pctile, type = "b")

  # G) SP_ROC
  # F) DS_Mag_50


  return(output_tab)
  # THEN: add them together!
  # Compare to smolt_to_fem ratios!



}

calc_annual_benefit_components_tab_old = function(flow_tab, components = c("BY_recon_10",
                                                                           "BY_recon_100")){

  component_list=vector(mode = "character", length = 0) #initialize

  # Part 1. Build component table with untransformed metrics
  recon_comps = components[grep(pattern = "recon", x = components)]
  if(length(recon_comps) > 0){
    recon_comp_matrix = matrix(data = unlist(strsplit(recon_comps, split = "_")), byrow = T, nrow = length(recon_comps))
    recon_thresh = as.numeric(recon_comp_matrix[,3])
    # add to component list
    component_list = c(component_list, paste0("fall_recon_days_since_aug31_", recon_thresh))
  }
  discon_comps = components[grep(pattern = "discon", x = components)]
  if(length(discon_comps) > 0){
    discon_comp_matrix = matrix(data = unlist(strsplit(discon_comps, split = "_")), byrow = T, nrow = length(discon_comps))
    discon_thresh = as.numeric(discon_comp_matrix[,3])
    # add to component list
    component_list = c(component_list, paste0("spring_discon_days_since_aug31_", discon_thresh))
  }


  ws_dur_comps = components[grep(pattern = "Wet_BFL_Dur", x = components)]
  if(length(ws_dur_comps) > 0){
    component_list = c(component_list, paste0("Wet_BFL_Dur"))  # add to component list
  }

  # main_component_list = c(paste0("fall_recon_jday_", connect_thresholds),
  #                         paste0("spring_discon_jday_", connect_thresholds), #"Wet_Tim",
  #                         "total_flow_sep_dec")
  # secondary_component_list = c("FA_Mag", "Wet_BFL_Mag_50","storm_days", "SP_ROC", "DS_Mag_50")

  n_comp = length(component_list) #+ length(secondary_component_list)
  wys = unique(flow_tab$wy)
  n_wy = length(wys)
  # Initialize output table
  output_tab = data.frame(matrix(data = NA, nrow = n_wy, ncol = n_comp+1))
  colnames(output_tab) = c("water_year", component_list) #, secondary_component_list)
  output_tab$water_year = unique(flow_tab$wy)

  # eliminate incomplete water years
  wy_record_days = aggregate(flow_tab$Date, by = list(fj_flow$wy), FUN = length)
  keep_wys = wy_record_days$Group.1[wy_record_days$x >= 365]
  output_tab = output_tab[output_tab$water_year %in% keep_wys,]

  for(i in 1:n_wy){
    wy = wys[i]

    # Spring recession dates (Feb. 1 to Oct. 31) (includes oct. of following WY)
    recession_date1 = as.Date(paste0(wy,"-02-01")); recession_date2 = as.Date(paste0(wy,"-10-31"))
    recession_dates = seq.Date(from=recession_date1, to = recession_date2, by = "day")
    discon_flow = flow_tab$Flow[flow_tab$Date %in% recession_dates]

    # Fall recon dates (Sep. 1 to Feb 28, to capture crazy 2014 feb 9 recon)
    recon_date1 = as.Date(paste0(wy-1,"-09-01")); recon_date2 = as.Date(paste0(wy,"-02-28"))
    recon_dates = seq.Date(from=recon_date1, to = recon_date2, by = "day")
    recon_flow = flow_tab$Flow[flow_tab$Date %in% recon_dates]

    # # Capture fall season, from Sept 1 of prev. water year to Dec 31 of current water year
    # wy_dates_sepdec = flow_tab$Date[year(flow_tab$Date) == (wy - 1) & month(flow_tab$Date) %in% c(9:12)]
    # wy_flow_sepdec = flow_tab$Flow[flow_tab$Date %in% wy_dates_sepdec]

    # Calculate for each connection threshold included
    if(length(recon_comps)>0){
      for(j in 1:length(recon_thresh)){
        # 1. fall_recon_jday

        pick_this_col = grepl(pattern="fall_recon_days_since_aug31", x = colnames(output_tab)) &
          grepl(pattern=recon_thresh[j], x = colnames(output_tab))
        output_tab[i, pick_this_col] = calc_recon_days_since_aug_31(dates = recon_dates,
                                                                    flow = recon_flow,
                                                                    recon_threshold = recon_thresh[j])
      }
    }

    if(length(discon_comps)>0){
      # 1. fall_recon_jday
      for(j in 1:length(discon_thresh)){
        pick_this_col = grepl(pattern="spring_discon_days_since_aug31", x = colnames(output_tab)) &
          grepl(pattern=recon_thresh[j], x = colnames(output_tab))
        output_tab[i, pick_this_col] = calc_discon_days_since_aug_31(dates = recon_dates,
                                                                     flow = discon_flow,
                                                                     discon_threshold = discon_thresh[j])
      }
    }

    # 3. total_flow_sep_dec. total flow includes sep of previous  water year.
    # output_tab[i, "total_flow_sep_dec"] = sum(wy_flow_sepdec * cfs_to_m3day)
    # 4. Wet_Tim see below

    # Secondary components
    # 5. FA_Mag see below
    # 6. Wet_BFL_Mag_50 see below
    # 7. "storm_days_fraction" see below
    # 8. SP_ROC see below
    # 9. DS_Mag_50 see below
  }

  output_tab$"Wet_BFL_Dur" = fflows$Wet_BFL_Dur[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"Wet_Tim" = fflows$Wet_Tim[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"FA_Mag" = fflows$FA_Mag[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"Wet_BFL_Mag_50" = fflows$Wet_BFL_Mag_50[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"SP_ROC" = fflows$SP_ROC[match(output_tab$water_year, fflows$Water_Year)]
  output_tab$"DS_Mag_50" = fflows$DS_Mag_50[match(output_tab$water_year, fflows$Water_Year)]

  # storm penalty should be small influence.
  # storm penalty. number of days per year of flow greater than the 90%ile threshold. currently 1500 cfs.
  pctile_90 = quantile(flow_tab$Flow, 0.9)
  storm_days = aggregate(flow_tab$Flow, by = list(flow_tab$wy), function(x){sum(x>pctile_90)})
  colnames(storm_days) = c("water_year", "num_days_flow_gt_90_pctile")
  output_tab$"storm_days" = storm_days$num_days_flow_gt_90_pctile[match(output_tab$water_year, storm_days$water_year)]

  # Part 2. Clean up output_tab
  # Bah. going to exclude FA_Mag because it doesn't exist every year. and it was a mixed signal.

  # # spring_discon: this is a hack; to do: do this more elegantly
  # # add numbers to spring discon julian days that went below the threshold after the set time period
  # for(j in 1:length(connect_thresholds)){
  #   pick_this_col = grepl(pattern="spring_discon_jday", x = colnames(output_tab)) &
  #     grepl(pattern=connect_thresholds[j], x = colnames(output_tab))
  #
  #   max_discon_day = max(output_tab[,pick_this_col], na.rm=T)
  #   output_tab[,pick_this_col][is.na(output_tab[,pick_this_col])] = max_discon_day +1
  #
  #   pick_this_col = grepl(pattern="fall_recon_jday", x = colnames(output_tab)) &
  #     grepl(pattern=connect_thresholds[j], x = colnames(output_tab))
  #
  #   min_recon_day = min(output_tab[,pick_this_col], na.rm=T)
  #   output_tab[,pick_this_col][is.na(output_tab[,pick_this_col])] = min_recon_day - 1
  #
  # }


  return(output_tab)
  # THEN: add them together!
  # Compare to smolt_to_fem ratios!



}

calc_hbf_annual_values_old = function(model_id = "lm2a", best_tab,
                                      coef_tab, fj_flow){
  model_components = best_tab[model_id,][!is.na(best_tab[model_id,]) &
                                           best_tab[model_id,]!=""]

  hbf_tab = calc_annual_benefit_components_tab(flow_tab = fj_flow,
                                               components = model_components)

  # slopes for selected model comps
  # component_slopes = coef_tab[model_name, model_components]
  # hbf_comps = data.frame(metric = model_components, slope = NA)

  #translation table
  translate = data.frame(lm_name = c("BY_recon_10",
                                     "BY_recon_100",
                                     "RY_Wet_BFL_Dur"),
                         hbf_name = c("fall_recon_days_since_aug31_10",
                                      "fall_recon_days_since_aug31_100",
                                      "Wet_BFL_Dur"))


  # Calculate weights and convert model components to spf values

  n_comp = length(model_components)
  for(j in 1:n_comp){
    translate_index = translate$lm_name==model_components[j]
    lm_name = model_components[j]
    hbf_name = translate$hbf_name[translate_index]

    new_col_name = paste0("comp_",j,"_in_spf")
    hbf_tab[,new_col_name] = hbf_tab[,hbf_name] * coef_tab[model_id,lm_name]
  }

  spf_cols = grepl(pattern = "spf", x = colnames(hbf_tab))
  if(sum(spf_cols)>1){hbf_tab$hbf_total = rowSums(hbf_tab[,spf_cols])
  } else {hbf_tab$hbf_total = hbf_tab[,spf_cols]  }

  return(hbf_tab)
}
