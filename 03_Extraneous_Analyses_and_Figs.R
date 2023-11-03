# load data from manuscript and figure scripts

# SVIHM 101 workshop 2023-------------------------------------------

out_dir = "C:/Users/Claire/Dropbox/Documents/UCD/Postdoc Project starting 2022.07.05/Presentations/2023.11 SVIHM 101 materials"

# California Oregon, Klamath, Siskiyou, Scott R watershed --------------
png(filename=file.path(out_dir,"cal ore figure.png"),
    width = 3, height =5, units = "in", res=300)
ca_or_figure(include_legend=F)
dev.off()

# Scott Valley Map: hillshade, watershed, tribs, river, towns, FJ gauge -----------------

setting_map = tm_shape(hill_wsh) +
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
  # tm_shape(qvir) + tm_polygons(color_qvir , border.lwd = 1)+
  tm_shape(cities_centroid, name = "Town or Community") +
  tm_symbols(color_cities, border.lwd=1, size = 0.5) +
  tm_text("NAME", size = 0.8, xmod = cities_centroid$label_xmod, ymod = cities_centroid$label_ymod) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_compass(type = "4star",#position = c("left", "top"), ) +
             position = c(.82,.07)) #+
  # # legend for main figure
  # tm_add_legend(type = "title", title = "Watershed Features") +
  # tm_add_legend( type = "line", lwd = c(2, 3, 2), col =  c(color_watershed, color_river, color_tribs),
  #                labels = c("Watershed Boundary", "Scott River", "Major Tributaries")) +
  # # tm_add_legend(type="fill", col = color_qvir, border.lwd = 1, labels = "QVIR")+
  # tm_add_legend(type="symbol", col = color_cities, border.lwd = NA, size = 0.7, labels = "Town or Place")+
  # tm_add_legend(type="symbol", col = color_gauges, size = 0.8, labels = "Fort Jones Gauge")+
  # tm_add_legend(type="symbol", col = color_confluence, shape = 25, size = 1, labels = "Scott-Klamath Confluence")+
  # tm_add_legend(type = "line", lwd = c(2, 1.5), col = c(color_interstate, color_state_road),
  #               labels = c("Interstate 5", "State Route 3")) +
  # # legend for inset map
# tm_add_legend(type = "title", title = "Inset Map") +
# tm_add_legend(type="fill", col = color_watershed_fill, border.col = "gray10", labels = "Scott River Watershed") +
# tm_add_legend(type="fill", col = color_klamath, border.col = "gray10", labels = "Klamath Basin")+
# tm_add_legend(type="fill", col = color_states, border.col = "gray10", labels = "Ore. and Calif.")+
# tm_add_legend(type="line", col = color_county, labels = "Siskiyou County", lwd=2)+
# # finish legend
# tm_layout(legend.bg.color = "white", legend.frame = T, legend.text.size = 2)

png(filename=file.path(out_dir,"setting map.png"),
    width = 5, height = 6, units = "in", res=300)
setting_map
dev.off()

# FJ hydrograph (straight y axis vs log)


wy = 2015
vert_lines = seq.Date(from=as.Date(paste(wy-1,11,01,sep="-")),
                      to=as.Date(paste(wy,11,01,sep="-")),
                      by="2 months")

dates = fj_flow$Date[fj_flow$wy==wy]
flow = fj_flow$Flow[fj_flow$wy==wy]

png(filename=file.path(out_dir,"fj hydrographs straight y_no lines.png"),
    width = 5, height = 5, units = "in", res=300)

plot(dates, flow, type = "l", col = "dodgerblue", lwd = 2,
     xlab = paste("Date in water year",wy),
     ylab = "Avg. Daily Flow (cfs)",
     main = "Fort Jones flow, standard y-axis")
abline(v=vert_lines, h=pretty(range(flow)), lty = 3, col = "gray")
# abline(h=10000, lty = 2, lwd=3, col = "goldenrod")
# abline(h=1000, lty = 2, lwd=3, col = "red")
# abline(h=100, lty = 2, lwd=3, col = "brown")
# abline(h=10, lty = 2, lwd=3, col = "black")

# abline(h=10000, lty = 2, lwd=3, col = "goldenrod")
# abline(h=8000, lty = 2, lwd=3, col = "red")
# abline(h=6000, lty = 2, lwd=3, col = "brown")
# abline(h=4000, lty = 2, lwd=3, col = "black")

dev.off()

png(filename=file.path(out_dir,"fj hydrographs log y_no lines.png"),
    width = 5, height = 5, units = "in", res=300)

plot(dates, flow, type = "l", col = "dodgerblue", lwd = 2,
     log="y",
     xlab = paste("Date in water year",wy),
     ylab = "Avg. Daily Flow (cfs)",
     main = "Fort Jones flow, Log y-axis",
     yaxt="n")
abline(v=vert_lines, h=10^(0:5), lty=3, col = "gray")
axis(side=2, at = 10^(0:5), tck=-0.04, labels = c("1","10","100","1,000","10,000","100,000"), )
axis(side=2, at= (1:9) * sort(rep(10^(0:5),9)), tck=-0.02, labels = NA)
# abline(h=10000, lty = 2, lwd=3, col = "goldenrod")
# abline(h=1000, lty = 2, lwd=3, col = "red")
# abline(h=100, lty = 2, lwd=3, col = "brown")
# abline(h=10, lty = 2, lwd=3, col = "black")
# abline(h=10000, lty = 2, lwd=3, col = "goldenrod")
# abline(h=8000, lty = 2, lwd=3, col = "red")
# abline(h=6000, lty = 2, lwd=3, col = "brown")
# abline(h=4000, lty = 2, lwd=3, col = "black")

dev.off()

# Fort Jones flow, 80 years
dates = fj_flow$Date
flow = fj_flow$Flow
vert_lines = seq.Date(from=as.Date("1940-01-01"),
                      to=as.Date("2040-01-01"),
                      by="10 years")

png(filename=file.path(out_dir,"fj hydrograph 80 yrs.png"),
    width = 12, height = 5, units = "in", res=300)

plot(dates, flow, type = "l", col = "dodgerblue", lwd = 2,
     log="y",
     xlab = "",
     ylab = "Avg. Daily Flow (cfs)",
     main = "Fort Jones flow, 1942 to 2023",
     yaxt="n")
abline(v=vert_lines, h=10^(0:5), lty=3, col = "gray")
axis(side=2, at = 10^(0:5), tck=-0.03, labels = c("1","10","100","1,000","10,000","100,000"), )
axis(side=2, at= (1:9) * sort(rep(10^(0:5),9)), tck=-0.015, labels = NA)

dev.off()


# Fort Jones flow, wy 1991-2023
dates = fj_flow$Date[fj_flow$Date >= as.Date("1990-10-01")]
flow = fj_flow$Flow[fj_flow$Date >= as.Date("1990-10-01")]
vert_lines = seq.Date(from=as.Date("1940-01-01"),
                      to=as.Date("2040-01-01"),
                      by="10 years")

png(filename=file.path(out_dir,"fj hydrograph 91-23.png"),
    width = 12, height = 5, units = "in", res=300)

plot(dates, flow, type = "l", col = "dodgerblue", lwd = 2,
     log="y",
     xlab = "",
     ylab = "Avg. Daily Flow (cfs)",
     main = "Fort Jones flow, 1991 to 2023 (model simulation period)",
     yaxt="n")
abline(v=vert_lines, h=10^(0:5), lty=3, col = "gray")
axis(side=2, at = 10^(0:5), tck=-0.03, labels = c("1","10","100","1,000","10,000","100,000"), )
axis(side=2, at= (1:9) * sort(rep(10^(0:5),9)), tck=-0.015, labels = NA)

dev.off()


# Wet and dry years

wy = 2014
vert_lines = seq.Date(from=as.Date(paste(wy-1,11,01,sep="-")),
                      to=as.Date(paste(wy,11,01,sep="-")),
                      by="2 months")

dates = fj_flow$Date[fj_flow$wy==wy]
flow = fj_flow$Flow[fj_flow$wy==wy]

png(filename=file.path(out_dir,"fj hydrograph dry year 2014.png"),
    width = 5, height = 5, units = "in", res=300)

plot(dates, flow, type = "l", col = "dodgerblue", lwd = 2,
     log="y", ylim = c(3,20000),
     xlab = paste("Date in water year",wy),
     ylab = "Avg. Daily Flow (cfs)",
     main = "Fort Jones flow, Dry Year (2014)",
     yaxt="n")
abline(v=vert_lines, h=10^(0:5), lty=3, col = "gray")
axis(side=2, at = 10^(0:5), tck=-0.04, labels = c("1","10","100","1,000","10,000","100,000"), )
axis(side=2, at= (1:9) * sort(rep(10^(0:5),9)), tck=-0.02, labels = NA)


dev.off()

wy = 2017
vert_lines = seq.Date(from=as.Date(paste(wy-1,11,01,sep="-")),
                      to=as.Date(paste(wy,11,01,sep="-")),
                      by="2 months")

dates = fj_flow$Date[fj_flow$wy==wy]
flow = fj_flow$Flow[fj_flow$wy==wy]

png(filename=file.path(out_dir,"fj hydrograph wet year 2017.png"),
    width = 5, height = 5, units = "in", res=300)

plot(dates, flow, type = "l", col = "dodgerblue", lwd = 2,
     log="y", ylim = c(3,20000),
     xlab = paste("Date in water year",wy),
     ylab = "Avg. Daily Flow (cfs)",
     main = "Fort Jones flow, Wet Year (2017)",
     yaxt="n")
abline(v=vert_lines, h=10^(0:5), lty=3, col = "gray")
axis(side=2, at = 10^(0:5), tck=-0.04, labels = c("1","10","100","1,000","10,000","100,000"), )
axis(side=2, at= (1:9) * sort(rep(10^(0:5),9)), tck=-0.02, labels = NA)


dev.off()
