# Retrieve and Clean Manuscript Datasets and Save as Local Copy


# This manuscript relies on a combination of publicly available (and
# downloadable) datasets and datasets that must be stored in this
# repository for access.


# Types of bit decay that can cause this script to throw an error:
# 1) May run into public datasets whose naming conventions have changed (i.e.,
# a shapefile that was called California_Counties.shp and is subsequently
# called i03_CaliforniaCounties.shp).
# 2) Alternatively, an agency might have changed its hosting service and the
# download link may have changed. In this case, finding the new link should be
# easy by checking the "accessed from" website.


# If desired, connect to Data Management System (DMS) Archive folder to
# save copies of each individual dataset (rather than one combined .Rdata environment).
# When downloading this set of spatial and tabular datasets for the first
# time, or updating them all, expect the script to take about 10-20 minutes to run,
# depending on your download speed.


# One workaround to avoid the long runtime, if updating only one dataset is desired:
# 1) In a new (empty-environment) session, load the manuscript_data.Rdata environment.
# 2) In this script, overwrite one of the dataset variables by running only the lines
# of code in which that dataset is downloaded or updated.
# 3) Run the final line of code in this script, in which the working environment is
# saved as manuscript_data.Rdata.

# library(rgdal)
# library(raster)
# library(rpostgis)
# library(rgeos)
# library(postGIStools)
library(here)
library(httr)
library(stringr)
library(rstudioapi)
library(readxl)
library(tidyr)
library(soilDB) #ssurgo
library(aqp) #ssurgo
library(plyr) #ssurgo
library(data.table) #fread function
library(dataRetrieval) # USGS flow data
library(readr) # for parsing downloaded NOAA data
# library(devtools); devtools::install_github("flowwest/CDECRetrieve") # This line only needs to be run once to install the CDECRetrieve package (which is not available on CRAN)
# library(CDECRetrieve) # for CDEC data
library(tidyr)
library(readxl)
library(geodata)
library(sf)
library(terra)

#Directories and local file name update
if(!exists("ms_dir")){ms_dir = here::here()}
data_dir = file.path(ms_dir, "Data")
### wl_dir = file.path(data_dir, "Wells and water levels")
scratch_dir = file.path(ms_dir, "scratch_work")

save_data_to_archive = F
# If saving data to an archive (such as cloud storage),
# set save_data_to_archive = TRUE, and populate archive directory name here
dms_archive_dir = "" #[INSERT ARCHIVE DIRECTORY]
### latest_vmp_wl_data_filename = "ScottValleyGWMonitProg_2022-11.csv"

read_in_model_results = F # no need in this manuscript for multiple management scenarios
# Directory for postprocessed, tabulated results
svihm_results_dir = file.path(data_dir, "SVIHM Model Results")

#_SPATIAL DATA ------------------------------------------------------------

# US States ---------------------------------------------------------------

us_states = gadm(country = "USA", level = 1, path = scratch_dir)
file.remove(file.path(scratch_dir,"gadm41_USA_1_pk.rds"))
ca_or = us_states[us_states$NAME_1 %in% c("California", "Oregon"),]
ca_or = st_transform(x = st_as_sf(ca_or), crs = crs("+init=epsg:3310"))

# California Counties -----------------------------------------------------
#Accessed from https://data.cnra.ca.gov/dataset/california-counties
counties_url = "http://atlas-dwr.opendata.arcgis.com/datasets/f26ff0ccda6e48a8b923927cfdb2a451_0.zip"

zipname = strsplit(counties_url, "/")[[1]][length(strsplit(counties_url, "/")[[1]])]
counties_dl = GET(counties_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
county_shp_name ="i03_CaliforniaCounties"
counties_all = st_read( file.path(scratch_dir, paste0(county_shp_name,".shp")))
county = counties_all[counties_all$COUNTY_NAM=="Siskiyou",]
# county = st_transform(x = county, crs = crs("+init=epsg:3310"))
# county_wgs84 = county
county = st_transform(x = county, crs = st_crs(3310))

#Copy to the box DMS archive
if(save_data_to_archive==T &
   !file.exists(file.path(dms_archive_dir, "siskiyou_county_boundary.shp"))){
  st_write(obj = county, dsn = dms_archive_dir, layer = "siskiyou_county_boundary", driver = "ESRI Shapefile")
}
#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml")
file.remove(file.path(scratch_dir,paste(county_shp_name, extension_list, sep = ".")))

# California Cities ----------------------------------------
# accessed from https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-california-current-place-state-based
cities_url = "https://www2.census.gov/geo/tiger/TIGER2016/PLACE/tl_2016_06_place.zip"

zipname = strsplit(cities_url, "/")[[1]][length(strsplit(cities_url, "/")[[1]])]
zipname = gsub("-","_",zipname)
cities_dl = GET(cities_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(zipfile = file.path(scratch_dir, zipname), exdir = scratch_dir)#, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
# cities_all = st_read(file.path(scratch_dir, "CA_Places_TIGER2016.shp"))
layer_name = gsub(zipname, pattern = ".zip",replacement="")#
# layer_name = "ca_places_boundaries"
cities_all = st_read(file.path(scratch_dir, paste0(layer_name,".shp")))
cities = st_transform(x=cities_all, crs=st_crs(3310))
cities = cities[county,]

#Copy to the box DMS archive
if(save_data_to_archive==T &
   !file.exists(file.path(dms_archive_dir, "siskiyou_city_boundaries.shp"))){
  st_write(obj = cities, dsn = dms_archive_dir, layer = "siskiyou_city_boundaries", driver = "ESRI Shapefile")
}
#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml", "sbn", "sbx", "shp.xml")
file.remove(file.path(scratch_dir,paste(layer_name, extension_list, sep = ".")))

# California Boundary ------------------------------------------------------
california = st_union(counties_all)
california = st_transform(california, st_crs(3310))

# Roads (TIGER) -----------------------------------------------------------
# Accessed from https://catalog.data.gov/dataset/tiger-line-shapefile-2018-county-siskiyou-county-ca-all-roads-county-based-shapefile
roads_url = "https://www2.census.gov/geo/tiger/TIGER2018/ROADS/tl_2018_06093_roads.zip"
zipname = strsplit(roads_url, "/")[[1]][length(strsplit(roads_url, "/")[[1]])]
layer_name = gsub(zipname, pattern = ".zip",replacement="")
roads_dl = GET(roads_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir))
#Read shapefiles into R

roads_all = st_read( file.path(scratch_dir, paste0(layer_name,".shp")))
roads_all = st_transform(roads_all, st_crs(3310))

# #Copy to the box DMS archive
if(save_data_to_archive==T &
   !file.exists(file.path(dms_archive_dir, "tiger_roads_siskiyou_county.shp"))){
  st_write(obj = roads_all, dsn = dms_archive_dir, layer = "tiger_roads_siskiyou_county", driver = "ESRI Shapefile")
}
#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "shp.ea.iso.xml", "shp.iso.xml")
file.remove(file.path(scratch_dir,paste(layer_name, extension_list, sep = ".")))


# DWR Basin Boundaries ----------------------------------------------------
#Accessed at https://data.cnra.ca.gov/dataset/ca-bulletin-118-groundwater-basins
# basins_url = "http://atlas-dwr.opendata.arcgis.com/datasets/b5325164abf94d5cbeb48bb542fa616e_0.zip"
basins_url = "https://gis.data.cnra.ca.gov/datasets/bdfc6550b4f3401a83ad1e2f468140ca_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
zipname1 = strsplit(basins_url, "/")[[1]][length(strsplit(basins_url, "/")[[1]])]
zipname = paste0(strsplit(zipname1, ".zip")[[1]][[1]], ".zip")
#retrieve zip file from url
basins_dl = GET(basins_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
#unzip in the scratch work directory
unzip(file.path(scratch_dir,zipname), exdir = file.path(scratch_dir))#, list = TRUE)
# Read shapefile
gw_basin_name = "i08_B118_CA_GroundwaterBasins"
basins_all = st_read(file.path(scratch_dir, paste0(gw_basin_name,".shp")))
#subset the Scott GSP basin and reproject to 3310
basin = basins_all[basins_all$Basin_Numb %in% c("1-005"),]
basin = st_transform(basin, st_crs(3310))
# Write to Dropbox
if(save_data_to_archive==T &
   !file.exists(file.path(dms_archive_dir, "scott_basin_boundary_2018.shp"))){
  st_write(obj = basin, dsn = dms_archive_dir, layer = "scott_basin_boundary_2018", driver = "ESRI Shapefile")
}
#Remove scratch work
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml")
file.remove(file.path(scratch_dir,paste(gw_basin_name, extension_list, sep = ".")))


# USGS Watersheds, waterbodies, and streams -------------------------------

# _Scott River watershed --------------------------------------------------
# Accessed at https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/HU8/Shape/
# Which was accessed via https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
HUC8_num = c(18010208)#, 18010207, 18010205) # scott, shasta, butte
valley_name = ("scott")#, "shasta", "butte")
# huc_table = data.frame(huc8 = HUC8_nums, basin = valley_names)

#Download the zipped files for each watershed, unzip, load into R, write to database (overwriting for each new basin)
# for(i in 1:3){
  huc = HUC8_num # huc_table$huc8[i]
  basin_name = valley_name #huc_table$basin[i]

  # wsh_url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_",huc,"_HU8_Shape.zip")
  wsh_url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_",huc,"_HU8_Shape.zip")
  zipname = paste0("NHD_H_",huc,"_HU8_Shape.zip")
  # Download from USGS site and write to the working drive (scratch_dir)
  wsh_dl = GET(wsh_url, write_disk(file.path(scratch_dir, zipname), overwrite = TRUE))
  # Unzip file and save in the working directory (defaults to Documents folder)
  unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
  #Read watershed and named stream shapefiles into R
  wbdhu8 = st_read(file.path(scratch_dir, "Shape", "WBDHU8.shp"))
  nhdwaterbody = st_read(file.path(scratch_dir, "Shape", "NHDWaterbody.shp"))
  flowlines = st_zm(st_read(file.path(scratch_dir, "Shape", "NHDFlowline.shp")))

  #Subset flowlines
  #Note: keep named streams and unnamed ReachCode 18010208002394.
  #This is a channelized ditch connecting Johnson Creek to Crystal Creek
  named_streams = flowlines[(!is.na(flowlines$gnis_name)) | flowlines$reachcode == 18010208002394,]
  #And, name this reach to make it part of Johnson Creek
  named_streams$gnis_name[named_streams$reachcode == 18010208002394] = "Johnson Creek"

  #reproject
  watershed = st_transform(wbdhu8, st_crs(3310))
  nhdwaterbody = st_transform(nhdwaterbody, st_crs(3310))
  named_streams = st_transform(named_streams, st_crs(3310))

  # Write shapefiles to Box archive
  if(save_data_to_archive==T &
     !file.exists(file.path(dms_archive_dir, paste0(basin_name,"_watershed_huc8.shp")))){
  st_write(obj = watershed, dsn = dms_archive_dir, layer = paste(basin_name,"watershed_huc8", sep="_"), driver = "ESRI Shapefile")
  st_write(obj = nhdwaterbody, dsn = dms_archive_dir, layer = paste(basin_name,"waterbody_nhd", sep="_"), driver = "ESRI Shapefile")
  st_write(obj = named_streams, dsn = dms_archive_dir, layer = paste(basin_name,"named_streams_nhd", sep="_"), driver = "ESRI Shapefile")
  }
# }

#Delete zipped files
file.remove(file.path(scratch_dir, "NHD_H_18010208_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010207_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010205_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "Shape", list.files(file.path(scratch_dir, "Shape")))) #deletes files in unzipped "Shape" folder
unlink(file.path(scratch_dir, "Shape"),recursive = TRUE) #Removes an empty file


# _Klamath Basin boundary -------------------------------------------------
kb_huc6 = 180102
# Accessed from https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/HU4/Shape/
# klamath_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_1801_HU4_Shape.zip"
klamath_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/Shape/NHD_H_1801_HU4_Shape.zip"
zipname = strsplit(klamath_url, "/")[[1]][length(strsplit(klamath_url, "/")[[1]])]
#retrieve zip file from url
klamath_dl = GET(klamath_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
#unzip in the scratch work directory
unzip(file.path(scratch_dir,zipname), exdir = file.path(scratch_dir))#, list = TRUE)
# Read shapefile
basins_1801_all = st_read(file.path(scratch_dir, "Shape","WBDHU6.shp"))
#subset the Klamath basin and reproject to 3310
klamath = basins_1801_all[basins_1801_all$huc6==kb_huc6,]
klamath = st_transform(klamath, crs("+init=epsg:3310"))
# Write to Dropbox
if(save_data_to_archive==T &
   !file.exists(file.path(dms_archive_dir, "klamath_basin_boundary_2018.shp"))){
  st_write(obj = klamath, dsn = dms_archive_dir, layer = "klamath_basin_boundary_2018", driver = "ESRI Shapefile")
}
#Remove scratch work
file.remove(file.path(scratch_dir, zipname))
file.remove(file.path(scratch_dir, "Shape", list.files(file.path(scratch_dir, "Shape")))) #deletes files in unzipped "Shape" folder
unlink(file.path(scratch_dir, "Shape"),recursive = TRUE) #Removes an empty file

# USGS Flow gages  -------------------------------------------------------------------


#find daily surface water data in CA
pCode = c("00060") #daily avg streamflow
swCA_dl = readNWISdata(stateCd="CA", parameterCd=pCode,
                       service="site", seriesCatalogOutput=TRUE)
swCA = swCA_dl[swCA_dl$parm_cd %in% pCode,]

HUC8_nums = c(18010208) #, 18010207, 18010205) # scott, shasta, butte
sw_scott = swCA[swCA$huc_cd %in% HUC8_nums,]

sw_scott_sp = st_as_sf(sw_scott, coords = c("dec_long_va","dec_lat_va"),
                       crs = crs("+init=epsg:4326")) #assign WGS84 projection to coordinates
sw_scott_sp = st_transform(sw_scott_sp, crs("+init=epsg:3310"))
usgs_gauges = sw_scott_sp[(watershed),]

# DEM layer (topography) ------------------------------------------------------

# USGS DEMs 1/3 arc-second
# Accessed via https://viewer.nationalmap.gov/basic/#productSearch

# old URLs
# dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/IMG/n42w123.zip"
# dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/IMG/n42w124.zip"
# dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/n42w123/USGS_13_n42w123_20210624.tif"
# dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/n42w124/USGS_13_n42w124_20210624.tif"
# Current URL as of May 2024
dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/historical/n42w123/USGS_13_n42w123_20210623.tif"
dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/historical/n42w124/USGS_13_n42w124_20210623.tif"
dem_urls = c(dem123_url, dem124_url)

dem_url = dem_urls[1]
# Download from USGS site and write to the working drive (scratch_dir)
tif_name = strsplit(dem_url, "/")[[1]][length(strsplit(dem_url, "/")[[1]])]
if(!file.exists(file.path(scratch_dir,tif_name))){
  dem_dl = GET(dem_url, write_disk(file.path(scratch_dir,tif_name), overwrite = TRUE))
  }
dem123 = rast(file.path(scratch_dir, tif_name))

dem_url = dem_urls[2]
# Download from USGS site and write to the working drive (scratch_dir)
tif_name = strsplit(dem_url, "/")[[1]][length(strsplit(dem_url, "/")[[1]])]
if(!file.exists(file.path(scratch_dir,tif_name))){
  dem_dl = GET(dem_url, write_disk(file.path(scratch_dir,tif_name), overwrite = TRUE))
  }
dem124 = rast(file.path(scratch_dir, tif_name))

m <- merge(dem123,
           dem124) # getting some weird errors but it still runs the merge.
# plot(m)
dem_3310 = terra::project(x = m, y = crs("+init=epsg:3310"))

# Generate hillshade
wsh_10km = st_buffer(watershed, 1e4) # give it a buffer for figure backgrounds
dem_watershed=terra::crop(dem_3310, wsh_10km)
dem_shade=gray(0:100 / 100)


if(!file.exists(file.path(scratch_dir, "hillshade_cropped_raster.tif"))){
  #generate hillshade (for smaller maps)
  # hill_basin = hillShade(slope = terrain(dem_basin, "slope"), aspect = terrain(dem_basin, "aspect"))
  #generate cropped hillshade
  dem_watershed_slope = terra::terrain(dem_watershed, v = "slope", unit = "radians")
  dem_watershed_asp = terra::terrain(dem_watershed, v = "aspect", unit = "radians")
  hill_wsh = shade(slope = dem_watershed_slope, aspect = dem_watershed_asp,
                   angle = 45, direction = 0)
  terra::writeRaster(hill_wsh, filename = file.path(data_dir, "hillshade_cropped_raster.tif"), overwrite=T)

}



# American Indian Areas ----------------------------------------------------
tribal_url = "https://www2.census.gov/geo/tiger/TIGER2021/AIANNH/tl_2021_us_aiannh.zip"

zipname = strsplit(tribal_url, "/")[[1]][length(strsplit(tribal_url, "/")[[1]])]
tribal_dl = GET(tribal_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir))#, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
tribal_all = st_read(file.path(scratch_dir, "tl_2021_us_aiannh.shp"))
tribal_all = st_transform(tribal_all, crs("+init=epsg:3310"))
# county = get_postgis_query(siskiyou_spatial, "SELECT * FROM siskiyou_county_boundary", geom_name = "geom")
# requires klamath loading (earlier in this script)
tribal = tribal_all[st_as_sf(klamath),] # clip to klamath

#Copy to the box DMS archive
if(save_data_to_archive==T &
   !file.exists(file.path(dms_archive_dir, "am_indian_areas_klamath.shp"))){
  st_write(obj = tribal, dsn = dms_archive_dir, layer = "am_indian_areas_klamath", driver = "ESRI Shapefile", overwrite=T)
}
#Write to database
#remove files from scratch drive
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml", "shp.ea.iso.xml", "shp.iso.xml")
file.remove(file.path(scratch_dir,paste("tl_2021_us_aiannh", extension_list, sep = ".")))
file.remove(file.path(scratch_dir,zipname))


#_TABULAR DATA ------------------------------------------------------------


# Fort Jones USGS flow ----------------------------------------------------

# Retrieve fort jones gage data
fj_num = "11519500"
fj_flow = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
fj_flow = renameNWISColumns(fj_flow)
fj_flow$wy = year(fj_flow$Date); fj_flow$wy[month(fj_flow$Date) > 9] = fj_flow$wy[month(fj_flow$Date) > 9]+1

date_string = format(Sys.Date(), "%Y.%m.%d")
write.csv(fj_flow, paste0("fj flow ", date_string,".csv"), row.names = F, quote=F)

# NOAA NCDC ---------------------------------------------------------------

#Subfunctions
get_bbox_string = function(poly, return_half = 0){
  poly = st_transform(poly, crs("+init=epsg:4326")) #convert to WGS84)
  north = st_bbox(poly)$ymax; west = st_bbox(poly)$xmin; south = st_bbox(poly)$ymin; east = st_bbox(poly)$xmax
  bbox_poly = paste(c(south, west, north, east), collapse = ",")
  if(return_half>0){ ns_midpoint = south + 0.5*(north-south)}
  if(return_half == 1){bbox_poly = paste(c(ns_midpoint, west, north, east), collapse = ",")}
  if(return_half == 2){bbox_poly = paste(c(south, west, ns_midpoint, east), collapse = ",")}
  return(bbox_poly)
}

get_noaa_stations = function(cmk_token = "scKXkYaFjbbLtxmSnNYjWKBKXDvOCoeU", #received from NOAA on 2019-08-09: https://www.ncdc.noaa.gov/cdo-web/token
                             list_of_bboxes){

  #Initialize station table
  colnames_station_table = c("results.elevation","results.mindate",
                             "results.maxdate","results.latitude","results.name",

                                                        "results.datacoverage","results.id",
                             "results.elevationUnit","results.longitude")
  station_table = data.frame(matrix(NA, nrow = 0, ncol = 9))
  colnames(station_table) = colnames_station_table

  for(i in 1:length(list_of_bboxes)){
    bbox_string = list_of_bboxes[[i]]
    base_url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/"
    stations_query_url = paste0(base_url, "stations?extent=", bbox_string)
    header_token <- structure(cmk_token , names = "token")
    # header_limit <- structure(200 , names = "limit")
    stations_dl = GET(stations_query_url, httr::add_headers(header_token))#, header_limit))

    #Parse download
    stations_dl_unlisted = unlist(content(stations_dl, "parsed" ))
    metadata=stations_dl_unlisted[1:3]
    stations_dl_unlisted = stations_dl_unlisted[-(1:3)] # scrape off 3 metatata arguments
    stations_colnames = names(stations_dl_unlisted[1:9])
    noaa_stations = data.frame(matrix(stations_dl_unlisted, ncol = 9, byrow = T))
    colnames(noaa_stations) = stations_colnames

    station_table = rbind(station_table, noaa_stations)
  }
  #Eliminate duplicates
  # sum(duplicated(station_table))
  station_table = station_table[!duplicated(station_table),]
  #Split up results ID into dataset type and station ID
  results_id_split=matrix(unlist(strsplit(as.character(station_table$results.id), split=":")), ncol=2, byrow=T)
  station_table$results.type = results_id_split[,1]
  station_table$station.id = results_id_split[,2]

  #Make stations table spatial
  stations_sp = station_table
  stations_sp$results.latitude = as.numeric(as.character(stations_sp$results.latitude))
  stations_sp$results.longitude = as.numeric(as.character(stations_sp$results.longitude))

  stations_sp = st_as_sf(stations_sp,
                         coords = c("results.longitude", "results.latitude"),
                         crs = 4326)

  return(list(stations_sp, station_table))
}

get_noaa_data = function(station_list){
  # More info: https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily

  base_url = "https://www.ncei.noaa.gov/access/services/data/v1"
  dataset = "daily-summaries"
  # station_list = c("USC00041316","USC00043182","USC00042899", "USC00043614", "USC00049866", "US1CASK0005")
  stations = paste(station_list,collapse = ",") # Callahan, Ft Jones, Etna, Greenview, Yreka, Yreka NW
  start_date = "1800-01-01"
  end_date = Sys.Date()
  data_types = paste(c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"), collapse=",")

  noaa_url = paste0(base_url, "?dataset=", dataset,
                    "&stations=", stations,
                    "&startDate=", start_date,
                    "&endDate=", end_date,
                    "&dataTypes=", data_types,
                    "&format=csv",
                    "&includeAttributes=0&includeStationName=true&includeStationLocation=true",
                    "&units=metric")

  noaa_dl = GET(noaa_url)
  noaa = as.data.frame(content(noaa_dl, "parsed",
                               col_types = cols("STATION" = col_character(),
                                                "DATE" = col_date(format = ""),
                                                "PRCP" = col_double(),
                                                "SNOW" = col_double(),
                                                "SNWD" = col_double(),
                                                "TMAX" = col_double(),
                                                "TMIN" = col_double()) ))
  return(noaa)
}

archive_noaa_data = function(noaa_data){

  #Update the live tables, and make an archive datestamped copy
  #Archive
  noaa_archive_name = paste0("noaa_daily_data_ghcnd_",format(Sys.Date(), "%Y.%m.%d"),".csv" )
  if(save_data_to_archive==T &
     !file.exists(file.path(dms_archive_dir, noaa_archive_name))){
    write.csv(noaa_data, file.path(dms_archive_dir,noaa_archive_name))
  }
}


#Pull station list from noaa website
bbox_scott = get_bbox_string(watershed)
station_info = get_noaa_stations(list_of_bboxes = bbox_scott)


station_sp = station_info[[1]]; station_sp = st_transform(station_sp, CRS("+init=epsg:3310"))
station_table = station_info[[2]]
#Only include GHCND stations. These are historical daily records.
ghcnd_stations = station_table$station.id[station_table$results.type == "GHCND"]
noaa_updated_dataset = get_noaa_data(station_list = ghcnd_stations)

archive_noaa_data(noaa_data = noaa_updated_dataset) # only archives if save_data_to_archive == TRUE

noaa = noaa_updated_dataset
noaa_stations = station_table[station_table$results.type == "GHCND",]
noaa_station_sp = station_sp

rm(list = c("noaa_updated_dataset", "station_table", "station_sp")) # remove older variable names

# Notes: optional extra weather datasets (for scott; could be others in other basins)
# TMAX 	Maximum temperature
# TMIN 	Minimum temperature
# TOBS 	Temperature at the time of observation
# DAPR 	Number of days included in the multiday precipitation total (MDPR) 	1949-12-19 	2018-11-29
# MDPR 	Multiday precipitation total (use with DAPR and DWPR, if available) 	1949-12-19 	2018-11-29
# PRCP 	Precipitation #tenths of a mm
# SNOW 	Snowfall
# SNWD 	Snow depth
# WT01 	Fog, ice fog, or freezing fog (may include heavy fog)
# WT03 	Thunder
# WT04 	Ice pellets, sleet, snow pellets, or small hail"
# WT05 	Hail (may include small hail)
# WT06 	Glaze or rime
# WT08 	Smoke or haze
# WT09 	Blowing or drifting snow
# WT11 	High or damaging winds
# WT14 	Drizzle
# WT16  Rain (may include freezing rain, drizzle, and freezing drizzle)"



# Flow regimes -------------------------------------------------------------------------

# CDFW 2017 interim instream flows
cdfw_tab = read.csv(file.path(data_dir,"cdfw_2017_instream_flows.csv"))
colnames(cdfw_tab) = c("start_date_month","start_date_day","end_date_month","end_date_day","rec_flow_cfs")
# Forest Service water right
fs_tab = read.csv(file.path(data_dir,"USFS Scott Water Right.csv"))
colnames(fs_tab) = c("start_date_month","start_date_day","end_date_month","end_date_day","rec_flow_cfs")
# CDFW 2021 emergency drought minimum flows
cdfw_2021 = read.csv(file.path(data_dir, "cdfw_2021c_emergency_drought_flows.csv"))


# Fish data ---------------------------------------------------------------

spawners = read.csv(file.path(data_dir,"cdfw_2023a_tab4_Klamath and Scott Chinook Natural Spawner escapment.csv"))
smolt_per_fem = read.csv(file.path(data_dir, "cdfw_2023a_tab6_Coho_smolt_production_per_female_2008-2020.csv"))
chinook_abun = read.csv(file.path(data_dir,"cdfw_2023a_tab3_Chinook abundance estimates 2008-2022.csv"))
chinook_spawn_and_juv = read.csv(file.path(data_dir,"massie_2020_Scott River Chinook Adult_juv_data_BY_1999_2020.csv"))
coho_abun = read.csv(file.path(data_dir,"cdfw_2023a_fig18_Coho abundance estimates 2007-2022.csv"))
outmigs = read.csv(file.path(data_dir,"cdfw_2023a_tab5_Coho smolt outmigrant survival 2004-2019.csv"))
outmigs$conditions_year = (outmigs$Brood.Year + outmigs$Smolt.Year)/2 # take middle year
outmigs$Percent.smolt.survival = as.numeric(outmigs$Percent.smolt.survival)
redds = read.csv(file.path(data_dir, "rcd_2020_coho_spawning_surveys.csv")) # not updated in 2024

#Clean
# Clean colnames, remove commas, convert from characters to numbers
colnames(spawners) = c("year", "chinook_klamath", "chinook_scott")
spawners$chinook_klamath = as.numeric(gsub(pattern = ",", replacement = "", x = spawners$chinook_klamath))
spawners$chinook_scott = as.numeric(gsub(pattern = ",", replacement = "", x = spawners$chinook_scott))



# Calc. functional flow metrics -------------------------------------------------------------------

# # auto pull and process FJ gauge
# # ffc$step_one_functional_flow_results(gage_id=fj_num,
# #                                      token = ffc_token,
# #                                      output_folder = ffc_dir)
#
#
# ffc_dir = file.path(data_dir, "Calculated Functional Flows")
# library(ffcAPIClient)
# ffc_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJDbGFpcmUiLCJsYXN0TmFtZSI6IktvdWJhIiwiZW1haWwiOiJjbGFpcmUua291YmFAeWFsZS5lZHUiLCJyb2xlIjoiVVNFUiIsImlhdCI6MTczNzQ4MDEyMH0.yk1GcEq-jDHEnFxembVZ44NuCs4d2K-ehbBA81xk3BI"
# # record lat and long from NWIS
# fj_data = readNWISsite(fj_num)
#
#
# # ffc = FFCProcessor$new()
#
# all_files = list.files(svihm_results_dir)
# fj_flow_files = all_files[grep(pattern = "FJ outflow", x = all_files)]
#
# # begin for loop
# i=1
# fj_flow_file = fj_flow_files[i]
# scen_id = gsub(pattern = " FJ outflow.csv", replacement = "", x = fj_flow_file)
# fj_flow_i = read.csv(file.path(svihm_results_dir, fj_flow_file))
# fj_flow_i$Date=as.Date(fj_flow_i$Date)
# fj_input = data.frame(flow = fj_flow_i$Flow,
#                       # date = fj_flow_i$Date)
#                       date = format(x=fj_flow_i$Date, "%m/%d/%Y"))
#
# ffcAPIClient::evaluate_alteration(
#   timeseries_df = fj_input,
#   token = ffc_token,
#   plot_output_folder = ffc_dir,
#   # comid=yoursegmentcomid # REQUIRED OR specify lat/lon
#   # longitude = fj_data$dec_long_va,
#   # latitude = fj_data$dec_lat_va
#   longitude = -123.01503707, latitude = 41.64069017)

# # rename file with scenario ID





# copy to Graphics and Supplements folder as a supplemental table
file.copy(from = file.path(data_dir, "ScottR_FJ_wy1942_2024.05.06_annual_flow_result.csv"),
          to = file.path(graphics_dir, "Supplemental Table 1. Annual Scott River Functional Flows.csv"))



# Save workspace as .RData ------------------------------------------------

save.image(file = file.path(ms_dir, "manuscript_data.RData"))
