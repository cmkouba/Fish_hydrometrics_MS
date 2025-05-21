# Scott River Salmon MARSS


library(MARSS)
library(lubridate)
library(here)
library(funtimes)

# Directory ----------

data_dir = file.path(here(), "Data")
cfs_to_m3day = 1 / 35.3147 * 60*60*24 # 1 cfs / cubic ft per cubic m * seconds per day


# Subfunctions ------

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
  output_tab$coho_smolt_abun_est = as.numeric(outmigs$Smolt.point.Estimate[match(output_tab$smolt_year, outmigs$Smolt.Year)])
  output_tab$coho_smolt_per_fem = as.numeric(smolt_per_fem$Smolts_Produced_Per_Female[match(output_tab$smolt_year, smolt_per_fem$Smolt_Year)])
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
    output_tab[i, "d1_DS_Dur_WS"] = fflows$DS_Dur_WS[fflows$Year == brood_yr] # match up brood year to water year
    output_tab[i, "d1_DS_Tim"] = fflows$DS_Tim[fflows$Year == brood_yr] # match up brood year to water year
    output_tab[i, "d1_DS_Mag_50"] = fflows$DS_Mag_50[fflows$Year == brood_yr] # match up brood year to water year
    output_tab[i, "d1_DS_Mag_90"] = fflows$DS_Mag_90[fflows$Year == brood_yr] # match up brood year to water year

    ## f1 - FFs for fall rewetting transition during parents' spawning
    # if(n_BY==122){
    output_tab[i, "f1_FA_Mag"] = fflows$FA_Mag[fflows$Year == brood_yr + 1] # match up brood year to water year
    output_tab[i, "f1_FA_Tim"] = fflows$FA_Tim[fflows$Year == brood_yr + 1] # match up brood year to water year
    output_tab[i, "f1_FA_Dur"] = fflows$FA_Dur[fflows$Year == brood_yr + 1] # match up brood year to water year
    output_tab[i, "f1_FA_Dif_num"] = fflows$FA_Dif_num[fflows$Year == brood_yr + 1] # match up brood year to water year
    # }
    ## w1 - FFs for first wet season as egg and fry
    # if(n_RY>=365){
    output_tab[i, "w1_Wet_BFL_Dur"] = fflows$Wet_BFL_Dur[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Wet_BFL_Mag_10"] = fflows$Wet_BFL_Mag_10[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Wet_BFL_Mag_50"] = fflows$Wet_BFL_Mag_50[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Wet_Tim"] = fflows$Wet_Tim[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_10"] = fflows$Peak_10[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_5"] = fflows$Peak_5[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_2"] = fflows$Peak_2[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Dur_10"] = fflows$Peak_Dur_10[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Dur_5"] = fflows$Peak_Dur_5[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Dur_2"] = fflows$Peak_Dur_2[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Fre_10"] = fflows$Peak_Fre_10[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Fre_5"] = fflows$Peak_Fre_5[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Fre_2"] = fflows$Peak_Fre_2[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Tim_10"] = fflows$Peak_Tim_10[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Tim_5"] = fflows$Peak_Tim_5[fflows$Year == brood_yr + 1]
    output_tab[i, "w1_Peak_Tim_2"] = fflows$Peak_Tim_2[fflows$Year == brood_yr + 1]

    ## s1 - FFs for first spring season as juvenile fish
    output_tab[i, "s1_SP_Dur"] = fflows$SP_Dur[fflows$Year == brood_yr + 1]
    output_tab[i, "s1_SP_Mag"] = fflows$SP_Mag[fflows$Year == brood_yr + 1]
    output_tab[i, "s1_SP_Tim"] = fflows$SP_Tim[fflows$Year == brood_yr + 1]
    output_tab[i, "s1_SP_ROC"] = fflows$SP_ROC[fflows$Year == brood_yr + 1]
    output_tab[i, "s1_SP_ROC_Max"] = fflows$SP_ROC_Max[fflows$Year == brood_yr + 1]

    # if(is.element(el=brood_yr+2, set = fflows$Year)){
    ## d2 - FFs for dry season as juvenile fish
    output_tab[i, "d2_DS_Dur_WS"] = fflows$DS_Dur_WS[fflows$Year == brood_yr +1] # match up brood year to water year
    output_tab[i, "d2_DS_Tim"] = fflows$DS_Tim[fflows$Year == brood_yr +1] # match up brood year to water year
    output_tab[i, "d2_DS_Mag_50"] = fflows$DS_Mag_50[fflows$Year == brood_yr +1] # match up brood year to water year
    output_tab[i, "d2_DS_Mag_90"] = fflows$DS_Mag_90[fflows$Year == brood_yr +1] # match up brood year to water year
    # }
    # FFs for fall pulse as juvenile fish
    output_tab[i, "f2_FA_Mag"] = fflows$FA_Mag[fflows$Year == brood_yr + 2] # match up brood year to water year
    output_tab[i, "f2_FA_Tim"] = fflows$FA_Tim[fflows$Year == brood_yr + 2] # match up brood year to water year
    output_tab[i, "f2_FA_Dur"] = fflows$FA_Dur[fflows$Year == brood_yr + 2] # match up brood year to water year
    output_tab[i, "f2_FA_Dif_num"] = fflows$FA_Dif_num[fflows$Year == brood_yr + 2] # match up brood year to water year

    # }
    # w2 - FFs during second winter as overwintering juveniles
    output_tab[i, "w2_Wet_BFL_Dur"] = fflows$Wet_BFL_Dur[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Wet_BFL_Mag_10"] = fflows$Wet_BFL_Mag_10[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Wet_BFL_Mag_50"] = fflows$Wet_BFL_Mag_50[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Wet_Tim"] = fflows$Wet_Tim[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_10"] = fflows$Peak_10[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_5"] = fflows$Peak_5[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_2"] = fflows$Peak_2[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Dur_10"] = fflows$Peak_Dur_10[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Dur_5"] = fflows$Peak_Dur_5[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Dur_2"] = fflows$Peak_Dur_2[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Fre_10"] = fflows$Peak_Fre_10[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Fre_5"] = fflows$Peak_Fre_5[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Fre_2"] = fflows$Peak_Fre_2[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Tim_10"] = fflows$Peak_Tim_10[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Tim_5"] = fflows$Peak_Tim_5[fflows$Year == brood_yr + 2]
    output_tab[i, "w2_Peak_Tim_2"] = fflows$Peak_Tim_2[fflows$Year == brood_yr + 2]

    ## s2 - FFs for second spring season as outmigrating smolt
    output_tab[i, "s2_SP_Dur"] = fflows$SP_Dur[fflows$Year == brood_yr + 2]
    output_tab[i, "s2_SP_Mag"] = fflows$SP_Mag[fflows$Year == brood_yr + 2]
    output_tab[i, "s2_SP_Tim"] = fflows$SP_Tim[fflows$Year == brood_yr + 2]
    output_tab[i, "s2_SP_ROC"] = fflows$SP_ROC[fflows$Year == brood_yr + 2]
    output_tab[i, "s2_SP_ROC_Max"] = fflows$SP_ROC_Max[fflows$Year == brood_yr + 2]

    output_tab[i, "wy1_Mean_Ann_Flow"] = fflows$Mean_Ann_Flow[fflows$Year == brood_yr + 1]
    output_tab[i, "wy2_Mean_Ann_Flow"] = fflows$Mean_Ann_Flow[fflows$Year == brood_yr + 2]
    output_tab[i, "wy1_WY_Cat"] = fflows$WY_Cat[fflows$Year == brood_yr + 1]
    output_tab[i, "wy2_WY_Cat"] = fflows$WY_Cat[fflows$Year == brood_yr + 2]

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


y_val_label_tab = function(){
  # calculate number of predictors for text below

  all_szns = paste(c("d1", "f1", "w1", "s1", "d2", "f2", "w2", "s2"), collapse =", ")
  y1_szns = paste(c("d1", "f1", "w1", "s1"), collapse = ", ")
  spawn_szns = paste(c("d1", "f1", "w1"), collapse = ", ")
  # spawn_szns = paste(c("d1", "f1"), collapse = ", ")

  y_val_label_tab = data.frame(y_val = c("coho_smolt_per_fem",
                                         "chinook_juv_per_adult",
                                         "coho_spawner_abundance",
                                         "coho_redds_in_brood",
                                         "coho_smolt_abun_est",
                                         "chinook_spawner_abundance",
                                         "chinook_juvenile_abundance"
  ),
  y_val_title = c("coho spf","Chinook jpa",
                  "coho escapement",
                  "coho redd abundace",
                  "est. coho smolt abundance",
                  "Chinook escapement",
                  "Chinook juv. abundance"
  ),
  y_val_label = c("Coho smolt per fem. spawner",
                  "Chinook juv. per adult",
                  "Num. coho spawners (escapement)",
                  "Num. obs. coho redds",
                  "Est. num. coho smolt",
                  "Num. Chinook spawners (escapement)",
                  "Num. Chinook juveniles"),
  influencing_seasons = c(all_szns,
                          y1_szns,
                          spawn_szns,
                          spawn_szns,
                          all_szns,
                          spawn_szns,
                          y1_szns))
  return(y_val_label_tab)
}

zscore_column_na_rm = function(x){
  mean_x = mean(x, na.rm=T)
  sd_x = sd(x, na.rm=T)
  return((x - mean_x) / sd_x)
}



# MARSS subfunctions ------------------------------------------------------

## The code in the functions called below follows the R script
## Baruch_2023_Putah_Creek_MARSS.R, published in support of:
## Baruch et al., 2024. "Mimicking Functional Elements of the Natural Flow Regime
## Promotes Native Fish Recovery in a Regulated River." https://doi.org/10.1002/eap.3013.


get_obs_data_for_MARSS = function(metrics_tab,
                                  y_val_names = c("coho_smolt_abun_est","chinook_juvenile_abundance")){

  y_obs = t(metrics_tab[,y_val_names])
  colnames(y_obs) = metrics_tab$brood_year
  rownames(y_obs) = y_val_names

  return(y_obs)
}

get_model_fit_for_MARSS = function(y_obs_tab, method = "kem"){
  species = rownames(y_obs_tab)
  nspp = length(species)

  # reduce observation data to years with observations in the selected metrics
  how_many_na = apply(X = y_obs_tab, MARGIN = 2, function(x){sum(is.na(x))})
  # currently assumes 4 missing values only for end-years outside a continuous string of years with < 4 NAs
  y_obs = y_obs_tab[ , how_many_na < nspp]

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

  mod$Q = "diagonal and equal" # single process error across species and replicates
  mod$U = "zero" # No drift
  mod$B = "identity" # No B
  mod$Z = Z
  mod$R = "diagonal and equal"
  mod$A = "zero"

  #### DS_Dur_WS

  flow_metric_ct = "w1_Wet_BFL_Mag_50"
  covar_ct_raw = t(metrics_tab[how_many_na < nspp,
                               flow_metric_ct])
  rownames(covar_ct_raw) = flow_metric_ct
  # <- PuFF_Obs %>%
  #   filter(Year > 1992) %>% #Include flow data spanning first to last year
  #   select(DS_Dur_WS) %>%
  #   t() #Transpose
  covar_ct <- zscore(covar_ct_raw) # z-score to standardize

  #Create C matrix: 1 column b/c one variable, specify if species abundance or normalized
  C_ct <- matrix("ScottR")#matrix(c("abundance","abundance","normalized","normalized"), ncol = 1, nrow = nspp)
  # C_ct <- matrix(rep("ScottR"), ncol = 1, nrow = nspp)

  ## Now fit a MARSS model
  mod_ct = mod
  mod_ct$C = C_ct # Abundance and normalized data be affected by the covariate differently
  mod_ct$c = covar_ct # Covariate data


  #### U = zero, B = identity, Q = equal
  # Fit the MARSS model
  # control$trace = 1
  mod_ct.fit = MARSS(y = y_obs, model=mod_ct, method = method)#, method="BFGS")

  return(mod_ct.fit)
  # mod_ct.CI <- MARSSparamCIs(mod_ct.fit)
  #
  # # what if we just start with the basics?
  #
  # kemfit2 = MARSS(y_obs, model = list(
  #   Z = matrix(1, 4, 1),
  #   R = "diagonal and equal"))
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


get_max_continuous_ts = function(x_raw, years){
  n = length(x_raw)
  missing = is.na(x_raw)
  change_missing_to_present = missing[1:(n-1)] & !(missing[2:n])
  change_present_to_missing = !missing[1:(n-1)] & (missing[2:n])
  mtp_i = which(change_missing_to_present)
  ptm_i = which(change_present_to_missing)
  both_0 = (length(ptm_i)==0 & length(mtp_i)==0)
  # if(length(ptm_i) < length(mtp_i) | both_0 ){ptm_i = c(ptm_i,n)}
  # if(length(mtp_i) < length(ptm_i) | both_0){mtp_i = c(0,mtp_i)}
  if(!is.na(x_raw[n])){ptm_i = c(ptm_i,n)}
  if(!is.na(x_raw[1])){mtp_i = c(0,mtp_i)}
  cont_tab = data.frame(miss_to_pres_i = mtp_i,
                        pres_to_miss_i = ptm_i,
                        length_cont = NA)
  cont_tab$length_cont = cont_tab$pres_to_miss_i - cont_tab$miss_to_pres_i

  max_length_i = which.max(cont_tab$length_cont)
  max_cont_start = cont_tab$miss_to_pres_i[max_length_i] + 1
  max_cont_end = cont_tab$pres_to_miss_i[max_length_i]

  return(list(x_cont = x_raw[max_cont_start:max_cont_end],
              cont_yrs = years[max_cont_start:max_cont_end]))
}

# Read in data ------------------------------------------------------------



# or just read in Flow Metrics by Brood Year Table 2A?

fj_flow = read.csv(file.path(data_dir, "fj flow 2024.05.06.csv"))
spawners = read.csv(file.path(data_dir,"cdfw_2023a_tab4_Klamath and Scott Chinook Natural Spawner escapment.csv"))
smolt_per_fem = read.csv(file.path(data_dir, "cdfw_2023a_tab6_Coho_smolt_production_per_female_2008-2020.csv"))
chinook_abun = read.csv(file.path(data_dir,"cdfw_2023a_tab3_Chinook abundance estimates 2008-2022.csv"))
chinook_spawn_and_juv = read.csv(file.path(data_dir,"massie_2020_Scott River Chinook Adult_juv_data_BY_1999_2020.csv"))
coho_abun = read.csv(file.path(data_dir,"cdfw_2023a_fig18_Coho abundance estimates 2007-2022.csv"))
outmigs = read.csv(file.path(data_dir,"cdfw_2023a_tab5_Coho smolt outmigrant survival 2004-2019.csv"))
outmigs$conditions_year = (outmigs$Brood.Year + outmigs$Smolt.Year)/2 # take middle year
outmigs$Percent.smolt.survival = as.numeric(outmigs$Percent.smolt.survival)
redds = read.csv(file.path(data_dir, "rcd_2020_coho_spawning_surveys.csv")) # not updated in 2024

fflows = read.csv(file.path(data_dir, "ScottR_FJ_wy1942_2025.01.21_annual_Flashy_Calc.csv"))



# Align data by cohort in metrics_tab ------------------------------------------

brood_year_hydro_tab = tabulate_hydro_by_affected_brood_year(brood_years = 1999:2022,
                                                             smolt_years = 2001:2024)
thresh_for_corr_fig = c(20,40,120)

metrics_tab = calc_metrics_hydro_by_affected_brood_year(
  hydro_by_brood_year = brood_year_hydro_tab,
  thresholds = thresh_for_corr_fig)

# Clean metrics tab:
# 1) factorize water year category
metrics_tab$wy1_WY_Cat = as.numeric(factor(metrics_tab$wy1_WY_Cat,
                                           levels = c("dry year", "mod year", "wet year")))
metrics_tab$wy2_WY_Cat = as.numeric(factor(metrics_tab$wy2_WY_Cat,
                                           levels = c("dry year", "mod year", "wet year")))

# Eco response obs. info table
yvlt = y_val_label_tab()
non_preds = c("brood_year", "smolt_year", yvlt$y_val)
num_predictors = length(colnames(metrics_tab))- length(non_preds)
preds_all = colnames(metrics_tab[,!colnames(metrics_tab) %in% non_preds])


# Transform data ---------------------------------------------------------------

# log-transform ecological observations
log_transform_eco_metrics = T # Log and then z-score??
if(log_transform_eco_metrics ==T){
  metrics_tab[,yvlt$y_val] = log10(metrics_tab[,yvlt$y_val])
}

# zscore ecological observations
zscore_eco_metrics = F # Log and then z-score??
if(zscore_eco_metrics ==T){
  zscore_these = yvlt$y_val

  metrics_mean = apply(X = metrics_tab[,zscore_these], MARGIN = 2, FUN = mean, na.rm=T)
  metrics_sd = apply(X = metrics_tab[,zscore_these], MARGIN = 2, FUN = sd, na.rm=T)
  metrics_tab[,zscore_these] = apply(X = metrics_tab[,zscore_these],
                                     MARGIN = 2, FUN = zscore_column_na_rm)
}


# Zscore flow metrics
zscore_flow_metrics = T
if(zscore_flow_metrics == T){
  zscore_these = !(colnames(metrics_tab) %in% non_preds)
  metrics_mean = apply(X = metrics_tab[,zscore_these], MARGIN = 2, FUN = mean, na.rm=T)
  metrics_sd = apply(X = metrics_tab[,zscore_these], MARGIN = 2, FUN = sd, na.rm=T)
  metrics_tab[,zscore_these] = apply(X = metrics_tab[,zscore_these],
                                     MARGIN = 2, FUN = zscore_column_na_rm)
}



# Autoregression ----------------------------------------------------------

# # How autocorrelated are each ecological metric?
# par(mfrow = c(3,2))
# for(y_val in yvlt$y_val){
#   max_cont = get_max_continuous_ts(x_raw = metrics_tab[,y_val],
#                         years = metrics_tab$brood_year)
#   x_all = max_cont$x_cont
#
#   ## Plot x vs lagged x
#   # par(mfrow = c(3,2))
#   # for(p in 1:6){
#   #   x_t = x_all[(1+p):length(x_all)]
#   #   x_t_1 = x_all[1:(length(x_all)-p)]
#   #
#   #   R_p = cor(x_t, x_t_1)
#   #
#   #   plot(x_t, x_t_1, main = paste0(y_val, ", Lag ",p, ", R ", round(R_p, 2)))
#   #
#   # }
#
#   acf(x = x_all, main = y_val)
# }

# How autocorrelated are hydro metrics?

# brood_year_hydro_tab_long = tabulate_hydro_by_affected_brood_year(brood_years = 1942:2022,
#                                                              smolt_years = 1944:2024)
# thresh_for_corr_fig = c(20,40,120)
#
# metrics_tab_long = calc_metrics_hydro_by_affected_brood_year(
#   hydro_by_brood_year = brood_year_hydro_tab,
#   thresholds = thresh_for_corr_fig)
#
# par(mfrow = c(3,2))
#
# for(pred in preds_all){
#   if(sum(is.na(metrics_tab_long[,pred])) != nrow(metrics_tab_long)){
#     max_cont = get_max_continuous_ts(x_raw = metrics_tab_long[,pred],
#                                      years = metrics_tab_long$brood_year)
#     x_all = max_cont$x_cont
#     acf(x = x_all, main = pred)
#
#   }
# }



# Specify MARSS model ------------------------------------------------------------------


## The code in the functions called below follows the R script
## Baruch_2023_Putah_Creek_MARSS.R, published in support of:
## Baruch et al., 2024. "Mimicking Functional Elements of the Natural Flow Regime
## Promotes Native Fish Recovery in a Regulated River." https://doi.org/10.1002/eap.3013.

# for z-scored x metrics, for non-transformed y metrics, on 5/19/2025
screened_cols = c("brood_year","smolt_year",
                  "coho_smolt_per_fem", "chinook_juv_per_adult",
                  "coho_spawner_abundance", "coho_redds_in_brood",
                  "coho_smolt_abun_est", "chinook_spawner_abundance",
                  "chinook_juvenile_abundance", "f2_recon_120",
                  "d1_DS_Mag_50", "d1_DS_Mag_90", "f1_FA_Dur",
                  "f1_FA_Tim", "f1_FA_Dif_num", "w1_Wet_BFL_Dur",
                  "w1_Wet_BFL_Mag_50", "s1_SP_ROC", "s1_SP_ROC_Max",
                  "f2_FA_Dur", "f2_FA_Dif_num", "w2_Wet_BFL_Mag_50",
                  "w2_Wet_Tim", "s2_SP_ROC", "s2_SP_ROC_Max",
                  "s2_SP_Tim", "f1_recon_120")
preds_screened = screened_cols[!(screened_cols %in% non_preds)]
metrics_tab_screened = metrics_tab[,screened_cols]

preds_season_list = strsplit(x = preds_screened, split = "_")
preds_season = unlist(lapply(X = preds_season_list, function(x){x[1]}))

aicc_tab = data.frame(matrix(data=NA, nrow=length(preds_screened), ncol = length(yvlt$y_val)))
coef_tab = data.frame(matrix(data=NA, nrow=length(preds_screened), ncol = length(yvlt$y_val)))

colnames(aicc_tab)=yvlt$y_val; rownames(aicc_tab)=preds_screened
colnames(coef_tab)=yvlt$y_val; rownames(coef_tab)=preds_screened
list_of_mods = list()


calc_models_single_covar=function(){
  for(i in 1:nrow(yvlt)){
    y_val_i = yvlt$y_val[i]
    y_seasons = unlist(strsplit(yvlt$influencing_seasons[i],", "))
    preds_for_y = preds_screened[preds_season %in% y_seasons]

    for(j in 1:length(preds_for_y)){ # CHANGE TO PREDS_SCREENED. TAKES A LONG TIME.
      pred_j = preds_for_y[j]
      if(sum(is.na(metrics_tab[,pred_j]))<1){
        print(paste(y_val_i, "on", pred_j))
        y_obs_tab = get_obs_data_for_MARSS(metrics_tab = metrics_tab,
                                           y_val_names = y_val_i)
        marss_mod = get_single_cov_model_fit_for_MARSS(y_obs_tab = y_obs_tab,
                                                       pred = pred_j
                                                       #, method = "BFGS"
        )
        list_of_mods[[length(list_of_mods)+1]] = marss_mod
        names(list_of_mods)[length(list_of_mods)] = paste0(y_val_i, "__", pred_j)
        print("")
      }
      # par(mfrow = c(4,2))
      # plot(marss_mod)
      aicc_tab[pred_j, y_val_i] = marss_mod$AICc
      coef_tab[pred_j, y_val_i] = marss_mod$coef["C.ScottR"]

    }
  }

  if(log_transform_eco_metrics == T & zscore_flow_metrics == T){aic_tab_file = "AIC tab_log eco_zscore flow.csv"}
  if(log_transform_eco_metrics == F & zscore_flow_metrics == T){aic_tab_file = "AIC tab_straight eco_zscore flow.csv"}
  # saveRDS(object = list_of_mods, file = "MARSS model objects.RDS")

  aicc_tab$pred = row.names(aicc_tab)
  coef_tab$pred = row.names(coef_tab)
  for(y_val in yvlt$y_val){
    # view predictors in order of AICc value (smallest first)
    results_y = data.frame(pred = aicc_tab[order(aicc_tab[,y_val]), "pred"])
    results_y$aicc = round(aicc_tab[order(aicc_tab[, y_val]), y_val], 2)
    results_y$coef = round(coef_tab[order(aicc_tab[,y_val]), y_val], 3)

    # aicc_y[,y_val] = round(aicc_y[,y_val], 2)
    # coef_y = coef_tab[order(aicc_tab[,y_val]),c("pred",y_val)]
    # coef_y[,y_val] = round(coef_y[,y_val], 3)
    print(y_val)
    print(head(results_y))

    print("")
  }
}


calc_models_flow_and_spawn=function(){

  smolts_is = which(yvlt$y_val %in% c("coho_smolt_abun_est", "chinook_juvenile_abundance"))
  spawn_is =  which(yvlt$y_val %in% c("coho_spawner_abundance", "chinook_spawner_abundance"))
  for(i in 1:2){
    smolts_i = smolts_is[i]
    spawn_i = spawn_is[i]
    y_val_i = yvlt$y_val[smolts_i]
    y_val_spawn = yvlt$y_val[spawn_i]
    y_seasons = unlist(strsplit(yvlt$influencing_seasons[i],", "))
    preds_for_y = preds_screened[preds_season %in% y_seasons]

    for(j in 1:length(preds_for_y)){
      pred_j = preds_for_y[j]
      if(sum(is.na(metrics_tab[,pred_j]))<1){
        y_obs_tab = get_obs_data_for_MARSS(metrics_tab = metrics_tab,
                                           y_val_names = y_val_i)
        marss_mod = get_flow_and_spawn_cov_model_fit_for_MARSS(y_obs_tab = y_obs_tab,
                                                               y_spawn_colname = y_val_spawn,
                                                               pred = pred_j)
        list_of_mods[[length(list_of_mods)+1]] = marss_mod
        names(list_of_mods)[length(list_of_mods)] = paste0(y_val_i, "__", pred_j)
      }
      print(paste(y_val_i, "on", pred_j))
      par(mfrow = c(4,2))
      # plot(marss_mod)
      aicc_tab[pred_j, y_val_i] = marss_mod$AICc
      coef_tab_flow[pred_j, y_val_i] = marss_mod$coef["C.ScottR"]
      coef_tab_spawn[pred_j, y_val_i] = marss_mod$coef["C.Spawners"]

    }
  }

  if(log_transform_eco_metrics == T & zscore_flow_metrics == T){aic_tab_file = "AIC tab_log eco_zscore flow.csv"}
  if(log_transform_eco_metrics == F & zscore_flow_metrics == T){aic_tab_file = "AIC tab_straight eco_zscore flow.csv"}
  # saveRDS(object = list_of_mods, file = "MARSS model objects.RDS")

  aicc_tab$pred = row.names(aicc_tab)
  coef_tab_flow$pred = row.names(coef_tab_flow)  # CURRENTLY HERE
  coef_tab_spawn$pred = row.names(coef_tab_spawn)

  for(i in 1:2){
    y_val = yvlt$y_val[smolts_is[i]]
    # view predictors in order of AICc value (smallest first)
    results_y = data.frame(pred = aicc_tab[order(aicc_tab[,y_val]), "pred"])
    results_y$aicc = round(aicc_tab[order(aicc_tab[, y_val]), y_val], 2)
    results_y$coef = round(coef_tab[order(aicc_tab[,y_val]), y_val], 3)

    # aicc_y[,y_val] = round(aicc_y[,y_val], 2)
    # coef_y = coef_tab[order(aicc_tab[,y_val]),c("pred",y_val)]
    # coef_y[,y_val] = round(coef_y[,y_val], 3)
    print(y_val)
    print(head(results_y))

    print("")
  }
}


#Select ecological response variable
# y_obs_tab_ch = get_obs_data_for_MARSS(metrics_tab = metrics_tab,
#                                    y_val_names = y_val_chinook)
#
# mod_ch = get_model_fit_for_MARSS(y_obs_tab = y_obs_tab_ch, method = "BFGS")

# View results -----------------------------------------------------------------
pdf(file = file.path(data_dir, paste0("MARSS ", y_val_coho,".pdf")),
    height = 6, width = 6)
plot(mod_co)
dev.off()

pdf(file = file.path(data_dir, paste0("MARSS ", y_val_chinook,".pdf")),
    height = 6, width = 6)
plot(mod_ch)
dev.off()


# Interpret results -----

# Do these indicate an extremely poor fit?

# Which predictors are important for each species?

# How much variation in fish outcomes can be explained by flow?
