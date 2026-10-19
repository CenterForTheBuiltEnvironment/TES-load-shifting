library(tidyverse)
library(magrittr)
library(lubridate)

source("code/import_data.R")
source("code/basic_functions.R")
source("code/TES_functions.R")

loads_to_site_energy = function(equip_scenario, df, detail=T){
  # TLDR: converts the measured loads to site energy consumption 
  #       (typ. electricity, also gas) based on elements of provided scenario
  # args :
  #   equip_scenario  : a single row of a tibble with columns describing the
  #   various elements of that scenario (e.g. boiler model, AWHP model, etc.)
  #   df : ~annual hourly timeseries containing datetime, outdoor drybulb (oa_t), 
  #   heating load (hhw) and cooling load (chw) data for the building/system
  #   detail: logical, controls level of output detail
  # 
  # Assumptions:
  # Heating loads: 
  # first served by WWHP in heat recovery mode (if specified in scenario) 
  # within the specified equipment's capacity range, 
  # followed by AWHP in  # heating mode (if specified) within its capacity range,
  # AWHp capacity either directly specified or sized based on highest # of AWHP
  # required to meet remaining HHW load under outdoor temp conditions 
  # when AWHP can still generate hot water
  # followed by a gas boiler (if specified), 
  # with remaining loads served by electric resistance.
  # 
  # Cooling loads:
  # first served by WWHP in heat recovery mode (if specified in scenario),
  # then by an AWHP in cooling mode (if specified) within its capacity range,
  # with any remaining loads served by an fixed COP chiller (unlimited capacity).
  # 
  # returns
  # timeseries dataframe with results and associated equipment scenario ID
  #
  # todos:
  # model wwhp for cooling only (i.e. generic water cooled chiller instead of fixed COP of 5)
  # add capability to model exhaust air heat recovery for specified cfm at 75F
  # add water consumption for tower water?
  # add utility rates for operating cost est?
  # sizing AWHP based on cooling needs?
  # correctly handle NA values for both loads and OAT. currently
  
  s = equip_scenario
  
  df %<>% mutate(hhw_remainder = hhw, 
               chw_remainder = chw, 
               hr_hhw = NA,
               hr_chw=NA,
               simult_h=NA,
               hr_cop_h=NA,
               awhp_num = NA,
               awhp_hhw=NA,
               awhp_cop_h=NA,
               awhp_cap_h=NA,
               elec = 0,
               elec_hr=0,
               elec_awhp_h = 0,
               elec_awhp_c = 0,
               elec_chiller = 0,
               elec_res = 0,
               gas = 0,
               TES_charge = NA,
               testing = NA
               )
  
  # putting here for now bc needed in tes also, update w checks later
  # extract awhp performance characteristics
  # heating
  if (!is.na(s$awhp_h)){
    awhp_h_curve = awhp_curves %>% filter(awhp_id == s$awhp_h) %>% drop_na(oa_t,cap_h,cop_h) %>% select(oa_t, cop_h,cap_h)
    df%<>%mutate(awhp_cop_h = interp1_zero(awhp_h_curve$oa_t, awhp_h_curve$cop_h, oa_t))  
    df%<>%mutate(awhp_cap_h = interp1_zero(awhp_h_curve$oa_t, awhp_h_curve$cap_h, oa_t)) 
    # cooling
    awhp_c_curve = awhp_curves %>% filter(awhp_id == s$awhp_c) %>% drop_na(oa_t,cap_c,cop_c) %>% select(oa_t, cop_c,cap_c)
    df%<>%mutate(awhp_cop_c = interp1_zero(awhp_c_curve$oa_t, awhp_c_curve$cop_c, oa_t)) 
    df%<>%mutate(awhp_cap_c = interp1_zero(awhp_c_curve$oa_t, awhp_c_curve$cap_c, oa_t))
  }

  if (!is.na(s$hr_wwhp)){
    bldg = df$building[1]
    hr_wwhp_bldg = bldgs |> filter(bldg_id == bldg) |> pull(hr_wwhp)

    # extract wwhp performance characteristics
    hr_plr_curve = hr_wwhp_curves %>% 
      filter(hr_wwhp_id == hr_wwhp_bldg) %>%  # s$hr_wwhp
      # note using diff for buildings does mess up the refrigerant inventory calc, fix if needed
      drop_na(cap_h,cop_h) %>% 
      select(cop_h, cap_h) %>% 
      arrange(cap_h)
    hr_plr_curve$cap_h_to_cap_c = (1-(1/hr_plr_curve$cop_h)) #conversion factor from getting cooling capacity from heating_capacity
    hr_plr_curve$cop_c = cop_h_to_cop_c(hr_plr_curve$cop_h)
    hr_plr_curve$cap_c = hr_plr_curve$cap_h * hr_plr_curve$cap_h_to_cap_c #calculate cooling capacity from heating_capacity
    
    least_waste_heat = hr_plr_curve %>% filter(cop_h == max(cop_h))
    max_cap_h = hr_plr_curve %>% filter(cap_h == max(cap_h)) %>% pull(cap_h)
    min_cap_h = hr_plr_curve %>% filter(cap_h == min(cap_h)) %>% pull(cap_h)
    df %<>% mutate(hhw_from_cooling = chw / least_waste_heat$cap_h_to_cap_c)
    df %<>% mutate(chw_from_heating = hhw * least_waste_heat$cap_h_to_cap_c)
    df %<>% mutate(hr_min_cap_h = min_cap_h)
    df %<>% mutate(hr_max_cap_h = max_cap_h)
    df %<>% mutate(hr_min_cap_c = min(hr_plr_curve$cap_c))
    df %<>% mutate(hr_max_cap_c = max(hr_plr_curve$cap_c))
  }

  # lead with TES
  if (!is.na(s$tes)){
    if (s$tes == "hhw") {
      cap_var = "awhp_cap_h"
      curve = awhp_h_curve
    } else if (s$tes  == "chw") {
      cap_var = "awhp_cap_c"
      curve = awhp_c_curve
    }
    tot_awhp_cap = str_c(cap_var, "_total")
    loop_remainder = str_c(s$tes, "_remainder")
    equip_sizes = equip_sizing(df, loop_remainder, s$tes_sizing, s$tes_control, curve)
    df %<>% mutate(hp_num_tes = equip_sizes$hp_num)

    # test = pull(df, testing)
    if (!is.na(s$hr_wwhp)){ # heat recovery logic - hhw tank only atm
      if (str_detect(s$tes, "hhw")) {
        hr_loop = "hr_hhw"
        recoverable = "hhw_from_cooling"
        max_cap = "hr_max_cap_h"
        min_cap = "hr_min_cap_h"
      } else if (str_detect(s$tes, "chw")) {
        hr_loop = "hr_chw"
        recoverable = "chw_from_heating"
        max_cap = "hr_max_cap_c"
        min_cap = "hr_min_cap_c"
      }
      hr_calc = df[[hr_loop]]
      hr_calc[1] = 0
      TES_charge_calc = pull(df, TES_charge)
      TES_charge_calc[1] = equip_sizes$TES

      for (t in 2:nrow(df)){
        avail_charge_delta = equip_sizes$TES - TES_charge_calc[t - 1]
        hr_disabled = min(df[[recoverable]][t], avail_charge_delta + df[[s$tes]][t]) < df[[min_cap]][t]

        hr_calc[t] = ifelse(hr_disabled, 0,
          min(
            df[[recoverable]][t], # limited by max heat recoverable
            avail_charge_delta + df[[s$tes]][t], # limited by available storage + load
            df[[max_cap]][t] # limited by max capacity
          )
        )

        charge_delta = min(
          avail_charge_delta,
          max(
            hr_calc[t] - df[[s$tes]][t], 
            -TES_charge_calc[t - 1] # to limit discharge to prev hour charge
          )
        )
        TES_charge_calc[t] = charge_delta + TES_charge_calc[t - 1] 
      }
      df %<>% mutate(TES_charge = TES_charge_calc)
      
      if (str_detect(s$tes, "hhw")) {
        df %<>% mutate(hr_hhw = hr_calc)
      } else if (str_detect(s$tes, "chw")) {
        df %<>% mutate(hr_chw = hr_calc)
      }

    } else { # AWHP-only logic, OAT-based reset to maintain min capacity
      df %<>% mutate(!!tot_awhp_cap := .data[[cap_var]] * equip_sizes$hp_num)
      # print(s$desc)
      # print(equip_sizes)

      if (str_detect(s$tes_control, "hp_priority")) {
        df %<>% mutate(operate_hp_tes = TRUE, hp_tes = .data[[tot_awhp_cap]])

      } else { # storage priority

        if (str_detect(s$tes_control, "emissionsXeff")) {
          cop_var = efficiency_var(s$tes)
          df = df |>
            mutate(emissions_1W = elec_emissions_rate * 1 / .data[[cop_var]])
          control_var = "emissions_1W"

        } else if (str_detect(s$tes_control, "emissions")) {
          control_var = "elec_emissions_rate"

        } else if (str_detect(s$tes_control, "load")) {
          control_var = s$tes
        }

        if (!(str_detect(s$tes_control, "predictive"))) {
          lagged_control_var = str_c(control_var, "_lagged")
          lagged_df = df |>
            mutate(datetime = datetime + hours(24)) |>
            select(building, datetime, .data[[control_var]]) |>
            rename(!!lagged_control_var := !!control_var)
          df = df |> ungroup() |>
            left_join(lagged_df, by = c("building", "datetime"))
        } else {
          lagged_control_var = control_var
        }

        df = df |> 
          hp_tes_operation(loop_remainder, s$tes_control, lagged_control_var, tot_awhp_cap, equip_sizes)
          
        if (!(str_detect(s$tes_control, "predictive"))) {
          # operate fully for the first day
          df = df |> mutate(operate_hp_tes = ifelse(date(datetime) == date(datetime[1]), TRUE, operate_hp_tes))
        }

        df = df |> mutate(hp_tes = operate_hp_tes * .data[[tot_awhp_cap]])
      }

      df = OAT_charge_reset(df, s$tes, s$tes_sizing, equip_sizes)
      if (str_detect(s$tes_control, "optimal_charging_reset")) {
        df = df |>
          optimal_charge_reset(equip_sizes, control_var) |> # check if using
          mutate(TES_charge_target = pmax(TES_charge_target_optimal, TES_charge_target_oat))
      } else {
        df = df |> rename(TES_charge_target = TES_charge_target_oat)
      }
      hp_tes_reset = pull(df, hp_tes)
      hp_tes_reset[1] = df[[tot_awhp_cap]][1]
      TES_charge_reset = pull(df, TES_charge)
      TES_charge_reset[1] = equip_sizes$TES

      for (t in 2:nrow(df)){
        TES_prev = TES_charge_reset[t - 1]
        deficit = max(0, df$TES_charge_target[t] - TES_prev)

        if (deficit > 0) {
          hp_tes_reset[t] = min(df[[tot_awhp_cap]][t], deficit + df[[loop_remainder]][t])
        }
        
        TES_charge_reset[t] = update_TES_charge(
          hp_tes_reset[t],
          df[[loop_remainder]][t],
          TES_prev,
          equip_sizes$TES
        )

        # below is for situations where deficit is 0 (previous hour charge meets target), and control logic might disable HP operation (based on emissions etc)
        # but this lowers the charge below target (tank is serving loads when it should not be), so check for that and correct
        # if this isn't corrected the charge oscillates as the logic above only checks the previous hour charge
        if (deficit == 0) {
          if (TES_charge_reset[t] < df$TES_charge_target[t]) {
            hp_tes_reset[t] = min(df[[tot_awhp_cap]][t], df[[loop_remainder]][t])
            # test[t] = hp_tes_reset[t]

            TES_charge_reset[t] = update_TES_charge(
              hp_tes_reset[t],
              df[[loop_remainder]][t],
              TES_prev,
              equip_sizes$TES
            )
          }
        }
      }
      df %<>% mutate(hp_tes = hp_tes_reset, TES_charge = TES_charge_reset)
    }

    # debugging
    # if (!(str_detect(s$tes_control, "hp_priority"))) {
    #   print(df |> filter(is.na(TES_avail_charge_delta)) |> select(building, datetime, hhw, operate_hp_tes, hp_tes, TES_avail_charge_delta), n = Inf)
    #   print(df |>
    #     filter(date(datetime) %in% c(ymd("2023-03-11"), ymd("2023-03-12"))) |>
    #     select(building, datetime, hhw, elec_emissions_rate, total_var, test_op_end, test_op_start, operate_hp_tes, hp_tes, TES_avail_charge_delta),
    #     n = Inf)
    # }

    df %<>%
      mutate(TES_charge_delta = TES_charge - lag(TES_charge, default = TES_charge[1])) %<>%
      mutate(!!loop_remainder := .data[[loop_remainder]] + TES_charge_delta) %<>%
      mutate(!!loop_remainder := ifelse(.data[[loop_remainder]] < 1e-3, 0, .data[[loop_remainder]])) %<>%
      mutate(TES_load = ifelse(TES_charge_delta < 0, -TES_charge_delta, 0)) #actual served load
  } else {
    df %<>%
      mutate(TES_charge = 0, TES_charge_delta = 0, TES_load = 0)
  }

  # lead with heat recovery if available
  if (!is.na(s$hr_wwhp)){
    # determine how much simultaneous load there is (accounting for waste heat for wwhp, 
    # leaving some CHW on the table in worst case scenario)
    # todo: more elegant way to resolve this simplification?  
    # to get around requiring cop to determine how much simultaneous load there 
    # is and associated circular reference. not worth the iteration time to 
    # solve given small associated error
    if (is.na(s$tes)){ 
      df%<>%mutate(simult_h = pmin(hhw,chw/least_waste_heat$cap_h_to_cap_c))
      df%<>%mutate(hr_hhw = ifelse(pmin(max_cap_h,simult_h)>=min_cap_h,pmin(max_cap_h,simult_h),0))
      df%<>%mutate(hr_cop_h = ifelse(hr_hhw>0,interp1_zero(hr_plr_curve$cap_h, hr_plr_curve$cop_h, hr_hhw),0))
      df%<>%mutate(hr_chw = ifelse(hr_cop_h==0, 0.0, hr_hhw*(1-(1/hr_cop_h))))
      df%<>%mutate(hr_served_hhw=hr_hhw)
      df%<>%mutate(hr_served_chw=hr_chw)
    } else { # if TES does exist, one of hr_hhw/hr_chw has already been calculated
      if (str_detect(s$tes, "hhw")) { # this will NOT work with switchover logic since some of these columns would be partly calc differently
        df%<>%mutate(hr_cop_h = ifelse(hr_hhw>0,interp1_zero(hr_plr_curve$cap_h, hr_plr_curve$cop_h, hr_hhw),0))
        df%<>%mutate(hr_chw = ifelse(hr_cop_h==0, 0.0, hr_hhw*(1-(1/hr_cop_h))))
        df%<>%mutate(hr_served_hhw=hr_hhw-pmax(0,TES_charge_delta))
        df%<>%mutate(hr_served_chw=hr_chw)
      } else if (str_detect(s$tes, "chw")) {
        df%<>%mutate(hr_cop_c = ifelse(hr_chw>0,interp1_zero(hr_plr_curve$cap_c, hr_plr_curve$cop_c, hr_chw),0))
        df%<>%mutate(hr_hhw = ifelse(hr_cop_c==0, 0.0, hr_chw*(1+(1/hr_cop_c))))
        df%<>%mutate(hr_cop_h = ifelse(hr_hhw>0,interp1_zero(hr_plr_curve$cap_h, hr_plr_curve$cop_h, hr_hhw),0))
        df%<>%mutate(hr_served_chw=hr_chw-pmax(0,TES_charge_delta))
        df%<>%mutate(hr_served_hhw=hr_hhw)
      }
    }
    df%<>%mutate(hhw_remainder = hhw_remainder-hr_hhw)
    df%<>%mutate(chw_remainder = chw_remainder-hr_chw)
    df%<>%mutate(elec_hr=ifelse(hr_cop_h>0,hr_hhw/hr_cop_h,0))
    df%<>%mutate(elec=elec+elec_hr)
  } 
  
  # then awhp if available
  if (!is.na(s$awhp_h)){
    # extract awhp performance characteristics
    # awhp_h_curve = awhp_curves %>% filter(awhp_id == s$awhp_h) %>% drop_na(oa_t,cap_h,cop_h) %>% select(oa_t, cop_h,cap_h)
    # df%<>%mutate(awhp_cop_h = interp1_zero(awhp_h_curve$oa_t, awhp_h_curve$cop_h, oa_t))  
    # df%<>%mutate(awhp_cap_h = interp1_zero(awhp_h_curve$oa_t, awhp_h_curve$cap_h, oa_t))  

    # determine actual number of AWHPs based on equipment sizing scenario
    if (str_detect(s$awhp_sizing, "%")) {
      # size number of heat pumps to meet 
      # find max HHW load at OATs where
      # which that AWHP can generate HHW
      max_num_awhps_h = max(df %>% drop_na(hhw_remainder,awhp_cap_h) %>% 
                            filter(awhp_cap_h>0)%>% mutate(num_awhps = hhw_remainder/awhp_cap_h) %>% .$num_awhps)
      max_num_awhps_c = max(df %>% drop_na(chw_remainder,awhp_cap_c) %>% 
                            filter(awhp_cap_c>0)%>% mutate(num_awhps = chw_remainder/awhp_cap_c) %>% .$num_awhps)
      max_num_awhps = max_num_awhps_h
      # max_num_awhps = max(max_num_awhps_h, max_num_awhps_c)
      # if (!is.na(s$tes)){
      #   if (str_detect(s$tes, "hhw")) {
      #     max_num_awhps = max_num_awhps_h
      #   } else if (str_detect(s$tes, "chw")) {
      #     max_num_awhps = max_num_awhps_c
      #   }
      # } else {
      #   max_num_awhps = max_num_awhps_h
      # }
      
      target_frac = as.numeric(str_extract(s$awhp_sizing, "[:digit:]*(?=%)"))/100
      
      if (str_detect(s$awhp_sizing, "RoundUp")) {
        # assign number of AWHPs based on min # of heat pumps required to serve 
        # that load fully 
        df%<>%mutate(awhp_num = ceiling(target_frac*max_num_awhps))
      } else{
        # use fractional heat pumps based on percentage
        df%<>%mutate(awhp_num = target_frac* max_num_awhps)
      }
    } else{
      # assumes that user specified number (or fraction) of heat pumps
      df%<>%mutate(awhp_num = as.numeric(s$awhp_sizing))
    }
    df%<>%mutate(awhp_hhw = ifelse(awhp_cap_h*awhp_num > hhw_remainder, hhw_remainder, awhp_cap_h*awhp_num))
    # df%<>%mutate(awhp_hhw_overmax = pmax(awhp_hhw-awhp_cap_h_total, 0)) 
    if (is.na(s$tes)){
      df%<>%mutate(awhp_cap_h_total = awhp_num*awhp_cap_h)
    }
    df%<>%mutate(hhw_remainder = hhw_remainder-awhp_hhw) 
    df%<>%mutate(backup_hhw = hhw_remainder) 
    df%<>%mutate(elec_awhp_h = ifelse(awhp_hhw>0 | awhp_cop_h>0,awhp_hhw/awhp_cop_h,0))  
    df%<>%mutate(elec = elec+elec_awhp_h)
  }
  # then gas boiler if available
  # assume boiler capacity is unlimited
  if (!is.na(s$boiler)){
    boiler = boilers %>% filter(boiler_id == s$boiler)
    df%<>%
      mutate(gas=hhw_remainder/boiler$eff) %>% # updated to divide
      mutate(hhw_remainder = ifelse(is.na(hhw_remainder),NA,0))
  } 
  # remaining HHW load assumed served by electric resistance 
  # (e.g. elec boiler, or awhp resistance backup)
  if (sum(df$hhw_remainder,na.rm=T)>0){
    df%<>%
      mutate(elec_res = hhw_remainder) %>% 
      mutate(elec=elec+elec_res) %>% 
      mutate(hhw_remainder = ifelse(is.na(hhw_remainder),NA,0))
  } else {
    df%<>%
      mutate(elec_res = 0)
  }
  
  # if awhp is specified in cooling, it is assumed to be used here
  # e.g. use to easily model an air_cooled chiller, or use of AWHP for cooling
  if (!is.na(s$awhp_c)){
    # awhp_c_curve = awhp_curves %>% filter(awhp_id == s$awhp_c) %>% drop_na(oa_t,cap_c,cop_c) %>% select(oa_t, cop_c,cap_c)
    # df%<>%mutate(awhp_cop_c = interp1_zero(awhp_c_curve$oa_t, awhp_c_curve$cop_c, oa_t)) 
    # df%<>%mutate(awhp_cap_c = interp1_zero(awhp_c_curve$oa_t, awhp_c_curve$cap_c, oa_t))
    
    df%<>%mutate(awhp_chw = ifelse(awhp_cap_c*awhp_num > chw_remainder, chw_remainder,awhp_cap_c*awhp_num))
    df%<>%mutate(chw_remainder = chw_remainder-awhp_chw)
    
    df%<>%mutate(elec_awhp_c = ifelse(awhp_chw >0 & awhp_cop_c>0,awhp_chw/awhp_cop_c,0))
    df%<>%mutate(elec = elec+elec_awhp_c)
  }
  
  # remaining CHW load assumed to be served by water cooled chiller (todo: fixed COP 5)
  # 09.02 changing to air cooled
  if (sum(df$chw_remainder,na.rm=T)>0){
    chiller_COP = 2.84
    df%<>%
      mutate(chiller_chw = chw_remainder) %>% 
      mutate(elec_chiller = chw_remainder/chiller_COP) %>% 
      mutate(elec=elec+elec_chiller) %>% 
      mutate(chw_remainder = ifelse(is.na(chw_remainder),NA,0))
  }
  
  if(!detail){
    # return just the loads and outdoor temps to reduce resources
    df %<>% select(datetime, oa_t, elec, gas)
  }
  # calculate total refrigerant inventory based on number of AWHPs and HRCs
  df %<>% 
    mutate(total_refrig_emissions_inventory = 0) %>% 
    mutate(total_refrig_emissions_inventory = 
             ifelse(is.na(s$awhp_refrig_emissions_inventory),0,s$awhp_refrig_emissions_inventory*awhp_num) + 
             ifelse(is.na(s$hr_wwhp_refrig_emissions_inventory),0,s$hr_wwhp_refrig_emissions_inventory))

  # store eq_scen_id in df before returning from function (for later joining)
  df %<>%  mutate(eq_scen_id = s$eq_scen_id)
  
  # have to set data (regardless of equipment scenario) 
  # to NA if OAT is missing it would skew any AWHP vs non AWHP scenarios
  df %<>%
    mutate(elec = ifelse(is.na(hhw) | is.na(oa_t), NA, elec)) %<>%
    mutate(gas = ifelse(is.na(hhw) | is.na(oa_t), NA, gas))         
  
  return(df)
}

# compute grid emissions rate 
grid_emissions = function(emissions_scenario, df){
  s = emissions_scenario
  df%<>%mutate(em_scen_id = s$em_scen_id)
  
  # handle misc input possibilities for grid region
  if ('grid_region' %in% names(s)){
    # grid region defined in scenario
    # if (is.na(s$grid_region)){
    #   # but is NA
    #   # if the user provides a grid region in the emissions scenarios
    #   # assume it is provided in the timeseries df
    #   s$grid_region = "CAISO"
    #   df%<>% mutate(grid_region = s$grid_region)    
    # } else{
      # use the specified grid region in the emissions scenario
      # and add to the results df
      df%<>% mutate(grid_region = s$grid_region)
    # }    
  } else {
    if ('grid_region' %in% names(df)) {
      # nothing defined in emissions scenario, but dataset includes grid region
      s$grid_region = df%>%distinct(grid_region) %>% pull(grid_region)
    } else {
      # nothing defined in emissions scenarios or in dataset
      s$grid_region = "CAISO"
      df%<>% mutate(grid_region = s$grid_region)      
    }
  }

  if (s$emissions_type=="Combustion only"){
    grid_data = grid_scenarios %>% 
      rename(grid_region = gea, grid_scenario = scenario, grid_year = year) %>% 
      inner_join(s) %>%
      mutate(elec_emissions_rate = lrmer_co2e_c*(1-shortrun_weighting)+ srmer_co2e_c*shortrun_weighting) %>% 
      select(month,hour,elec_emissions_rate)

  } else if (s$emissions_type=="Includes pre-combustion") {
    grid_data = grid_scenarios %>% 
      rename(grid_region = gea, grid_scenario = scenario, grid_year = year) %>% 
      inner_join(s) %>%
      mutate(elec_emissions_rate = (lrmer_co2e_c+lrmer_co2e_p)*(1-shortrun_weighting)+ (srmer_co2e_c+srmer_co2e_p)*shortrun_weighting) %>% 
      select(month,hour,elec_emissions_rate)
    
  } else {
    return(F)
  }

  df %<>% 
    # mutate(month=month(datetime), hour=hour(datetime)) %>% 
    inner_join(grid_data)
    # select(-month,-hour)

  # print(select(df, datetime, hhw, month, hour, lrmer_co2e_p, lrmer_co2e_c, srmer_co2e_p, srmer_co2e_c, elec_emissions_rate)) # checking

  return(df)
}

# converts site energy consumption to source using provided emissions rates
# also estimates fugitive refrigerant related emissions 
site_to_source <- function(emissions_scenario, df){
  s = emissions_scenario

  df%<>%mutate(gas_emissions = s$gas_emissions_rate*gas/1000/1000) #Wh to kWh & g to kg
  
  df%<>%mutate(elec_emissions = elec_emissions_rate*elec/1000/1000) #Wh to kWh & g to kg
  df%<>%mutate(awhp_h_emissions = elec_emissions_rate*elec_awhp_h/1000/1000) #Wh to kWh & g to kg
  df%<>%mutate(elec_h_emissions = elec_emissions_rate*(elec_awhp_h+elec_hr+elec_res)/1000/1000) #Wh to kWh & g to kg
  df%<>%mutate(elec_c_emissions = elec_emissions_rate*(elec_awhp_c+elec_hr+elec_chiller)/1000/1000) #Wh to kWh & g to kg
  
  df%<>%
    mutate(refrig_emissions = total_refrig_emissions_inventory*s$annual_refrig_leakage/8760)

  return(df)
}