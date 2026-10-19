library(tidyverse)
library(lubridate)
library(slider)

update_TES_charge = function(hp, load, prev_charge, TES_size){
  available_delta = hp - load

  low_lim = 0
  min( # smaller of calculated and max size
    TES_size,
    max( # larger of 0 and prev + delta
      low_lim,
      prev_charge + available_delta
    )
  )
}

OAT_charge_reset = function(df, loop, sizing, equip_sizes){
  oat_lim = c(equip_sizes$design_oat+2, equip_sizes$design_oat+6)# ; print(oat_lim)
  charge_lim = c(1, 0)
  df = df |>
    mutate(maintain_tes_oat = interp1_TES_reset(oat_lim, charge_lim, oa_t))
  if (str_detect(loop, "chw")) {
    df = df |>
      mutate(maintain_tes_oat = 0)
  }

  
  if (str_detect(sizing, "2x")) {
    load_leveling_size = equip_sizes$TES / 2
  } else if (str_detect(sizing, "0.5x")) {
    load_leveling_size = equip_sizes$TES
  } else if (str_detect(sizing, "3x")) {
    load_leveling_size = equip_sizes$TES / 3
  } else if (str_detect(sizing, "4x")) {
    load_leveling_size = equip_sizes$TES / 4
  } else if (str_detect(sizing, "5x")) {
    load_leveling_size = equip_sizes$TES / 5
  } else {
    load_leveling_size = equip_sizes$TES
  }

  df = df |>
    mutate(TES_charge_target_oat = maintain_tes_oat * load_leveling_size) |>
    mutate(design_oat = equip_sizes$design_oat)

  return(df)
}

optimal_charge_reset = function(df, equip_sizes, ctrl_var){
  ctrl_lim = c(0, 1)
  charge_lim = c(1, 0)

  df = df |>
    group_by(date) |>
    mutate(ctrl = as.vector(normalize(.data[[ctrl_var]]))) |>
    ungroup() |> 
    mutate(maintain_tes_optimal = interp1_TES_reset(ctrl_lim, charge_lim, ctrl)) |>
    mutate(TES_charge_target_optimal = maintain_tes_optimal * equip_sizes$TES)

  return(df)
}

optimal_charging_hrs = function(df, loop, control, cap_var){
  hp_operation_hrs = df |> 
    summarize(total_wh = sum(.data[[loop]]), daily_design_cap = min(.data[[cap_var]][.data[[cap_var]] != 0])) 

  if (!(str_detect(control, "predictive"))) {
    # if not predictive, shift total_wh to be prev. day loads
    hp_operation_hrs = hp_operation_hrs |> 
      ungroup() |>
      mutate(total_wh = lag(total_wh, default = 24)) |>
      mutate(daily_design_cap = lag(daily_design_cap, default = 1))
  } # operate HP all day for the first day

  hp_operation_hrs = hp_operation_hrs |> 
    mutate(hrs_hp = ceil(total_wh/daily_design_cap))

  return(hp_operation_hrs)
}

# for storage priority controls
efficiency_var = function(loop){
  if (str_detect(loop, "hhw")) {
    eff = "awhp_cop_h"
  } else if (str_detect(loop, "chw")) {
    eff = "awhp_cop_c"
  }
  return(eff)
}

hp_tes_operation = function(df, loop, control, control_var, cap_var, equip_sizes){
  df = df |> mutate(date = date(datetime)) |> group_by(date)

  if (str_detect(control, "continuous")) {

    if (str_detect(control, "optimal_charging_hours")) { # continuous, optimal charging w/ calc. hours
      hp_operation_hrs = optimal_charging_hrs(df, loop, control, cap_var)

      df = df |>
        left_join(hp_operation_hrs, by = "date") |>
        mutate(total_var = slide_dbl(.data[[control_var]], sum, .before = max(0, hrs_hp[1]-1), .complete = T)) |>
        arrange(date, total_var) |>
        group_by(date) |>
        mutate(operate_hp_tes = ((datetime <= datetime[1] & datetime >= datetime[1]-dhours(max(0, hrs_hp[1]-1)))))
        # operate during best (i.e. lowest) hours block
    
    } else if (str_detect(control, "optimal_charging_reset")) {  # continuous, optimal charging w/ charge reset
      df = df |> mutate(operate_hp_tes = FALSE)

    } else {  # continuous, not optimal charging
      df = df |>
        mutate(total_var = slide_dbl(.data[[control_var]], sum, .before = peak_hr_period-1, .complete = T)) |>
        arrange(date, desc(total_var)) |>
        group_by(date) |>
        mutate(operate_hp_tes = !(datetime <= datetime[1] & datetime >= datetime[1]-dhours(peak_hr_period-1))) #|>
        # do not operate during worst (i.e. highest) hours
        # mutate(test_op_end = datetime[1], test_op_start = datetime[1]-dhours(peak_hr_period-1))

      # debugging
      # if (df$building[1] == "180" & control == "emissions_continuous") {
      #   printer = df |> 
      #     filter(date == ymd("2023-03-29")) |>
      #     select(building, datetime, .data[[control_var]], total_var, operate_hp_tes) |>
      #     arrange(datetime)
      #   print(printer, n=Inf)
      # }
    }

  } else {
    if (str_detect(control, "optimal_charging_hours")) {  # not continuous, optimal charging w/ calc. hours
      hp_operation_hrs = optimal_charging_hrs(df, loop, control)

      df = df |> 
        left_join(hp_operation_hrs, by = "date") |>
        arrange(date, .data[[control_var]]) |>
        group_by(date) |>
        mutate(rank = row_number()) |> 
        mutate(operate_hp_tes = (rank <= hrs_hp))

    } else if (str_detect(control, "optimal_charging_reset")) {  # not continuous, optimal charging w/ charge reset
      df = df |> ungroup() |>
        mutate(operate_hp_tes = FALSE)
      
    } else { # not continuous, not optimal charging
      df = df |>
        arrange(date, desc(.data[[control_var]])) |> 
        # sort by worst emissions hours
        group_by(date) |>
        mutate(rank = row_number()) |>
        mutate(operate_hp_tes = (rank > peak_hr_period))
        # operate for all hours after the peak length 
    }
  }

  df = df |> ungroup() |> arrange(datetime) #arrange(month_pos, day, hour)
  return(df)
}

peak_dates = function(df, loop){
  df_peak = df |>
    mutate(date = date(datetime)) |>
    group_by(date) |>
    summarize(total_wh = sum(.data[[loop]]), num_hrs = n()) |>
    # filter(num_hrs == 24) |>
    arrange(desc(total_wh))
  # peakdate = head(df_peak, 1)$date
  # peakdate_hrs = head(df_peak, 1)$num_hrs
  # # print(df_peak)

  # df_peak_rolling = df |>
  #   mutate(date = date(datetime)) |>
  #   mutate(total_rolling = slide_dbl(.data[[loop]], sum, .before = 23, .complete = T)) |>
  #   arrange(desc(total_rolling))

  return(df_peak)
}

equip_sizing = function(df, loop, sizing, control, hp_curve){ 
  # df is one building's 8760
  # loop is 'hhw' or 'chw'
  df_peak = peak_dates(df, loop)
  peakdate = head(df_peak, 1)$date
  peakdate_hrs = head(df_peak, 1)$num_hrs
  peakdayloads = filter(df, date(datetime) == peakdate)

  # rolling 24-hr peak instead of by date
  # peakdate_hrs = 24
  # peakdate_end = head(df_peak, 1)$hr_id
  # peakdate_start = peakdate_end - 23
  # peakdayloads = filter(df, hr_id >= peakdate_start, hr_id <= peakdate_end)
  # print(df_peak)

  peakdaytotal = sum(peakdayloads[,loop])
  if (str_detect(sizing, "Partial")) {
    peakdayloads = peakdayloads |>
      mutate(operate_hp = TRUE)

  } else { # full storage

    if (str_detect(control, "emissionsXeff")) {
      cop_var = efficiency_var(loop)
      peakdayloads = peakdayloads |>
        mutate(emissions_1W = elec_emissions_rate * 1 / .data[[cop_var]])
      control_var = "emissions_1W"

    } else if (str_detect(control, "emissions")) {
      control_var = "elec_emissions_rate"

    } else if (str_detect(control, "load")) {
      control_var = loop
    }

    if (str_detect(control, "continuous")) {

      peakdayloads = peakdayloads |>
        mutate(total_var = slide_dbl(.data[[control_var]], sum, .before = peak_hr_period-1, .complete = T)) |>
        arrange(desc(total_var)) |>
        # sort by worst emissions/load hours rolling sum
        mutate(operate_hp = !(datetime <= datetime[1] & datetime >= datetime[1] - dhours(peak_hr_period - 1)))
        # operate for all hours after the peak length 

    } else {

      peakdayloads = peakdayloads |>
        arrange(desc(.data[[control_var]])) |>
        # sort by worst emissions/load hours
        mutate(rank = row_number()) |>
        mutate(operate_hp = (rank > peak_hr_period))
        # operate for all hours after the peak length 
    }  
  }

  off_peak_hrs_length = sum(peakdayloads$operate_hp)
  # print(off_peak_hrs_length)
  hp_size = peakdaytotal/off_peak_hrs_length
  if (str_detect(loop, "hhw")) {
    design_oat_func = min
    cap_var = "cap_h"
  } else if (str_detect(loop, "chw")) {
    design_oat_func = max
    cap_var = "cap_c"
  }
  design_oat = peakdayloads |>
    # filter(operate_hp) |>
    summarize(design_oat = design_oat_func(oa_t)) |>
    pull(design_oat)
  hp_num = hp_size / interp1_zero(hp_curve$oa_t, hp_curve[[cap_var]], design_oat)
  # hp_num = ceil(hp_num)
  # this will break if design_oat is below the lowest operating temp in curve, fix if needed

  peakdayloads = peakdayloads |> 
    arrange(datetime) |>
    # mutate(awhp_cap_h = interp1_zero(hp_curve$oa_t, hp_curve$cap_h, oa_t)) |>
    # mutate(hp = operate_hp * hp_num * awhp_cap_h)
    mutate(hp = operate_hp * hp_size)

  peakdayloads = peakdayloads |> 
    mutate(TES_charge_delta = hp - .data[[loop]]) |>
    mutate(charging = TES_charge_delta > 0, discharging = TES_charge_delta < 0)

  TES_sums = peakdayloads |>
    summarise(sum_charge = sum(TES_charge_delta[charging]), 
      sum_discharge = sum(TES_charge_delta[discharging]))
  if (TES_sums$sum_charge + TES_sums$sum_discharge > 1e-3 ) {
    print(TES_sums)
    stop(glue("TES sizing for building {df$building[1]}, control {control} is incorrect"))
  }
  TES_size = TES_sums$sum_charge

  # debugging
  # if (str_detect(control, "full_storage_priority_emissions_continuous_optimal_charging")) {
  #   # print(df_peak)
  #   # print(peak_hrs)
  #   print(control)
  #   # print(hp_curve)
  #   print(glue("peak day total is {peakdaytotal}"))
  #   print(glue("design oat is {design_oat}"))
  #   print(glue("hp size is {hp_size}"))
  #   print(glue("hp num is {hp_num}"))
  #   print(glue("TES size is {TES_size}"))
  #   print(select(peakdayloads, datetime, oa_t, hhw, elec_emissions_rate, operate_hp, hp, TES_charge_delta, mode), n=24)
  # }

  if (str_detect(sizing, "2x")) {
    TES_size = TES_size * 2
  } else if (str_detect(sizing, "0.5x")) {
    TES_size = TES_size * 0.5
  } else if (str_detect(sizing, "3x")) {
    TES_size = TES_size * 3
  } else if (str_detect(sizing, "4x")) {
    TES_size = TES_size * 4
  } else if (str_detect(sizing, "5x")) {
    TES_size = TES_size * 5
  }
  size = list("hp" = hp_size, "hp_num" = hp_num, "design_oat" = design_oat, "TES" = TES_size)
  # print(size)
  return(size)
}