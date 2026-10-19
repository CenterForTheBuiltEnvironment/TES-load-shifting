library(tidyverse)
library(lubridate)
library(magrittr)

source("../code/basic_functions.R")
source("../code/import_data.R")
source("../code/model_scenarios.R")

# -------- multiple buildings --------
results_all = df %>% 
  mutate(splitter = building) %>% 
  group_by(splitter) %>%
  group_map( ~ scenario_analysis(df=., equip_scenarios=equip_scenarios, 
                      emissions_scenarios=emissions_scenarios, 
                      detail=T)) %>% 
  list_rbind() %>% 
  left_join(select(equip_scenarios, eq_scen_id, tes_sizing, tes_control_id, desc),
    by = "eq_scen_id")


# correct for end-of-year charge
for (id in bldgs$bldg_id) {
    for (d in equip_scenarios$desc) {
      if (!(d %in% c("BaselineGas", "No TES"))) {
        bldg_results = filter(results_all, building == id, desc == d)
        final_timestamp = tail(bldg_results, n = 1)
        initial_timestamp = head(bldg_results, n = 1)
        if (final_timestamp$TES_charge < initial_timestamp$TES_charge) {
          delta = initial_timestamp$TES_charge - final_timestamp$TES_charge
          # percent = 100*delta/initial_timestamp$TES_charge
          nom_cap = head(bldg_results, 1)$awhp_cap_h_total
          hrs = 3 + delta / nom_cap  # 3 hrs extra for safety since not sure what the next hours look like
          # print(hrs)
          awhp_operate = delta / hrs
          extra_df = head(bldg_results, hrs) |> 
            mutate(
              datetime = datetime+years(1),
              awhp_hhw = awhp_operate,
              elec_awhp_h = awhp_hhw/awhp_cop_h,
              elec = elec_awhp_h,
              awhp_h_emissions = elec_emissions_rate*elec_awhp_h/1000/1000,
              elec_h_emissions = awhp_h_emissions,
              elec_emissions = awhp_h_emissions
            )
          results_all = rbind(results_all, extra_df)
        }
      }
  }
}

for (id in bldgs$bldg_id) {
  df_peak = peak_dates(filter(df, building == id), "hhw")
  peakdate = head(df_peak, 1)$date
  # print(df_peak)
  plot_dates = plot_dates |>
    add_row(
      building = as.double(id),
      season = "Peak Day",
      date = peakdate
    )
}
plot_dates = plot_dates |> arrange(building, date)

# results_all
summary_totals = results_all %>% 
  group_by(building, tes_sizing, tes_control_id, desc) %>% # highest level comparison is equipment performance
  mutate(tot_emissions = gas_emissions+elec_emissions+refrig_emissions) %>% 
  select(tes_sizing, tes_control_id, desc, tot_emissions, elec_emissions,elec_h_emissions,awhp_h_emissions, gas_emissions, elec, gas, elec_hr, elec_awhp_h, elec_res, elec_awhp_c, elec_chiller, elec_c_emissions) %>% 
  summarize_if(is.numeric, sum)

summary_sizes = results_all %>% 
  group_by(building, tes_sizing, tes_control_id, desc) %>%
  select(building,tes_sizing, tes_control_id, desc,hp_num_tes,awhp_num, TES_charge,eq_scen_id) %>% 
  summarize_if(is.numeric, max)

# hp_sizes_calc = results_all |> 
#   group_by(building, desc) |> 
#   distinct(hp_tes) |> 
#   filter(hp_tes !=0) |>
#   rename(AWHP_size = hp_tes)

summary = summary_totals |>
  # left_join(summary_totals, hp_sizes_calc, by = c("building", "desc")) |>
  left_join(summary_sizes, by = c("tes_sizing", "tes_control_id", "desc", "building")) |>
  left_join(tes_control_scenarios, by = "tes_control_id") |>
  left_join(select(equip_scenarios, desc, tes), by = "desc") |>
  rename(TES_size = TES_charge, AWHP_num = hp_num_tes, AWHP_num_over = awhp_num) |>
  mutate(AWHP_num = ifelse(desc=="No TES", AWHP_num_over, AWHP_num)) |>
  arrange(building, eq_scen_id) |> ungroup()

summary

TES_sums = results_all |> # test equal
  group_by(building, desc) |>
  mutate(charging = TES_charge_delta > 0, discharging = TES_charge_delta < 0) |>
  summarise(sum_charge = sum(TES_charge_delta[charging]), 
    sum_discharge = sum(TES_charge_delta[discharging])) |>
  mutate(check = ifelse(sum_charge + sum_discharge <= 0, "ok", "error")) # often ends up where TES is not fully charged back up at the end of the year
print(TES_sums, n=Inf)

# -------- single building --------
# bldg_id = "189"
# results1 = scenario_analysis(
#     df=filter(df, building == bldg_id), 
#     equip_scenarios=equip_scenarios,
#     emissions_scenarios=emissions_scenarios,
#     detail=T) |>
#   left_join(select(equip_scenarios, eq_scen_id, tes_sizing, tes_control_id, desc),
#     by = "eq_scen_id")

# summary_totals = results1 %>% 
#   group_by(tes_sizing, tes_control_id, desc) %>% # highest level comparison is equipment performance
#   mutate(tot_emissions = gas_emissions+elec_emissions+refrig_emissions) %>% 
#   select(tes_sizing, tes_control_id, desc, tot_emissions, elec_emissions, awhp_h_emissions,elec, gas, elec_hr, elec_awhp_h, elec_res, elec_awhp_c) %>% #, elec_chiller) %>% 
#   summarize_if(is.numeric, sum)
# summary_sizes = results1 %>% 
#   group_by(building,tes_sizing, tes_control_id, desc) %>%
#   select(building,tes_sizing, tes_control_id, desc,hr_hhw, TES_charge,hp_num_tes,awhp_num,eq_scen_id) %>% 
#   summarize_if(is.numeric, max)
# summary = left_join(summary_totals, summary_sizes, by = "desc") |> 
#   left_join(tes_control_scenarios, by = "tes_control_id") |>
#   arrange(eq_scen_id) |>
#   rename(TES_size = TES_charge, AWHP_num = hp_num_tes, AWHP_num_over = awhp_num) |>
#   mutate(AWHP_num = ifelse(desc=="No TES", AWHP_num_over, AWHP_num))
# summary

# df_peak = peak_dates(filter(df, building == bldg_id), "hhw")
# peakdate = head(df_peak, 1)$date
# # print(df_peak)
# plot_dates = plot_dates |>
#   add_row(
#     building = as.double(bldg_id),
#     season = "Peak",
#     date = peakdate
#   ) |>
#   arrange(building, date)

# TES_sums = results1 |> # test equal
#   group_by(eq_scen_id, desc) |>
#   mutate(charging = TES_charge_delta > 0, discharging = TES_charge_delta < 0) |>
#   summarise(sum_charge = sum(TES_charge_delta[charging]), 
#     sum_discharge = sum(TES_charge_delta[discharging])) |>
#   mutate(check = ifelse(sum_charge + sum_discharge <= 0, "ok", "error")) # often ends up where TES is not fully charged back up at the end of the year
# print(TES_sums)

# results_all = results1