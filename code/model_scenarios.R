library(tidyverse)

source("../code/model_functions.R")

# higher level function
# for assessing different equipment and emissions scenarios (combined, full factorial)
scenario_analysis <- function(df, equip_scenarios, emissions_scenarios, detail=T){
  # add'l first step for emissions-based TES control
  emissions_data = emissions_scenarios %>% 
    split(.$em_scen_id) %>% 
    map(grid_emissions, df=df) %>% 
    list_rbind() 
  # print(emissions_data) 

  equip_loads = equip_scenarios %>% 
    split(.$eq_scen_id) %>% 
    map(loads_to_site_energy, df=emissions_data, detail=detail) %>% 
    list_rbind()
    
  results = emissions_scenarios %>% 
    split(.$em_scen_id) %>% 
    map(site_to_source, df=equip_loads) %>% 
    list_rbind()    
  
  return(results)
}

# define scenarios
equip_scenarios = tibble(
  desc = c("BaselineGas", "No TES",
  ### HR 
  # "Partial Sizing, HHW TES",
  # "2x Partial Sizing, HHW TES"
  ### AWHP 
  "TES, AWHP Priority", 
  "2x TES, AWHP Priority", "3x TES, AWHP Priority", "4x TES, AWHP Priority", "5x TES, AWHP Priority",
  "TES, Fixed On-Peak Discharging", 
  "2x TES, Fixed On-Peak Discharging", "3x TES, Fixed On-Peak Discharging", "4x TES, Fixed On-Peak Discharging", "5x TES, Fixed On-Peak Discharging",
  "TES, Calc. Off-Peak Charging",
  "2x TES, Calc. Off-Peak Charging", "3x TES, Calc. Off-Peak Charging", "4x TES, Calc. Off-Peak Charging", "5x TES, Calc. Off-Peak Charging"
  # "CHW TES, AWHP Priority",
  # "CHW TES, Fixed On-Peak Discharging",
  # "CHW TES, Calc. Off-Peak Charging"
  # "TES, Emissions Control (Fixed Hrs) Pred"
  # "Partial Sizing, Emissions & COP Control (Calc. Hrs)",
  # "Partial Sizing, Emissions & COP Control (Charge Reset)",
  # "Full Sizing, Emissions Control (Fixed Hrs)",
  # "Full Sizing, Emissions & COP Control (Calc. Hrs)"
  # "Full Sizing, Emissions & COP Control (Charge Reset)"
  ), #description of scenario
  hr_wwhp = c(NA), # select a HR chiller model (if any) # "tool_generic_modular", tool_generic_centrifugal for bigger
  exhaust = c(NA), # specify amount of exhaust air (todo)
  awhp_h = c(NA, rep("tool_generic_h_52", 16)), # select and AWHP for heating (if any)
  awhp_sizing = c(NA, rep("100%", 16)), # specify number of AWHP in heating (if any), or % to size based on max HHW load at temp AWHP can operate
  awhp_c = c(NA, rep("tool_generic_c", 16)), # select AWHP for cooling
  boiler = c("high", rep(NA, 16)), # boiler efficiency
  tes = c(NA, NA, rep("hhw", 15)), # TES tank loop
  tes_sizing = c("", "", 
  "Partial", "2x Partial", "3x Partial", "4x Partial", "5x Partial",
  "Partial", "2x Partial", "3x Partial", "4x Partial", "5x Partial",
  "Partial", "2x Partial", "3x Partial", "4x Partial", "5x Partial"
  # "Partial", "Partial", "Partial"
  # "Full" # "Full"
  ), # TES sizing strategy (full, partial, maximum),
  tes_control_id = c(0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3),
  tes_control = c(NA, NA,
  ### HR
  # "max_discharge", "max_discharge"
  ### AWHP
  "hp_priority", "hp_priority", "hp_priority", "hp_priority", "hp_priority",
  "emissionsXeff_continuous", "emissionsXeff_continuous", "emissionsXeff_continuous", "emissionsXeff_continuous", "emissionsXeff_continuous",
  # "emissions_continuous_predictive"
  "emissionsXeff_continuous_optimal_charging_hours",
  "emissionsXeff_continuous_optimal_charging_hours", "emissionsXeff_continuous_optimal_charging_hours", "emissionsXeff_continuous_optimal_charging_hours", "emissionsXeff_continuous_optimal_charging_hours"
  # "hp_priority",
  # "emissions_continuous",
  # "emissions_continuous_optimal_charging_hours"
  # "emissionsXeff_continuous_optimal_charging_reset",
  # "emissions_continuous"
  # "emissionsXeff_continuous_optimal_charging_hours"
  # "emissionsXeff_continuous_optimal_charging_reset"
  ), 
  # TES control strategy
  # hp_priority only works with partial sizing; the rest assumed to be storage priority
)

tes_control_scenarios = tibble(
  tes_control_id = c(1, 2, 3),
  tes_control_desc = c(
    # "Max. Discharge"
    ### AWHP
    "AWHP Priority",
    "Fixed On-Peak Discharging",
    "Calc. Off-Peak Charging"
    # "Emissions, Fixed Hours, Pred"
    # "Emissions & COP, Min. Charge Reset"
  )
) |>
  mutate(tes_legend = str_c(tes_control_id, " - ", tes_control_desc))
  # tes_control = c(NA, "partial_hp_priority",
  # # "partial_storage_priority_load",
  # # "partial_storage_priority_load_continuous",
  # # "partial_storage_priority_emissions",
  # # "partial_storage_priority_emissions_optimal_charging",
  # # "partial_storage_priority_emissions_continuous",
  # # "partial_storage_priority_emissions_continuous_optimal_charging",
  # "partial_storage_priority_emissionsXeff_continuous",
  # "partial_storage_priority_emissionsXeff_continuous_optimal_charging",
  # # "partial_storage_priority_emissions_continuous_optimal_charging_predictive"
  # # "full_storage_priority_load",
  # # "full_storage_priority_load_continuous",
  # # "full_storage_priority_emissions",
  # # "full_storage_priority_emissions_optimal_charging"
  # # "full_storage_priority_emissions_continuous",
  # # "full_storage_priority_emissions_continuous_optimal_charging"
  # "full_storage_priority_emissionsXeff_continuous",
  # "full_storage_priority_emissionsXeff_continuous_optimal_charging"
  # # "full_storage_priority_emissions_continuous_optimal_charging_predictive"
  # )

equip_scenarios%<>%
  rowid_to_column("eq_scen_id") 

# extract refrigerant emission inventory (total kgCO2e) for these 
# equipment scenarios
equip_scenarios %<>%  
    left_join(awhps %>% select(awhp_id,refrig_id,charge) %>% rename(awhp_h = awhp_id)) %>%
    left_join(refrigs %>% select(refrig_id,gwp)) %>% 
    mutate(awhp_refrig_emissions_inventory = gwp*charge) %>% 
    select(-charge, -refrig_id, -gwp) %>% 
    left_join(hr_wwhps %>% select(hr_wwhp_id,refrig_id,charge) %>% rename(hr_wwhp = hr_wwhp_id)) %>%
    left_join(refrigs %>% select(refrig_id,gwp)) %>% 
    mutate(hr_wwhp_refrig_emissions_inventory = gwp*charge) %>% 
    select(-charge, -refrig_id, -gwp) %>%
    mutate(legend = str_c(eq_scen_id, " - ", desc))
    
# combinatorial expansion of different grid scenarios
emissions_scenarios = 
  expand_grid(
    annual_refrig_leakage = 0.05,
    # grid_region = "CAISO", # if grid region is specified here (e.g. "CAISO") it will override whatever is entered in the bldg table (allows testing out moving buildings to other grid regions, of dubious value) 
    grid_scenario = c("MidCase"), #c("MidCase", "Decarb95by2050", "HighDemandGrowth"),
    grid_year = c(2025), #c(2025,2030,2035,2040,2045, 2050),
    emissions_type = c("Includes pre-combustion"), #c("Combustion only","Includes pre-combustion") 
    shortrun_weighting = 0, # seq(0,1), #weighting between short and long run emissions
    gas_emissions_rate = ng_combustion_to_co2e,
  ) 

emissions_scenarios%<>%
  rowid_to_column("em_scen_id")