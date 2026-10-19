library(tidyverse)
library(arrow)
library(janitor)

# Read in data, misc initial prep/cleaning tasks

# tables of relevant information
bldgs <- "data/bldg.csv" %>% read_csv(skip=3)  |> mutate(bldg_id = as.character(bldg_id))
refrigs <- "data/refrigs.csv" %>% read_csv(skip=3)
awhps <- "data/awhps.csv" %>% read_csv(skip=3)
hr_wwhps <- "data/hr_wwhps.csv" %>% read_csv(skip=3)
boilers <- "data/boilers.csv" %>% read_csv(skip=3)
awhp_curves <- "data/awhp_curves.csv" %>% read_csv(skip=3)
hr_wwhp_curves <- "data/hr_wwhp_curves.csv" %>% read_csv(skip=3)
plot_dates <- "data/loads/plot_dates.csv" %>% read_csv(skip=3) |> mutate(date = dmy(date))

awhp_curves %<>%
  mutate(oa_t = ifelse(unit_system == "IP",F_to_C(oa_t),oa_t),
         cap_c = ifelse(unit_system == "IP",ton_to_W*cap_c,cap_c),
         cap_h = ifelse(unit_system == "IP",BTUh_to_W*cap_h,cap_h)) %>% 
  select(-unit_system)

# assume/simplify that AWHPs can continue making HHW even at high OATs,
# even if the performance curve data does not show that they can
max_oat_awhp_h = awhp_curves %>% group_by(awhp_id) %>% filter(cap_h >0) %>% filter(oa_t == max(oa_t)) %>% mutate(oa_t =60)
awhp_curves %<>% add_row(max_oat_awhp_h)

hr_wwhp_curves%<>%
  mutate(cap_h = ifelse(unit_system == "IP",BTUh_to_W*cap_h,cap_h))

# load in elec emissions rates
grid_scenarios = readRDS("data/grid_scenarios.rds")
# if needed, extract updated information from the latest cambium data
# available from: https://scenarioviewer.nrel.gov/
# download month-hour average data for all GEA regions
# read_csv("./data/Cambium24_allScenarios_month-hour_gea.csv", skip=5) %>%
#   rename(year = t) %>%
#   select("scenario","gea","year","month","hour","lrmer_co2e_p","lrmer_co2e_c","srmer_co2e_p","srmer_co2e_c") %>%
#   # view
#   saveRDS("./data/grid_scenarios.rds")

df_tool = read_parquet("data/loads/B1_to_B213.parquet", as_data_frame=T)
names(df_tool) = c("building","datetime", "hhw", "chw", "oa_t", "source")
df_measured = filter(df_tool, source == "measured")
df = df_measured
# unique(df_measured$building)

df = df |>
  clean_names() |>
  mutate(hhw = ifelse(hhw < 0, NA, hhw), chw = ifelse(chw < 0, NA, chw)) |> # set negative load hours to account for common measurement error issues/simplification 
  mutate(datetime = with_tz(datetime, "US/Pacific")) |> # set local datetime
  mutate(month = month(datetime), day = day(datetime), hour = hour(datetime)) |>
  # mutate(month_pos = case_when( # to start the year in june
  #       month >= 6 ~ month - 6,
  #       month <= 5 ~ month + 7)) |>
  filter(!is.na(hhw), !is.na(chw), !is.na(oa_t)) |>
  mutate(hr_id = row_number()) |>
  inner_join(bldgs, by = join_by(building == bldg_id)) |>
  arrange(building, datetime) #month_pos, day, hour)

# bldg_id = "181"
# df_bldg = filter(df_measured, building == bldg_id)
# testSorted = function(y){all(do.call(order,y)==1:nrow(y))}
# testSorted(df_bldg)