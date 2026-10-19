library(tidyverse)
library(pracma)

# -------- any constants used in the analysis --------
# unit conversions
# all are multiplied, right to left
ton_to_W = 12000*.293 # refrigerantton to W
W_to_ton = 1/ton_to_W
BTUh_to_W = .293 # BTU/hr to W
W_to_BTUh = 1/BTUh_to_W
lbs_to_ton = .454/1000 # lbs (e.g. of CO2) to metric tons
ton_to_lbs = 1/lbs_to_ton
lb_to_kg = .454 # lbs (e.g. of CO2) to metric tons
kg_to_lb = 1/lb_to_kg
dC_to_dF = 1.8 # conversion of temperature differences (i.e. delta Ts)
dF_to_dC = 1/dC_to_dF
cfm_to_lps = 0.47194745 # cfm to l/s
lps_to_cfm = 1/cfm_to_lps 
gal_to_m3 = 3.785 /1000
m3_to_gal = 1/ gal_to_m3
cp_water = 4.18 # kJ/kg-K
rho_water = 1000 # kg/m3

# fixed emissions conversions factors
# ng_combustion_to_co2e = 5.3*1000/29.3 # 5.3kg/therm to g/kWh (same unit as cambium emissions data, kg/MWh)
# from EPA: https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator-calculations-and-references
# if including pre-combustion emissions to end use that match Cambium assumptions, increase by 29% (31% reported in paper adjusted to account for newer IPCC AR6 values)
ng_combustion_to_co2e = 1.29*5.3*1000/29.3 # 5.3kg/therm to g/kWh (same unit as cambium emissions data, kg/MWh = g/kWh)

# peak_hrs = c(16, 17, 18, 19, 20, 21) # CA TOU peak
peak_hr_period = 4 # length(peak_hrs)

# -------- basic unit/conversion functions --------
C_to_f = function(C_val){return(C_val*1.8+32)}
F_to_C = function(F_val){return((F_val-32)/1.8)}
# note these cop conversions are simplified, and assume all heat transferred from CHW to HHW
# in reality some is lost from the chiller, particularly for open drive chillers, and
# this can be relatively large (e.g. 5% of electricity input)
cop_c_to_cop_h = function(cop_c_val){return(cop_c_val+1)}
cop_h_to_cop_c = function(cop_h_val){return(cop_h_val-1)}
cop_h_to_cop_hc = function(cop_h_val){return((cop_h_val*2)-1)}
cop_c_to_cop_hc = function(cop_c_val){return((cop_c_val*2)+1)}
cop_hc_to_cop_c = function(cop_hc){return((cop_hc-1)/2)}
cop_hc_to_cop_h = function(cop_hc){return(((cop_hc-1)/2)+1)}

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# interp1 modified to return
# return 0 for values outside of curve range
interp1_zero <- function(x, y, xi = x, ...) {
    yi <- rep(0, length(xi));
    sel <- which(xi >= range(x)[1] & xi <= range(x)[2]);
    yi[sel] <- interp1(x = x, y = y, xi = xi[sel], ...);
    return(yi);
}

# return 0 for values over max x, return 1 for values under min x
interp1_TES_reset <- function(x, y, xi = x, ...) {
    yi <- rep(0, length(xi));
    over <- xi > range(x)[2];
    under <- xi < range(x)[1];
    yi[!(over|under)] <- interp1(x = x, y = y, xi = xi[!(over|under)], ...);
    yi[under] <- 1;
    return(yi);
}