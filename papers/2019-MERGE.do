// =======================================================
// TUS 2019 Mongolia - Master Dataset Merge
// =======================================================

global clean_data "/Users/o/Documents/TUS/clean_data"
clear all

// -------------------------------------------------------
//  Prepare Household Dataset (hh)
// -------------------------------------------------------

use "/Users/o/Downloads/TUS-2019-ruralHH-EN.dta", clear

** Create a unique household id**
egen HHID=concat(HH2 HH3 HH5)
destring HHID, replace

save "/Users/o/Downloads/TUS-2019-ruralHH-EN.dta", replace


use "/Users/o/Downloads/TUS-2019-HL-EN.dta", clear 
// merge with hh rural aimag
merge m:1 HH2 HH3 using "/Users/o/Downloads/TUS-2019-ruralHH-EN.dta"
keep if _merge == 3 // keep only rural hh
drop _merge


**Create a unique personal ID**
egen PID=concat(HH2 HH3 HH5 HL1)
destring PID, replace
duplicates drop PID, force



merge 1:1 PID using "$clean_data/2019TUS_dairyvars.dta"
keep if _merge == 3 // keep only individuals in the dairy data
drop _merge 

/* 

  Result                      Number of obs
    -----------------------------------------
    Not matched                         3,151
        from master                     3,151  (_merge==1)
        from using                          0  (_merge==2)

    Matched                             3,178  (_merge==3)
    -----------------------------------------



*/



drop HH4
ren HH5 HH4
ren HL6 age
ren HL4 gender

// =======================================================
// Reclassify Drinking Water Sources (WS1)
// =======================================================

// Generate a new broad categorical variable
gen water_source = .

// 1. Centralized / Piped into dwelling
replace water_source = 1 if WS1 == 1 

// 2. Public distribution facilities / Kiosks (Connected or unconnected)
replace water_source = 2 if inlist(WS1, 2, 3) 

// 3. Wells & Springs (Protected and unprotected)
replace water_source = 3 if inlist(WS1, 4, 5, 6, 7, 8, 9) 

// 4. Surface water & Rain
replace water_source = 4 if inlist(WS1, 12, 13) 

// 5. Portable, trucked, or bottled water services
replace water_source = 5 if inlist(WS1, 10, 11, 14) 

// Apply labels to the new categorical variable
label define water_source_lbl 1 "Piped/Centralized" ///
                              2 "Public Distribution/Kiosk" ///
                              3 "Wells & Springs" ///
                              4 "Surface Water" ///
                              5 "Delivered/Bottled"
label values water_source water_source_lbl
label variable water_source "Grouped Drinking Water Source"

// -------------------------------------------------------
// Generate Dummy Variables for Regressions
// -------------------------------------------------------
gen piped_dw    = (water_source == 1)
gen piped_pub   = (water_source == 2)
gen well_spring = (water_source == 3)
gen surface     = (water_source == 4)
gen delivered   = (water_source == 5)
 
gen central_heat = (EW2 == 1) // centralized heating system
gen elect_heat = (EW2 == 2) // electric heater
gen central_elect = (EW1 == 1) // centralized electric system 

// Uneducated
gen noedu = (ED3 == 1)

// Primary / Junior
gen primedu = (ED3 == 2)

// Lower Secondary / Base
gen lowsec = (ED3 == 3)

// Upper Secondary / Completed Secondary
gen highsch = (ED3 == 4)

// Associate / Vocational / Technical / Diploma
gen assovoc = inlist(ED3, 5, 6, 7)

// Bachelor's and Higher (Master's, Doctor)
gen bachigh = inlist(ED3, 8, 9, 10)




// =======================================================
// Create Household Composition Variables
// =======================================================

// Generate the roster count
bysort HHID: gen HHsize = _N

// Create temporary dummy markers for individuals in these age brackets
// (Assuming HL6 is the age variable; adjust if necessary)
gen elder_over_60  = (age > 60) if !missing(age)

// Aggregate these markers at the household level
// 'egen total' sums the dummy variables for everyone sharing the same HHID
bysort HH4 HH3: egen hh_elders_over60 = total(elder_over_60)

// Drop the temporary individual markers to keep the dataset clean
drop elder_over_60
ren MS5 hh_kids_under16
// Label the final variables for your regression tables
label variable hh_kids_under16 "Number of children below 12 in household"
label variable hh_elders_over60 "Number of members above 60 in household"




// =======================================================
// Create Composite Sick/Disabled Variable
// =======================================================

// Generate the dummy (default to 0)
gen disabled = 0

// A person is considered disabled if they have "Very difficult" (3) 
// or worse (4) in ANY of the functioning domains.
replace disabled = 1 if inlist(HE2, 3, 4) | ///
                        inlist(HE4, 3, 4) | ///
                        inlist(HE5, 3, 4) | ///
                        inlist(HE6, 3, 4) | ///
                        inlist(HE7, 3, 4)

// Handle missing data: if a respondent has missing data for ALL of these 
// questions, set their disabled status to missing rather than 0.
replace disabled = . if missing(HE2) & missing(HE4) & missing(HE5) & missing(HE6) & missing(HE7)

bysort HHID: egen hh_disabled_members = total(disabled)
label variable hh_disabled_members "Number of members disabled"

drop disabled


// =======================================================
// Create Employed Dummy Variable
// =======================================================

gen employed = 0

// Example: Replace with 1 if they worked for pay, worked on a farm, 
// helped in a family business, or had a job but were absent.
replace employed = 1 if inlist(EP1, 1) | inlist(EP2, 1) | inlist(EP3, 1) | inlist(EP4, 1)

label variable employed "1 = Currently Employed (Any work/profit/absent), 0 = Not Employed"

save "$clean_data/full2019merge.dta", replace

keep HHID PID hh_kids_under16 gender age HH4 employed hh_disabled_members hh_elders_over60 HHsize bachigh assovoc highsch lowsec primedu noedu central_elect elect_heat central_heat delivered surface well_spring piped_pub piped_dw water_source CareWork DomesticWork FarmingHusbandry PaidWork 


save "$clean_data/reg2019merge.dta", replace



// ========================================================================
// Climate Data Processing: Inter-Wave Averages vs Long-Run Baseline
// ========================================================================

global raw_climate "/Users/o/Downloads/TUS/Diana/1. Bases/Climate data"
global clean_data "/Users/o/Documents/TUS/clean_data"

// --- 1. PRECIPITATION (Relative Share) ---
foreach season in jja son mam {
    use "$raw_climate/pcp_`season'_mong.dta", clear
    keep ID NAME YEAR MEAN
    replace ID = 46 if NAME == "O'mnogovi"
    
    // Calculate the 2001-2023 Long-Run Mean 
    bysort ID: egen lr_mean_raw = mean(MEAN) if inrange(YEAR, 2001, 2023)
    bysort ID: egen baseline_pcp = max(lr_mean_raw)
    
    // Group into 4-year inter-wave blocks
    gen survey_wave = .
    replace survey_wave = 2011 if inrange(YEAR, 2008, 2011)
    replace survey_wave = 2015 if inrange(YEAR, 2012, 2015)
    replace survey_wave = 2019 if inrange(YEAR, 2016, 2019)
    replace survey_wave = 2023 if inrange(YEAR, 2020, 2023)
    
    drop if missing(survey_wave)
    
    // Collapse to the wave level (averaging the 4 years of actual climate)
    collapse (mean) wave_avg_pcp = MEAN (first) baseline_pcp, by(ID NAME survey_wave)
    
    // Calculate the Relative Share
    gen pcp_share_`season' = wave_avg_pcp / baseline_pcp
    label variable pcp_share_`season' "`season' Precip: Wave Avg as share of Long-Run Mean"
    
    keep ID NAME survey_wave pcp_share_`season'
    save "$clean_data/pcp_`season'_interwave.dta", replace
}

// --- 2. TEMPERATURE (Relative Share) ---
// Note: If you prefer to keep your original Absolute Deviation method, 
// replace the division (/) with subtraction (-) in the share generation line.
foreach season in jja son mam djf {
    use "$raw_climate/temp_`season'_mong.dta", clear
    keep ID NAME YEAR MEAN
    replace ID = 46 if NAME == "O'mnogovi"
    
    // Calculate the 2001-2023 Long-Run Mean
    bysort ID: egen lr_mean_raw = mean(MEAN) if inrange(YEAR, 2001, 2023)
    bysort ID: egen baseline_temp = max(lr_mean_raw)
    
    // Group into 4-year inter-wave blocks
    gen survey_wave = .
    replace survey_wave = 2011 if inrange(YEAR, 2008, 2011)
    replace survey_wave = 2015 if inrange(YEAR, 2012, 2015)
    replace survey_wave = 2019 if inrange(YEAR, 2016, 2019)
    replace survey_wave = 2023 if inrange(YEAR, 2020, 2023)
    
    drop if missing(survey_wave)
    
    // Collapse to the wave level
    collapse (mean) wave_avg_temp = MEAN (first) baseline_temp, by(ID NAME survey_wave)
    
    // Calculate the Absolute Difference (retaining original variable name for code consistency)
    gen temp_share_`season' = wave_avg_temp - baseline_temp
    label variable temp_share_`season' "`season' Temp: Wave Avg as share of Long-Run Mean"
    
    keep ID NAME survey_wave temp_share_`season'
    save "$clean_data/temp_`season'_interwave.dta", replace
}

// --- 3. NDVI (Relative Share / Standardized Z-Score) ---
// Note: The meeting specified "share" for all variables, but NDVI is often best kept 
// as a Z-score. The code below calculates the wave-average Z-score. 
foreach season in jja son mam {
    capture confirm file "$raw_climate/ndvi_`season'_mong.dta"
    if _rc == 0 {
        use "$raw_climate/ndvi_`season'_mong.dta", clear
        keep ID NAME YEAR MEAN
        replace ID = 46 if NAME == "O'mnogovi"
        
        // Calculate the 2001-2023 Long-Run Mean AND Standard Deviation
        bysort ID: egen lr_mean_raw = mean(MEAN) if inrange(YEAR, 2001, 2023)
        bysort ID: egen lr_sd_raw = sd(MEAN) if inrange(YEAR, 2001, 2023)
        
        bysort ID: egen baseline_ndvi = max(lr_mean_raw)
        bysort ID: egen baseline_sd = max(lr_sd_raw)
        
        // Group into 4-year inter-wave blocks
        gen survey_wave = .
        replace survey_wave = 2011 if inrange(YEAR, 2008, 2011)
        replace survey_wave = 2015 if inrange(YEAR, 2012, 2015)
        replace survey_wave = 2019 if inrange(YEAR, 2016, 2019)
        replace survey_wave = 2023 if inrange(YEAR, 2020, 2023)
        
        drop if missing(survey_wave)
        
        // Collapse to the wave level
        collapse (mean) wave_avg_ndvi = MEAN (first) baseline_ndvi baseline_sd, by(ID NAME survey_wave)
        
        // Calculate the Inter-Wave Z-Score
        gen ndvi_z_`season' = (wave_avg_ndvi - baseline_ndvi) / baseline_sd
        label variable ndvi_z_`season' "`season' NDVI: Wave Avg Z-Score from Long-Run Mean"
        
        keep ID NAME survey_wave ndvi_z_`season'
        save "$clean_data/ndvi_`season'_interwave.dta", replace
    }
}



// ========================================================================
// Master Merge: Combine All Climate Panels into One File
// ========================================================================

// Start with one of the datasets as the base
use "$clean_data/pcp_jja_interwave.dta", clear

// Merge Precipitation
merge 1:1 ID survey_wave using "$clean_data/pcp_son_interwave.dta", nogen
merge 1:1 ID survey_wave using "$clean_data/pcp_mam_interwave.dta", nogen

// Merge Temperature
foreach season in jja son mam djf {
    merge 1:1 ID survey_wave using "$clean_data/temp_`season'_interwave.dta", nogen
}

// Merge NDVI
foreach season in jja son mam {
    capture confirm file "$clean_data/ndvi_`season'_interwave.dta"
    if _rc == 0 {
        merge 1:1 ID survey_wave using "$clean_data/ndvi_`season'_interwave.dta", nogen
    }
}

sort ID survey_wave

ren ID HH4 
// Save the final unified climate file
save "$clean_data/climate_anomalies.dta", replace

keep if survey_wave == 2019

save "$clean_data/climate2019_anomalies.dta", replace


///--------------------------------
/// MERGE CLIMATE AND LABOR DATA 
///--------------------------------

clear all
set more off
// 2019
use "$clean_data/reg2019merge.dta" // 2019
// Sort by household ID and count the number of observations (_N) per household
merge m:1 HH4 using "/Users/o/Downloads/laborforce2019.dta" // merge labor force data 2019

/* 
     Result                      Number of obs
    -----------------------------------------
    Not matched                             0
    Matched                             3,178  (_merge==3)
    -----------------------------------------


*/ 

drop _merge
merge m:1 HH4 using "$clean_data/climate2019_anomalies.dta" // merge climate data 2019

/*
   Result                      Number of obs
    -----------------------------------------
    Not matched                            14
        from master                         0  (_merge==1)
        from using                         14  (_merge==2)

    Matched                             3,178  (_merge==3)
    -----------------------------------------

	. tab HH4 if _merge != 3

Province/ca |
 pital city |
   name and |
       code |      Freq.     Percent        Cum.
------------+-----------------------------------
Ulaanbaatar |          1        7.14        7.14
         22 |          1        7.14       14.29
         42 |          1        7.14       21.43
         43 |          1        7.14       28.57
         44 |          1        7.14       35.71
         45 |          1        7.14       42.86
         46 |          1        7.14       50.00
         61 |          1        7.14       57.14
         62 |          1        7.14       64.29
         64 |          1        7.14       71.43
         65 |          1        7.14       78.57
         81 |          1        7.14       85.71
         83 |          1        7.14       92.86
         85 |          1        7.14      100.00
------------+-----------------------------------

	
*/


keep if _merge == 3
drop _merge

/* =====================================================================
   FULL 4-EQUATION TOBIT SUR SYSTEM (cmp)
====================================================================== */

// 1. Set up Tobit indicators (Left-censored at 0 hours)
cmp setup
cap drop ind_care ind_house ind_herd ind_paid
gen ind_care  = cond(CareWork == 0, $cmp_left, $cmp_cont)
gen ind_house = cond(DomesticWork == 0, $cmp_left, $cmp_cont)
gen ind_herd  = cond(FarmingHusbandry == 0, $cmp_left, $cmp_cont)
gen ind_paid  = cond(PaidWork == 0, $cmp_left, $cmp_cont)

/* =====================================================================
   IHS TRANSFORMED
====================================================================== */

// 1. Generate the IHS transformed dependent variables
cap drop ihs_care ihs_house ihs_herd ihs_paid
gen ihs_care  = asinh(CareWork)
gen ihs_house = asinh(DomesticWork)
gen ihs_herd  = asinh(FarmingHusbandry)
gen ihs_paid  = asinh(PaidWork)

// 2. Set up Tobit indicators (Left-censored at 0)
// Since asinh(0) = 0, the censoring point remains exactly 0.
cmp setup
cap drop ind_care ind_house ind_herd ind_paid
gen ind_care  = cond(ihs_care == 0, $cmp_left, $cmp_cont)
gen ind_house = cond(ihs_house == 0, $cmp_left, $cmp_cont)
gen ind_herd  = cond(ihs_herd == 0, $cmp_left, $cmp_cont)
gen ind_paid  = cond(ihs_paid == 0, $cmp_left, $cmp_cont)



/* =====================================================================
   1. EXPORT SUMMARY IHS TUS vars
====================================================================== */

global TUS_vars PaidWork FarmingHusbandry DomesticWork CareWork ihs_paid ihs_herd ihs_house ihs_care

eststo clear
quietly estpost summarize $TUS_vars
esttab using "$clean_data/TUS_statistics_model2019.tex", replace ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.3f)) sd(fmt(%9.3f)) min(fmt(%9.3f)) max(fmt(%9.3f))") ///
    label booktabs nonumber nomtitle ///
    collabels("Obs" "Mean" "Std. Dev." "Min" "Max") ///
    title("Summary Statistics of TUS Variables 2015")



/* =====================================================================
   1. EXPORT SUMMARY 
====================================================================== */

global dep_vars_num pcp_share_* temp_share_* ndvi_z_* laborParticipRateTotal UnemplyRte HHsize hh_kids_under16 hh_elders_over60 hh_disabled_members livestocknumbers age

eststo clear
quietly estpost summarize $dep_vars_num
esttab using "$clean_data/summary_statistics_model2019.tex", replace ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.3f)) sd(fmt(%9.3f)) min(fmt(%9.3f)) max(fmt(%9.3f))") ///
    label booktabs nonumber nomtitle ///
    collabels("Obs" "Mean" "Std. Dev." "Min" "Max") ///
    title("Summary Statistics of Model Variables 2015")


/* =====================================================================
   1. EXPORT CATEGORICAL TABULATIONS 
====================================================================== */

local cat_vars water_source central_heat central_elect noedu primedu lowsec highsch assovoc bachigh employed gender
					
foreach var of local cat_vars {
    eststo clear
    quietly estpost tabulate `var'
    
    esttab using "$clean_data/tabs19.tex", append ///
        cells("b(label(Freq.) fmt(%9.0fc)) pct(label(Percent) fmt(2)) cumpct(label(Cum.) fmt(2))") ///
        nonumber nomtitle label booktabs ///
        title("Distribution of `var'_2015")
}

/* =====================================================================
   2. FULL SAMPLE TOBIT MODEL & MARGINAL EFFECTS
====================================================================== */

global pcp_climate pcp_share_mam pcp_share_jja pcp_share_son
global ndvi_climate ndvi_z_mam ndvi_z_jja ndvi_z_son
global temp_climate1 temp_share_son temp_share_djf temp_share_jja 
global temp_climate2 temp_share_mam temp_share_djf temp_share_jja

global exog_vars i.gender piped_dw piped_pub well_spring delivered central_heat central_elect primedu lowsec highsch assovoc bachigh HHsize hh_kids_under16 hh_elders_over60 hh_disabled_members employed laborParticipRateTotal UnemplyRte 

/* =====================================================================
   Model 1: Precipitation Shocks
====================================================================== */

// Define exogenous variables 
global exog_vars i.gender piped_dw piped_pub well_spring delivered central_heat central_elect primedu lowsec highsch assovoc bachigh HHsize hh_kids_under16 hh_elders_over60 hh_disabled_members employed laborParticipRateTotal UnemplyRte $pcp_climate



// Estimate the full system and save it to Stata's internal memory
 cmp (ihs_care = $exog_vars) ///
            (ihs_house = $exog_vars) ///
            (ihs_herd = $exog_vars) ///
            (ihs_paid = $exog_vars), ///
            indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_full

// Calculate Average Marginal Effects (AME) for each equation
// We only calculate dydx for the climate variables to save processing time
eststo clear

disp "=== CALCULATING MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($pcp_climate) predict(eq(ihs_care)) post
estadd local controls "Yes"
eststo m_care

disp "=== CALCULATING MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($pcp_climate) predict(eq(ihs_house)) post
estadd local controls "Yes"
eststo m_house

disp "=== CALCULATING MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx($pcp_climate) predict(eq(ihs_herd)) post
estadd local controls "Yes"
eststo m_herd

disp "=== CALCULATING MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($pcp_climate) predict(eq(ihs_paid)) post
estadd local controls "Yes"
eststo m_paid

// Export the Marginal Effects to a single LaTeX table
esttab m_care m_house m_herd m_paid ///
    using "$clean_data/cmp_margins_pcp_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: Climate Shocks on Time Use (Full Sample)") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls") ///
    addnotes("Average marginal effects shown. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	
	
	


// 1. Calculate Average Marginal Effects for ALL variables
eststo clear

disp "=== CALCULATING FULL MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_care)) post
eststo m_care_full

disp "=== CALCULATING FULL MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_house)) post
eststo m_house_full

disp "=== CALCULATING FULL MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_herd)) post
eststo m_herd_full

disp "=== CALCULATING FULL MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_paid)) post
eststo m_paid_full

// 2. Export the full Marginal Effects to LaTeX (no 'keep' command)
esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_fullpcp_results2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: Precipitation Shocks and Controls") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    addnotes("Average marginal effects for all variables. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

	

	
/* =====================================================================
  Model 2: NDVI / Vegetation Shocks
====================================================================== */
	
// Define exogenous variables 
global exog_vars i.gender piped_dw piped_pub well_spring delivered central_heat central_elect primedu lowsec highsch assovoc bachigh HHsize hh_kids_under16 hh_elders_over60 hh_disabled_members employed laborParticipRateTotal UnemplyRte $ndvi_climate



// Estimate the full system and save it to Stata's internal memory
disp "=== ESTIMATING FULL SUR TOBIT SYSTEM (THIS MAY TAKE A MINUTE) ==="
 cmp (ihs_care = $exog_vars) ///
            (ihs_house = $exog_vars) ///
            (ihs_herd = $exog_vars) ///
            (ihs_paid = $exog_vars), ///
            indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_full

// Calculate Average Marginal Effects (AME) for each equation
// We only calculate dydx for the climate variables to save processing time
eststo clear

disp "=== CALCULATING MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($ndvi_climate) predict(eq(ihs_care)) post
estadd local controls "Yes"
eststo m_care

disp "=== CALCULATING MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($ndvi_climate) predict(eq(ihs_house)) post
estadd local controls "Yes"
eststo m_house

disp "=== CALCULATING MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx($ndvi_climate) predict(eq(ihs_herd)) post
estadd local controls "Yes"
eststo m_herd

disp "=== CALCULATING MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($ndvi_climate) predict(eq(ihs_paid)) post
estadd local controls "Yes"
eststo m_paid

// Export the Marginal Effects to a single LaTeX table
esttab m_care m_house m_herd m_paid ///
    using "$clean_data/cmp_margins_ndvi_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: Climate Shocks on Time Use (Full Sample)") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls") ///
    addnotes("Average marginal effects shown. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	
	
	


// 1. Calculate Average Marginal Effects for ALL variables
eststo clear

disp "=== CALCULATING FULL MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_care)) post
eststo m_care_full

disp "=== CALCULATING FULL MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_house)) post
eststo m_house_full

disp "=== CALCULATING FULL MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_herd)) post
eststo m_herd_full

disp "=== CALCULATING FULL MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_paid)) post
eststo m_paid_full

// 2. Export the full Marginal Effects to LaTeX (no 'keep' command)
esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_full_ndviresults2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: Climate Shocks and Controls") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    addnotes("Average marginal effects for all variables. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

	
	
	
	
	
	
/* =====================================================================
 Model 3A: Temperature Shocks (Autumn/Winter Focus)
====================================================================== */
	
	
// Define exogenous variables 
global exog_vars i.gender piped_dw piped_pub well_spring delivered central_heat central_elect primedu lowsec highsch assovoc bachigh HHsize hh_kids_under16 hh_elders_over60 hh_disabled_members employed laborParticipRateTotal UnemplyRte $temp_climate1


// Estimate the full system and save it to Stata's internal memory
disp "=== ESTIMATING FULL SUR TOBIT SYSTEM (THIS MAY TAKE A MINUTE) ==="
 cmp (ihs_care = $exog_vars) ///
            (ihs_house = $exog_vars) ///
            (ihs_herd = $exog_vars) ///
            (ihs_paid = $exog_vars), ///
            indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_full

// Calculate Average Marginal Effects (AME) for each equation
// We only calculate dydx for the climate variables to save processing time
eststo clear

disp "=== CALCULATING MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate1) predict(eq(ihs_care)) post
estadd local controls "Yes"
eststo m_care

disp "=== CALCULATING MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate1) predict(eq(ihs_house)) post
estadd local controls "Yes"
eststo m_house

disp "=== CALCULATING MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate1) predict(eq(ihs_herd)) post
estadd local controls "Yes"
eststo m_herd

disp "=== CALCULATING MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate1) predict(eq(ihs_paid)) post
estadd local controls "Yes"
eststo m_paid

// Export the Marginal Effects to a single LaTeX table
esttab m_care m_house m_herd m_paid ///
    using "$clean_data/cmp_margins_temp1_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: Climate Shocks on Time Use (Full Sample)") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls") ///
    addnotes("Average marginal effects shown. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	
	
	


// 1. Calculate Average Marginal Effects for ALL variables
eststo clear

disp "=== CALCULATING FULL MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_care)) post
eststo m_care_full

disp "=== CALCULATING FULL MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_house)) post
eststo m_house_full

disp "=== CALCULATING FULL MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_herd)) post
eststo m_herd_full

disp "=== CALCULATING FULL MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_paid)) post
eststo m_paid_full

// 2. Export the full Marginal Effects to LaTeX (no 'keep' command)
esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_full_temp1results2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: Climate Shocks and Controls") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    addnotes("Average marginal effects for all variables. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

	
	

	
/* =====================================================================
 Model 3B: Temperature Shocks (Spring/Winter Focus)
====================================================================== */
	
	
// Define exogenous variables 
global exog_vars i.gender piped_dw piped_pub well_spring delivered central_heat central_elect primedu lowsec highsch assovoc bachigh HHsize hh_kids_under16 hh_elders_over60 hh_disabled_members employed laborParticipRateTotal UnemplyRte $temp_climate2


// Estimate the full system and save it to Stata's internal memory
disp "=== ESTIMATING FULL SUR TOBIT SYSTEM (THIS MAY TAKE A MINUTE) ==="
 cmp (ihs_care = $exog_vars) ///
            (ihs_house = $exog_vars) ///
            (ihs_herd = $exog_vars) ///
            (ihs_paid = $exog_vars), ///
            indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_full

// Calculate Average Marginal Effects (AME) for each equation
// We only calculate dydx for the climate variables to save processing time
eststo clear

disp "=== CALCULATING MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate2) predict(eq(ihs_care)) post
estadd local controls "Yes"
eststo m_care

disp "=== CALCULATING MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate2) predict(eq(ihs_house)) post
estadd local controls "Yes"
eststo m_house

disp "=== CALCULATING MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate2) predict(eq(ihs_herd)) post
estadd local controls "Yes"
eststo m_herd

disp "=== CALCULATING MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx($temp_climate2) predict(eq(ihs_paid)) post
estadd local controls "Yes"
eststo m_paid

// Export the Marginal Effects to a single LaTeX table
esttab m_care m_house m_herd m_paid ///
    using "$clean_data/cmp_margins_temp2_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: Climate Shocks on Time Use (Full Sample)") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls") ///
    addnotes("Average marginal effects shown. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	
	

// 1. Calculate Average Marginal Effects for ALL variables
eststo clear

disp "=== CALCULATING FULL MARGINS: CAREWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_care)) post
eststo m_care_full

disp "=== CALCULATING FULL MARGINS: HOUSEWORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_house)) post
eststo m_house_full

disp "=== CALCULATING FULL MARGINS: HERDING ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_herd)) post
eststo m_herd_full

disp "=== CALCULATING FULL MARGINS: PAID WORK ==="
quietly estimates restore cmp_full
quietly margins, dydx(*) predict(eq(ihs_paid)) post
eststo m_paid_full

// 2. Export the full Marginal Effects to LaTeX (no 'keep' command)
esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_full_temp2results2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: Climate Shocks and Controls") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    addnotes("Average marginal effects for all variables. Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

	
save  "$clean_data/reg2019main.dta" 
	
	
	
	

/* =====================================================================
   FULL SAMPLE TOBIT MODEL GLOBALS
====================================================================== */

global pcp_climate pcp_share_mam pcp_share_jja pcp_share_son
global ndvi_climate ndvi_z_mam ndvi_z_jja ndvi_z_son
global temp_climate1 temp_share_son temp_share_djf temp_share_jja
global temp_climate2 temp_share_mam temp_share_djf temp_share_jja

global exog_vars i.gender piped_dw piped_pub well_spring delivered central_heat central_elect primedu lowsec highsch assovoc bachigh HHsize hh_kids_under16 hh_elders_over60 hh_disabled_members employed laborParticipRateTotal UnemplyRte

/* =====================================================================
   Model 1: Precipitation Shocks
====================================================================== */

disp "=== ESTIMATING MODEL 1: PRECIPITATION SHOCKS ==="
cmp (ihs_care = $exog_vars $pcp_climate) ///
    (ihs_house = $exog_vars $pcp_climate) ///
    (ihs_herd = $exog_vars $pcp_climate) ///
    (ihs_paid = $exog_vars $pcp_climate), ///
    indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_model1

// --- 1A. Climate-Only Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING CLIMATE MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model1
    foreach var of global pcp_climate {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx($pcp_climate) predict(eq(`eq')) post
    estadd local controls "Yes"
    foreach var of global pcp_climate {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_clim
}

esttab m_care_clim m_house_clim m_herd_clim m_paid_clim ///
    using "$clean_data/cmp_margins_pcp_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: Precipitation Shocks on Time Use \label{tab:cmp_margins_pcp}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls" ///
            "p_boot_pcp_share_mam Wild Boot p (MAM)" ///
            "p_boot_pcp_share_jja Wild Boot p (JJA)" ///
            "p_boot_pcp_share_son Wild Boot p (SON)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")

// --- 1B. Full Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING FULL MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model1
    foreach var of global pcp_climate {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx(*) predict(eq(`eq')) post
    foreach var of global pcp_climate {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_full
}

esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_fullpcp_results2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: Precipitation Shocks and Controls \label{tab:cmp_margins_all_pcp}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("p_boot_pcp_share_mam Wild Boot p (MAM)" ///
            "p_boot_pcp_share_jja Wild Boot p (JJA)" ///
            "p_boot_pcp_share_son Wild Boot p (SON)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")


/* =====================================================================
  Model 2: NDVI / Vegetation Shocks
====================================================================== */

disp "=== ESTIMATING MODEL 2: NDVI SHOCKS ==="
cmp (ihs_care = $exog_vars $ndvi_climate) ///
    (ihs_house = $exog_vars $ndvi_climate) ///
    (ihs_herd = $exog_vars $ndvi_climate) ///
    (ihs_paid = $exog_vars $ndvi_climate), ///
    indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_model2

// --- 2A. Climate-Only Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING CLIMATE MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model2
    foreach var of global ndvi_climate {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx($ndvi_climate) predict(eq(`eq')) post
    estadd local controls "Yes"
    foreach var of global ndvi_climate {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_clim
}

esttab m_care_clim m_house_clim m_herd_clim m_paid_clim ///
    using "$clean_data/cmp_margins_ndvi_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: NDVI Shocks on Time Use \label{tab:cmp_margins_ndvi}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls" ///
            "p_boot_ndvi_z_mam Wild Boot p (MAM)" ///
            "p_boot_ndvi_z_jja Wild Boot p (JJA)" ///
            "p_boot_ndvi_z_son Wild Boot p (SON)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")

// --- 2B. Full Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING FULL MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model2
    foreach var of global ndvi_climate {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx(*) predict(eq(`eq')) post
    foreach var of global ndvi_climate {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_full
}

esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_full_ndviresults2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: NDVI Shocks and Controls \label{tab:cmp_margins_all_ndvi}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("p_boot_ndvi_z_mam Wild Boot p (MAM)" ///
            "p_boot_ndvi_z_jja Wild Boot p (JJA)" ///
            "p_boot_ndvi_z_son Wild Boot p (SON)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")


/* =====================================================================
 Model 3A: Temperature Shocks (Autumn/Winter Focus)
====================================================================== */

disp "=== ESTIMATING MODEL 3A: TEMP SHOCKS (SON/DJF/JJA/MAM) ==="
cmp (ihs_care = $exog_vars $temp_climate1) ///
    (ihs_house = $exog_vars $temp_climate1) ///
    (ihs_herd = $exog_vars $temp_climate1) ///
    (ihs_paid = $exog_vars $temp_climate1), ///
    indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_model3a

// --- 3A. Climate-Only Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING CLIMATE MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model3a
    foreach var of global temp_climate1 {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx($temp_climate1) predict(eq(`eq')) post
    estadd local controls "Yes"
    foreach var of global temp_climate1 {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_clim
}

esttab m_care_clim m_house_clim m_herd_clim m_paid_clim ///
    using "$clean_data/cmp_margins_temp1_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: Temperature Shocks (Focus 1) on Time Use \label{tab:cmp_margins_temp1}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls" ///
            "p_boot_temp_share_son Wild Boot p (SON)" ///
            "p_boot_temp_share_djf Wild Boot p (DJF)" ///
            "p_boot_temp_share_jja Wild Boot p (JJA)" ///
            "p_boot_temp_share_mam Wild Boot p (MAM)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")

// --- 3A. Full Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING FULL MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model3a
    foreach var of global temp_climate1 {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx(*) predict(eq(`eq')) post
    foreach var of global temp_climate1 {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_full
}

esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_full_temp1results2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: Temperature Shocks (Focus 1) and Controls \label{tab:cmp_margins_all_temp1}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("p_boot_temp_share_son Wild Boot p (SON)" ///
            "p_boot_temp_share_djf Wild Boot p (DJF)" ///
            "p_boot_temp_share_jja Wild Boot p (JJA)" ///
            "p_boot_temp_share_mam Wild Boot p (MAM)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")


/* =====================================================================
 Model 3B: Temperature Shocks (Spring/Winter Focus)
====================================================================== */

disp "=== ESTIMATING MODEL 3B: TEMP SHOCKS (MAM/DJF/JJA) ==="
cmp (ihs_care = $exog_vars $temp_climate2) ///
    (ihs_house = $exog_vars $temp_climate2) ///
    (ihs_herd = $exog_vars $temp_climate2) ///
    (ihs_paid = $exog_vars $temp_climate2), ///
    indicators(ind_care ind_house ind_herd ind_paid) vce(cluster HHID)

estimates store cmp_model3b

// --- 3B. Climate-Only Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING CLIMATE MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model3b
    foreach var of global temp_climate2 {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx($temp_climate2) predict(eq(`eq')) post
    estadd local controls "Yes"
    foreach var of global temp_climate2 {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_clim
}

esttab m_care_clim m_house_clim m_herd_clim m_paid_clim ///
    using "$clean_data/cmp_margins_temp2_climate2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Average Marginal Effects: Temperature Shocks (Focus 2) on Time Use \label{tab:cmp_margins_temp2}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("controls Controls" ///
            "p_boot_temp_share_mam Wild Boot p (MAM)" ///
            "p_boot_temp_share_djf Wild Boot p (DJF)" ///
            "p_boot_temp_share_jja Wild Boot p (JJA)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")


// --- 3B. Full Margins Table ---
eststo clear

foreach pair in "care ihs_care" "house ihs_house" "herd ihs_herd" "paid ihs_paid" {
    tokenize "`pair'"
    local dep "`1'"
    local eq "`2'"
    
    disp "=== CALCULATING FULL MARGINS & BOOTSTRAP: `dep' ==="
    
    quietly estimates restore cmp_model3b
    foreach var of global temp_climate2 {
        quietly boottest [`eq']`var', cluster(HH4) weighttype(webb) reps(999) seed(12345)
        local p_`var' = r(p)
    }
    
    quietly margins, dydx(*) predict(eq(`eq')) post
    foreach var of global temp_climate2 {
        estadd scalar p_boot_`var' = `p_`var''
    }
    eststo m_`dep'_full
}

esttab m_care_full m_house_full m_herd_full m_paid_full ///
    using "$clean_data/cmp_margins_full_temp2results2019.tex", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) booktabs label ///
    title("Full Average Marginal Effects: Temperature Shocks (Focus 2) and Controls \label{tab:cmp_margins_all_temp2}") ///
    mtitles("Carework" "Housework" "Herding" "Paid Work") ///
    scalars("p_boot_temp_share_mam Wild Boot p (MAM)" ///
            "p_boot_temp_share_djf Wild Boot p (DJF)" ///
            "p_boot_temp_share_jja Wild Boot p (JJA)") ///
    sfmt(3) /// 
    addnotes("Average marginal effects shown. Robust standard errors clustered at HHID in parentheses." ///
             "Wild cluster bootstrap p-values (Webb weights, 14 clusters at HH4 level) reported below scalars." ///
             "*** p<0.01, ** p<0.05, * p<0.1")
