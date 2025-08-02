// REPLICATION PAPER
*Reference: Emerick, K., De Janvry, A., Sadoulet, E., Dar, M. H. (2016). Technological innovations, downside risk, and the modernization of agriculture. American Economic Review, 106(6), 1537-1561
*Link to the publish article: https://www.aeaweb.org/articles?id=10.1257/aer.20150474
*Replication file prepared by: Camila Carrion Castillo, Pauline Belhassine-Paillet, Mariana Navarro Catal  ́an and Isabella Suarez
* Replicating: Table 2, Table 6, Figure 2 & Table 7


//Working Environment 

clear all 

cd "/Users/USER/Documents/Replication Project/" // set directory pathway 

log using "Logs/log_file_name", replace 



////// SUMMARY STATISTICS 
/// Table 2

// setting control variables 
global control "landheld privatetube pipewater refrig telev educ_farm age_farm thatchroof latrine elechh bplcard storsc" 

// importing plot level data
use "Data Bases/r_plotlevel_year1.dta", clear

// creating variables 
egen total_area = sum(areaw_), by(farmer_id)
gen weight=areaw_/total_area
gen flood_weight  = weight*durflood_

// creating dummy variables 
gen low = (typel_==1) // indicator for low land 
gen med = (typel_==2)  // indicator for medium land
gen high = (typel_==3) // indicator for high land

// indicator for each observation equals 1 plot owned by a farmer 
gen nplot=1

// collapsing data into summary statistics for all plots owned by farmer (this will change the data format)
collapse (sum) low med high flood_weight nplot, by(farmer_id) 

// dividing variables low med high by nplot with a loop  
foreach i in low med high  { 
replace `i' = `i' / nplot // turn them into shares
}

// Saving the collapesed data 
tempfile Shareflood
save "Data Bases/Shareflood", replace

// Merging farm level data set with "Shareflood" data
use "Data Bases/r_farmlevel_year1.dta", clear
merge 1:1 farmer_id using "Data Bases/Shareflood"

// Changing acres to hetares
replace landheld= landheld* 0.404686  


// Labeling 
label variable landheld "Land owned in hectaress"
label variable privatetube "HH has private tubewell"
label variable pipewater "HH has piped water"
label variable refrig "HH has regrigerator"
label variable telev "HH has television"
label variable educ_farm "Education of farmer"
label variable age_farm  "Age of farmer"
label variable thatchroof "HH has thatch roof"
label variable latrine "HH has latrine"
label variable elechh "HH has electricity"
label variable bplcard "HH has below poverty line card"
label variable storsc "ST or SC"
label variable farmer_id "Farmers Id"
label variable low "Share plots low land"
label variable med "Share plots medium land"
label variable high "Share plots high land"
label variable flood_weight "Average flood duration in year 1"
drop _merge

// Saving data 
save "Data Bases/Landflood_HHC_Year1.dta", replace

// Means table 
estpost ttest ${control} low med high flood_weight, by(gotminikit) esample // t-test
// Storing means and p-values in local macros
local Control = r(mu_1)
local Treatment = r(mu_2)
local P_value = r(p)
estimates clear // clearing previous estimates 

//creating table using esttab
esttab, cells("mu_1(fmt(%9.3f)) mu_2(fmt(%9.3f)) p(fmt(%9.3f))") /// 
 unstack collabels("Control" "Treatment" "p_value of difference") ///
 label ///
 title(Table 2 - Mean Values of Household Characteristics and Flood Exposure of Plots by treatment status) 

 
 
////// MAIN RESULTS 
/// Table 6 
estimates clear

// COLUMN 1
// importing plot level data (year 1)
use "Data Bases/r_plotlevel_year1.dta", clear

// regressing variable yield on gotminikit 
xi: reg yield gotminikit i.block, vce(cl village_id) // xi indicates regression involves categorical variables (block)
// adding summary statistics for the dependent variable 
estadd ysumm
// storing the results of the regression
estimates store yield_year1

// COLUMNS 2-5
// importing plot level data (year 2)
use "Data Bases/r_plotlevel_year2.dta", clear

// dropping observations where variable kharifcrop_ is not equal to 1.
drop if kharifcrop_ != 1 // only keep rice plots in kharif 2012 for rest of plot level analysis
// creating  dummy =1 if methodw_ is equal to 2 or 3, and = 0 otherwise
gen broadcast=(methodw_==2 | methodw_==3)
replace broadcast = . if methodw_==. // "." if missing variable 
// creating dummy =1 if tvormv_ is equal to 1, and = 0 otherwise 
gen tv = (tvormv_==1)
replace tv = . if tvormv_==. // "." if missing variable 
// generating var for total fertilizer applied per unit of area
gen fert_pa = (kg_dap + kg_potash + kg_urea + kg_gromor ) / (1000*paddyarea)
// fert_pa squared
gen fpa2 = fert_pa*fert_pa
// creating dummy =1 if value of srceln_1 is not missing, and 0 otherwise
gen credituse = (srceln_1 != .)

// labeling
label variable broadcast "Broadcast planting"
label variable tv  "Traditional variety"
label variable fert_pa "Tons of fertilizer per hectare"
label variable fpa2 "Tons of fertilizer per hectare sqr"
label variable credituse  "Has credit"

// saving data for later :) 
save "Data Bases/PlotLevel2_T6", replace

// column 2
xi: reg yield_ha gotminikit i.block, vce(cl village_id) // xi indicates regression involves categorical variables (block)
estimates store yield_year2 // storing the results of the regression
// column 3
xi: reg yield_ha gotminikit broadcast i.block, vce(cl village_id) 
estimates store yield_year2_1
// column 4
xi: reg yield_ha gotminikit broadcast fert_pa fpa2 i.block, vce(cl village_id) 
estimates store yield_year2_2
//column 5 
xi: reg yield_ha gotminikit fert_pa fpa2 broadcast tv irrigated credituse i.block, vce(cl village_id) 
eststo yield_year2_3

// Regression table 
esttab yield_year1 yield_year2 yield_year2_1 yield_year2_2 yield_year2_3, ///
 se nostar brackets /// 
 title(Table 6 - Effects on Productivity) /// 
 mtitles("Year 1" "Year 2" "Year 2" "Year 2" "Year 2") ///
 label ///
 nodepvars ///
 r2 ///
 drop (_Iblock_2 _Iblock_3 _Iblock_4 _Iblock_5 _Iblock_6  _Iblock_7 _Iblock_8)
 
 
/// Figure 2 
estimates clear

// Panel A
// import plot level data year 1
use "Data Bases/r_plotlevel_year1.dta", clear

// plot density 
twoway (kdensity yield if gotminikit==1, lcolor(blue) bw(500) lpattern(dash) lwidth(medthick)) ///
(kdensity yield if gotminikit==0, lcolor(black) bw(500) lwidth(medthick)), scheme(s1color) ///
legend(label(1 "Minikit") label(2 "Non-recipient") pos(2) ring(0) size(small) keygap(0.25) symysize(4) symxsize(6) ///
region(style(none))) xtitle(Yield in kg/hectare) ytitle(Density) title("Panel A: Year 1") name(year1_kd, replace)

// Panle B 
// import plot level data year 2
use "Data Bases/r_plotlevel_year2.dta", clear

// plot k density 
twoway (kdensity yield_ha if yield_ha<=8000 & gotminikit==1, lcolor(blue) bw(500) lpattern(dash) lwidth(medthick)) /// 
(kdensity yield_ha if yield_ha<=8000 & gotminikit==0, lcolor(black) bw(500) lwidth(medthick)), scheme(s1color) ///
legend(label(1 "Minikit") label(2 "Non-recipient") pos(2) ring(0) size(small) keygap(0.25) symysize(4) symxsize(6) ///
region(style(none))) xtitle(Yield in kg/hectare) ytitle(Density) title("Panel B: Year 2") name(year2_kd, replace) 

// combine both kdensity graphs into one 
graph combine year1_kd year2_kd, scheme(s1color) xsize(11) ysize(6) ///
	   title(Figure 2. Kernel Densities of Plot-Level Yield by Treatment Status)    

	   
	   
////// ROBUSTESS
// Table 7 
estimates clear
// Importing data from table 6 
use "Data Bases/PlotLevel2_T6", clear

// acres to hectares
gen area_ha = areaw_*.404686 
// creating indicator for owned land 
gen own = (tenurel_==1) 
// creating dummy equal to 1 if ricevar == "swarna" and 0 otherwise 
gen swarna = (ricevar=="swarna")



// calculating expenditures on different types of fertilizers
gen dap_expend = dapft_*p_dap 
gen mop_expend = mopft_*p_potash 
gen gromor_expend = gromorft_*p_gromor
gen urea_expend = ureaft_*p_urea
gen all_expend = dap_expend+mop_expend+gromor_expend+urea_expend

// labeling 
label variable gotminikit "Original minikit recipient"
label variable area_ha  "Area of plot"
label variable own "Owned land"

// regressing different dependent variables 
foreach i in yield_ha swarna tv broadcast all_expend urea_expend dap_expend mop_expend gromor_expend {
xi: reg `i' gotminikit area_ha own i.landqual_ i.typel_ i.block if yield_ha!=. & ricevar != "swarna-sub1", vce(cl village_id)
estimates store model_`i'
}

// table 
esttab model_yield_ha model_swarna model_tv model_broadcast model_all_expend model_dap_expend, ///
	se nostar brackets /// 
	title(Table 7 - Effects Estimated for Sample of Fields Not Cultivated with Swarna-Sub1) /// 
	mtitles("Yield" "Use Swarna" "Use TV" "Broadcast" "Fertilizer - All" "Fertilizer - DAP") ///
	nodepvars ///
	label /// 
	r2 ///
	drop (_Ilandqual__2 _Ilandqual__3 _Ilandqual__4 _Ilandqual__5 _Itypel__2 _Itypel__3 _Iblock_2 _Iblock_3 _Iblock_4 _Iblock_5 _Iblock_6 _Iblock_7 _Iblock_8) 


log close
	
////// CODEBOOK 
ssc install codebookout
ssc install htmlcb

codebookout "Codebook.xls", replace 
htmlcb, saving(codebook.html) replace

