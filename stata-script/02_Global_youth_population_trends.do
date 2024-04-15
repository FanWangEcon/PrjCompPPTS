/*
Description: 
	Genertaing Figure 1 presenting global youth population trends
Input files: 
	ppts_easia_weuro_world_pchg.csv
Output files: 
	Figure_1_By_region_school_age_population_pct_change.eps
	Figure_1_By_region_school_age_population_pct_change.csv
	Figure_1a_By_region_school_age_population_nostd.eps
	Figure_1a_By_region_school_age_population_nostd.csv
*/

*Data restriction
import delimited "$PPTSdatadir\ppts_easia_weuro_world_pchg.csv", case(lower) clear

destring pchg_interp1 value_interp1, replace force

keep if inlist(year_bins_type, "1940t2020i01")
drop if inlist(variable,"gdp")

drop location_level year_bins_type pchg pchg_interp1 value

duplicates drop location_code variable year_bins value_interp1, force

destring, replace

collapse (sum) value_interp1, by(location_code year_bins variable)

********************************************************************************
*Reshaping
reshape wide value_interp1, i(location_code year_bins) j(variable) string
rename (value*) (*)
rename (_*) (*)
rename (interp1*) (*)
********************************************************************************

* percentage changes
foreach x in youthpop {
	gen `x'2020_ = `x' if year==2020
	bysort location_code: egen `x'2020 = mean(`x'2020) 
	bysort location_code (year): gen pct_change=100*(`x'[_n]-`x'2020)/`x'2020
}
drop *2020*
********************************************************************************
gen region = "Sub-Saharan Africa" if inlist(location_code,"SSF")
replace region = "Middle East and North Africa" if inlist(location_code,"MEA")
replace region = "Latin America and Caribbean" if inlist(location_code,"LCN")
replace region = "North America" if inlist(location_code,"NAC")
replace region = "South Asia" if inlist(location_code,"SAS")
replace region = "Europe and Central Asia" if inlist(location_code,"ECS")
replace region = "East Asia and Pacific" if inlist(location_code,"EAS")
********************************************************************************
********************************************************************************

*Percentage change
sort location_code year
gen mlabel = pct_change if inlist(year,1961, 1970, 1980, 1990, 2000, 2010, 2020)
replace mlabel = round(mlabel, 1)
graph drop _all
twoway (line pct_change year if location_code=="SSF", msize(tiny) lpattern(shortdash) lcolor(red*0.7)) ///
	   (line pct_change year if location_code=="MEA", msize(tiny) lpattern(dash) lcolor(red*1.3)) ///
	   (scatter mlabel year if location_code=="SSF", ms(none) mlabel(mlabel) mlabposition(12) mlabcolor(red*0.7))  ///
	   (scatter mlabel year if location_code=="MEA", ms(none) mlabel(mlabel) mlabposition(12) mlabcolor(red*1.3)) ///
	 ,legend(order(1 "Sub-Saharan Africa" 2 "Middle East & North Africa") r(2) position(6) size(small))  ///
	  xtitle("") ytitle(% change in population ages 0-14 compared to 2020, size(medium)) name(africa) xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

twoway (line pct_change year if location_code=="LCN", msize(tiny) lpattern(shortdash) lcolor(blue*0.6)) ///
	(line pct_change year if location_code=="NAC", msize(tiny) lpattern(dash) lcolor(blue*1.0)) ///
	 (line pct_change year if location_code=="SAS", msize(tiny) lpattern(longdash) lcolor(blue*2.0))  ///
	 (scatter mlabel year if location_code=="LCN", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(blue*0.6))  ///
	 (scatter mlabel year if location_code=="NAC", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(blue*1.0)) ///
	  (scatter mlabel year if location_code=="SAS", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(blue*2.0)) ///
	 ,legend(order(1 "Latin America & Caribbean" 2 "North America" 3 "South Asia") r(2) position(6) size(small) holes(2)) ///
	  xtitle("") ytitle("")  name(america) xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

twoway  (line pct_change year if location_code=="ECS", msize(tiny) lpattern(shortdash) lcolor(green*0.7)) ///	 
	 (line pct_change year if location_code=="EAS", msize(tiny) lpattern(dash) lcolor(green*1.8)) ///
	 (scatter mlabel year if location_code=="ECS", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(green*0.7))  ///	 
	 (scatter mlabel year if location_code=="EAS", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(green*1.8)) ///
	 ,legend( order(1 "Europe & Central Asia"  2 "East Asia & Pacific") r(2) position(6) size(small)) ///
	  xtitle("") ytitle("") name(euro) xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

graph combine africa america euro, r(1) 
graph export "$figuredir\Figure_1_By_region_school_age_population_pct_change.eps", as(eps) name("Graph") preview(off) replace
outsheet location_code region year pct_change using "$figuredir\Figure_1_By_region_school_age_population_pct_change.csv" if inlist(location_code,"SSF","MEA","LCN","NAC","SAS","ECS","EAS"), replace comma


*not-normalized
replace youthpop = youthpop/10000000
replace youthpop = youthpop/10 if location_code=="SSF" | location_code=="MEA" ///
		| location_code=="LCN" | location_code=="SAS" | location_code=="EAS" | location_code=="ECS"
drop mlabel
gen mlabel = youthpop if inlist(year,1961, 1970, 1980, 1990, 2000, 2010, 2020)
replace mlabel = round(mlabel, 0.01)
graph drop _all
twoway (line youthpop year if location_code=="SSF", msize(tiny) lpattern(shortdash) lcolor(red*0.7)) ///
	   (line youthpop year if location_code=="MEA", msize(tiny) lpattern(dash) lcolor(red*1.3)) ///
	   (scatter mlabel year if location_code=="SSF", ms(none) mlabel(mlabel) mlabposition(12) mlabcolor(red*0.7))  ///
	   (scatter mlabel year if location_code=="MEA", ms(none) mlabel(mlabel) mlabposition(12) mlabcolor(red*1.3)) ///
	 ,legend(order(1 "Sub-Saharan Africa" 2 "Middle East & North Africa") r(2) position(6) size(small))  ///
	  xtitle("") ytitle(Population ages 0-14, size(medium)) name(africa) xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

twoway (line youthpop year if location_code=="LCN", msize(tiny) lpattern(shortdash) lcolor(blue*0.6)) ///
	(line youthpop year if location_code=="NAC", msize(tiny) lpattern(dash) lcolor(blue*1.0)) ///
	 (line youthpop year if location_code=="SAS", msize(tiny) lpattern(longdash) lcolor(blue*2.0))  ///
	 (scatter mlabel year if location_code=="LCN", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(blue*0.6))  ///
	 (scatter mlabel year if location_code=="NAC", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(blue*1.0)) ///
	  (scatter mlabel year if location_code=="SAS", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(blue*2.0)) ///
	 ,legend(order(1 "Latin America & Caribbean" 2 "North America" 3 "South Asia") r(2) position(6) size(small) holes(2)) ///
	  xtitle("") ytitle("")  name(america) xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

twoway  (line youthpop year if location_code=="ECS", msize(tiny) lpattern(shortdash) lcolor(green*0.7)) ///	 
	 (line youthpop year if location_code=="EAS", msize(tiny) lpattern(dash) lcolor(green*1.8)) ///
	 (scatter mlabel year if location_code=="ECS", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(green*0.7))  ///	 
	 (scatter mlabel year if location_code=="EAS", ms(none) mlabel(mlabel) mcolor(black) mlabposition(12) mlabcolor(green*1.8)) ///
	 ,legend( order(1 "Europe & Central Asia"  2 "East Asia & Pacific") r(2) position(6) size(small)) ///
	  xtitle("") ytitle("") name(euro) xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

graph combine africa america euro, r(1) caption("Unit: 10 million for North America, 100 million for others", size(small))
graph export "$figuredir\Figure_1a_By_region_school_age_population_nostd.eps", replace
outsheet location_code region year youthpop using "$figuredir\Figure_1a_By_region_school_age_population_nostd.csv" if inlist(location_code,"SSF","MEA","LCN","NAC","SAS","ECS","EAS"), replace comma




