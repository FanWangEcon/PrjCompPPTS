/*
Description: 
	Generating Figure 2 presenting gloabal education responses
Input files: 
	ppts_easia_weuro_world_elas_interp1.csv
Output files:
	Figure_2_teacher_percentage_boundary_weighted_nolabel.eps
	Figure_2a_teacher_percentage_boundary_weighted.eps
	Figure_2b_teacher_percentage_boundary_weighted_subregion.eps
	Figure_2_teacher_percentage_boundary_weighted_nolabel.csv
	Figure_2a_teacher_percentage_boundary_weighted.csv
	Figure_2b_teacher_percentage_boundary_weighted_subregion.csv
Note: 
*/
clear 
clear mata 
clear matrix 
set more off 

********************************************************************************
*Data restriction
import delimited "$PPTSdatadir\ppts_easia_weuro_world_elas_interp1.csv", case(lower) clear

destring pchg_interp1, replace force

keep if inlist(location_level, "country", "multicountry")
keep if inlist(year_bins_type, "1920t2020i20")
drop if inlist(variable,"gdp")
drop if inlist(year_bins,"1921-1940", "1941-1960", "1961-1980")

drop year_bins_type variable_numerator pchg_interp1_numerator elasticity_interp1

duplicates drop location_code variable year_bins pchg_interp1, force

encode year_bins, gen(year_bins_n)
drop year_bins

replace pchg_interp1 = pchg_interp1*100
********************************************************************************
*Reshaping
reshape wide pchg_interp1 value_interp1, i(location_code year_bins_n) j(variable) string
reshape wide pchg_interp1student pchg_interp1teacher pchg_interp1youthpop pchg_interp1school value_interp1school value_interp1student value_interp1teacher value_interp1youthpop, i(location_code ) j(year_bins_n)

********************************************************************************
*Having location_code
merge m:1 location_code using "$worldbankdir\country_code_region.dta", keep(1 3) nogen
replace region = "East Asia and Pacific" if region == "East Asia & Pacific"
replace region = "Europe and Central Asia" if region == "Europe & Central Asia"
replace region = "Latin America and Caribbean" if region == "Latin America & Caribbean"
replace region = "Middle East and North Africa" if region == "Middle East & North Africa"
   
gen category=.
replace category=1 if inlist(region, "Middle East and North Africa", "Sub-Saharan Africa")
replace category=2 if inlist(region, "Latin America and Caribbean", "North America", "South Asia")
replace category=3 if inlist(region, "Europe and Central Asia", "East Asia and Pacific")
gen category_l = "Africa" if inlist(category,1)
replace category_l = "America and South Asia" if inlist(category,2)
replace category_l = "East Asia and Europe" if inlist(category,3)
********************************************************************************

** Global
preserve 
foreach x in pchg_interp1teacher1  pchg_interp1teacher2 {
	replace `x' = 200 if `x'>200 & `x'!=.
}
foreach x in pchg_interp1youthpop1 pchg_interp1youthpop2 {
	replace `x' = 100 if `x'>100 & `x'!=.
}

* Keeping boundary & weighted & W/O label & unbalanced
graph drop _all	
twoway (scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash))  ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 1980-2000") ytitle("% change in teachers 1980-2000") ///
		legend(order(1 "Africa & Middle East" 2 "Americas & South Asia" 3 "East Asia & Pacific and Europe & Central Asia") r(1) position() size(small)) name(first) ///
		xscale(range(-50 100)) xlabel(,labsize(medium_small) ) yscale(range(-50 200)) ylabel(,labsize(medium_small))  

twoway (scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash)) ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 2000-2020") ytitle("% change in teachers 2000-2020") ///
		legend(off) name(second) xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))

grc1leg first second 		
graph export "$figuredir\Figure_2c_teacher_percentage_boundary_weighted_nolabel_unbalanced.eps", replace		
outsheet location_code region category_l pchg_interp1teacher1 pchg_interp1youthpop1 pchg_interp1teacher2 pchg_interp1youthpop2 value_interp1teacher1 value_interp1teacher2 value_interp1youthpop1 value_interp1youthpop2 using "$figuredir\Figure_2c_teacher_percentage_boundary_weighted_nolabel_unbalanced.csv" if inlist(category,1,2,3), replace comma
 
* Keeping boundary & weighted & W/O label
keep if pchg_interp1teacher1!=. & pchg_interp1youthpop1 !=. & value_interp1teacher1 !=. & pchg_interp1teacher2 !=. & pchg_interp1youthpop2 !=. & value_interp1teacher2 !=. //balanced
graph drop _all	
twoway (scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash))  ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 1980-2000") ytitle("% change in teachers 1980-2000") ///
		legend(order(1 "Africa & Middle East" 2 "Americas & South Asia" 3 "East Asia & Pacific and Europe & Central Asia") r(1) position() size(small)) name(first) ///
		xscale(range(-50 100)) xlabel(,labsize(medium_small) ) yscale(range(-50 200)) ylabel(,labsize(medium_small))  

twoway (scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2], legend(off) ms(i) mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash)) ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 2000-2020") ytitle("% change in teachers 2000-2020") ///
		legend(off) name(second) xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))

grc1leg first second 		
graph export "$figuredir\Figure_2_teacher_percentage_boundary_weighted_nolabel.eps", replace		
outsheet location_code region category_l pchg_interp1teacher1 pchg_interp1youthpop1 pchg_interp1teacher2 pchg_interp1youthpop2 value_interp1teacher1 value_interp1teacher2 value_interp1youthpop1 value_interp1youthpop2 using "$figuredir\Figure_2_teacher_percentage_boundary_weighted_nolabel.csv" if inlist(category,1,2,3), replace comma

* Keeping boundary & weighted & size of country
graph drop _all	
twoway (scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1], legend(off) ms(i) mlabel(location_code) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1], legend(off) ms(i) mlabel(location_code) mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1], legend(off) ms(i) mlabel(location_code) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash))  ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 1980-2000") ytitle("% change in teachers 1980-2000") ///
		legend(order(1 "Africa" 2 "Americas & South Asia" 3 "East Asia & Pacific and Europe & Central Asia") r(1) position() size(small)) name(first) ///
		xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))  

twoway (scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2], legend(off) ms(i) mlabel(location_code) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2], legend(off) ms(i) mlabel(location_code) mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2], legend(off) ms(i) mlabel(location_code) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash)) ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 2000-2020") ytitle("% change in teachers 2000-2020") ///
		legend(off) name(second) xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))

grc1leg first second 		
graph export "$figuredir\Figure_2a_teacher_percentage_boundary_weighted.eps", replace		
restore

* Keeping boundary by subregion
preserve 
keep if location_level=="multicountry"
keep if inlist(location_code, "SSF", "MEA", "LCN", "NAC", "SAS", "EAS", "ECS")
replace category=1 if inlist(location_code, "SSF", "MEA")
replace category=2 if inlist(location_code, "LCN", "NAC", "SAS")
replace category=3 if inlist(location_code, "EAS", "ECS")
gen label = "East Asia & Pacific" if location_code == "EAS"
replace label = "Europe and Central Asia" if location_code == "ECS"
replace label = "Latin America & Caribbean" if location_code == "LCN"
replace label = "North America" if location_code == "NAC"
replace label = "South Asia" if location_code == "SAS"
replace label = "Sub-Saharan Africa" if location_code == "SSF"
replace label = "Middle East & North Africa" if location_code == "MEA"		

graph drop _all	
twoway (scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==1 [w=value_interp1teacher1], legend(off) ms(i) mlabel(label) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==2 [w=value_interp1teacher1], legend(off) ms(i) mlabel(label) mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher1 pchg_interp1youthpop1 if category==3 [w=value_interp1teacher1], legend(off) ms(i) mlabel(label) mlabsize(vsmall) mcolor(green) mlabcolor(green)) /// 
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash))  ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 1980-2000") ytitle("% change in teachers 1980-2000") ///
		legend(order(1 "Africa" 2 "Americas & South Asia" 3 "East Asia & Pacific and Europe & Central Asia") r(1) position() size(small)) name(first) ///
		xscale() xlabel(,labsize(medium_small)) yscale() ylabel(,labsize(medium_small))  

twoway (scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==1 [w=value_interp1teacher2], legend(off) ms(i) mlabel(label) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==2 [w=value_interp1teacher2], legend(off) ms(i) mlabel(label) mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1teacher2 pchg_interp1youthpop2 if category==3 [w=value_interp1teacher2], legend(off) ms(i) mlabel(label) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash)) ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change in school age population 2000-2020") ytitle("% change in teachers 2000-2020") ///
		legend(off) name(second) xscale() xlabel(,labsize(medium_small)) yscale() ylabel(,labsize(medium_small))

grc1leg first second 		
graph export "$figuredir\Figure_2b_teacher_percentage_boundary_weighted_subregion.eps", replace		

replace label = "East Asia and Pacific" if label == "East Asia & Pacific" 
replace label = "Latin America and Caribbean" if label == "Latin America & Caribbean" 
replace label = "Middle East and North Africa" if label == "Middle East & North Africa" 	
outsheet location_code region label pchg_interp1teacher1 pchg_interp1youthpop1 pchg_interp1teacher2 pchg_interp1youthpop2 value_interp1teacher1 value_interp1teacher2 value_interp1youthpop1 value_interp1youthpop2 using "$figuredir\Figure_2b_teacher_percentage_boundary_weighted_subregion.csv" if inlist(category,1,2,3), replace comma
restore


** Figure2(a)

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

gen region_n = 1 if region=="Sub-Saharan Africa"
replace region_n = 2 if region=="Middle East and North Africa" 
replace region_n = 3 if region=="Latin America and Caribbean"
replace region_n = 4 if region=="North America"
replace region_n = 5 if region=="South Asia"
replace region_n = 6 if region=="Europe and Central Asia"
replace region_n = 7 if region=="East Asia and Pacific"

gen division = 1 if inlist(region, "Middle East and North Africa", "Sub-Saharan Africa")
replace division = 2 if inlist(region, "Latin America and Caribbean", "North America", "South Asia")
replace division = 3 if inlist(region, "East Asia and Pacific", "Europe and Central Asia")

keep if inlist(year_bins,1980,2000,2020)
keep if region!=""

gen ratio = youthpop/teacher 
keep year_bins region region_n ratio division



graph bar (mean) ratio, over(year_bins) over(region_n, relabel(1 `" "Sub-Saharan" "Africa" "'  2 `" "Middle East" "and North Africa" "'  3 `" "Latin America" "and Caribbean" "' 4 `" "North" "America" "' 5 `" "South" "Asia" "' 6 `" "Europe and" "Central Asia" "' 7 `" "East Asia" "and Pacific" "') label(labsize(vsmall))) over(division, relabel(1 `" "Africa & Middle East" "' 2 `" "Americas & South Asia" "' 3 `" "East Asia & Pacific" "and Europe & Central Asia" "') label(labsize(small))) nofill ytitle("Child Population to primary Teacher Ratio") bar(1,color(red*0.95)) bar(2,color(blue*0.95)) bar(3,color(green*0.95)) blabel(bar, format(%4.0f) size(vsmall)) legend(holes(4) ring(0) pos(1))

graph export "$figuredir\Figure_2_child_population_teacher_ratio.eps", replace		


exit

********************************************************************************
**Not using -possible removed later
********************************************************************************

*Original
foreach x in teacher  {
graph drop _all	
twoway (scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==1,  mlabel(countrycode) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==2,  mlabel(countrycode) mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==3,  mlabel(countrycode) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash))  ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change of school age population 1980-2000") ytitle("% change of `x'") ///
		legend(order(1 "Africa" 2 "America & South Asia" 3 "East Asia & Europe") r(1) position() size(medium_small)) name(first) ///
		xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 300)) ylabel(,labsize(medium_small))  

twoway (scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==1,  mlabel(countrycode) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==2,  mlabel(countrycode) mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==3,  mlabel(countrycode) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash)) ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change of school age population 2000-2020") ytitle("") ///
		legend(off) name(second) xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 300)) ylabel(,labsize(medium_small))

grc1leg first second 		
graph export "$figuredir\Figure 2 `x' percentage original.png", replace		
}

* Keeping boundary
preserve
foreach x in pchg_interp1teacher1  pchg_interp1teacher2 {
	replace `x' = 200 if `x'>200 & `x'!=.
}
foreach x in pchg_interp1youthpop1 pchg_interp1youthpop2 {
	replace `x' = 100 if `x'>100 & `x'!=.
}
 
foreach x in teacher  {
graph drop _all	
twoway (scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==1,  mlabel(countrycode) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==2,  mlabel(countrycode) mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==3,  mlabel(countrycode) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash))  ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change of school age population 1980-2000") ytitle("% change of `x'") ///
		legend(order(1 "Africa" 2 "America & South Asia" 3 "East Asia & Europe") r(1) position() size(medium_small)) name(first) ///
		xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))  

twoway (scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==1,  mlabel(countrycode) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==2,  mlabel(countrycode) mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==3,  mlabel(countrycode) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash)) ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change of school age population 2000-2020") ytitle("") ///
		legend(off) name(second) xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))

grc1leg first second 		
graph export "$figuredir\Figure 2 `x' percentage boundary.png", replace		
}
restore


* Keeping boundary & size of country & only big countries
preserve
foreach x in pchg_interp1teacher1  pchg_interp1teacher2 {
	replace `x' = 200 if `x'>200 & `x'!=.
}
foreach x in pchg_interp1youthpop1 pchg_interp1youthpop2 {
	replace `x' = 100 if `x'>100 & `x'!=.
}
keep if value_interp1youthpop1>=500000 & value_interp1youthpop2>=500000

foreach x in teacher  {
graph drop _all	
twoway (scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==1 [w=value_interp1`x'1],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==2 [w=value_interp1`x'1],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==3 [w=value_interp1`x'1],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==1 [w=value_interp1`x'1], legend(off) ms(i) mlabel(countrycode) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==2 [w=value_interp1`x'1], legend(off) ms(i) mlabel(countrycode) mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1`x'1 pchg_interp1youthpop1 if category==3 [w=value_interp1`x'1], legend(off) ms(i) mlabel(countrycode) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash))  ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change of school age population 1980-2000") ytitle("% change of `x'") ///
		legend(order(1 "Africa" 2 "America & South Asia" 3 "East Asia & Europe") r(1) position() size(medium_small)) name(first) ///
		xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))  

twoway (scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==1 [w=value_interp1`x'2],  mlabel() mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==2 [w=value_interp1`x'2],  mlabel() mlabsize(vsmall) mcolor(blue) mlabcolor(blue) msymbol(Th)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==3 [w=value_interp1`x'2],  mlabel() mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==1 [w=value_interp1`x'2], legend(off) ms(i) mlabel(countrycode) mlabsize(vsmall) mcolor(red) mlabcolor(red)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==2 [w=value_interp1`x'2], legend(off) ms(i) mlabel(countrycode) mlabsize(vsmall) mcolor(blue) mlabcolor(blue)) ///
		(scatter pchg_interp1`x'2 pchg_interp1youthpop2 if category==3 [w=value_interp1`x'2], legend(off) ms(i) mlabel(countrycode) mlabsize(vsmall) mcolor(green) mlabcolor(green)) ///
		(function y=x, range(-50 100) yvarlab("y=x") clwidth() lcolor(black) lpattern(shortdash)) ///
		, xline(0, lpattern(solid) lcolor(black)) yline(0, lpattern(solid) lcolor(black)) xtitle("% change of school age population 2000-2020") ytitle("") ///
		legend(off) name(second) xscale(range(-50 100)) xlabel(,labsize(medium_small)) yscale(range(-50 200)) ylabel(,labsize(medium_small))

grc1leg first second 		
graph export "$figuredir\Figure 2 `x' percentage boundary weighted over500.png", replace		
}
restore


*quad Table
gen quad1 = 1 if pchg_interp1teacher1 >= 0 & pchg_interp1youthpop1 >= 0 & pchg_interp1teacher1>pchg_interp1youthpop1
replace quad1 = 2 if pchg_interp1teacher1 >= 0 & pchg_interp1youthpop1 >= 0 & pchg_interp1teacher1<pchg_interp1youthpop1
replace quad1 = 3 if pchg_interp1teacher1 < 0 & pchg_interp1youthpop1 >= 0
replace quad1 = 4 if pchg_interp1teacher1 < 0 & pchg_interp1youthpop1 < 0 & pchg_interp1teacher1<pchg_interp1youthpop1
replace quad1 = 5 if pchg_interp1teacher1 < 0 & pchg_interp1youthpop1 < 0 & pchg_interp1teacher1>pchg_interp1youthpop1
replace quad1 = 6 if pchg_interp1teacher1 >= 0 & pchg_interp1youthpop1 < 0

bysort quad1: egen total1_q = total(value_interp1youthpop1)
egen total1 = total(value_interp1youthpop1)
gen share1_q = total1_q/total1

gen quad2 = 1 if pchg_interp1teacher2 >= 0 & pchg_interp1youthpop2 >= 0 & pchg_interp1teacher2>pchg_interp1youthpop2
replace quad2 = 2 if pchg_interp1teacher2 >= 0 & pchg_interp1youthpop2 >= 0 & pchg_interp1teacher2<pchg_interp1youthpop2
replace quad2 = 3 if pchg_interp1teacher2 < 0 & pchg_interp1youthpop2 >= 0
replace quad2 = 4 if pchg_interp1teacher2 < 0 & pchg_interp1youthpop2 < 0 & pchg_interp1teacher2<pchg_interp1youthpop2
replace quad2 = 5 if pchg_interp1teacher2 < 0 & pchg_interp1youthpop2 < 0 & pchg_interp1teacher2>pchg_interp1youthpop2
replace quad2 = 6 if pchg_interp1teacher2 >= 0 & pchg_interp1youthpop2 < 0

bysort quad2: egen total2_q = total(value_interp1youthpop2)
egen total2 = total(value_interp1youthpop2)
gen share2_q = total2_q/total2



matrix mat1_q = J(6, 1, .)

su share1_q  if quad1==1
matrix mat1_q[1, 1] = r(mean)
su share1_q  if quad1==2
matrix mat1_q[2, 1] = r(mean)
su share1_q  if quad1==3
matrix mat1_q[3, 1] = r(mean)
su share1_q  if quad1==4
matrix mat1_q[4, 1] = r(mean)
su share1_q  if quad1==5
matrix mat1_q[5, 1] = r(mean)
su share1_q  if quad1==6
matrix mat1_q[6, 1] = r(mean)

matrix mat2_q = J(6, 1, .)

su share2_q  if quad2==1
matrix mat2_q[1, 1] = r(mean)
su share2_q  if quad2==2
matrix mat2_q[2, 1] = r(mean)
su share2_q  if quad2==3
matrix mat2_q[3, 1] = r(mean)
su share2_q  if quad2==4
matrix mat2_q[4, 1] = r(mean)
su share2_q  if quad2==5
matrix mat2_q[5, 1] = r(mean)
su share2_q  if quad2==6
matrix mat2_q[6, 1] = r(mean)


matrix list mat1_q
matrix list mat2_q


