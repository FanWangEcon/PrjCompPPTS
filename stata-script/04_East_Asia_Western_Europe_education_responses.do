/*
Description: 
	Generating Figure 3 and 4 presenting demographic trends and educational responses in East Asia and Western Europe
Input files:
	ppts_easia_weuro_world_pchg.csv
Output files:
	Figure_3_Primay_Education_in_East_Asia_percentage_same_scale.eps
	Figure_4_Primay_Education_in_Western_Europe_percentage_same_scale.eps
	Figure_3_Primay_Education_in_East_Asia_percentage_same_scale.csv
	Figure_4_Primay_Education_in_Western_Europe_percentage_same_scale.csv
Note: 
*/

clear 
clear mata 
clear matrix 
set more off 

********************************************************************************
*Data restriction
import delimited "$PPTSdatadir\ppts_easia_weuro_world_pchg.csv", case(lower) clear

destring pchg_interp1 value_interp1, replace force

keep if inlist(year_bins_type, "1940t2020i01")
drop if inlist(variable,"gdp")

drop location_level year_bins_type pchg pchg_interp1 value

duplicates drop location_code variable year_bins value_interp1, force

destring, replace

********************************************************************************
*Reshaping
reshape wide value_interp1, i(location_code year_bins) j(variable) string
rename (value*) (*)
rename (_*) (*)
rename (interp1*) (*)
********************************************************************************

**Generating varilables
gen student_teacher_ratio =  student/teacher
gen student_school_ratio =  student/school

* percentage changes
foreach x in student teacher school youthpop {
	gen `x'2020_ = `x' if year==2020
	bysort location_code: egen `x'2020 = mean(`x'2020) 
	bysort location_code (year): gen pchange_`x'=100*(`x'[_n]-`x'2020)/`x'2020
}
drop *2020*

********************************************************************************
* Last restriction
drop if location_code=="CHN" & year==1965 //dropping since it has too big change
keep if year>=1970
replace pchange_school = pchange_school*0.05 if location_code=="CHN" //rescale China school
********************************************************************************
********************************************************************************
*Same scale
********************************************************************************

**East Asia
graph drop _all
twoway (scatter pchange_student year if location_code=="CHN", msize(tiny) mcolor(red)) ///
		(line pchange_student year if location_code=="JPN", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_student year if location_code=="KOR", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line pchange_student year if location_code=="TWN", msize(tiny) lpattern(longdash) lcolor(black)) ///
	 ,legend(c(1) bmargin(7 0 5 0) order(1 "China" 2 "Japan" 3 "Korea" 4 "Taiwan") size(small)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(student) title("Percentage change in students", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(120 -20)) ylabel(120(20)-20,labsize(medium_small))

twoway (scatter pchange_teacher year if location_code=="CHN", msize(tiny) mcolor(red)) ///
		(line pchange_teacher year if location_code=="JPN", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_teacher year if location_code=="KOR", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line pchange_teacher year if location_code=="TWN", msize(tiny) lpattern(longdash) lcolor(black)) ///
	 ,legend(r(1) order(1 "China" 2 "Japan" 3 "Korea" 4 "Taiwan")) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher) title("Percentage change in teachers", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(40 -60)) ylabel(40(20)-60,labsize(medium_small))
 
twoway 	(line pchange_school year if location_code=="JPN", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_school year if location_code=="KOR", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line pchange_school year if location_code=="TWN", msize(tiny) lpattern(longdash) lcolor(black)) ///
		(scatter pchange_school year if location_code=="CHN", msize(tiny) mcolor(red)) ///
	 ,legend(r(1) order(1 "China" 2 "Japan" 3 "Korea" 4 "Taiwan")) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school) title("Percentage change in schools", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(40 -10)) ylabel(40(10)-10,labsize(medium_small))

twoway (scatter student_teacher_ratio year if location_code=="CHN", msize(tiny) mcolor(red)) ///
		(line student_teacher_ratio year if location_code=="JPN", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_teacher_ratio year if location_code=="KOR", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line student_teacher_ratio year if location_code=="TWN", msize(tiny) lpattern(longdash) lcolor(black)) ///
	 ,legend(r(1) order(1 "China" 2 "Japan" 3 "Korea" 4 "Taiwan")) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher_ratio) title("Pupil-teacher ratio", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))
	 
twoway (scatter student_school_ratio year if location_code=="CHN", msize(tiny) mcolor(red)) ///
		(line student_school_ratio year if location_code=="JPN", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_school_ratio year if location_code=="KOR", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line student_school_ratio year if location_code=="TWN", msize(tiny) lpattern(longdash) lcolor(black)) ///
	 ,legend(r(1) order(1 "China" 2 "Japan" 3 "Korea" 4 "Taiwan")) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school_ratio) title("Pupil-school ratio", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

grc1leg student teacher school teacher_ratio school_ratio, legendfrom(student) holes(4) ring(0) pos(8)
graph export "$figuredir\Figure_3_Primay_Education_in_East_Asia_percentage_same_scale.eps", replace
outsheet location_code year pchange_teacher pchange_school student_teacher_ratio student_school_ratio using "$figuredir\Figure_3_Primay_Education_in_East_Asia_percentage_same_scale.csv" if inlist(location_code,"CHN","JPN","KOR","TWN"), replace comma

** Western Europe
graph drop _all
twoway (scatter pchange_student year if location_code=="AUT", msize(tiny) mcolor(red)) ///
		(line pchange_student year if location_code=="DEU", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_student year if location_code=="FRA", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line pchange_student year if location_code=="NLD", msize(tiny) lpattern(longdash) lcolor(black)) ///
		(line pchange_student year if location_code=="CHE", msize(tiny) lpattern(dash_dot) lcolor(purple)) ///
	 ,legend(c(1) bmargin(7 0 5 0) order(1 "Austria" 2 "Germany" 3 "France" 4 "Netherlands" 5 "Switzerland") size(small)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(student) title("Percentage change in students", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(120 -20)) ylabel(120(20)-20,labsize(medium_small))

twoway (scatter pchange_teacher year if location_code=="AUT", msize(tiny) mcolor(red)) ///
		(line pchange_teacher year if location_code=="DEU", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_teacher year if location_code=="FRA", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line pchange_teacher year if location_code=="NLD", msize(tiny) lpattern(longdash) lcolor(black)) ///
		(line pchange_teacher year if location_code=="CHE", msize(tiny) lpattern(dash_dot) lcolor(purple)) ///
	 ,legend(r(1) order(1 "Austria" 2 "Germany" 3 "France") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher) title("Percentage change in teachers", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(40 -60)) ylabel(40(20)-60,labsize(medium_small))
 
twoway 	(scatter pchange_school year if location_code=="AUT", msize(tiny) mcolor(red)) ///
		(line pchange_school year if location_code=="DEU", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_school year if location_code=="FRA", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line pchange_school year if location_code=="NLD", msize(tiny) lpattern(longdash) lcolor(black)) ///
		(line pchange_school year if location_code=="CHE", msize(tiny) lpattern(dash_dot) lcolor(purple)) ///
	 ,legend(r(1) order(1 "Austria" 2 "Germany" 3 "France") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school) title("Percentage change in schools", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(40 -10)) ylabel(40(10)-10,labsize(medium_small))

twoway (scatter student_teacher_ratio year if location_code=="AUT", msize(tiny) mcolor(red)) ///
		(line student_teacher_ratio year if location_code=="DEU", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_teacher_ratio year if location_code=="FRA", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line student_teacher_ratio year if location_code=="NLD", msize(tiny) lpattern(longdash) lcolor(black)) ///
		(line student_teacher_ratio year if location_code=="CHE", msize(tiny) lpattern(dash_dot) lcolor(purple)) ///
	 ,legend(r(1) order(1 "Austria" 2 "Germany" 3 "France") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher_ratio) title("Pupil-teacher ratio", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(60 10)) ylabel(60(10)10,labsize(medium_small))
	 
twoway (scatter student_school_ratio year if location_code=="AUT", msize(tiny) mcolor(red)) ///
		(line student_school_ratio year if location_code=="DEU", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_school_ratio year if location_code=="FRA", msize(tiny) lpattern(dash) lcolor(green)) ///
		(line student_school_ratio year if location_code=="NLD", msize(tiny) lpattern(longdash) lcolor(black)) ///
		(line student_school_ratio year if location_code=="CHE", msize(tiny) lpattern(dash_dot) lcolor(purple)) ///
	 ,legend(r(1) order(1 "Austria" 2 "Germany" 3 "France") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school_ratio) title("Pupil-school ratio", size(medium)) ///
	 xscale() xlabel(,labsize(medium_small)) yscale(range(1000 0)) ylabel(1000(200)0,labsize(medium_small))

grc1leg student teacher school teacher_ratio school_ratio, legendfrom(student) holes(4) ring(0) pos(8)
graph export "$figuredir\Figure_4_Primay_Education_in_Western_Europe_percentage_same_scale.eps", replace
outsheet location_code year pchange_teacher pchange_school student_teacher_ratio student_school_ratio using "$figuredir\Figure_4_Primay_Education_in_Western_Europe_percentage_same_scale.csv" if inlist(location_code,"AUT","DEU","FRA","NLD","CHE"), replace comma

