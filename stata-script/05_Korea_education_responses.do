/*
Description: 
	Generating Figure 5 presenting demographic trends and educational responses in Korea
Input files:
	ppts_easia_weuro_world_pchg.csv
Output files:
	Figure_5_Primay_Education_Korea_percentage.eps
	Figure_5a_Primay_Education_Korea_province_percentage.eps
	Figure_5_Primay_Education_Korea_percentage.csv
	Figure_5a_Primay_Education_Korea_province_percentage.csv
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

*simplifying Korean province
replace location_code = "KOR_Chungcheong" if inlist(location_code, "KOR_Chungbuk", "KOR_Chungnam")
replace location_code = "KOR_Gyeongsang" if inlist(location_code, "KOR_Gyeongbuk", "KOR_Gyeongnam")
replace location_code = "KOR_Jeolla" if inlist(location_code, "KOR_Jeonbuk", "KOR_Jeonnam")
replace location_code = "KOR_capital" if inlist(location_code, "KOR_Gyeong-gi", "KOR_Seoul", "KOR_Incheon")

collapse (sum) value_interp1, by(location_code year_bins variable)

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
********************************************************************************
** Korea: metro-nonmetro
********************************************************************************

graph drop _all
twoway (scatter pchange_student year if location_code=="KOR_Metro", msize(tiny) mcolor(red)) ///
		(line pchange_student year if location_code=="KOR_NonMetro", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(c(1) bmargin(7 0 5 0) order(1 "Metro areas" 2 "Non-metro areas") size(small)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(student) title("Percentage change in students", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

twoway (scatter pchange_teacher year if location_code=="KOR_Metro", msize(tiny) mcolor(red)) ///
		(line pchange_teacher year if location_code=="KOR_NonMetro", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher) title("Percentage change in teachers", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))
 
twoway 	(scatter pchange_school year if location_code=="KOR_Metro", msize(tiny) mcolor(red)) ///
		(line pchange_school year if location_code=="KOR_NonMetro", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school) title("Percentage change in schools", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small)) // ytitle("",axis(2))

twoway (scatter student_teacher_ratio year if location_code=="KOR_Metro", msize(tiny) mcolor(red)) ///
		(line student_teacher_ratio year if location_code=="KOR_NonMetro", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher_ratio) title("Pupil-teacher ratio", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))
	 
twoway (scatter student_school_ratio year if location_code=="KOR_Metro", msize(tiny) mcolor(red)) ///
		(line student_school_ratio year if location_code=="KOR_NonMetro", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school_ratio) title("Pupil-school ratio", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

grc1leg student teacher school teacher_ratio school_ratio, legendfrom(student) hole(4) ring(0) pos(8)
graph export "$figuredir\Figure_5_Primay_Education_Korea_percentage.eps", replace
outsheet location_code year pchange_teacher pchange_school student_teacher_ratio student_school_ratio using "$figuredir\Figure_5_Primay_Education_Korea_percentage.csv" if inlist(location_code,"KOR_Metro","KOR_NonMetro"), replace comma

********************************************************************************
** Korea: province
********************************************************************************
graph drop _all
twoway (scatter pchange_student year if location_code=="KOR_Busan", msize(tiny) mcolor(red)) ///
		(scatter pchange_student year if location_code=="KOR_Daegu", msize(tiny) mcolor(red)) ///
		(scatter pchange_student year if location_code=="KOR_Daejeon", msize(tiny) mcolor(red)) ///
		(scatter pchange_student year if location_code=="KOR_Gwangju", msize(tiny) mcolor(red)) ///
		(scatter pchange_student year if location_code=="KOR_Ulsan", msize(tiny) mcolor(red)) ///
		(scatter pchange_student year if location_code=="KOR_capital", msize(tiny) mcolor(red)) ///
		(line pchange_student year if location_code=="KOR_Chungcheong", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_student year if location_code=="KOR_Gangwon", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_student year if location_code=="KOR_Gyeongsang", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_student year if location_code=="KOR_Jeolla", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(c(1) bmargin(7 0 5 0) order(1 "Metro areas" 7 "Non-metro areas") size(small)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(student) title("Percentage change in students", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

twoway (scatter pchange_teacher year if location_code=="KOR_Busan", msize(tiny) mcolor(red)) ///
		(scatter pchange_teacher year if location_code=="KOR_Daegu", msize(tiny) mcolor(red)) ///
		(scatter pchange_teacher year if location_code=="KOR_Daejeon", msize(tiny) mcolor(red)) ///
		(scatter pchange_teacher year if location_code=="KOR_Gwangju", msize(tiny) mcolor(red)) ///
		(scatter pchange_teacher year if location_code=="KOR_Ulsan", msize(tiny) mcolor(red)) ///
		(scatter pchange_teacher year if location_code=="KOR_capital", msize(tiny) mcolor(red)) ///
		(line pchange_teacher year if location_code=="KOR_Chungcheong", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_teacher year if location_code=="KOR_Gangwon", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_teacher year if location_code=="KOR_Gyeongsang", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_teacher year if location_code=="KOR_Jeolla", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher) title("Percentage change in teachers", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))
 
twoway 	(scatter pchange_school year if location_code=="KOR_Busan", msize(tiny) mcolor(red)) ///
		(scatter pchange_school year if location_code=="KOR_Daegu", msize(tiny) mcolor(red)) ///
		(scatter pchange_school year if location_code=="KOR_Daejeon", msize(tiny) mcolor(red)) ///
		(scatter pchange_school year if location_code=="KOR_Gwangju", msize(tiny) mcolor(red)) ///
		(scatter pchange_school year if location_code=="KOR_Ulsan", msize(tiny) mcolor(red)) ///
		(scatter pchange_school year if location_code=="KOR_capital", msize(tiny) mcolor(red)) ///
		(line pchange_school year if location_code=="KOR_Chungcheong", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_school year if location_code=="KOR_Gangwon", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_school year if location_code=="KOR_Gyeongsang", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line pchange_school year if location_code=="KOR_Jeolla", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school) title("Percentage change in schools", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small)) // ytitle("",axis(2))

twoway (scatter student_teacher_ratio year if location_code=="KOR_Busan", msize(tiny) mcolor(red)) ///
		(scatter student_teacher_ratio year if location_code=="KOR_Daegu", msize(tiny) mcolor(red)) ///
		(scatter student_teacher_ratio year if location_code=="KOR_Daejeon", msize(tiny) mcolor(red)) ///
		(scatter student_teacher_ratio year if location_code=="KOR_Gwangju", msize(tiny) mcolor(red)) ///
		(scatter student_teacher_ratio year if location_code=="KOR_Ulsan", msize(tiny) mcolor(red)) ///
		(scatter student_teacher_ratio year if location_code=="KOR_capital", msize(tiny) mcolor(red)) ///
		(line student_teacher_ratio year if location_code=="KOR_Chungcheong", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_teacher_ratio year if location_code=="KOR_Gangwon", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_teacher_ratio year if location_code=="KOR_Gyeongsang", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_teacher_ratio year if location_code=="KOR_Jeolla", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(teacher_ratio) title("Pupil-teacher ratio", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))
	 
twoway (scatter student_school_ratio year if location_code=="KOR_Busan", msize(tiny) mcolor(red)) ///
		(scatter student_school_ratio year if location_code=="KOR_Daegu", msize(tiny) mcolor(red)) ///
		(scatter student_school_ratio year if location_code=="KOR_Daejeon", msize(tiny) mcolor(red)) ///
		(scatter student_school_ratio year if location_code=="KOR_Gwangju", msize(tiny) mcolor(red)) ///
		(scatter student_school_ratio year if location_code=="KOR_Ulsan", msize(tiny) mcolor(red)) ///
		(scatter student_school_ratio year if location_code=="KOR_capital", msize(tiny) mcolor(red)) ///
		(line student_school_ratio year if location_code=="KOR_Chungcheong", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_school_ratio year if location_code=="KOR_Gangwon", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_school_ratio year if location_code=="KOR_Gyeongsang", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
		(line student_school_ratio year if location_code=="KOR_Jeolla", msize(tiny) lpattern(shortdash) lcolor(blue)) ///
	 ,legend(r(1) order(1 "Metro areas" 2 "Non-metro areas") size(medium)) xtitle("") ytitle("") ///
	 caption("", size(small))  name(school_ratio) title("Pupil-school ratio", size(medium)) ///
	 xlabel(,labsize(medium_small)) ylabel(,labsize(medium_small))

grc1leg student teacher school teacher_ratio school_ratio, legendfrom(student) hole(4) ring(0) pos(8)
graph export "$figuredir\Figure_5a_Primay_Education_Korea_province_percentage.eps", replace
outsheet location_code year pchange_teacher pchange_school student_teacher_ratio student_school_ratio using "$figuredir\Figure_5a_Primay_Education_Korea_province_percentage.csv" if inlist(location_code,"KOR_Busan","KOR_Daegu","KOR_Daejeon","KOR_Gwangju","KOR_Ulsan") | inlist(location_code,"KOR_capital","KOR_Chungcheong","KOR_Gangwon","KOR_Gyeongsang","KOR_Jeolla"), replace comma
