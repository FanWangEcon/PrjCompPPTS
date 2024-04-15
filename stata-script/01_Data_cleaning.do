/*
Description: 
	Cleaning data into working file.

Input files: 

	*Korea
	Korean elementary school 1965-2021.xlsx

	*Japan
	japan_school_count_1948_2022.xlsx
	japan_student_count_1948_2022.xlsx
	japan_teacher_count_1948_2022.xlsx

	*Taiwan
	taiwan_teachers_count_by_levels_1976_2021.xlsx
	taiwan_students_count_by_levels_1976_2021.xlsx
	taiwan_schools_count_by_levels_1976_2021.xlsx
	taiwan_gross_enrollment_ratio_by_levels_1976_2021.xlsx
	taiwan_gdp_per_capita_1975_2021.xlsx

	*China
	Data_China_School_Teachers_Students.xlsx
	
	*Germany
	Germany (Grundschulen(grade 1-4))
	germany_schools_classes_bystates_30years.xlsx
	germany_students_total_24years.xlsx

	*Austria
	austria_fw_national_primary1t4_school_teacher_class_students_count_1923_2020.xslx

	*France
	france_student_school_teacher_count_1980_2021.xlsx

	*Netherland
	Number of schools in Finland and Netherland.xlsx

	*World Bank
	Data_Extract_From_World_Development_Indicators_including_ratio.xlsx
	country_code_region.dta

Output files:
	all_cleaned.dta
	all_cleaned.csv
*/

clear 
clear mata 
clear matrix 
set more off 

********************************************************************************
********************************************************************************
** Cleaning each country input files
*Korea
import excel "$koreadir\Korean elementary school 1965-2021.xlsx", sheet("Schools_imputed") cellrange(A33:BF52) firstrow case(lower) clear	 
rename year* school*
reshape long school, i(region) j(year) 
save "$tempdir\Korea_school.dta", replace	 
	 
import excel "$koreadir\Korean elementary school 1965-2021.xlsx", sheet("Teachers_imputed") cellrange(A33:BF52) firstrow case(lower) clear	 
rename year* teacher*
reshape long teacher, i(region) j(year) 
save "$tempdir\Korea_teacher.dta", replace		 
	 
import excel "$koreadir\Korean elementary school 1965-2021.xlsx", sheet("Students_imputed") cellrange(A33:BF52) firstrow case(lower) clear	 
rename year* student*
reshape long student, i(region) j(year) 
save "$tempdir\Korea_student.dta", replace		 
	 
merge 1:1 region year using "$tempdir\Korea_school.dta", nogen	 
merge 1:1 region year using "$tempdir\Korea_teacher.dta", nogen

drop if region=="Chungnam"
replace region = "Chungnam" if region=="Chungnam_after_Sejong"
rename (student school teacher) (students schools teachers) 

preserve
gen area_in_country = 1 if inlist(region, "Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon", "Ulsan", "Sejong", "Gyeong-gi")
replace area_in_country = 0 if inlist(region, "Gangwon", "Chungbuk", "Chungnam", "Jeonbuk", "Jeonnam", "Gyeongbuk", "Gyeongnam", "Jeju")
collapse (sum) school students teachers, by(area_in_country year)
drop if area_in_country==.
tempfile temp
save `temp'
restore
append using `temp'

replace region = "Kor_metro" if area_in_country==1
replace region = "Kor_nonmetro" if area_in_country==0

rename region location_name
*gen location_code_ = substr(location_name,1,5)
gen location_code = "KOR" + "_" + location_name

replace location_code = "KOR_Metro" if location_code=="KOR_Kor_metro"
replace location_code = "KOR_NonMetro" if location_code=="KOR_Kor_nonmetro"
replace location_code = "KOR" if location_code=="KOR_Nationwide"
replace location_name = "Korea" if location_code=="KOR"
gen location_level = "province"
replace location_level = "multiprovince" if inlist(location_name, "Kor_metro", "Kor_nonmetro")
replace location_level = "country" if inlist(location_name, "Korea")
drop area_in_country

save "$tempdir\Korea_cleaned.dta", replace	

********************************************************************************
*Japan
import excel "$japandir/japan_school_count_1948_2022.xlsx", cellrange(B5:E79) case(lower) clear	
keep B E
rename (B E) (year schools)
save "$tempdir\japan_school.dta", replace	 
import excel "$japandir/japan_teacher_count_1948_2022.xlsx",  cellrange(B4:E78) case(lower) clear	
keep B E
rename (B E) (year teachers)
save "$tempdir\japan_teacher.dta", replace	 
import excel "$japandir/japan_student_count_1948_2022.xlsx", cellrange(B4:E78) case(lower) clear	
keep B E
rename (B E) (year students)
save "$tempdir\japan_student.dta", replace	 

merge 1:1 year using "$tempdir\japan_school.dta", nogen	 
merge 1:1 year using "$tempdir\japan_teacher.dta", nogen
destring, replace

gen location_level = "country"
gen location_name = "Japan"
gen location_code = "JPN" 

save "$tempdir\japan_cleaned.dta", replace	

********************************************************************************
*Taiwan
import excel "$taiwandir\taiwan_schools_count_by_levels_1976_2021.xlsx", sheet("Cleaned") firstrow clear
save "$tempdir\taiwan_school.dta", replace	 
import excel "$taiwandir\taiwan_teachers_count_by_levels_1976_2021.xlsx", sheet("Cleaned") firstrow clear
save "$tempdir\taiwan_teacher.dta", replace	 
import excel "$taiwandir\taiwan_students_count_by_levels_1976_2021.xlsx", sheet("Cleaned") firstrow clear
save "$tempdir\taiwan_students.dta", replace	


use "$tempdir\taiwan_students.dta", clear
merge 1:1 year using "$tempdir\taiwan_school.dta", nogen	 
merge 1:1 year using "$tempdir\taiwan_teacher.dta", nogen

gen location_level = "country"
gen location_name = "Taiwan"
gen location_code = "TWN" 

save "$tempdir\taiwan_cleaned.dta", replace	

********************************************************************************
*China
import excel "$chinadir\Data_China_School_Teachers_Students.xlsx", sheet("China-Data-Online") firstrow case(lower) clear
rename (years chinadataonlineprimaryschoo chinadataonlinenumberoftea chinadataonlinenumberofstu chinadataonlinepupiltoteac) ///
		(year schools teachers students x)
drop x
destring year, replace

gen location_level = "country"
gen location_name = "China"
gen location_code = "CHN" 

save "$tempdir\china_cleaned.dta", replace	

********************************************************************************
*Germany
import excel "$germanydir/germany_schools_classes_bystates_30years.xlsx", sheet("Cleaned") firstrow case(lower) clear	
drop if year==.
keep year schools teachers
save "$tempdir\germany_schools_teachers.dta", replace	

import excel "$germanydir/germany_students_total_24years.xlsx", sheet("Cleaned") firstrow case(lower) clear	
drop if year==.
merge 1:1 year using "$tempdir\germany_schools_teachers.dta", nogen

gen location_level = "country"
gen location_name = "Germany"
gen location_code = "DEU" 

save "$tempdir\germany_cleaned.dta", replace

********************************************************************************
*Austria
import excel "$austriadir/austria_fw_national_primary1t4_school_teacher_class_students_count_1923_2020.xlsx", firstrow case(lower) clear	
rename (a primaryschools) (year schools)
drop classes

replace year = substr(year,1,4)
destring, replace

gen location_level = "country"
gen location_name = "Austria"
gen location_code = "AUT" 

save "$tempdir\austria_cleaned.dta", replace	

********************************************************************************
*France
import excel "$francedir/france_student_school_teacher_count_1980_2021.xlsx", sheet("schools") cellrange(A1:B43) firstrow clear
save "$tempdir\france_school.dta", replace

import excel "$francedir/france_student_school_teacher_count_1980_2021.xlsx", sheet("teachers") cellrange(A1:B43) firstrow clear
save "$tempdir\france_teacher.dta", replace

import excel "$francedir/france_student_school_teacher_count_1980_2021.xlsx", sheet("students") cellrange(A1:B43) firstrow clear
save "$tempdir\france_student.dta", replace

use "$tempdir\france_school.dta", clear
merge 1:1 year using "$tempdir\france_teacher.dta", nogen
merge 1:1 year using "$tempdir\france_student.dta", nogen

replace students = students*100

* Because of change of definition of statistics over time, jumps exist. To remove inconsistency, I remove those jumps in the data.
foreach x in schools teachers students{
	levelsof `x' if year==2010, local(y2010)
	levelsof `x' if year==2011, local(y2011)
	replace `x' = `x' - (`y2011'-`y2010') if year>=2011
	
	levelsof `x' if year==1998, local(y1998)
	levelsof `x' if year==1999, local(y1999)
	replace `x' = `x' - (`y1999'-`y1998') if year>=1999
}

gen location_level = "country"
gen location_name = "France"
gen location_code = "FRA" 

save "$tempdir\france_cleaned.dta", replace	

********************************************************************************
*Netherland
import excel "$netherlanddir/netherlands_student_school_count_1990_2021.xlsx", firstrow case(lower) sheet("Ne cleaned") clear	
save "$tempdir\netherlands_school_student.dta", replace
import delimited "$netherlanddir\netherlands_teachers-in-primary-education_2003_2017.csv",  case(lower) clear	
keep in 1/15
destring, replace
gen teachers = male + female
drop male female
merge 1:1 year using "$tempdir\netherlands_school_student.dta", nogen
replace teachers =  teachers*1000

gen location_level = "country"
gen location_name = "Netherlands"
gen location_code = "NLD" 

save "$tempdir\netherland_cleaned.dta", replace	

********************************************************************************
*Swiss
import delimited "$swissdir\swiss_data_students_teachers_schools.csv",  case(lower) clear	
drop v5 v6 v7 v8 v9 v10
rename v1 year

gen location_level = "country"
gen location_name = "Switzerland"
gen location_code = "CHE" 

save "$tempdir\swiss_cleaned.dta", replace	
********************************************************************************
**Cleaning Word Bank files

** Getting population 0-14
import excel "$worldbankdir\Data_Extract_From_World_Development_Indicators_including_ratio.xlsx", sheet("Data") firstrow case(lower) clear

drop in 2661/2665
drop seriescode
replace seriesname = "youthpop" if seriesname == "Population ages 0-14, total"
replace seriesname = "gdp" if seriesname == "GDP per capita (constant 2015 US$)"
replace seriesname = "primary_teachers" if seriesname == "Primary education, teachers"
replace seriesname = "lower_secondary_ratio" if seriesname == "Pupil-teacher ratio, lower secondary"
replace seriesname = "primary_ratio" if seriesname == "Pupil-teacher ratio, primary"
replace seriesname = "secondary_ratio" if seriesname == "Pupil-teacher ratio, secondary"
replace seriesname = "upper_secondary_ratio" if seriesname == "Pupil-teacher ratio, upper secondary"
replace seriesname = "secondary_teachers" if seriesname == "Secondary education, teachers"
replace seriesname = "primary_pupils" if seriesname == "Primary education, pupils"
replace seriesname = "secondary_pupils" if seriesname == "Secondary education, pupils"

reshape wide yr*, i(countrycode) j(seriesname) string

reshape long yr@gdp yr@youthpop yr@primary_teachers yr@lower_secondary_ratio yr@primary_ratio yr@secondary_ratio yr@upper_secondary_ratio yr@secondary_teachers yr@primary_pupils yr@secondary_pupils, i(countrycode) j(year)

rename (yr*) (*)

keep countrycode year gdp youthpop countryname

keep if inlist(countryname, "China", "Korea, Rep.", "Japan", "Germany", "Austria", "Netherlands", "France", "Switzerland") 
replace countryname = "Korea" if countryname == "Korea, Rep."

rename countryname location_name
gen location_level = "country"
rename countrycode location_code

save "$tempdir\population_gdp_cleaned.dta", replace	

********************************************************************************
* Including Taiwan population 0-14 and gdp per capita
import excel "$taiwandir/taiwan_youthpop_1975_2021.xlsx", firstrow sheet(cleaned) clear	
save "$tempdir\taiwan_youthpop_cleaned.dta", replace	
import excel "$taiwandir/taiwan_gdp_per_capita_1975_2021.xlsx", firstrow cellrange(A3:B50) clear	
rename PerCapitaGDPUSatCurrent gdp
destring year, replace
merge 1:1 year using "$tempdir\taiwan_youthpop_cleaned.dta"
drop _merge
gen location_code = "TWN"
gen location_name = "Taiwan"

save "$tempdir\taiwan_youthpop_gdp_cleaned.dta", replace	

use "$tempdir\population_gdp_cleaned.dta", clear	
append using "$tempdir\taiwan_youthpop_gdp_cleaned.dta"
save "$tempdir\population_gdp_cleaned_w_taiwan.dta", replace	

********************************************************************************
** Getting enrollment ratio
import excel "$taiwandir\taiwan_gross_enrollment_ratio_by_levels_1976_2021", sheet("Cleaned") firstrow clear
gen location_name = "Taiwan"
gen location_code = "TWN"
save "$tempdir\taiwan_enroll_ratio.dta", replace	

import excel "$worldbankdir\Data_Extract_From_World_Development_Indicators_enrollment.xlsx", sheet("Data") firstrow case(lower) clear

drop in 267/271
drop seriescode
replace seriesname = "enroll_ratio" if seriesname == "School enrollment, primary (% gross)"

reshape wide yr*, i(countrycode) j(seriesname) string

reshape long yr@enroll_ratio, i(countrycode) j(year)

rename (yr*) (*)

keep countrycode year enroll_ratio countryname

replace countryname = "Korea" if countryname == "Korea, Rep."
rename countryname location_name
rename countrycode location_code
destring enroll_ratio, replace force

append using "$tempdir\taiwan_enroll_ratio.dta"

save "$tempdir\enroll_ratio_cleaned.dta", replace	

********************************************************************************
********************************************************************************
** Appending

clear
append using "$tempdir\Korea_cleaned.dta"
append using "$tempdir\japan_cleaned.dta"
append using "$tempdir\taiwan_cleaned.dta"
append using "$tempdir\austria_cleaned.dta"
append using "$tempdir\netherland_cleaned.dta"
append using "$tempdir\china_cleaned.dta"
append using "$tempdir\germany_cleaned.dta"
append using "$tempdir\france_cleaned.dta"
append using "$tempdir\swiss_cleaned.dta"

merge m:1 location_name year using "$tempdir\population_gdp_cleaned_w_taiwan.dta", keep(1 3) nogen update

order location_name location_code location_level year youthpop students teachers schools

save "$tempdir/eight_cleaned.dta", replace

** Having excel file including world
import excel "$worldbankdir\Data_Extract_From_World_Development_Indicators_including_ratio.xlsx", sheet("Data") firstrow case(lower) clear

drop in 2661/2665
drop seriescode
replace seriesname = "youthpop" if seriesname == "Population ages 0-14, total"
replace seriesname = "gdp" if seriesname == "GDP per capita (constant 2015 US$)"
replace seriesname = "primary_teachers" if seriesname == "Primary education, teachers"
replace seriesname = "primary_pupils" if seriesname == "Primary education, pupils"
drop if inlist(seriesname, "Pupil-teacher ratio, lower secondary", "Pupil-teacher ratio, secondary", "Pupil-teacher ratio, upper secondary", "Secondary education, teachers", "Secondary education, pupils", "Pupil-teacher ratio, primary")
reshape wide yr*, i(countrycode) j(seriesname) string

reshape long yr@gdp yr@youthpop yr@primary_teachers yr@primary_pupils, i(countrycode) j(year)

rename (yr*) (*)

rename (primary_pupils primary_teachers) (students teachers)

drop if inlist(countryname, "China", "Korea, Rep.", "Japan", "Germany", "Austria", "Netherlands", "France", "Switzerland") 
rename countryname location_name
rename countrycode location_code
gen location_level = "country"
foreach x in "Africa Eastern and Southern" "Africa Western and Central" "Central Europe and the Baltics" "Caribbean small states" "East Asia & Pacific (excluding high income)" "Early-demographic dividend" "East Asia & Pacific" "Europe & Central Asia (excluding high income)" "Europe & Central Asia" "European Union" "Fragile and conflict affected situations" "High income" "Heavily indebted poor countries (HIPC)" "IBRD only" "IDA & IBRD total" "IDA total" "IDA blend" "IDA only" "Not classified" "Latin America & Caribbean (excluding high income)" "Latin America & Caribbean" "Least developed countries: UN classification" "Low income" "Lower middle income" "Low & middle income" "Late-demographic dividend" "Middle East & North Africa" "Middle income" "Middle East & North Africa (excluding high income)" "North America" "OECD members" "Pre-demographic dividend" "Pacific island small states" "Post-demographic dividend" "South Asia" "Sub-Saharan Africa (excluding high income)" "Sub-Saharan Africa" "Small states" "East Asia & Pacific (IDA & IBRD countries)" "Europe & Central Asia (IDA & IBRD countries)" "Latin America & the Caribbean (IDA & IBRD countries)" "Middle East & North Africa (IDA & IBRD countries)" "South Asia (IDA & IBRD)" "Sub-Saharan Africa (IDA & IBRD countries)""Upper middle income" "World" "South Africa" "Arab World" "Euro area" "Other small states"{
	replace location_level = "multicountry" if location_name=="`x'"
}
 
append using "$tempdir/eight_cleaned.dta"

merge m:1 location_name year using "$tempdir\enroll_ratio_cleaned.dta", keep(1 3) nogen

gen youthpop_enr = students*(1/enroll_ratio)*100

*Merging region names
merge m:1 location_code using "$worldbankdir\country_code_region.dta", keep(1 3) nogen
foreach x in KOR_Busan KOR_Chungbuk KOR_Chungnam KOR_Daegu KOR_Daejeon KOR_Gangwon KOR_Gwangju KOR_Gyeong-gi KOR_Gyeongbuk KOR_Gyeongnam KOR_Incheon KOR_Jeju KOR_Jeonbuk KOR_Jeonnam KOR_Metro KOR_NonMetro KOR_Sejong KOR_Seoul KOR_Ulsan{
	replace region = "East Asia & Pacific" if location_code=="`x'"
}

order location_name location_code location_level region year youthpop students teachers schools

save "$cleandir/all_cleaned.dta", replace
export excel using "$cleandir/all_cleaned", firstrow(variables) replace	
	
	
	