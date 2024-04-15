/*
** Project: Comparative Consolidation Abstract
** Date: 09/23/2023
*/

clear 
clear mata 
clear matrix 
set more off 


**** Define macros ****
set scheme cleanplots2
global topdir "C:\Users\jhk62\Dropbox\Shared folder\Comparative Consolidation"
	global datadir "$topdir\Data"
		global tempdir "$datadir/Temp"
		global rawdir "$datadir\Raw"
			global austriadir "$rawdir\austria"
			global chinadir "$rawdir\china"
			global francedir "$rawdir\france"
			global germanydir "$rawdir\germany"
			global japandir "$rawdir\japan"
			global koreadir "$rawdir\korea"
			global netherlanddir "$rawdir\netherland"
			global swissdir "$rawdir\switzerland"
			global taiwandir "$rawdir\taiwan"
			global worldbankdir "$rawdir\worldbank"
		global cleandir "$datadir/Cleaned"
	global dodir "$topdir\Program"
	global figuredir "$topdir\Figure"
	global PPTSdir "$topdir\PrjCompPPTS"
		global PPTSdatadir "$PPTSdir\data"
		

**** Run do files ****		
		
do "$dodir/01_Data_cleaning.do"
do "$dodir/02_Global_youth_population_trends.do"
do "$dodir/03_Global_education_responses.do"
do "$dodir/04_East_Asia_Western_Europe_education_responses.do"
do "$dodir/05_Korea_education_responses.do"


