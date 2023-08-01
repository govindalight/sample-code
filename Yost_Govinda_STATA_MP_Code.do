*STATA code for Master's Thesis

clear
set mem 200m
use "C:\Users\yostx053\Downloads\NP_Merged_2.dta"
*log using "C:\Users\yostx053\OneDrive\Univ. of Min. MS\Masters_Project\Data\NP_Merged_2.dta", text replace

*Create percentage of FPA variable
gen per_fpa = prop_fpa*100

*Variable Modification
label var per_fpa  `"NP Full Practice Authority (% of pop.)"'
label var prop_fpa `"Proportion of Pop. under NP Full Practice Authority"'
label var obnpvis_mean `"Mean Annual Total Office-Based Visits to NPs"'
label var obpavis_mean `"Mean Annual Total Office-Based Visits to PAs"'
label var obphvis_mean `"Mean Annual Total Office-Based Visits to NPs and PAs"'
label var obnppavis_mean `"Mean Annual Total Office-Based Visits to Physicians"'
label var obtotvis_mean `"Mean Annual Total Office-Based Visits"'
label var obnpvis_prop `"Proportion of pop. who made a vist to an office-based NP"'
label var obpavis_prop `"Proportion of pop. who made a vist to an office-based PA"'
label var obnppavis_prop `"Proportion of pop. who made a vist to an office-based NP or PA"'
label var obphvis_prop `"Proportion of pop. who made a vist to a Physician"'
label var obtotvis_prop `"Proportion of pop. who made an office-based visit"'

*Set Data to Survey
svyset psup [pweight=perweight],strata(stratap)
*svyset [pweight=perweight],strata(stratap) psu(psup)

*svyset [pweight=perwt11f], strata(varstr) psu(varpsu)

*Create Region and Year and Insurance variables for fixed effects
tab (regionmeps), gen(regionmeps)
tab (year), gen(year)
*Create Marital, Race Variables for covariates
tab (marital), gen(marital)
tab (race), gen(race)
tab (ins), gen(ins)

*Percent NP FPA variable Creation
gen per_fpa = prop_fpa*100

*NP+PA Variable Creation
gen obnppavis = obnpvis + obpavis


*Binary Variable Creation
gen obnpvis1 = obnpvis > 0
gen obpavis1 = obpavis > 0
gen obphvis1 = obphvis > 0
gen obnppavis1 = obnppavis1 > 0
gen obtotvis1 = obtotvis > 0

*Age Squared Variable Creation
gen age_sq = (age)^2
*Natural log income Variable Creation
gen ftotincmeps_ln = ln(ftotincmeps)
*Race Variable Creation
gen race = 5 if race!=100 | race!=200 | race!=410 & hispyn==1
replace race = 2 if racea==200 & hispyn==1
replace race = 3 if hispyn==2
replace race = 4 if racea==410 & hispyn==1
replace race = 1 if racea==100 & hispyn==1

*Insurance Variable Creation
gen ins = 1 if hiprivate==2
replace ins = 2 if himcare==2
replace ins = 3 if himachip==2
replace ins = 4 if hichampany==2 | hiothgova==2 | hiothgovb==2
replace ins = 0 if ins==.
	*Adjustment to Ins

*Education variable edit
gen education13 = edrecode if year==2013
gen education1 = 0 if educyr < 12 | educyr >=96 | education13 < 300 | education13 >= 996
replace education1 = 1 if educyr >=12 & educyr <= 17 | education13 >=300 & education13 <=600


*Marital Status Year edit
gen marital = 1 if marstat==10 | marstat==11 | marstat==12 | marstat==13
replace marital = 2 if marstat==50
replace marital = 3 if marstat==30 | marstat==20 | marstat==40 | marstat==99 | marstat==00

*Health1 (health status) variable creation
gen health1 = 1 if health <= 3 & health > 0
replace health1 = 0 if health > 3 & health < 10 | health==0

*Health status check
gen health2 = health if health > 0 & health <= 5
replace health2 = 0 if health==9 | health==8 | health==7


*evwork variable creation
gen evwork = 1 if workev==2
replace evwork=0 if workev==1 | workev==7 | workev==8 | workev==9 | workev==0
*Female variable creation
gen female = 1 if sex==2
replace female = 0 if sex==1

*NP proportion of total OB Visits variable creation
gen npovertot = obnpvis / obtotvis
replace npovertot = 0 if obtotvis==0

*Summary Stats on Controls
svy: mean age sex ftotincmeps , over(regionmeps)
svy: proportion racea , over(regionmeps)
*GRAPH FPA over time by Region
local ps "scatter per_fpa year [pweight=perweight]"
local fit "lfit per_fpa year [pweight=perweight]"
twoway (`ps' if regionmeps==1, msymbol(circle_hollow) msize(*.4)) (`ps' if regionmeps==2, msymbol(circle_hollow) msize(*.4)) (`ps' if regionmeps==3, msymbol(circle_hollow) msize(*.4)) (`ps' if regionmeps==4, msymbol(circle_hollow) msize(*.4)) (`fit' if regionmeps==1)(`fit' if regionmeps==2)(`fit' if regionmeps==3)(`fit' if regionmeps==4), legend(order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") row(1))
*twoway (`ps' if regionmeps==1, msize(medsmall)) (`ps' if regionmeps==2, msize(medsmall)) (`ps' if regionmeps==3, msize(medsmall)) (`ps' if regionmeps==4, msize(medsmall)) (`fit' if regionmeps==1)(`fit' if regionmeps==2)(`fit' if regionmeps==3)(`fit' if regionmeps==4), legend(order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") row(1))

*Graph OBNPVIS over time
twoway (scatter obnpvis year) (lfit obnpvis year)
twoway (scatter obnpvis year) (lfit obnpvis year) if obnpvis > 0, by(regionmeps)
svy: reg obnpvis year
svy: reg obnpvis year if obnpvis > 0
svy: reg obnpvis year if obnpvis > 1

*DIFF-IN-DIFF
*1996-2014 Regression
svy: reg obnpvis per_fpa regionmeps2-regionmeps4 year2-year17 year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 ins6 marital1 marital3 education1 anylmt

*2000-2014 Regression
*Mean
svy, subpop(if year>1999): reg obnpvis per_fpa regionmeps2-regionmeps4 year6-year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using Diff-in-Diff_NP_Mean.xls, replace
svy, subpop(if year>1999): reg obnppavis per_fpa regionmeps2-regionmeps4 year6-year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using Diff-in-Diff_NPPA_Mean.xls, replace
svy, subpop(if year>1999): reg obphvis per_fpa regionmeps2-regionmeps4 year6-year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using Diff-in-Diff_Phys_Mean.xls, replace
svy, subpop(if year>1999): reg obtotvis per_fpa regionmeps2-regionmeps4 year6-year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using Diff-in-Diff_TotOB_Mean.xls, replace
svy, subpop(if year>1999): reg npovertot per_fpa regionmeps2-regionmeps4 year6-year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using Diff-in-Diff_npovertot_mean.xls, replace
svy, subpop(if year>1999): reg obnpvis1 per_fpa regionmeps2-regionmeps4 year6-year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using Diff-in-Diff_obnpvis1.xls, replace

*Prop
capture log close
log using Prop_Diff-in-Diff.log, replace
svy, subpop(if year>1999): reg obnpvis1 per_fpa regionmeps2-regionmeps4 year6-year19 age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
log close

*NP over Tot DESCRIPTIVE REGRESSION
*Raw Regression for each region
svy, subpop(if regionmeps==1 & year>1999): reg npovertot per_fpa
outreg2 using NPoverTot_Unadjusted_Northeast.xls, replace

svy, subpop(if regionmeps==2 & year>1999): reg npovertot per_fpa
outreg2 using NPoverTot_Unadjusted_Midwest.xls, replace

svy, subpop(if regionmeps==3 & year>1999): reg npovertot per_fpa
outreg2 using NPoverTot_Unadjusted_South.xls, replace

svy, subpop(if regionmeps==4 & year>1999): reg npovertot per_fpa
outreg2 using NPoverTot_Unadjusted_West.xls, replace

*Regression with Pat. Char
svy, subpop(if regionmeps==1 & year>1999): reg npovertot per_fpa age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using NPoverTot_Adjusted_Northeast.xls, replace

svy, subpop(if regionmeps==2 & year>1999): reg npovertot per_fpa age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using NPoverTot_Adjusted_Midwest.xls, replace

svy, subpop(if regionmeps==3 & year>1999): reg npovertot per_fpa age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using NPoverTot_Adjusted_South.xls, replace

svy, subpop(if regionmeps==4 & year>1999): reg npovertot per_fpa age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1

*Regression coef for each region with time trend
svy, subpop(if regionmeps==1 & year>1999): reg npovertot per_fpa year age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using NPoverTot_Adjusted_TT_Northeast.xls, replace

svy, subpop(if regionmeps==2 & year>1999): reg npovertot per_fpa year age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using NPoverTot_Adjusted_TT_Midwest.xls, replace

svy, subpop(if regionmeps==3 & year>1999): reg npovertot per_fpa year age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using NPoverTot_Adjusted_TT_South.xls, replace

svy, subpop(if regionmeps==4 & year>1999): reg npovertot per_fpa year age age_sq female ftotincmeps ftotincmeps_ln race2-race5 ins1 ins3 ins4 ins5 marital2 marital3 education1 health1
outreg2 using NPoverTot_Adjusted__TT_West.xls, replace

*Loop for creating means for each region and year combination
	*NP Visits mean
gen obnpvis_mean=.

foreach r in 0 1 2 3 4 {

forvalues yr=1996(1)2014 {

svy: mean obnpvis if regionmeps==`r' & year==`yr'

matrix define temp=e(b)

replace obnpvis_mean=temp[1,1] if regionmeps==`r' & year==`yr'
}
}

bysort obnpvis_mean regionmeps year: keep if _n==1


twoway scatter obnpvis_mean year, by(regionmeps)

twoway (connected obnpvis_mean year if regionmeps==3, yaxis(1)) (connected per_fpa year if regionmeps==3, yaxis(2)), title("South")

	*PA Visits mean
gen obpavis_mean=.

foreach r in 0 1 2 3 4 {

forvalues yr=1996(1)2014 {

svy: mean obpavis if regionmeps==`r' & year==`yr'

matrix define temp=e(b)

replace obpavis_mean=temp[1,1] if regionmeps==`r' & year==`yr'
}
}


*SVY doesn't allow if to be used for variance estimations. only subpop


*Plot for NP Visits for each Region
*Mean
twoway (connected obnpvis_mean year if regionmeps==4, yaxis(1)) (connected per_fpa year if regionmeps==4, yaxis(2)), legend(rows(2)) title("West")
twoway (connected obnpvis_mean year if regionmeps==3, yaxis(1)) (connected per_fpa year if regionmeps==3, yaxis(2)), legend(rows(2)) title("South")
twoway (connected obnpvis_mean year if regionmeps==2, yaxis(1)) (connected per_fpa year if regionmeps==2, yaxis(2)), legend(rows(2)) title("Midwest")
twoway (connected obnpvis_mean year if regionmeps==1, yaxis(1)) (connected per_fpa year if regionmeps==1, yaxis(2)), legend(rows(2)) title("Northeast")
*Plot for NP proportion of total OB Visits by Region
twoway (connected npovertot_mean year if regionmeps==1 & year>1999,lpattern(dash) mstyle(none)) (connected npovertot_mean year if regionmeps==2 & year>1999,lpattern(dash_dot) mstyle(none))(connected npovertot_mean year if regionmeps==3 & year>1999, lpattern(--.) mstyle(none))(connected npovertot_mean year if regionmeps==4 & year>1999, lpattern(solid) mstyle(none)),legend(order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West"))
twoway (connected npovertot_mean year if regionmeps==1 & year>1999,lwidth(medthick) mstyle(none)) (connected npovertot_mean year if regionmeps==2 & year>1999, lwidth(medthick)mstyle(none))(connected npovertot_mean year if regionmeps==3 & year>1999, lwidth(medthick) mstyle(none))(connected npovertot_mean year if regionmeps==4 & year>1999,  lwidth(medthick) mstyle(none)),legend(order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") rows(1))

*Labeling Values in Race Variable
label define race_lbl 1 `"White, Non-Hispanic"'
label define race_lbl 2 `"Black/African-American, Non-Hispanic"', add
label define race_lbl 3 `"Hispanic/Latino"', add
label define race_lbl 4 `"Asian"', add
label define race_lbl 5 `"Other"', add
label values race race_lbl

*Labeling Values in Ins Variable
label define ins_lbl 0 `"Uninsured"'
label define ins_lbl 1 `"Covered Private Insurance Only"', add
label define ins_lbl 2 `"Has Medicare Insurance"', add
label define ins_lbl 3 `"Covered by Medicaid and/or SCHIP"', add
label define ins_lbl 4 `"Covered by Other"', add
label values ins ins_lbl

*Labeling Values in Marital Variable
label define marital_lbl 0 `"Unknown Marital Status"'
label define marital_lbl 1 `"Married"', add
label define marital_lbl 2 `"Never Married"', add
label define marital_lbl 3 `"Divorced, Widowed, or Separated"', add
label values marital marital_lbl

label define marital_lbl 3 `"Divorced, Widowed, or Separated"', modify

*Summary Statistics Calculations
	*Ind Variable
svy, subpop(if year > 1999): mean per_fpa, over(regionmeps)
estat sd

*Outcomes
svy, subpop(if year > 1999): mean obnpvis, over(regionmeps)
estat sd

svy, subpop(if year > 1999): mean obnpvis1, over(regionmeps)
estat sd

svy, subpop(if year > 1999): mean obnppavis, over(regionmeps)
estat sd

svy, subpop(if year > 1999): mean obnppavis1, over(regionmeps)
estat sd

svy, subpop(if year > 1999): mean obtotvis, over(regionmeps)
estat sd

svy, subpop(if year > 1999): mean obtotvis1, over(regionmeps)
estat sd

*Patient Char

svy, subpop(if year > 1999): mean age, over(regionmeps)
estat sd

svy, subpop(if year > 1999): prop female, over(regionmeps)

svy, subpop(if year > 1999): prop race, over(regionmeps)

svy, subpop(if year > 1999): mean ftotincmeps, over(regionmeps)
estat sd

svy, subpop(if year > 1999): prop ins, over(regionmeps)

svy, subpop(if year > 1999): prop marital, over(regionmeps)

svy, subpop(if year > 1999): prop education1, over(regionmeps)

svy, subpop(if year > 1999): prop evwork, over(regionmeps)

svy, subpop(if year > 1999): prop health1, over(regionmeps)

*Export Graph

graph export Prop_NP_Visits_by_Region.png