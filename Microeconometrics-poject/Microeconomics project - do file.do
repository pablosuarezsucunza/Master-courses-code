clear all
cd "..."

//data from istat
import delimited "GEPPS_Microdati_Anno_2015.txt", clear

//clean variables
keep v3_3 v4_49 v0_5 cittad v1_7d scuola_pubblica v0_3_micro v1_1 v0_8 v1_3 v6_5 v6_10

drop if v1_7d == "  "
drop if v0_3_micro == " "

destring v0_3_micro, replace
destring v1_7d, replace



// Perform transformations
*enrollment to university
tab v3_3
gen uni_ins = .
replace uni_ins = 0 if v3_3 == 2
replace uni_ins = 1 if v3_3 == 1 
*working or not in 2012
tab v4_49
gen work2012 = .
replace work2012 = 0 if v4_49 == 2
replace work2012 = 1 if v4_49 == 1 
*gender
tab v0_5
gen female = .
replace female = 0 if v0_5 == 1
replace female = 1 if v0_5 == 2
*nationality
tab cittad
gen italian = .
replace italian = 0 if cittad == 2
replace italian = 1 if cittad == 1
*level of satisfaction reported by the students
tab v1_7d
sum v1_7d, detail
gen hs_satisfied = 0
replace hs_satisfied = 1 if v1_7d>=8 // We chose 8 as the threshold so as to have the most even split possible (42.44% satisfied, 47.56% not). Results are robust to changen the definition of this variable to equal or higher than 7.
*public school
rename scuola_pubblica public_school
*type of high school
tab v0_3_micro
gen hs_professionali = 0
replace hs_professionali = 1 if v0_3_micro == 1 
gen hs_tecnici = 0
replace hs_tecnici = 1 if v0_3_micro == 2
gen hs_liceo = 0
replace hs_liceo = 1 if inlist(v0_3_micro,3,4,5,6,7)
*If ever changed type of high school
gen changed_hs = 0
replace changed_hs = 1 if v1_1 == 1 
*graduating grade
rename v0_8 grade
*if ever failed a subject
gen ever_failed=0
replace ever_failed=1 if v1_3==1
*father education variables
tab v6_5
gen father_elementary=0
replace father_elementary=1 if v6_5==1
gen father_middle=0
replace father_middle=1 if v6_5==2
gen father_hs=0
replace father_hs=1 if v6_5==3
gen father_uni=0
replace father_uni=1 if v6_5==4
gen father_postgrad=0
replace father_postgrad=1 if v6_5==5	
drop if v6_5==6 //drop because this correspond to "Don't know" answers
sum father*
*mother education variables
tab v6_10
gen mother_elementary=0
replace mother_elementary=1 if v6_10==1
gen mother_middle=0
replace mother_middle=1 if v6_10==2
gen mother_hs=0
replace mother_hs=1 if v6_10==3
gen mother_uni=0
replace mother_uni=1 if v6_10==4
gen mother_postgrad=0
replace mother_postgrad=1 if v6_10==5	
drop if v6_10==6 //drop because this correspond to "Don't know" answers
sum mother*

// table of sum stats
label var grade "grade"
est clear 
estpost sum uni_ins work2012 hs_satisfied female italian public_school hs_professionali hs_tecnici hs_liceo changed_hs grade ever_failed mother* father*
esttab using "sum_table.tex", replace nonumber /// 
   cells("mean sd min max count") ///
   title(Summary statistics \label{sumtable}) ///
   nonote noobs label collabels("Mean" "SD" "Min" "Max" "N")



///////////////////////////////////////////////////////////////
//baseline probs by groups (for the heterogeneity analysis)
///////////////////////////////////////////////////////////////
sum work2012 uni_ins

preserve
keep if female==1
sum work2012 uni_ins
restore

preserve
keep if female==0
sum work2012 uni_ins
restore

preserve
keep if hs_professionali==1
sum work2012 uni_ins
restore

preserve
keep if hs_tecnici==1
sum work2012 uni_ins
restore

preserve
keep if hs_liceo==1
sum work2012 uni_ins
restore

preserve
keep if father_uni == 1 | father_postgrad == 1
keep if mother_uni == 1 | mother_postgrad == 1
sum work2012 uni_ins
restore

preserve
drop if (father_uni == 1 & mother_uni == 1) | (father_uni == 1 & mother_postgrad == 1) | (father_postgrad == 1 & mother_uni == 1) | (father_postgrad == 1 & mother_postgrad == 1)
sum work2012 uni_ins
restore






//drop one category to avoid collinearity in the regressions, these levels become the base category of the model.
drop mother_hs father_hs hs_liceo 
// save "final_data", replace
***********************************************

***********************************************
/*
3.- Model
*/
global firsteq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"
global secondeq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"

probit uni_ins $firsteq, robust
eststo uni: margins, dydx(*) post

probit work2012 $secondeq, robust
eststo work: margins, dydx(*) post


// global firsteq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"
// global secondeq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"

qui biprobit (uni_ins = $firsteq ) (work2012=$secondeq ), robust
scalar define rh = e(rho)
scalar define tstat_rh = e(chi2_c)

qui eststo uni_bi: margins, dydx(*) predict(pmarg1) post 
estadd scalar rho = rh
estadd scalar tsat_rho = tstat_rh

qui biprobit (uni_ins = $firsteq ) (work2012=$secondeq ), robust
qui eststo work_bi: margins, dydx(*) predict(pmarg2) post
estadd scalar rho = rh
estadd scalar tsat_rho = tstat_rh


qui biprobit (uni_ins = $firsteq ) (work2012=$secondeq ), robust
 esttab uni_bi work_bi using "bi probit.tex", replace ///
 mtitles( "Study" "Work") nonumbers ///
 stats(N rho tsat_rho , labels("Obs" "$\rho$" "$\chi^2_1(\rho=0$)")) ///
 booktabs  ///
 title(Marginal effects on the probability of studying and work \label{tab:uni_bi_probit}) ///
 
 

// Endogeneity of high school satisfaction

//model
global firsteq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"
global secondeq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"
global thirdeq "ever_failed changed_hs public_school grade hs_professionali hs_tecnici father* mother* female italian"

mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(1) seed(683)
estimates store main_model



//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean), seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean), seed(683) reps(1000): sum APE_hssat_work
cap restore













capture log close
log using "heterogeneous effects", replace
//#HETEROGENOUS EFFECTS
global firsteq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"
global secondeq "hs_satisfied public_school hs_professionali hs_tecnici father* mother* female italian"
global thirdeq "ever_failed changed_hs public_school grade hs_professionali hs_tecnici father* mother* female italian"

//SEX OF THE STUDENT

//Women
use "final_data.dta", clear

keep if female == 1

qui mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(50) seed(683)
estimates store main_model

//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean),  seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean),  seed(683) reps(1000): sum APE_hssat_work
cap restore


//Men
use "final_data.dta", clear

keep if female == 0

//Men's case
qui mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(50) seed(683)
estimates store main_model

//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean),  seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean),  seed(683) reps(1000): sum APE_hssat_work
cap restore





//PARENTS EDUCATION
use "final_data.dta", clear

keep if father_uni == 1 | father_postgrad == 1
keep if mother_uni == 1 | mother_postgrad == 1

qui mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(50) seed(683)
estimates store main_model

//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean),  seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean),  seed(683) reps(1000): sum APE_hssat_work
cap restore


// At least one of the parents has lower than university education.
use "final_data.dta", clear
drop if (father_uni == 1 & mother_uni == 1) | (father_uni == 1 & mother_postgrad == 1) | (father_postgrad == 1 & mother_uni == 1) | (father_postgrad == 1 & mother_postgrad == 1)

qui mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(50) seed(683)
estimates store main_model

//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean),  seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean),  seed(683) reps(1000): sum APE_hssat_work
cap restore





//TYPE OF HS
//liceo
use "final_data.dta", clear

keep if hs_tecnici == 0 & hs_professionali == 0

qui mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(50) seed(683)
estimates store main_model

//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean),  seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean),  seed(683) reps(1000): sum APE_hssat_work
cap restore


//hs tecnico
use "final_data.dta", clear

keep if hs_tecnici == 1

qui mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(50) seed(683)
estimates store main_model

//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean),  seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean),  seed(683) reps(1000): sum APE_hssat_work
cap restore


//hs professionali
use "final_data.dta", clear

keep if hs_professionali == 1

qui mvprobit (uni_ins = $firsteq ) (work2012=$secondeq ) (hs_satisfied=$thirdeq ), robust draws(50) seed(683)
estimates store main_model

//marginal effect of hs_satisfied
cap preserve 
estimates restore main_model
replace hs_satisfied=1
mvppred pred_xb, xb
replace hs_satisfied=0
mvppred pred_xb_, xb

//marginal effect of high school satisfaction on prob(uni_ins)
cap gen APE_hssat_uni=normal(pred_xb1)-normal(pred_xb_1)
bootstrap r(mean),  seed(683) reps(1000): sum APE_hssat_uni

//marginal effect of high school satisfaction on prob(work2012)
cap gen APE_hssat_work=normal(pred_xb2)-normal(pred_xb_2)
bootstrap Mean=r(mean),  seed(683) reps(1000): sum APE_hssat_work
cap restore



// log close
// translate "heterogeneous effects.smcl" heterogeneous effects.pdf", translator(smcl2pdf)









