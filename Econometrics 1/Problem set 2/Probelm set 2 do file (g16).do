clear all
set more off

cd ""

// capture log close
// log using "Probelm set 2 log - group 16", text replace 



*Q1***********************
clear all
use "ps2_data1_group16"
browse
*Q1 - prelim analysis***

*1 //average of gdp, tabel
estpost tabstat lngdpcap, by(Country) stat(mean sd) nototal
est clear
esttab using "table 1a.tex", replace ///
   cells("Counrty mean sd") ///
   title(GDP - mean and sd)

   
   
*2 average of co2 also sd min max, table
est clear
estpost tabstat lnco2cap, by(Country) stat(mean sd min max) nototal
esttab using "table 1b.tex", replace ///
   cells("Counrty mean sd min max") ///
   title(CO2 mean, sd, min and max)

   
   
*3 //with tabke from the previous


*4 // graph with linear and quadratic fit
twoway scatter lnco2cap lngdpcap || lfit lnco2cap lngdpcap || qfit lnco2cap lngdpcap, ///
legend(order(2 "Linear"  3 "Quadratic")) ///
graphregion(color(white)) 
gr export "Q1prelim4.png", replace



*Q1 - reg analysis***

*1 //run basic regression
reg lnco2cap lngdpcap, robust
 
*2 //add controls
reg lnco2cap lngdpcap lnfdist lnenergycap lnpopden lnunemp lnhc lncap, robust

*3 //add quadratic term
reg lnco2cap c.lngdpcap##c.lngdpcap lnfdist lnenergycap lnpopden lnunemp lnhc lncap, robust


*4 //test qudratic term // test marginal effect of gdp // get turning point
qui reg lnco2cap c.lngdpcap##c.lngdpcap lnfdist lnenergycap lnpopden lnunemp lnhc lncap, robust

test c.lngdpcap#c.lngdpcap //test significance of the quadratic term

		//mean lngdpcap = 7.979501
test _b[lngdpcap]+2*_b[c.lngdpcap#c.lngdpcap]*7.979501=0 // test marginal effect in model 1 at average gdp

di _b[lngdpcap]/(-2*_b[c.lngdpcap#c.lngdpcap])	//turning point



*5 //test marginal at idff levels of gdp
qui reg lnco2cap c.lngdpcap##c.lngdpcap lnfdist lnenergycap lnpopden lnunemp lnhc lncap, robust

lincom _b[lngdpcap]+2*_b[c.lngdpcap#c.lngdpcap]*(5)

lincom _b[lngdpcap]+2*_b[c.lngdpcap#c.lngdpcap]*(7)

lincom _b[lngdpcap]+2*_b[c.lngdpcap#c.lngdpcap]*(9)




*6 //add high duummy
	//create high variable
centile lngdpcap, level(50)
drop high
gen high=1 if lngdpcap>=8.170279 
replace high = 0 if lngdpcap<8.170279 
	//run regression
reg lnco2cap i.high##c.lngdpcap##c.lngdpcap lnhc lncap lnunemp lnpopden lnenergycap lnfdist, robust
	// testing whether marginal effect is different from high lo low, 
test 1.high#c.lngdpcap 1.high#c.lngdpcap#c.lngdpcap



*7 - chow test // do it manually because  command contrast doesnt work
quietly reg lnco2cap c.lngdpcap##c.lngdpcap lnhc lncap lnunemp lnpopden lnenergycap lnfdist if high == 0, robust
scalar rss0 = e(rss)
scalar n0 = e(N)
quietly reg lnco2cap c.lngdpcap##c.lngdpcap lnhc lncap lnunemp lnpopden lnenergycap lnfdist if high == 1, robust
scalar rss1 = e(rss)
scalar n1 = e(N)
quietly reg lnco2cap c.lngdpcap##c.lngdpcap lnhc lncap lnunemp lnpopden lnenergycap lnfdist, robust
scalar rss = e(rss)
scalar n = e(N)
scalar F = [(rss-rss0-rss1)/9]/[(rss0+rss1)/(n0+n1-18)]
di F
// p-value F(9,390)> 45.708259 = 0.00000




*make table for all regressions in Q1-regression analysis
est clear
eststo: reg lnco2cap lngdpcap, robust
 estadd local RSE "Yes"
eststo: reg lnco2cap lngdpcap lnfdist lnenergycap lnpopden lnunemp lnhc lncap, robust
 estadd local RSE "Yes"
eststo: reg lnco2cap c.lngdpcap##c.lngdpcap lnfdist lnenergycap lnpopden lnunemp lnhc lncap, robust
 estadd local RSE "Yes"
eststo: reg lnco2cap high##c.lngdpcap##c.lngdpcap lnhc lncap lnunemp lnpopden lnenergycap lnfdist, robust
 estadd local RSE "Yes"

esttab using "table q1 - reg analysis tex", replace  ///
 b(3) se(3) r2(4) nomtitle  label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("All regressions for Q1 - regression analysis")  ///
 scalars(F "RSE Robust SE") 




 
 
 
 
 
 
**============================================================================================
**============================================================================================
*Q2***********************
clear all
use "ps2_data2_group16"
des
browse


*Q2 - reg analysis***

*1 //3 univariate regression
	//p1_8 is also standardized
reg p1_8 chisqethwvs_50  //overlap between culture and ethnicity
reg p1_8 elfethwvs_50 	// ethnolinguistic fractionalisation
reg p1_8 cultelfethwvs_50  	//cultural fractionalisation

*2 //multivariate regression with controls  (robust and not robust)
reg p1_8 chisqethwvs_50 elfethwvs_50 cultelfethwvs_50 abs_lat i.ssafrica i.seasia i.laamcarib  i.legor_ge i.legor_sc i.legor_fr

reg p1_8 chisqethwvs_50 elfethwvs_50 cultelfethwvs_50 abs_lat i.ssafrica i.seasia i.laamcarib  i.legor_ge i.legor_sc i.legor_fr, robust

*3 //tet three main coefficients
qui reg p1_8 chisqethwvs_50 elfethwvs_50 cultelfethwvs_50 abs_lat i.ssafrica i.seasia i.laamcarib  i.legor_ge i.legor_sc i.legor_fr
test chisqethwvs_50 elfethwvs_50 cultelfethwvs_50

*4 //provaide evidence oon heteroskedasticty
reg p1_8 chisqethwvs_50 elfethwvs_50 cultelfethwvs_50 abs_lat i.ssafrica i.seasia i.laamcarib i.legor_ge i.legor_sc i.legor_fr

		//test
estat hettest         // just linear heteroskedasticy
estat imtest, white   
rvfplot, yline(0) 

*5 // not stata related

*6 //do table for regressions
est clear

eststo: reg p1_8 chisqethwvs_50  
 estadd local RSE "No"
eststo: reg p1_8 elfethwvs_50 
 estadd local RSE "No"
eststo: reg p1_8 cultelfethwv~50 
 estadd local RSE "No"

eststo: reg p1_8 chisqethwvs_50 elfethwvs_50 cultelfethwv~50 abs_lat i.ssafrica i.seasia i.laamcarib  i.legor_ge i.legor_sc i.legor_fr
 estadd local RSE "No"

eststo: reg p1_8 chisqethwvs_50 elfethwvs_50 cultelfethwv~50 abs_lat i.ssafrica i.seasia i.laamcarib  i.legor_ge i.legor_sc i.legor_fr, robust
 estadd local RSE "Yes"

esttab using "table q2.6.tex", replace  ///
 b(3) se(3) r2(4) nomtitle  label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regressions estimated for part Q.21 and Q2.2")  ///
 scalars(F "RSE Robust SE") 
 


// log close


