set more off
clear all
*capture log close
*log using "log_file", text replace

cd ""

*--------------------------------------------------------
*------------------------ Part 1 ------------------------
*--------------------------------------------------------

*1.1

use "data.dta", clear
xtset iso year
gsort country year

xtdes

*1.2

ssc install unique
unique country
unique year

bysort country: gen T=_N //T is the number of observations for the same country

xttab iso

unique country if T<18
unique country if T<=10
tab country if T<=10

//there are 25 countries with strictly less than 18 observations, and 15 with less or equal than 10

drop if T<18

*1.3

preserve

collapse (mean) a_asylums=asylums a_temp=temp a_minor_conflict=minor_conflict a_major_conflict=major_conflict (sd) sd_asylums=asylums sd_temp=temp , by(iso)

gsort -a_asylums
list in 1/5
//we only show the first 5, because all the countries are too many

*1.4

tw (sc a_asylums sd_temp if a_major_conflict==0, mcolor(green) msize(vsmall) mlabel(iso) mlabc(black) mlabs(vsmall)) (sc a_asylums sd_temp if a_major_conflict>0, mcolor(red) mlabc(black) msize(vsmall) mlabel(iso) mlabs(vsmall)) (lfit a_asylums sd_temp, lc(blue)), legend(order( 1 "Never in a major conflict" 2 "At least one year in a major conflict" 3 "Fitted line")) title("Graph 1") xtitle("Temperature's standard deviation") ytitle("Average asylum applications") saving(Graph_1, replace)

graph export Graph_1.png, replace

restore

*1.6

//program (called xtsum2) to store output of xtsum and output it to latex
program define xtsum2, eclass

syntax varlist

foreach var of local varlist {
    xtsum `var'

    tempname mat_`var'
    matrix mat_`var' = J(3, 5, .)
    matrix mat_`var'[1,1] = (`r(mean)', `r(sd)', `r(min)', `r(max)', `r(N)')
    matrix mat_`var'[2,1] = (., `r(sd_b)', `r(min_b)', `r(max_b)', `r(n)')
    matrix mat_`var'[3,1] = (., `r(sd_w)', `r(min_w)', `r(max_w)', `r(Tbar)')
    matrix colnames mat_`var'= Mean "Std. Dev." Min Max "N/n/T-bar"
    matrix rownames mat_`var'= `var' " " " "

    local matall `matall' mat_`var'
    local obw `obw' overall between within
}

if `= wordcount("`varlist'")' > 1 {
    local matall = subinstr("`matall'", " ", " \ ",.)
    matrix allmat = (`matall')
    ereturn matrix mat_all = allmat
}
else ereturn matrix mat_all = mat_`varlist'
ereturn local obw = "`obw'"

end


xtsum2 asylums temp rain minor_conflict major_conflict
esttab  e(mat_all) using "xtsum.tex", replace mlabels(none) labcol2(`e(obw)') varlabels(asylums "Asylum applications" temp "Temperature" rain "Rainfall" minor_conflict "Minor conflict" major_conflict "Major conflict" r2 " " r3 " ") ///
prehead("\begin{table}[h!]" ///
"\centering" ///
"\caption{Summary statistics}" ///
"\label{xtsumtab}" ///
"\resizebox{\textwidth}{!}{%" ///
"\begin{tabular}{l*{6}{c}}" ///
"\hline\hline") ///
postfoot("\hline\hline" ///
"\end{tabular}" ///
"}" ///
"\end{table}")

*--------------------------------------------------------
*------------------------ Part 2 ------------------------
*--------------------------------------------------------

ssc install estout
est clear

*2.1

gen l_asylums=log(asylums)

eststo Model_1: reg l_asylums temp, robust

margins, at(temp=(0(1)40))

marginsplot, xti(Temperature in C°) yti(Predicted log asylum applications) saving(Graph_2, replace) recast(line) ciopt(color(%30))

graph export Graph_2.png, replace

*2.2

eststo Model_2: reg l_asylums c.temp##c.temp, robust

margins, at(temp=(0(1)40))

marginsplot, xti(Temperature in C°) yti(Predicted log asylum applications) saving(Graph_3, replace) recast(line) ciopt(color(%30))

graph export Graph_3.png, replace

*2.3

eststo Model_3: xtreg l_asylums c.temp##c.temp i.year, fe robust
 estadd local CFE "Yes"
 estadd local YFE "Yes"

*2.4

eststo Model_4: xtreg l_asylums c.temp##c.temp c.rain##c.rain minor_conflict major_conflict i.year, fe robust
 estadd local CFE "Yes"
 estadd local YFE "Yes"

*2.5

margins, at(temp=(0(1)40))

marginsplot, xti(Temperature in C°) yti(Predicted log asylum applications) saving(Graph_4, replace) recast(line) ciopt(color(%30))

di "The optimal temperature is " _b[temp]/(-2*_b[c.temp#c.temp]) " °C"

graph export Graph_4.png, replace

esttab Model_1 Model_2 Model_3 Model_4 using "table_1.tex", replace star(* 0.10 ** 0.05 *** 0.01) se nogaps mti("(1)" "(2)" "(3)" "(4)") nonum k(temp c.temp#c.temp rain c.rain#c.rain minor_conflict major_conflict) ///
varlabel(temp "Temperature" c.temp#c.temp "Temperature squared" rain "Rainfall" c.rain#c.rain "Rainfall squared" minor_conflict "Minor conflict" major_conflict "Major conflict") ///
stats(N r2 CFE YFE, labels("\(N\)" "\(R^2\)" "Country FE" "Year FE")) ///
prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" ///
"\begin{table}[h!]" ///
"\centering" ///
"\caption{Regressions results for log asylum applications}" ///
"\label{tab1}" ///
"\begin{tabular}{l*{4}{c}}" ///
"\hline\hline") ///
postfoot("\hline\hline" ///
"\multicolumn{5}{l}{\footnotesize Robust standard errors in parentheses} \\" ///
"\multicolumn{5}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}" ///
"\end{tabular}" ///
"\end{table}")

*log close
