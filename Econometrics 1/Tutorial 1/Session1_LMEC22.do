*===============================================================================
* Date: 5/10/2022
* Econometrics 1, LMEC
*===============================================================================
*                	 Introduction to Stata - Lecture 1
*===============================================================================
*===============================================================================
*                          DATA MANAGEMENT          
*===============================================================================

* Clean
clear all // clean everything
set more off // to run the commands continuously without worrying about the capacity of the Results window to display the results

* Set directory
cd "C:/Users/Standard/Dropbox/Stata_tutorials/2022-2023/Lecture01/StataFiles/" // Set working directory

* Check directory
dir // display the files inside the directory

* Start log-file
capture log close
log using "lecture1", text replace // Open log: text such that it gives you a .txt file

* Useful commands!
help tabulate // open the help file from the Stata in-software documentation; look for command
search normal distribution // also for words that are not commands; give more hits and search within Stata files
help normal distribution
hsearch normal distribution // search the term in help files

* findit // to find and then install new commands; similar to net search
* update // look for updates 

* Load data
use "data_1_original.dta"
use "data_1_original.dta", clear

* Quick look at the dataset
browse // quick name: br
describe // quick name: des -> without arguments offers basic description about data in memory: #obs, #variables, type, name, label. In the first column you have the name of the variable that can be changed by the command rename.In the second and third columns there are the type and the display format that can be changed by the command format.
codebook // like describe but more specific; also give domain, unique values, frequencies, missing values
summarize // (quick name: sum)

* Count
count 
count if edu == 0
count if edu > 3
count if edu >= 3

* List 
list country if y_birth == 1900
list in 1/4

* Tabulate (quick name: tab)
tab y_birth
tab edu 
tab country female
tab country female, row // display row frequencies
tab country female, column // display col frequencies
tab country female, cell
tab edu, miss

* Replace missing valued when year of education is strictly smaller than 0
replace edu = . if edu <0 

* Generate a new variable
gen gender = 1 if female == 1 // long name of gen is generate
replace gender = 2 if female == 0
gen gender2 = 1 if female != 1 // equal to "==", not equal to "!="
replace gender2 = 2 if female != 0
des gender female gender2
bro female gender gender2

* Attach labels to values of a variable
label define sex 1 "female" 2 "male" // labels that associate numeric values with character strings. They exist separately from variables and will be displayed in printed output instead of the numeric values; they are useful for improving the comprehension of a variable that we desire to maintain in a numeric format.
label values gender sex
describe gender female
br female gender

* Check all labels
label dir //shows you the labels that you have actually defined

* If you do not specify the add, modify, or replace options, label define can be used only to create new value labels
label define sex 3 nonbinary, add // The add option lets you add codings to an existing label
label list sex
label define sex 3 NA, modify // correct a mislabeled a value
label list sex
label drop sex // We can eliminate the label sex 
label drop _all // we can eliminate all the labels in memory

* Rename vars
rename gender2 male
drop male

* Count variable
gen id = _n
bysort country: gen obs_country = _N
tab obs_country country

* Summary statistics
mean edu
centile edu
centile edu, centile (5 50 95)
centile edu, level(99)

* Performs t tests on the equality of means.
ttest edu, by(female)

* Attach label to a variable
label var gender "gender of the respondent"

* Save
save "data_1.dta"
save "data_1.dta", replace



*===============================================================================
*  			Import an excel spreadsheet; you can use also the menu

import excel data_2_original.xls, sheet("Sheet1") firstrow clear
save "data_2_original.dta", replace

clear all
import excel data_3_original.xls, sheet("Sheet1") firstrow clear
save "data_3_original.dta", replace


*===============================================================================
*                           	APPEND 
* "data_2_orignal" is the master dataset, "data_3_original" is the using dataset

/* Syntax: append using filename [filename . . .] , options; 
 OPTIONS: generate(newvar) > newvar marks source of resulting observations; 
          keep(varlist)    > keep specified variables from appending dataset(s)
		  nolabel          > do not copy value-label definitions from dataset(s) on disk
		  nonotes          > do not copy notes from dataset(s) on disk
		  force            > append string to numeric or numeric to string without error */
 
* Load master data 
use "data_2_original.dta", clear
count

* Append using data
append using "data_3_original.dta"
count

*===============================================================================

* Let's add some labels
label var health "Health in general"
label var lt_illness "Long-term illness"
label var lim_activities "Limited in activities because of health"
label var hearth_attack "Doctor told you you had hearth attack"
label var stroke "Doctor told you you had stroke"
label var diabetes "Doctor told you you had diabetes"
label var arthritis "Doctor told you you had arthritis"
label var cancer "Doctor told you you had cancer"
label var weight "Weight of respondent"
label var height "Height of respondent"
label var glasses "Wear glasses/contact lenses"
label var health_work "Health problem that limits paid work"

label define h 1 "Excellent" ///
               2 "Very good" ///
               3 "Good" ///
               4 "Fair" ///
               5 "Poor"
label values health h


label define la 1 "Severely limited" ///
                2 "Limited, but not severely" ///
                3 "Not limited"
label values lim_activities la


replace lt_illness = 0 if lt_illness == 5

* Save
save "data_2_3.dta", replace


*===============================================================================
*                                    MERGE
/* The merge command combines two Stata-format data-sets that posses variables 
   in common, adding other variables to the existing ones. It works on a master 
   data-set, currenlty in memory, and a using data-set, both sorted on one or 
   more merge variables. 
   
   data_1" is the master dataset, "data_2_3" is the using dataset 			  */

* Load master data
use "data_1.dta", clear

* Merge with using data
merge 1:1 mergeid using "data_2_3.dta"

tab _merge // By default, merge creates a new variable, merge, containing numeric codes concerning the source and the contents of each observation in the merged dataset.

*===============================================================================

* Let's move on
* Cleaning the data 
replace health=. if health<0
replace lt_illness=. if lt_illness<0
replace lim_activities=. if lim_activities<0
replace hearth_attack=. if hearth_attack<0
replace stroke=. if stroke<0
replace diabetes=. if  diabetes<0
replace arthritis=. if arthritis<0
replace cancer=. if cancer<0
replace height=. if height<0
replace glasses=. if  glasses<0
replace health_work=. if  health_work<0

* OR! You can use looping commands to save time
foreach var in health lt_illness lim_activities hearth_attack stroke diabetes ///
 arthritis cancer height glasses health_work {
 
replace `var' =. if `var' < 0

}

save "data_final.dta", replace


*===============================================================================
*                                RESHAPE

* Here we are using another dataset!
webuse reshape1, clear
br
reshape long inc ue, i(id) j(year)
br
reshape wide inc ue, i(id) j(year)
br

*===============================================================================


*===============================================================================
*                 		Examle 1: California School              
*===============================================================================

* Import data from excel files
import excel caschool.xls, sheet("caschool") firstrow clear // sheet option is needed when your excel file has more than one sheets and firstrow option tells to stata to recognise the first row as the name of the variable. You can also imprt the data from the stata menu
rename ObservationNumber id
br
save "caschool.dta" , replace // save them as .dta


import excel "caschool_expenditure.xls", firstrow clear
rename ObservationNumber id
br
save "caschool_expenditure.dta", replace // save them as .dta

* Open the data
use "caschool.dta", clear

* Labelling the variables
label var read_scr "Avg reading score"
label var math_scr "Avg math score"
label var str  "Student Teacher ratio"                               
label var testscr   "Avg test score"                          
label var dist_cod  "Disctrict code"
label var county    "County"                        
label var district  "District"                      
label var gr_span   "Grade span of district"                        
label var enrl_tot  "Total enrollment"                         
label var teachers  "Number of teachers"                          
label var el_pct    "Perc. of english learners"                             
label var meal_pct  "Perc. qualifying for reduced price lunch"                             
label var calw_pct  "Perc. qualifying for calworks"                           
label var avginc    "District avg income (in 1000's dollars)"

* Generate and Egen
gen big = str > 20
tab big
bys county: egen cscore = mean(testscr) 
label var cscore "County average test score"

* Merge with expenditure data
merge 1:1 id dist_cod using "caschool_expenditure.dta"
tab _merge
drop _merge

label var computer  "Number of computers"                          
label var comp_stu  "Computers per student"                          
label var expn_stu  "Expenditures per student"                          

*Summmarize variables of interest and Correlation Matrix
sum read_scr math_scr testscr str 
sum read_scr math_scr testscr str , detail
corr read_scr math_scr testscr str

* Graphs: Association btw tscore and str  
graph twoway (scatter testscr str) (lfit testscr str),title("Test Score and STR") ytitle("Test Score") xtitle("Student Teacher Ratio") graphregion(color(white)) // Scatterplot with overlaid linear prediction plot; lfit calculates the prediction for yvar from a linear regression of yvar on xvar and plots the resulting line.
gr save testscore , replace
gr export "testscore.pdf", replace

graph twoway (scatter testscr str) (lfitci testscr str), ///
    title("Test Score and STR") ytitle("Test Score") xtitle("Student Teacher Ratio") graphregion(color(white))
gr save testscore_ci , replace
gr export "testscore_ci.pdf", replace



/*===============================================================================
                                 FACTOR VARIABLES
===============================================================================*/
/* Although to Stata a variable is a variable, it is helpful to distinguish 
   among three conceptual types:

• A continuous variable measures something. Such a variable might measure 
  a person’s age, height, or weight; a city’s population or land area; 
  or a company’s revenues or costs.

• A categorical variable identifies a group to which the thing belongs. 
  You could categorize persons according to their race or ethnicity, cities 
  according to their geographic location, or companies according to their 
  industry. Sometimes, categorical variables are stored as strings.

• An indicator variable denotes whether something is true. 
  For example, is a person a veteran, does a city have a mass transit system, 
  or is a company profitable? 

> Stata handles categorical variables as factor variables
> A string can be transformed into a categorical variable typing: encode varname, gen(newvar) */

* Example: create a categorical variable
sum avginc
gen income =.
replace income = 1 if avginc < 10.63
replace income = 2 if avginc >= 10.63 & avginc < 13.72 // recall & = "and", while | = "or" 
replace income = 3 if avginc >= 13.72 & avginc < 17.63
replace income = 4 if avginc >= 17.63
tab income

* Graph Bar; Syntax: graph bar (mean) numeric_var, over(cat_var). Graph hbar draws horizontal bar charts; Syntax: graph hbar (mean) numeric_var, over(cat_var)
gen expend = expn_stu/10 // generate expenditure per student/10 just for a matter of scale in the graph 

gr bar (mean) computer expend , over(income , label(angle(45) labsize(small) labgap(5))) ///
   blabel(bar, position(inside) format(%9.1f) color(white)) /// blabel > add lebels to bars
   ytitle("") graphregion(color(white)) ///
   title("Average N. of Computer and Expenditure") subtitle("by Income Distribution") ///
   legend(order(1 "Number of Computers" 2 "Expenditure per student") rows(1) size(*.9)) 
gr save computer , replace
gr export "computer.pdf", replace

/* Check this link for more options: https://www.stata.com/manuals/g-2graphbar.pdf#g-2graphbarRemarksandexamples    */


* Density function; Many options for continuous and discrete cases
histogram testscr ,  bin(30) kden legend(off) color(gs13) // #of bins is needed for the continuous case. Adding the discrete option makes a histogram with a bin for each of values.
// kden= add a kernel density estimate to the graph

histogram testscr ,  bin(30) kden normal legend(off) color(gs13) title(PDF) graphregion(color(white)) //normal = add a normal density to the graph
gr save histscore , replace
gr export "histscore.pdf", replace

/* Check this link for more options:  https://www.stata.com/manuals13/rhistogram.pdf */

*  Cumulative function
cumul testscr, gen(cumtscore) // cumul creates newvar, defined as the empirical cumulative distribution function of varname.
line cumtscore testscr, sort title(CDF) graphregion(color(white))
gr save cumtestscr , replace


* More options: https://www.stata.com/manuals13/rcumul.pdf
graph combine "histscore" "cumtestscr" , title("Testscore distributions") graphregion(color(white)) 
gr save pdfcdf , replace
gr export "pdfcdf.pdf", replace


* Regression analysis
reg testscr str 
reg testscr str avginc
reg testscr str avginc el_pct

* predict fitted values
predict y_hat, xb

* predict residuals
predict u_hat, res

* Look!
br testscr y_hat u_hat

* Close the log file
log close 





