
/*===============================================================================
 Date: 11/10/2022
 Econometrics 1, LMEC 
================================================================================
                    Introduction to Stata - Lecture 2
================================================================================*/
clear all
set more off

cd "..."

capture log close
log using "lecture2", text replace 

*===============================================================================
*                              OLS with MATA: Example 1
*===============================================================================
ssc install bcuse
bcuse bwght, clear // from Wooldrige's textbook

/* The data set:   
  1. faminc                   1988 family income, $1000s
  2. cigtax                   cig. tax in home state, 1988
  3. cigprice                 cig. price in home state, 1988
  4. bwght                    birth weight, ounces
  5. fatheduc                 father's yrs of educ
  6. motheduc                 mother's yrs of educ
  7. parity                   birth order of child
  8. male                     =1 if male child
  9. white                    =1 if white
 10. cigs                     cigs smked per day while preg
 11. lbwght                   log of bwght
 12. bwghtlbs                 birth weight, pounds
 13. packs                    packs smked per day while preg
 14. lfaminc                  log(faminc)										*/

* OLS using regress
reg bwght cigs, noconst

* OLS using Mata
mata

Y = st_data(.,"bwght")
X = st_data(.,"cigs")
beta = invsym(X'X)*X'Y
beta

end

/*Instead of st_data() you can use "putmata" which also exports the contents of Stata variables to Mata vectors and matrices.
putmata Y = (bwght) , replace
putmata X = (cigs) , replace
*/

/*==============================================================================
                             OLS in MATA: Example 2           
===============================================================================*/

sysuse lifeexp
des
reg lexp gnppc region
keep if e(sample)==1   // Drop observations with missing values. Mata understands missing values, but Mata is a matrix language, not a statistical system, so Mata does not always ignore observations with missing values.

/*
1. genertate a constant
2. create y vector from Stata dataset, nX1
3. create X matrix from Stata dataset, nXk
4. matrix computation to generate OLS estimates, residuals, no. of obs and no. of regressors */

gen cons=1

mata

st_view(y =.,.,"lexp")  // st_view() serves the same purpose as st_data(), except that, rather than returning a matrix that is a copy of the underlying values, st_view() create a matrix that is a view onto the Stata dataset itself. Alternatively, y=st_data(.,"lexp" )
st_view(X =.,.,("gnppc", "region", "cons"))
beta = invsym(X'X)*X'y
beta

u = y - X*beta //residuals 

n = rows(X)
k = cols(X)
s2 = (u'u)/(n-k)   // Variance of the OLS residuals
Var = s2*invsym(X'X) // Variance matrix
Var
     
sst = y'y-rows(y)*mean(y)^2 
sst
sse = beta'X'y-rows(y)*mean(y)^2
sse
ssr = y'y-beta'X'y // u'u = sst-sse
ssr

u_hat = y-X*beta
u_hat_mean = mean(u_hat)
u_hat_mean

end


*===============================================================================
*                         	  Data Generating             
*===============================================================================

clear all

** Uniform random-number generation
* Obtain and display one draw from the uniform
set seed 123456 // Recall always to set a seed!
scalar u = runiform() // N(m,s^2)
display u 

* Obtain 10000 draws from the uniform distribution and some details on these draws
quietly set obs 10000
set seed 123
generate x = runiform()
list x in 1/5, clean
summarize x

** Draws from normal distribution 
clear all

* For simulations of standard estimators such as OLS all that is needed are draws from the uniform and normal distributions because normal errors are a natural starting point and the most common choices of distribution 
* for generating regressors are normal and uniform.
quietly set obs 1000
set seed 12345
generate uniform = runiform()
generate stnormal = rnormal() // N(0,1)
generate norm5and2 = rnormal(5,2)
tabstat uniform stnormal norm5and2, stat(mean sd skew kurt min max) col (stat) // Try with larger sample! (but not too big, it will take much time!)


** Example 1: UNIVARIATE distribution             
clear all

set obs 30 // set the number of observations
drawnorm x, means(0) cov(2) // drawing form N(0,2) normal distrubution
gen x1 = rnormal(0,2) // alternative way like above
sum x x1 // look at the mean and standard deviation
hist x, normal // make an histogram overlapping a normal distribution

gen y=(1-4*x) + rnormal(0,1) // generate a variable y centred on 1-4x with a random error (0,1)


* Example 2: BIVARIATE distribution
clear all
set obs 100
matrix C = (1, 0.7 \ 0.7, 1) // set the variance covariance matrix of the bivariate distribution -> NB: here we are not in a Mata environment! We are using Stata directly
matrix m = (20, 10) // set the vector of the means of the bivariate distribution
drawnorm y x1, cov(C) means(m)  // generate a bivariae standard normal distribution with the parameters defined above
sum y x1
corr y x1


* Example 3: TRIVARIATE distribution             
clear all
set obs 100
matrix C = (1,0.3, 0.2\ 0.3,1, 0.1\ 0.2, 0.1,1) // set the varcov matrix of the trivariate distribution
matrix m = (10, 10, 15) // set the vector of the means of the bivariate distribution
drawnorm y x1 x2,cov(C) means(m)  // generate a trivariate standard normal distribution with the parameters defined above
sum y x1 x2
corr y x1 x2



*===============================================================================
*                         	  		Macros              
*===============================================================================
/*

Global macros, once defined, are available anywhere in Stata.Global tells Stata to store everything in the command line in its memory until you exit Stata. If you open another data set before exiting, the global macro will still be in memory.
Local macros exist solely within the program or do-file in which they are defined.

Syntax of Macro Assignment:
------------------------------
local macroname "string" 
local macroname = expression 
global macroname "string" 
global macroname = expression
------------------------------ 
*/

* Let's see some examples
local value "hello" // it works only while the do-file is running
display "`value'"

clear all                                                                 
bcuse bwght 

lab var bwght "birth weight" 
lab var cigs "number of cigarettes mother smoked" 
lab var parity "birth order" 
lab var faminc "annual family income" 
lab var motheduc  "years of schooling for mother" 
lab var fatheduc  "years of schooling for father"

local y "bwght"
display "`y'"

global x "parity cigs fatheduc motheduc" // it is always present
display "$x"

local myvars "faminc bwght fatheduc motheduc cigs"
summarize `myvars'

local model_1 "cigs"
local model_2 "cigs fatheduc motheduc"

reg bwght `model_1'
reg bwght `model_2'



*===============================================================================
*                         	   Rstats and Estats              
*===============================================================================

* Summarise
sum bwght, detail
return list
display r(N)
gen x = r(N)
scalar y = r(N)
scalar list y

* OLS using regress
reg bwght cigs

* Rstats - results showned in the table
return list
matrix A = r(table)
matrix list A

* Estats - estimation results
ereturn list
display e(N)
scalar list y
scalar z = e(N)
matrix B = e(b)
matrix list B



*===============================================================================
*                         	  Simulation Program              
*===============================================================================

/*                               Simulation Program 

    Example: Consider two random variables (y,x) that are jointly 
	normally distributed in the population. 
	The bivariate distribution is characterized by:
	
	mu_y = 30, 
	mu_x = 25,
	sigma2_y=1, 
	sigma2_x=1,
	cov_yx=0,

	Aim: generate 100 random samples of (y,x) by 50 observations each (N = 50).
	For each sample estimate parameters of univariate regression model: beta0 and beta1
	and report estimates into a new data set with 100 observations where each observation
	corresponds to a realization of beta0 and beta1.
*/

clear all

* First: set observations and seed
set obs 50
set seed 123456

* Second: Find the POPULATION coefficients b0 and b1 - the TRUE values
mata 

sigma = (1, 0 \ 0, 1)      //  h= sigma[0,2]   // This is variance-covariance matrix
sigma_x = (1)              // This is the Var(x). Be careful! Here we have only one independent variable. If we had more than one then sigma_x would be a vector                
sigma_xy = (0) 			   // This is Cov(xy). Again, if we had more than one independent variables then sigma_xy would be a vector
mu = (35,25)                //   This is the sample mean vector

B = invsym(sigma_x) * sigma_xy // Finding b1; B would be a vector if we had more than one independent variables
B
b0 = mu[1,1]- (mu[1,2])*B     // b0 = y- xb / mu[element row 1, column 1]
b0           
b1 = B[1,1] 
b1           
Beta = (b0 \ b1) 
Beta
                       
 end
 

* Third: Create a program to generate a SINGLE random sample from a bivariate distribution (y and x)
capture program drop random_sample // to drop the program if already in use

program define random_sample, rclass // Define the name of the program

	drop _all
	scalar drop _all
	matrix drop _all
	
	set more off
	set obs 50 // Here you set the sample size
	
	matrix sigma = (1, 0 \ 0, 1) // Here you set the var/cov matrix of the bivariate distribution
	matrix mu = (30, 25)  // Here you set the vector of means of the bivariate distribution
	drawnorm y x, cov(sigma) means(mu) // This command generates a bivariate standard normal distribution with the parameters defined above

	reg y x // Store regression coefficients in r() in order to return them in the simulation
	return scalar beta0 = _b[_cons]  // _b[namevariable]
	return scalar beta1 = _b[x]    
	
end // End of program 



* Fourth: we use the command "simulate" to run a monte carlo simulation - it requires the specification of a number of statistics it needs to generate, and the number of repetitions (100)
simulate beta0 = r(beta0) beta1 = r(beta1), reps(100) saving("montecarlo.dta", replace) seed(1234567): random_sample // this specifies the use of programe random_sample we just created and to create a new dataset to store the results in


* Test the unbiasedness property of OLS estimators
putmata B0 = (beta0), replace
putmata B1 = (beta1), replace

mata

BETA = (B0,B1)			    // matrix of column vectors of estimates // Beta(cons, x)
EBETA = (mean(BETA))'		// column vectors mean estimates
EBETA
Beta						// Theoretical values of betas
bias = EBETA - Beta		    // Vector of bias = (Eb-b)
bias

end


*===============================================================================
*                         	  		Loops              
*===============================================================================

/*                                                                              

Foreach is a more general loop. String, numeric, and variables are allowed as list, and lists do not have to have a pattern.
Forvalues is a more specific loop. Only numeric is allowed as lists, and lists should have a clear pattern.

*/

* Some Examples:   

clear all                                                                 
bcuse bwght 

forvalues i=1/3 {
	display "i is now `i'" 
 }

tab parity           
                                                                                   
foreach i in 1 2 3 4 5 6 {
	summarize bwght if parity ==`i'
}

foreach i in fatheduc motheduc {
	tabulate `i', missing
}
 
 foreach i in bwght fatheduc motheduc parity {
 	local varlabel: variable label `i'
	display "`i'"  "`varlabel'"
 }

foreach i in 1 2 3 4 5 6 {
	regress bwght cigs if parity ==`i'
}

forvalue i = 1(1)6 {
	  summarize bwght if parity ==`i'
}

log close
