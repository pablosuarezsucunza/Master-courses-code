clear all
set more off

*set seed 1016



capture log close
log using "Probelm set 1 log - group 16", text replace 

*=========================================
*QUESTION 1
*=========================================ç

*Q1.1***
clear all
set seed 1016
set obs 1000
matrix C = (1, 0.3, -0.2\ 0.3, 1, -0.4\ -0.2, -0.4, 1) 
matrix m = (25, 18, 9) 
drawnorm y x1 x2, cov(C) means(m) 


*q1.1a*
mata
covxy = (0.3 \ -0.2) // covxy matrix
varcovx1x2 = (1, -0.4 \ -0.4, 1) // var cov x1x2
meany = (25)
meanx = (18, 9)

beta = invsym(varcovx1x2) * covxy
beta
beta0 = meany - meanx * beta
beta0

popbeta = (beta0 \ beta[1,1] \ beta[2,1])
popbeta
end




*q1.1d*
gen const = 1
mata
Y = st_data(., "y")
X = st_data(., ("const", "x1", "x2"))
beta = invsym(X'X)*X'Y
beta
end







*Q1.2***

capture program drop progq12
program define progq12, rclass 

	drop _all
	scalar drop _all
	matrix drop _all
	
	set more off
	set obs 100
	
	matrix C = (1, 0.3, -0.2\ 0.3, 1, -0.4\ -0.2, -0.4, 1) 
	matrix M = (25, 18, 9) 
	drawnorm y x1 x2, cov(C) means(M)
	
	reg y x1 x2
	return scalar beta0 = _b[_cons]  
	return scalar beta1 = _b[x1]    
	return scalar beta2 = _b[x2]  
end 

*q1.2a*
set seed 1016
simulate beta0 = r(beta0) beta1 = r(beta1) beta2 = r(beta2), reps(1000) saving("simulationQ12.dta", replace) seed(1016): progq12


*q1.2b*
use"simulationQ12.dta"
putmata B0 = (beta0), replace
putmata B1 = (beta1), replace
putmata B2 = (beta2), replace

mata

BETA = (B0,B1,B2)			    
EBETA = (mean(BETA))'		
EBETA
popbeta						
bias = EBETA - popbeta		    
bias

end


*q1.2c*

use"simulationQ12.dta"

histogram beta1, normal
graph export "dist beta1 part2.png", replace
histogram beta2, normal
graph export "dist beta2 part2.png", replace



*Q1.3***
set seed 1016
simulate beta0 = r(beta0) beta1 = r(beta1) beta2 = r(beta2), reps(10000) saving("simulationQ13.dta", replace) seed(1016): progq12

*q1.3a
clear all
use"simulationQ13.dta"

histogram beta1, normal
graph export "dist beta1 part3.png", replace
histogram beta2, normal
graph export "dist beta2 part3.png", replace






*=========================================
*QUESTION 2
*=========================================
clear all

*Q2.1***
use"ps1_data_group16.dta"

count //to see how many observations in the data
set obs 151
drop const
gen const = 1

mata 
st_view(y =.,.,"rgdpl")
st_view(X =.,.,("const", "wtem", "wpre"))

*q2.1a*
beta = invsym(X'X)*X'y
beta

*q2.1b*
sst = y'y-rows(y)*mean(y)^2 
sst
sse = beta'X'y-rows(y)*mean(y)^2
sse
ssr = y'y-beta'X'y // u'u = sst-sse
ssr

*q2.1c*
r2 = 1 - ssr/sst
r2


*q2.1d*
y_hat = X*beta
y_hat
u_hat = y-X*beta
u_hat


*q2.1e*
u_hat_avg = mean(u_hat)
u_hat_avg

	//covariance of regressors and residuals
cov_Xu = X'u_hat / 151
cov_Xu

*q2.1f*
y_avg = mean(y)
y_avg
y_hat_avg = mean(y_hat)
y_hat_avg

end






*Q2.2***
clear all
set more off
use"ps1_group16.dta"
reg rgdpl wtem wpre

predict y_hat, xb
predict u_hat, res

*q2.2b*
mean(y_hat)
mean(rgdpl)

gen diff = rgdpl - y_hat
browse

*q2.2c*
mean(u_hat)




*Q2.3***
*q2.3b*
reg rgdpl wtem 



*q2.3c* partialling out model
clear all
use"ps1_group16.dta"

reg rgdpl
predict u_hat1, res

reg wtem
predict u_hat2, res

reg u_hat1 u_hat2


log close

