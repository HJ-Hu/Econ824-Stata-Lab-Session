*******************************************************************************
*******************************************************************************
                     *** ECON 824 - CLASS EXERCISE ***
                        *** Professor: Amos Golan ***
                    *** Lab instructor: Tina Mumladze ***
					  *** Last Updated 10/25/2023 ***

*******************************************************************************
*******************************************************************************


*******************************************************************************
                      *** Introduction to Stata ***
*******************************************************************************


*******************************************************************************
                      *** Exercise 1: Linear Regression ***
*******************************************************************************
clear all

* Set seed for replication
set seed 1234

* Set the number of obserations to create
set obs 50

* Create a normally distributed error term for x2 and y
gen e2  = rnormal(0,2) // 0 mean and st dev of 2
gen u2  = rnormal(0,5) // 0 mean and st dev of 5

* Create two covariates
gen x1 = runiform(0,10)    // x1 is uniformly distributed
gen x2 = 1 + (-1)*x1 + u2  // x2 is a function of x1 and a normally distributed variable u2

* Create the population outcome variable
gen y = 10 + (-1)*x1 + (-0.5)*x2 + e2 // y id a function of x1, x2, and the error term e2
 
* Before diving into analysis, always get to know your data first!
summarize
sum
sum, detail	

* Now, let's look at the correlations between our variables
corr // Method 1: drops all missing values in the entire dataset before calculating correlations
pwcorr, sig star(5) // Method 2: drops missing values in variable pairs before calculationg correltaions

* Reorder the variables (no real implication)
order y x1 x2 u2 e2

* Now, start regression analysis
* Regress y on our first covariate
reg y x1
* Regress y on our second covariate
reg y x2
* Regress y on both of the covariates
reg y x1 x2
* Look at the correlation between the covariates
corr(x1 x2) // Covariates are colinear
* Conduct F-test
test x1 x2 
*******************************************************************************
*******************************************************************************





*******************************************************************************
              *** Exercise 2: Cobb-Douglas Production function ***
*******************************************************************************

clear all

* Set seed for replication
set seed 1234

* Import data: CSV files are common in real world
import excel "/Users/tmumladze/Desktop/AU/5.1 Fall 2023/Stata Lab/TableF5-3.xlsx", sheet("Sheet1") firstrow clear // first row variable names

* Descriptive statistics
sum
corr

* View original data: Histograms are excellent for visualizing the distribution of data
histogram labor
hist capital

* Scatter plots are helpful two-dimension tools that can help us identify trends and patterns in the data
twoway (scatter valueadd labor)
twoway (scatter valueadd capital)

* Cobb-Douglas can be expressed as a linear equation, using Log-Log
gen LN_Y = log(valueadd)
gen LN_L = log(labor)
gen LN_K = log(capital)

* Descriptive statistics
sum

* View transformed data
twoway (scatter LN_Y LN_L)
twoway (scatter LN_Y LN_K)

* Hypothesis Testing: means statistically different from 0 or not
ttest LN_Y == 0 //try 7.5
ttest LN_L == 0 // try 5.7
ttest LN_K == 0 //try 7.5
* The means of all three are statistically different from zero.

* Cobb Douglas Production Function
regress LN_Y LN_L LN_K // a good fit (i.e., high R^2)
* Display the coefficients and standard errors
di "Coefficient for LN_L: " _b[LN_L]
di "Standard error for LN_L: " _se[LN_L]
* Display R-squared and F-statistic
di "R-squared: " e(r2)
di "F-statistic: " e(F)

* Export results to a doc/text file
#delimit ;
outreg2 using "/Users/tmumladze/Desktop/AU/5.1 Fall 2023/Stata Lab/cobb-douglas.doc", replace
	title(Estimated parameters of a linear Cobb-Douglas model)
	addstat(F-statistic:, e(F));
#delimit cr


regress LN_Y LN_L // a good fit (i.e., high R^2)


#delimit ;
outreg2 using "/Users/tmumladze/Desktop/AU/5.1 Fall 2023/Stata Lab/cobb-douglas.doc", append
	title(Estimated parameters of a linear Cobb-Douglas model)
	addstat(F-statistic:, e(F));
#delimit cr

*******************************************************************************
*******************************************************************************





*******************************************************************************
                      *** Exercise 3: Central Limit Theorem ***
*******************************************************************************

clear all
set seed 1234

* Small dataset
set obs 20
* Generate a random variable
gen Rand_1 = rnormal(0,10) // ~ N with Mean=0 and St. Dev =10
* View the summary statistics of the randam data
summarize
* Simple t tests
ttest Rand_1 == 0
ttest Rand_1 == 5

* Large Data Set 
set obs 10000
gen Rand_2 = rnormal(0,10)
summarize
ttest Rand_2 == 0
ttest Rand_2 == 5
* Central Limit Theorem in action!
* As sample size increases, the mean of the sample approaches the mean of the population. This is a fundamental concept that underlies many statistical methods and hypothesis testing. 

*******************************************************************************
*******************************************************************************





*******************************************************************************
							*** Cry for HELP! ***
*******************************************************************************

help
help reg
/* HELP is a very useful tool
-> provides comprehensive documentation for every command
-> includes examples and syntax
-> offers comprehensive explanation
-> reflects updates and edits to commands
*/

*******************************************************************************
*******************************************************************************





*******************************************************************************
				*** Exercise 4: Non-Linear Regressions ***
*******************************************************************************
clear all
set seed 1234
set obs 25

* Generate exogenous variables
gen x0 = 1 // the intercept
gen x1 = 0+20*runiform()
gen x2 = 0+20*runiform()
gen x3 = 0+20*runiform()
gen x4 = 0+20*runiform()
gen e = rnormal(0,5)

* Define coefficients
gen b0 = 1
gen b1 = 2
gen b2 = 0.5

* y is a non-linear function of exogenous variables and the error term
gen y = b0*x0 + b1*x1 + b1*b1*x2 + b1*b2*x3*x3 + b2*x4 + e

** Non-Linear Least Squares **
* nl fits an arbitrary nonlinear regression function by least squares
gen z=x3*x3
reg y x0 x1 x2 z x4
nl (y= {b0}*x0+ {b1}*x1 + {b1*b1}*x2 + {b1*b2}*x3*x3+ {b2}*x4)
nl (y= {a0}*x0+ {a1}*x1 + {a2}*x2 + {a3}*x3*x3+ {a4}*x4)
* We specify the relationship between y and the exogenous variables, with the coefficients represented within curly braces.


** Non-Linear Maximum Likelohood **
* mlexp is a maximum likelihood estimation of user-specified expressions
mlexp (-0.5 * ln(2 * _pi) - ln(sqrt(5)) - 0.5 * (y - ({b0} * x0 + {b1} * x1 + {b1} * {b1} * x2 + {b1} * {b2} * x3 * x3 + {b2} * x4))^2)
* Here, we rely on a standard likelihood function of a normal distribution. 
*******************************************************************************
*******************************************************************************





*******************************************************************************
					*** Exercise 5: Simultaneous Equations  ***
*******************************************************************************

* Labor supply and demand data for working women
use "/Users/tmumladze/Desktop/AU/5.1 Fall 2023/Stata Lab/MROZ.DTA", clear
* keep only working women
keep if inlf==1

lookfor years
describe hours lwage educ exper expersq age kidslt6 nwifeinc
summarize hours lwage educ exper expersq age kidslt6 nwifeinc
list hours lwage educ exper expersq age kidslt6 nwifeinc in 1/10 // list top 10 observations

* Simulatenous equations:
* (1) Regression for hours using OLS estimation
reg hours lwage educ age kidslt6 nwifeinc
* (2) Regression for lwage using OLS estimation
reg lwage hours educ exper expersq

* Regression for hours using 2SLS estimation: suspect endogeneity btw hours worked and wage
* lwage is instrumented 
ivreg hours (lwage = exper expersq) educ age kidslt6 nwifeinc, first
* Here, we're instrumenting lwage with exper expersq and all other variables to address potential endogeneity issues. We use the ivreg command to run these 2SLS regressions.
test lwage

* Regression for lwage using 2SLS estimation
* hours is instrumented 
ivreg lwage (hours = age kidslt6 nwifeinc) educ exper expersq, first
* Here, we're instrumenting hours with age kidslt6 nwifeinc and all other variables to address potential endogeneity issues. We use the ivreg command to run these 2SLS regressions.
test hours


** Rank condition **
* Testing for rank condition involves estimating the reduced form equation 
* and testing for significance of the instrumented variables.


* Reduced form equation for hours, identifying equation for lwage
reg hours educ age kidslt6 nwifeinc exper expersq
test age kidslt6 nwifeinc
test exper expersq

* Reduced form equation for lwage, identifying equation for hours
reg lwage age kidslt6 nwifeinc educ exper expersq
test age kidslt6 nwifeinc
test exper expersq

*******************************************************************************
*******************************************************************************





*******************************************************************************
						*** Exercise 6: Discrete Choice Models ***
*******************************************************************************

clear all 
set obs 100
set seed 1234

* Step 1: generating x vars
generate x0 = 1             // this is the intercept 

gen age=runiform(18,65)     // age from 18 to 65

gen i=runiform()
gen sex=0
replace sex=1 if i<=0.465   // shares of male and female

generate u=runiform(0,1)
generate cardio=0
replace cardio=1 if u<=0.08 // share of people with cardiovacular condition

* Step 2: generating  betas
gen b0 = 0.0001
gen b1 = 0.005
gen b2 = -0.00001
gen b3 = 0.02

* Step 3: creating the correct ("unobserved") data
generate s1 = b0 + b1*age + b2*sex + b3*cardio
* Calculate odds of an event occuring
generate omega = 1+exp(s1)
generate pn0 = 1/omega // probability of surviving
generate pn1 = exp(s1)/omega // probability of dying

* Step 4: observed random information
generate unobserved = runiform(0,1)
generate y = 0 if unobserved <= pn0 // survived
replace y = 1 if unobserved > pn0 // died 

* Create labels
label variable y "OBSERVED STATE"
label define yvals ///
	0 "Survive" ///
	1 "Death" 
label values y yvals
	
* Step 5: estimation
*** Multinomial Logit (ordered or unordered)
logit 
mlogit y age sex cardio, robust base(0)
mfx, nonlinear

* Prediction
predict prdL*
	rename prdL1 prdL0
	rename prdL2 prdL1
egen prdL_max = rowmax(prdL*)
generate yprdL = .
forvalues i=0/1 {
	replace yprdL = `i' if (prdL_max == prdL`i')
}

* Create labels for predictions
label variable yprdL "Predicted_LOGIT"
label define yvalsL ///
	0 "Survive" ///
	1 "Death" 
label values yprdL yvalsL
tab yprdL y


*** Multinomial Probit (unordered)
mprobit y age sex cardio, robust base(0)
mfx, nonlinear

* Prediction
predict prdP*
	rename prdP1 prdP0
	rename prdP2 prdP1
egen prdP_max = rowmax(prdP*)
generate yprdP = .
forvalues i=0/1 {
	replace yprdP = `i' if (prdP_max == prdP`i')
}

* Create labels for predictions
label variable yprdP "Predicted_PROBIT"
label define yvalsP ///
	0 "Survive" ///
	1 "Death" 
label values yprdP yvalsP
tab yprdP y

*******************************************************************************
*******************************************************************************






*******************************************************************************
					*** Bonus: DSGE Modeling in Stata ***
*******************************************************************************

clear all

* Set the working directory to store data
cd "/Users/tmumladze/Desktop/AU/5.1 Fall 2023/Stata Lab"

* Load the data for the DSGE model from the Stata Press website
use https://www.stata-press.com/data/r18/usmacro2

* Summary statistics of selected variables in a loop
local myMacro y p r c n i // define macro variables
foreach i of local myMacro {
	di "Summarizing `i'"
	sum `i'

}

* Create a scatter plot matrix
graph matrix y p r c n i, title("Scatter Plot Matrix")

* Save the graph
graph save scatter_plot_matrix, replace


/* DSGE model: a New Keynesian model

(1) The Phillips curve: The inflation rate p is a function of the lagged inflation rate and the output gap x.
p = {beta} * F.p + {kappa} * x
beta and kappa are parameters.

(2) Output Gap: Determined by the lagged output gap, the real interest rate r, lagged inflation rate, and an exogenous shock g.
x = F.x - (r - F.p - g)

(3) Interest Rate Rule: The nominal interest rate r is a function of the price level p and another exogenous shock u.
r = {psi} * p + u
psi is a parameter.

(4) Exogenous Shock to Interest Rate: The exogenous shock to the interest rate, u, follows an autoregressive process.
F.u = {rhou} * u
rhou is a parameter controlling the persistence of the shock.

(5)Exogenous Shock to Output Gap: The exogenous shock to the output gap, g, also follows an autoregressive process.
F.g = {rhog} * g
rhog is a parameter controlling the persistence of the shock.

*/


* Impose a constraint on the DSGE model: β is 0.96
constraint 1 _b[beta]=0.96 // imposed either for economic or identification reasons

// DSGE model estimated
dsge (p = {beta}*F.p + {kappa}*x) ////
	(x = F.x - (r - F.p - g), unobserved) ////
	(r = {psi}*p + u) ////
	(F.u = {rhou}*u, state) ////
	(F.g = {rhog}*g, state), ////
	from(psi=1.5) constraint(1)
	
* Export results to a text file
#delimit ;
outreg2 using "dsge_results.doc", replace
	title(Estimated parameters of a New Keynesian model)
	addstat(Log likelihood , e(ll));
#delimit cr

*******************************************************************************
*******************************************************************************
