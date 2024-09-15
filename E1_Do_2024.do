################################################################################################# LOAD FACTOR AND FINANCIAL PERFORMANCE #################


#######################  SECTION ONE DATA PREPARATION  #####################
* import file
import delimited "/Users/kuang/Desktop/Essay 1 2022_New/essay1_final_corrected.csv"

* check number of quarters of each carrier
egen occasioncount = count(yearq), by (carriercode)
tab carriercode occasioncoun

* check duplicates
duplicates list carriercode occasion

* generate new variables
gen LFP2 = lfp*lfp
gen LFP3 = lfp*lfp*lfp

gen pOntime = ontime/totalrecords
gen carrierdelay = aircarrierdelay + latearrivingaircraftdelay

gen pCarrierdelay = carrierdelay/totalrecords
gen pSecurityDelay = securitydelay/totalrecords
gen fleetutil = (total_air_hours/24)/ air_days_assign

* generate DV and logged IV
gen OPOR = op_profit_loss/op_revenues
gen ltdomt_cost =ln(tdomt_cost)
gen lrevpaxenplaned = ln(revpaxenplaned)
gen lempfte = ln(empfte)
gen lop_revenues = ln(op_revenues)

gen ltotaldelay = ln(totaldelay)
gen lcarrierdelay = ln(carrierdelay)
gen lsecuritydelay = ln(securitydelay)

gen lpOntime = ln(pOntime)
gen lpCarrierdelay = ln(pCarrierdelay)
gen lpSecurityDelay = ln(pSecurityDelay)

gen lyield = ln(yieldpax)
gen lfleetutil = ln(fleetutil)
gen lontime = ln(ontime)

* generate market share
egen revpaxmileTotal = sum(revpaxmiles), by(yearq)
gen MKTshare = revpaxmiles/revpaxmileTotal

* generate mean varibles
egen LFP_mean = mean(lfp), by(carriercode)
egen LFP2_mean = mean(LFP2), by(carriercode)
egen LFP3_mean = mean(LFP3),by(carriercode)
egen OPOR_mean = mean(OPOR), by(carriercode)
egen ltdomt_cost_mean = mean(ltdomt_cost), by(carriercode)
egen lrevpaxenplaned_mean = mean(lrevpaxenplaned), by(carriercode)
egen MKTshare_mean = mean(MKTshare), by(carriercode)
egen lempfte_mean = mean(lempfte), by(carriercode)
egen lyield_mean = mean(lyield), by(carriercode)
egen yield_mean = mean(yieldpax), by(carriercode)
egen lfleetutil_mean = mean(lfleetutil), by(carriercode)
egen ltotaldelay_mean = mean(ltotaldelay), by(carriercode)
egen lcarrierdelay_mean = mean(lcarrierdelay), by(carriercode)
egen lsecuritydelay_mean = mean(lsecuritydelay), by(carriercode)

* generate within variables
gen LFPw = lfp - LFP_mean
gen LFP2_w= LFP2 - LFP2_mean
gen ltdomt_costw= ltdomt_cost - ltdomt_cost_mean
gen lrevpaxenplaned_w = lrevpaxenplaned - lrevpaxenplaned_mean
gen MKTshare_w = MKTshare- MKTshare_mean
gen lempfte_w = lempfte - lempfte_mean
gen lyield_w = lyield - lyield_mean
gen yield_w = yieldpax - yield_mean
gen lfleetutil_w = lfleetutil - lfleetutil_mean
gen ltotaldelay_w = ltotaldelay - ltotaldelay_mean
gen lcarrierdelay_w = lcarrierdelay - lcarrierdelay_mean
gen lsecuritydelay_w = lsecuritydelay - lsecuritydelay_mean

* generate centered group mean variables
sum LFP_mean, meanonly
gen cLFP_mean =  LFP_mean - r(mean)

sum LFP2_mean, meanonly
gen cLFP2_mean = LFP2_mean - r(mean)

sum LFP3_mean, meanonly
gen cLFP3_mean = LFP3_mean - r(mean)

sum ltdomt_cost_mean, meanonly
gen cltdomt_cost_mean = ltdomt_cost_mean - r(mean)

sum lrevpaxenplaned_mean, meanonly
gen clrevpaxenplaned_mean = lrevpaxenplaned_mean - r(mean)

sum MKTshare_mean, meanonly
gen cMKTshare_mean = MKTshare_mean - r(mean)

sum lempfte_mean, meanonly
gen clempfte_mean = lempfte_mean - r(mean)

sum lyield_mean, meanonly
gen clyield_mean = lyield_mean - r(mean)

sum yield_mean, meanonly
gen cyield_mean = yield_mean - r(mean)

sum lfleetutil_mean, meanonly
gen clfleetutil_mean = lfleetutil_mean - r(mean)

sum ltotaldelay_mean, meanonly
gen cltotaldelay_mean = ltotaldelay_mean - r(mean)

sum lcarrierdelay_mean, meanonly
gen clcarrierdelay_mean = lcarrierdelay_mean - r(mean)

sum lsecuritydelay_mean, meanonly
gen clsecuritydelay_mean = lsecuritydelay_mean - r(mean)

* generate cross interaction term
gen LFBW = cLFP_mean*LFPw
gen LFBW2 = cLFP2_mean*LFPw

gen LCC_inter = cLFP_mean*lcc
gen LCC_inter2= cLFP2_mean*lcc
gen LCC_inter_re = lfp*lcc
gen LCC_inter_re2 = LFP2*lcc


################### SECTION TWO Correlation and Sum Table ###################
* correlation 
cor OPOR cLFP_mean cLFP2_mean LFPw LCC_inter cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc

* sum
sum OPOR cLFP_mean cLFP2_mean LFPw LCC_inter cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc


################### SECTION THREE ICC Calculation ###########################

xtmixed OPOR || carriercode:, var cov(un) mle 
estat ic
estat icc

################### SECTION FOUR BUILDING AND TESTING MODELS ################

********************* OPOR MODEL 1 Base Model********************************
*****************************************************************************

* test Random intercept VS Random Slope (chi2 = 92.35, p = .000)
qui xtmixed OPOR cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:, cov(un) var mle
estimates store m1RI

qui xtmixed OPOR cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:LFPw, cov(un) var mle
estimates store m1RS

lrtest m1RI m1RS 

* add random slope ()
xtmixed OPOR cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:LFPw, cov(un) var mle



estat ic
estimates store oporm1
predict oporm1hat, fitted // Variance     .0132337
sum oporm1hat, detail
cor OPOR oporm1hat // 0.8024


*************** OPOR MODEL 2 TEST U SHAPE and OUTLIERS *********************
****************************************************************************

* TEST BETWEEN Random Intercetp and Random Slope (chi2 = 82.02, p = .000)
qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc  i.year i.quarter||carriercode:, cov(un) var mle 
estimates store m2RI

qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:LFPw, cov(un) var mle 
estimates store m2RS

lrtest m2RI m2RS

*************************************************************************
* add randome slope of load factor

xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:LFPw, cov(un) var mle 

nlcom _b[ cLFP_mean ]/(-2*_b[ cLFP2_mean ]) //.88056

estat ic
estimates store oporm2
predict oporm2hat, fitted //   .0132237
sum oporm2hat, detail
cor OPOR oporm2hat //80.33


* test yield2
gen clyield_mean2 = clyield_mean*clyield_mean
gen clyield_mean3 = clyield_mean*clyield_mean*clyield_mean

gen cyield_mean2 = cyield_mean*cyield_mean
gen cyield_mean3 = cyield_mean*cyield_mean*cyield_mean

gen yield_mean2 = yield_mean*yield_mean
gen yield_mean3 = yield_mean*yield_mean*yield_mean

xtmixed OPOR clyield_mean clyield_mean2 clyield_mean3 yield_w cLFP_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:lyield_w, cov(un) var mle 

sum clyield_mean // -.6099844   .5806291
twoway function  0.12*x + 0.36*x^2 + 0.78*x^3, range(-0.6 .6) ytitle("OPOR") xtitle("Yield")

sum yield_mean // .0159722   .054
twoway function  34*x - 1029*x^2 + 12192*x^3, range(0.016 0.05) ytitle("OPOR") xtitle("Yield")

* test Southwest Outlier
gen swoutlier = 0
replace swoutlier =1 if carriercode == 24

gen swinter = swoutlier*lfleetutil_w

xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc swinter i.year i.quarter||carriercode:lfleetutil_w, cov(un) var mle 


***** ADDTIONAL TESTS FOR MODEL 2 
* 1. Test upper and lower bounds of U curve
sum cLFP_mean // -.1485095   .0453273

lincom _b[ cLFP_mean ] + 2*_b[ cLFP2_mean ]*( -.1485095) // coef = 13.87, p = 0.006, z = 2.74
lincom _b[ cLFP_mean ] + 2*_b[ cLFP2_mean ]*( .0453273) // coef = 11.26, p = 0.005, z = 2.82

* using actual LF
lincom _b[ cLFP_mean ] + 2*_b[ cLFP2_mean ]*( .53) // coef = 4.72, p = 0.000, z = 3.65
lincom _b[ cLFP_mean ] + 2*_b[ cLFP2_mean ]*( .95) // coef = 11.26, p = 0.005, z = 2.82

* 2. Plot U curve. All significant coefficents*mean values of variables = intercept
sum lfp //.5294144   .9275762
sum OPOR // -1.886015   .5232041
sum LFP_mean .7258012   .8481011

* using actual load factor as the range
twoway function -4.8 + 11.87*x -6.74*x^2, range(0.53 .92) ytitle("OPOR") xtitle("Load Factor")


* 3. Plot Residual Kernal Density. Level 2 was predicted in previous steps
predict lev1, residuals
predict u0_OPOR u1_OPOR, ref

kdensity u0_OPOR, normal
kdensity u1_OPOR, normal
kdensity lev1, normal

* 4. Test ourliers of residuals for Intercept and Slope
* predict u0_OPOR u1_OPOR, ref
predict u0se_OPOR u1se_OPOR, reses

egen pickone_OPOR = tag(carriercode)

sort u0_OPOR
generate u0rank_OPOR = sum(pickone_OPOR)
list carriercode u0_OPOR u0se_OPOR u0rank_OPOR if pickone_OPOR== 1
serrbar u0_OPOR u0se_OPOR u0rank_OPOR if pickone_OPOR==1, scale(1.96) yline(0) // 7 is outlier for intercept

sort u1_OPOR
generate u1rank_OPOR = sum(pickone_OPOR)
list carriercode u1_OPOR u1se_OPOR u1rank_OPOR if pickone_OPOR== 1
serrbar u1_OPOR u1se_OPOR u1rank_OPOR if pickone_OPOR==1, scale(1.96) yline(0) // 7 and 9 (both can actualy be ignored) is outlier for slope


* 5. THIS IS MODEL 4; generate and add outlier. Adding AR1 (Random Slope does not converge so remove)
gen outlier_opor1 = 0
replace outlier_opor1 = 1 if carriercode == 7
gen outlier1inter = outlier_opor1*LFPw

gen outlier_opor2 = 0
replace outlier_opor2 = 1 if carriercode == 9
gen outlier2inter = outlier_opor2*LFPw

xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc outlier1inter outlier2inter i.year i.quarter||carriercode:, cov(un) var mle res(ar1, t(occasion))

nlcom _b[ cLFP_mean ]/(-2*_b[ cLFP2_mean ]) // .8784

estat ic
estimates store oporm2_outlier
predict oporm2hat_outlier, fitted //   .0122161
sum oporm2hat_outlier, detail
cor OPOR oporm2hat_outlier //0.7995


* 6. ADD CUBIC TERM (ROBUSTNESS TEST)

xtmixed OPOR cLFP_mean cLFP2_mean cLFP3_mean  LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:LFPw, cov(un) var mle 

twoway function  200*x -264*x^2 + 116*x^3, range(0.5 .92) ytitle("OPOR") xtitle("Load Factor")


xtmixed OPOR cLFP_mean cLFP2_mean cLFP3_mean  LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc outlier_opor1  i.year i.quarter||carriercode:LFPw, cov(un) var mle 

twoway function  -538*x +670*x^2 -276*x^3, range(0.5 .92) ytitle("OPOR") xtitle("Load Factor")


* 7. TEST split regressions of two parts of the curve (need to drop carriers with few observations, i.e., carrier need to have lfp on both sides of cutoff)
sum cLFP_mean // -.0781364   .0441636
hist cLFP_mean
hist LFP_mean
hist lfp

xtmixed OPOR cLFP_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc outlier_opor1  i.year i.quarter if cLFP_mean<0||carriercode:LFPw, cov(un) var mle


xtmixed OPOR cLFP_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc  outlier_opor1 i.year i.quarter if cLFP_mean>0 ||carriercode:, cov(un) var mle 


xtmixed OPOR cLFP_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc outlier_opor1 i.year i.quarter if lfp<0.83||carriercode:, cov(un) var mle res(ar1, t(occasion))

xtmixed OPOR cLFP_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc outlier_opor1 i.year i.quarter if lfp>0.80||carriercode:, cov(un) var mle res(ar1, t(occasion))


*************** OPOR Model 3 TEST MODERATION ********************************
****************************************************************************

xtmixed OPOR cLFP_mean cLFP2_mean LFPw LCC_inter LCC_inter2 cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc  i.year i.quarter||carriercode: LFPw, cov(un) var mle 

nlcom _b[ cLFP_mean ]/(-2*_b[ cLFP2_mean ]) // .8389

* using actual LF
lincom _b[ cLFP_mean ] + 2*_b[ cLFP2_mean ]*( .75) // coef = 4.72, p = 0.000, z = 3.65
lincom _b[ cLFP_mean ] + 2*_b[ cLFP2_mean ]*( .85) // coef = -.173357, p = 0.005, z = -2.82

estat ic
estimates store oporm3
predict oporm3hat, fitted
sum oporm3hat, detail
cor OPOR oporm3hat

sum LFP_mean


***** FOR MODEL 3
*1. TEST IF TURNING POINT SHIFTED OR NOT
sum lcc // take the mean of LCC of  .2299517
nlcom (_b[cLFP_mean]*_b[LCC_inter2] -_b[cLFP2_mean]*_b[LCC_inter])/2*(_b[cLFP2_mean] + _b[LCC_inter2]*.2299517)*(_b[cLFP2_mean] + _b[LCC_inter2]*.2299517) // coef = 536.83, z = 0.64, p = 0.525 meaning turning point not shifted

*2. Test if the curve if flattened or not
* This is to test the coefficent of LCC_inter2 (coef = -7.3, z = -0.6, p=0.548). Non significant hence no impact. 


************* Likelihood Ratio Test of the three models
*******************************************************
lrtest oporm1 oporm2
lrtest oporm2 oporm3


############################################################################
################## TEST Alternative Covariance Structure ###################

**** TEST ACS FOR OPOR (model does not converge with random slope so RS get removed)
qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:, cov(un) var mle
estimates store min
estat ic


qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:, cov(un) var mle residuals(ar1, t(occasion))
estimates store ar1
estat ic

qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:, cov(un) var mle residuals(ar2, t(occasion))
estimates store ar2
estat ic

qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:, cov(un) var mle residuals(ar3, t(occasion))
estimates store ar3
estat ic

qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc i.year i.quarter||carriercode:, cov(un) var mle residuals(ar4, t(occasion))
estimates store ar4
estat ic

lrtest min ar1
lrtest ar1 ar2
lrtest ar2 ar3
lrtest ar3 ar4



############################################################################
############################# XT UNIT ROOT TEST #############################
* xt set data
xtset carriercode occasion
xtdescribe

* test for stationarity at panel level
* only fisher type and IPS tests can be used due to unbalanced panel.
* not enought observations for IPS test. 
xtunitroot fisher lyield, dfuller drift lags (1) demean
xtunitroot fisher lfp, dfuller drift lags (1) demean
xtunitroot fisher OPOR, dfuller drift lags (1) demean

* cannot do cointegration test due to gap
* test optimal lags. Cannot do this due to gaps in panels.
* varsoc lfp lyield if carriercode == 11, maxlag (10)


############################################################################
################## GRANGER CAUSALITY TEST ################################
**************** pvar using all 28 carrires (Abrigo's Method)

* LF-Yield
pvar lyield lfp, lags (4) ex(lrevpaxenplaned MKTshare lempfte lcarrierdelay OPOR)
pvargranger
pvarstable, graph
pvarirf, oirf mc(200) byoption(yrescale) porder(lyield lfp)

* Yield and OPOR
pvar lyield OPOR
pvargranger
pvarstable, graph
pvarirf, oirf mc(200) byoption(yrescale) porder(lyield OPOR)

* LF and OPOR
pvar lfp OPOR
pvargranger
pvarstable, graph
pvarirf, oirf mc(200) byoption(yrescale) porder(lfp OPOR)



**** pvar using ONLY those carriers with 64 quarters. Same results
egen occasioncount =count(yearq), by (carriercode)
by carriercode: keep if occasioncount == 64

xtset carriercode occasion
xtdescribe

xtunitroot fisher lyield, dfuller drift lags (10) demean
xtunitroot fisher lfp, dfuller drift lags (10) demean

varsoc lfp lyield, maxlag (10)

pvar lyield lfp
pvargranger

pvar lyield lfp, lags (1)
pvargranger

*********
drop if carriercode ==3
drop if carriercode ==4
drop if carriercode ==5
drop if carriercode ==12
drop if carriercode ==17
drop if carriercode ==21
drop if carriercode ==22

drop if carriercode == 13 & occasion >= 57
drop if carriercode == 14 & occasion <= 32
drop if carriercode == 19 & occasion >= 57
drop if carriercode == 24 & occasion >= 43
drop if carriercode == 27 & occasion >= 45


*************************************************
***************** Lopez Method ******************
* all panel level
xtgcause lyield lfp, lags (2)
xtgcause lfp lyield, lags (2)


* airline level
* yield to load factor
xtgcause lfp lyield if carriercode == 1, lags (2)
xtgcause lfp lyield if carriercode == 2, lags (2)
xtgcause lfp lyield if carriercode == 6, lags (2)
xtgcause lfp lyield if carriercode == 7, lags (2)
xtgcause lfp lyield if carriercode == 8, lags (2)
xtgcause lfp lyield if carriercode == 9, lags (2)
xtgcause lfp lyield if carriercode == 10, lags (2)
xtgcause lfp lyield if carriercode == 11, lags (2)
xtgcause lfp lyield if carriercode == 13, lags (2)
xtgcause lfp lyield if carriercode == 14, lags (2)
xtgcause lfp lyield if carriercode == 15, lags (2)
xtgcause lfp lyield if carriercode == 16, lags (2)
xtgcause lfp lyield if carriercode == 18, lags (2)
xtgcause lfp lyield if carriercode == 19, lags (2)
xtgcause lfp lyield if carriercode == 20, lags (2)
xtgcause lfp lyield if carriercode == 23, lags (2)
xtgcause lfp lyield if carriercode == 24, lags (2)
xtgcause lfp lyield if carriercode == 25, lags (2)
xtgcause lfp lyield if carriercode == 26, lags (2)
xtgcause lfp lyield if carriercode == 27, lags (2)
xtgcause lfp lyield if carriercode == 28, lags (2)

* load factor to yield
xtgcause lyield lfp if carriercode == 1, lags (2)
xtgcause lyield lfp if carriercode == 2, lags (2)
xtgcause lyield lfp if carriercode == 6, lags (2)
xtgcause lyield lfp if carriercode == 7, lags (2)
xtgcause lyield lfp if carriercode == 8, lags (2)
xtgcause lyield lfp if carriercode == 9, lags (2)
xtgcause lyield lfp if carriercode == 10, lags (2)
xtgcause lyield lfp if carriercode == 11, lags (2)
xtgcause lyield lfp if carriercode == 13, lags (2)
xtgcause lyield lfp if carriercode == 14, lags (2)
xtgcause lyield lfp if carriercode == 15, lags (2)
xtgcause lyield lfp if carriercode == 16, lags (2)
xtgcause lyield lfp if carriercode == 18, lags (2)
xtgcause lyield lfp if carriercode == 19, lags (2)
xtgcause lyield lfp if carriercode == 20, lags (2)
xtgcause lyield lfp if carriercode == 23, lags (2)
xtgcause lyield lfp if carriercode == 24, lags (2)
xtgcause lyield lfp if carriercode == 25, lags (2)
xtgcause lyield lfp if carriercode == 26, lags (2)
xtgcause lyield lfp if carriercode == 27, lags (2)
xtgcause lyield lfp if carriercode == 28, lags (2)


############################################################################
#################### COMPARE BETWEEN DIFFERENT MODELS ######################

xtset carriercode occasion

* cannot use cLFP_mean becasue FE wil deduct mean. All mean variables get omitted.

xtreg OPOR lfp LFP2 ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc i.year i.quarter, fe

nlcom _b[lfp]/(-2*_b[LFP2]) //79.06% 

 lfp |   5.606395   1.300533     4.31   0.000     3.054032    8.158758
LFP2 |  -3.545555   .8426052    -4.21   0.000    -5.199211   -1.891899


predict OPOR_fe
estimates store femod
cor OPOR OPOR_fe //.2996 = 0.08976

xtreg OPOR lfp LFP2 ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc i.year i.quarter, re

nlcom _b[lfp]/(-2*_b[LFP2]) //83.71%

 lfp |    8.28529   1.330131     6.23   0.000      5.67828     10.8923
LFP2 |  -4.948412   .8628412    -5.74   0.000     -6.63955   -3.257274


predict OPOR_re
estimates store remod
cor OPOR OPOR_re //.6440 = .4147

hausman femod remod, sigmaless // chi2 = 205.86, p= 0.000 Reject RE accept FE


xtmixed OPOR cLFP_mean cLFP2_mean LFPw cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w clfleetutil_mean lfleetutil_w percent_gdp percent_chg_fare recession lcc  i.year i.quarter||carriercode:LFPw, cov(un) var mle 

nlcom _b[cLFP_mean]/(-2*_b[cLFP2_mean]) 

predict OPOR_xtmixed
estimates store mixedmodel

* line plot
label variable OPOR_xtmixed "Between-Within Prediction"
label variable OPOR_re "Random Effects Prediction"
label variable OPOR_fe "Fixed Effects Prediction"
line OPOR OPOR_re OPOR_fe OPOR_xtmixed occasion if carriercode == 2


################################################################################################ STATA BUILT IN FORECAST MODEL############################

* Stata Forecast only allows balanced panel
egen occasioncount = count(yearq), by (carriercode)
by carriercode: keep if occasioncount == 64

* forecast fixed effects
xtset carriercode occasion

xtreg OPOR lfp LFP2 LCC_inter_re ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc  i.year i.quarter, fe

estimates store femodel

forecast create Fmodel_fe,replace
forecast estimates femodel
forecast solve, begin(61)


* forecast random effects
xtreg OPOR lfp LFP2 LCC_inter_re ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc  i.year i.quarter, re

estimates store remodel

forecast create Fmodel_re,replace
forecast estimates remodel
forecast solve, begin(61)

* forecast DOES NOT support mixed effects
qui mixed OPOR cLFP_mean cLFP2_mean LFPw LCC_inter cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w percent_gdp clfleetutil_mean lfleetutil_w percent_chg_fare recession lcc i.year i.quarter||carriercode:LFPw, cov(un) var mle

estimates store mixedmodel_new
forecast create mixedfmodel, replace

forecast estimates mixedmodel_new
forecast solve, begin(60)




***************************************************************************
************************* OUT OF SAMPLE ***********************************
* Create Training Dataset. Each carrier is deducted by 8 quarters.
xtset carriercode occasion
by carriercode: gen n1 = _n
by carriercode: gen n2 = _N
by carriercode: gen n3  = _N - 8

drop if carriercode == 3 | carriercode ==4 |carriercode ==5|carriercode == 7|carriercode == 12|carriercode == 17|carriercode == 21|carriercode == 22 | carriercode == 20|carriercode == 25|carriercode == 28


qui xtreg OPOR lfp LFP2 LCC_inter_re ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc  i.year i.quarter if n1<= n3, fe
predict pout_fe if occasion > n3


qui xtreg OPOR lfp LFP2 LCC_inter_re ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc  i.year i.quarter if n1<= n3, re
predict pout_re if occasion > n3

qui xtmixed OPOR cLFP_mean cLFP2_mean LFPw LCC_inter cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w percent_gdp clfleetutil_mean lfleetutil_w percent_chg_fare recession lcc i.year i.quarter if n1<= n3||carriercode:LFPw, cov(un) var mle

predict pout_xtmixed if occasion > n3

* Re Read file to get all 64 quarters 
label variable pout_xtmixed "Between-Within Out of Sample Forecast"
label variable pout_re "Random Effects Out of Sample Forecast"
label variable pout_fe "Fixed Effects Out of Sample Forecast"

line OPOR pout_fe pout_re pout_xtmixed occasion if carriercode == 2

* check forecast accurary
gen msefe = (pout_fe - OPOR)*(pout_fe - OPOR)
gen msere = (pout_re - OPOR)*(pout_re - OPOR)
gen msemixed = (pout_xtmixed - OPOR)*(pout_xtmixed - OPOR)

**** MSE
* total valid is 1035 - 939 = 96
total msefe //  6.250247
total msere  //  .4378028
total msemixed // .2346292 

* MAPE for carriers more than 20 quarters after deleting carriers less than 20 
* only 17 left. observations = 128
gen mapefe = abs(pout_fe - OPOR)/abs(OPOR)
gen mapere = abs(pout_re - OPOR)/abs(OPOR)
gen mapemixed = abs(pout_xtmixed - OPOR)/abs(OPOR)

total mapefe //  817.7877 
total mapere //     249.7068
total mapemixed //    177.9063 

********************************************************************************************************* ALL CARRIERS **********************************
qui xtreg OPOR lfp LFP2 LCC_inter_re ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc  i.year i.quarter, fe

predict pout_fe_all

* forecast random effects
qui xtreg OPOR lfp LFP2 LCC_inter_re ltdomt_cost lrevpaxenplaned MKTshare lempfte lcarrierdelay lyield lfleetutil percent_gdp percent_chg_fare recession lcc  i.year i.quarter, re

predict pout_re_all

* forecast DOES NOT support mixed effects
qui mixed OPOR cLFP_mean cLFP2_mean LFPw LCC_inter cltdomt_cost_mean ltdomt_costw clrevpaxenplaned_mean lrevpaxenplaned_w cMKTshare_mean MKTshare_w clempfte_mean lempfte_w clcarrierdelay_mean lcarrierdelay_w clyield_mean lyield_w percent_gdp clfleetutil_mean lfleetutil_w percent_chg_fare recession lcc i.year i.quarter||carriercode:LFPw, cov(un) var mle

predict pout_xtmixed_all

* total valid is 1035
* MSE
gen msefe_all = (pout_fe_all - OPOR)*(pout_fe_all - OPOR)
gen msere_all = (pout_re_all - OPOR)*(pout_re_all - OPOR)
gen msemixed_all = (pout_xtmixed_all - OPOR)*(pout_xtmixed_all - OPOR)

total msefe_all //  66.77606 
total msere_all  //  11.66085
total msemixed_all // 10.06839

***** MAPE
gen mapefe_all = abs(pout_fe_all - OPOR)/abs(OPOR)
gen mapere_all = abs(pout_re_all - OPOR)/abs(OPOR)
gen mapemixed_all = abs(pout_xtmixed_all - OPOR)/abs(OPOR)

total mapefe_all //   8294.869
total mapere_all //   1670.96
total mapemixed_all //  1500.202


#############################################################################
########################### OTHER ROBUSTNESS TEST #########################


* Robustness Test 1 Only keep carriers with more than 20 quarters
* egen cellsize=count(yearq), by (carriercode)
* drop if cellsize < 20

* Robustness Test 2 Only keep carriers with 64 quarters
* drop if cellsize < 64

* drop carriers with less than 8 quarters
* drop if carriercode == 3 | carriercode ==4 |carriercode ==5|carriercode == 7|carriercode == 12|carriercode == 17|carriercode == 21|carriercode == 22

* Only keep carriers with more than 50 quarters
* if n3 > 50
