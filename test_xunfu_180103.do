clear all
set more off

*cd "C:\Users\boubou\Box Sync\Boubou-Roro\I. Mandarin\Analysis\stata"
cd "C:\Users\WB512331\OneDrive - WBG\Boubou-Roro\I. Mandarin\Analysis\stata"

****************************************
****** data cleaning: grain price ******
****************************************

/*Goal:
1) clean data: eliminate duplicate obs
2) generate at province and annual level: pmin, pmax, mean, median (of prefecture midpoints), spread, log values, growth rate (mean, median)
3) collapse prefecture/monthly to province annual
*/

use XM_R1.dta, clear

gen edate = mdy(month, day, year)

sort prefecture edate
quietly by prefecture edate:  gen dup = cond(_N==1,0,_n)
tabulate dup

drop if dup>1

tsset prefecture edate

**replace price==0 with missing value

replace pmin =. if pmin == 0
replace pmax =. if pmax == 0

** generate lowerest/highest prices, and spread (provincial annual)
egen pmin_prov = min(pmin), by (province year)
egen pmax_prov = max(pmax), by (province year)
gen spread_prov = pmax_prov - pmin_prov

** generate mean price
local pvars "pmin pmax"
egen pmean_pref = rmean(`pvars')
egen pmean_prov = mean(pmean_pref), by (province year)

** generate median (get mid-points at prefecture-month level, then get median)
gen midpoint = (pmin+pmax)/2
egen pmedian_prov = median(midpoint), by (province year)

** collapse to provincial annual
drop prefecture county date month day pmin pmax var11 var12 var13 archive oldid dynasty chyear chmonth sn number grainname grain edate dup midpoint pmean_pref
bysort province year: keep if _n == _N
rename ( provincename province ) (province province_code)
save rice_prov_annual.dta, replace

***********************************
****** data cleaning - xunfu ******
***********************************

import excel using "xunfu_statacompact.xlsx", firstrow clear
rename province - Position_Chinese_y, lower

drop position name start_date_str end_date_str name_cn position_chinese_x position_chinese_y
rename (province name_chinese name_pinyin adminarea1 rank_english start_date end_date indexyear entryyear address address_cn examrank)(province_cn name_cn name province position_rank term_start term_end year_index cs_entryyear placeorigin placeorigin_cn cs_examrank)
desc

ren (dismissed year_death)(dismissed_str year_death_str)
list province year name if (dismissed_str != "0" & dismissed_str != "1")
replace dismissed_str = "0" if (province == "Guangxi" & year == 1825 & name == "suchenge")

destring dismissed_str, generate (dismissed)
destring year_death_str, gen(year_death)

drop dismissed_str year_death_str

** info correction on promotion/demotion
gen promoted =.
replace promoted = 1 if (position_rank == "1a" | position_rank == "1b" | position_rank == "2a" | position_rank == "SP")
replace promoted = 0 if (position_rank == "2b" | position_rank == "3a" | position_rank == "3b")

gen demoted =.
replace demoted = 1 if (position_rank == "3a" | position_rank == "3b")
replace demoted = 0 if (position_rank == "2b" | position_rank == "2a" | position_rank == "1b"  | position_rank == "1a"| position_rank == "SP")

** generate rectangle
fillin prov_code year

encode province, gen(prov_code)
encode name_cn, gen(name_code)
duplicates report prov_code year
duplicates list prov_code year

order province_cn province year name_cn name term_start term_end future_position position_rank promoted demoted dismissed dead retired position_order year_birth year_death cs_entryyear cs_examrank ethnic_manchu ethnic_mongol ethnic_hanjun ethnic_han royal_family emperor_family banner_byellow banner_pyellow banner_pwhite banner_pred banner_bwhite banner_bred banner_pblue banner_bblue placeorigin placeorigin_cn x y xycount year_index promotion demotion

save xunfu.dta, replace

***************************************
****** generate new vars: xunfu *******
***************************************
use xunfu.dta, clear
tsset prov_code year

/*vars to generate: age, tenure length, total yrs of service (career length),
                    year of service in the same province (term_yr),
                    promo/demo upon the year */

gen age = year-year_birth+1

gen str prov_name = province+"_"+name
bysort prov_name (year): gen term_yr = _n

gen tenure = hours(term_end-term_start)/(24*365.25)
gen career = year-cs_entryyear+1

foreach var of varlist promoted demoted dismissed {

gen `var'_nextyr = 0
bysort prov_name (year): replace `var'_nextyr = 1 if (`var' == 1 & prov_name[_n] != prov_name[_n+1])
replace `var'_nextyr =. if `var' ==.
order `var'_nextyr, after(`var')
}

order age, before (year_birth)
order term_yr tenure career, after (year_death)

save, replace

*** basic graph
use xunfu.dta, clear

egen han_year = total (ethnic_han), by (year)
egen all_year = count (name_code), by (year)
list year han_year all_year

graph twoway scatter han_year year
graph save graph\han_per_year.png, replace
graph twoway scatter han_year all_year year, title("Number of Han Provincial Governors") legend( order(1 "Han Governors" 2 "Total Governors") )

egen han_prov = sum (ethnic_han), by (province)
egen all_prov = count (name_code), by (province)
list province han_prov all_prov

gen percentage_han = 100*han_prov/all_prov
list percentage_han province

graph bar percentage_han, over (province) title (Percentage of Han Governors)


***************************
****** data merging *******
***************************

use xunfu.dta, replace
merge 1:m province year using rice_prov_annual.dta
tab province if _merge ==2
tab year if _merge ==2
keep if _merge ==3

rename (pmin_prov pmax_prov spread_prov pmean_prov pmedian_prov)(pmin pmax spread pmean pmedian)


save xunfu_econ.dta, replace

*****************************************
****** Analysis: generate new vars ******
*****************************************

use xunfu_econ.dta, clear
xtset prov_code year

gen pmean_lag = l.pmean
gen pmedian_lag = l.pmedian
gen lpmean = ln(pmean)
gen lpmedian = ln(pmedian)
gen lpmean_lag = l.lpmean
gen lpmedian_lag = l.lpmedian

*************************************
****** Analysis: test all vars ******
*************************************

*** Pooled OLS ***
local depvar "pmin pmax spread pmean pmedian"

foreach var in `depvar' {
gen `var'_ln=ln(`var')
gen `var'_gr=100*(`var'/l.`var'-1)
local cont1 "ethnic_han promoted dismissed dead pmean_lag"
local cont2 "ethnic_han promoted dismissed dead lpmean_lag"
local cont3 "ethnic_han age tenure career term_yr promoted dismissed dead pmean_lag"
local cont4 "ethnic_han age tenure career term_yr promoted dismissed dead lpmean_lag"

reg `var' `cont1'
eststo
reg `var'_ln `cont2'
eststo
reg `var'_gr `cont1'
eststo
reg `var' `cont3'
eststo
reg `var'_ln `cont4'
eststo
reg `var'_gr `cont3'
eststo
}
esttab using "output/test_allvars_pooledols.csv", keep (ethnic_han age tenure career term_yr promoted dismissed dead pmean_lag lpmean_lag) label star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

** only 240 obs, consider remove some controls. 

foreach var in `depvar' {
egen `var'_avg = mean(`var'), by(year)
gen `var'_diff= `var'-`var'_avg
reg `var'_diff ethnic_han age tenure career term_yr promoted dismissed dead pmean_lag
eststo
}
esttab using "output/test_allvars_diff.csv", keep (ethnic_han age tenure career term_yr promoted dismissed dead pmean_lag) label star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*** collapse into prov-name pair:
* question: did governance characteristics (rotation, type of govenor, etc) impact the econ attributes of then and today.
