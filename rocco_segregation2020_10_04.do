* -- rocco_segregation.do
version 14

cd E:\projekte\segregation\rocco\

* --- read data from csv file
import delimited Brent_RP.csv, clear

tab choice, mis

* ---- select 2.5% random sample for faster preparation
gen r = runiform()
drop if r > 0.025

* ---- individual level variables
list id choice  in 1/100 , clean
unique(choice) // ssc install unique
sort choice 
list id choice  in 1/100 , clean
bysort choice: gen erste = _n if _n == 1
gen district_id = sum(erste)
list id choice district_id erste in 1/100 , clean

quietly tab choice
gen anz=r(r)
tab anz // 173 districts

* --- generate one row for each alternative within each subject
expand anz, generate(cops)

drop erste 
drop cops
sort district_id id

sort district_id id
by district_id id: gen alternative = _n

sort id district_id alternative
list id choice district_id  alternative  in 1/500 , clean

* --- go ahead: write data of "alteri"-district out and merge to have all 
* --- their characteristics for ego available
gen link = .
recast long link
*replace link = district_id* 10000 + alternative
replace link = district_id

* --- rename variable to allow merge
foreach var of varlist district_id choice frac_white frac_black frac_asian frac_high frac_mid frac_low frac_homos frac_homoe{
	gen `var'_=`var'
	}
bysort district_id: gen erste = 1 if _n == 1
gen link_ = link

* --- save data to become rematched many_to_one
savin link link_ choice_ frac_white_ - frac_homoe_ district_id_ using alternatives.dta if erste == 1, replace
drop link erste anz
quietly tab choice
gen anz=r(r)

gen link = .
recast long link
*replace link = alternative* 10000 + district_id
replace link =  alternative 

sort link
sort id  choice   alternative   district_id 
list id  choice   alternative   district_id  link  in 1/200, clean
sum link 

* --- check result
keep  id choice link ses ethnic frac_white district_id
sort id link
sort link id

* -- merge the alteri-districts here
merge m:1 link using alternatives.dta
sort id link
*browse(id choice link ses ethnic frac_white  choice_ district_id district_id_)
*browse
gen chosen = 0
replace chosen = 1 if choice == choice_

*browse(id choice link choice_ chosen frac_white_)

sort id  choice     district_id district_id_ 
list id choice link choice_ chosen frac_white_  district_id   link link_ in 1/200, clean

* --- asclogit needs numeric ids
by id: gen id_num = 1 if _n == 1
replace id_num = sum(id_num)
tab ses, gen(ses_)
tab ethnic, gen(ethnic_)

drop id link link_ _merge ses ethnic choice frac_white district_id choice_

* ---- centering of continious alternative variables to prepare interaction 
foreach var of varlist frac_white_ frac_black_ frac_asian_ frac_high_ frac_mid_ frac_low_ frac_homos_ frac_homoe_ {
	quietly sum `var'
	gen `var'M = r(mean)
	gen `var'Z = `var' - `var'M
	}
* --- generate interactions
foreach var of varlist frac_white_Z frac_black_Z frac_asian_Z frac_high_Z frac_mid_Z frac_low_Z frac_homos_Z frac_homoe_Z {
	gen `var'Xses_1 =`var'*ses_1
	gen `var'Xses_2 =`var'*ses_2
	gen `var'Xses_3 =`var'*ses_3
	
	gen `var'Xethnic_1 =`var'*ethnic_1
	gen `var'Xethnic_2 =`var'*ethnic_2
	gen `var'Xethnic_3 =`var'*ethnic_3
	}
	
* --- estimate models here	
clogit chosen frac_white_ , group(id_num)
clogit chosen frac_black_ frac_asian_  frac_mid_ frac_low_ frac_homos_ frac_homoe_ , group(id_num)

* --- clogit with interactions
clogit chosen frac_white_ frac_asian_   ///
		frac_white_ZXethnic_1 frac_white_ZXethnic_3 frac_asian_ZXses_1 frac_asian_ZXses_3, group(id_num)



* --- think about switching to RE model, estimate 
* --- person variables work only in interaction framework, like in clogit
xtlogit chosen frac_black_ frac_asian_ , re i(id_num) // ses_1 ses_3 ethnic_1 ethnic_3 

* --- go ahead with interaction, e.g. frac_black_X high_ses
* --- interpretation: interaction estimates the degree of how strongly the "varying" district characteristic
* --- is under-/overestimated for different ses groups 

*set matsize 900
*asclogit chosen frac_black_ frac_asian_  frac_mid_ frac_low_ frac_homos_ frac_homoe_, ///
*	casevars(ses_1 ses_3 ethnic_1 ethnic_3) case(id_num) alternatives(district_id_)
* swith to clogit with interactions help fvvarlist f

