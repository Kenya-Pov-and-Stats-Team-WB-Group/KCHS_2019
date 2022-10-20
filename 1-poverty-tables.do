*Set paths parameters
clear all
set max_memory .

use "${gsdDataRaw}/mi-imp.dta", clear
drop weight weight_pop weight_adq
rename (weight_hh_resid_prur weight_pop_resid_prur) (weight_hh weight_pop)
rename resid_prur resid 

qui svyset clid [pw=weight_hh], strata(strat) singleunit(certainty) 

decode county,gen(cstring)
decode resid,gen(rstring)
gen nat=1

*Generate age-bracket specific weights 
preserve 
use "${gsdDataRaw}/2019_KCHSP_individual_level.dta", clear 
replace b05_years=2019-b05_years if b05_years>120 & !mi(b05_years)
bys clid hhid: egen ctry_adq=sum(adq_scale)
*Flag individuals by age brackets
gen yrs05=inrange(b05_years,0,5)
gen yrs613=inrange(b05_years,6,13)
gen yrs1417=inrange(b05_years,14,17)
gen yrs017=inrange(b05_years,0,17)
gen yrs1835=inrange(b05_years,18,35)
gen yrs3659=inrange(b05_years,36,59)
gen yrs6069=inrange(b05_years,60,69)
qui summ b05_years,d
gen yrs70plus=inrange(b05_years,70,`r(max)')

merge m:m clid hhid using "${gsdDataRaw}/mi-imp.dta", keepusing(weight_hh_resid_prur) nogen keep(match) assert(match)
rename weight_hh_resid_prur weight

*Approach with age-bracket-specific weights
local agebr yrs05 yrs613 yrs1417 yrs017 yrs1835 yrs3659 yrs6069 yrs70plus
foreach a of local agebr {
	bys clid hhid: egen n_`a'=total(`a')
	gen weight_`a'=weight*n_`a'
	gen hh_has_`a'=n_`a'>0 & !mi(n_`a')
	drop n_`a'
}
duplicates drop clid hhid, force
qui savesome clid hhid weight_yrs05- hh_has_yrs70plus ctry_adq using "${gsdTemp}/child_weights.dta", replace
restore
merge m:1 clid hhid using "${gsdTemp}/child_weights.dta", keep(match) nogen assert(match)

gen weight_adq=ctry_adq* weight_hh

*Generate consumption quintiles
qui xtile cons_qtile=cons, n(5)
qui tab cons_qtile,gen(cons_qtile_)
gen fprop=fcons/cons
gen nfprop=1-fprop
gen nfcons=cons-fcons

***Generate FGTs indicators 
/*gen food_fgt0 = fpoor* ((fpline-fcons)/fpline)^0
gen food_fgt1 = fpoor * ((fpline-fcons)/fpline)^1
gen food_fgt2 = fpoor * ((fpline-fcons)/fpline)^2

gen abs_fgt0 = poor * ((abspline-cons)/abspline)^0
gen abs_fgt1 = poor * ((abspline-cons)/abspline)^1
gen abs_fgt2 = poor * ((abspline-cons)/abspline)^2

gen hc_poor=cons<fpline
gen hc_fgt0 = hc_poor * ((fpline-cons)/fpline)^0
gen hc_fgt1 = hc_poor * ((fpline-cons)/fpline)^1
gen hc_fgt2 = hc_poor * ((fpline-cons)/fpline)^2
*/

global foodpov (mean) food_fgt0 (sem) food_fgt0_se=food_fgt0 (mean) food_fgt1 (sem) food_fgt1_se=food_fgt1 (mean) food_fgt2 (sem) food_fgt2_se=food_fgt2
global abspov (mean) abs_fgt0 (sem) abs_fgt0_se=abs_fgt0 (mean) abs_fgt1 (sem) abs_fgt1_se=abs_fgt1 (mean) abs_fgt2 (sem) abs_fgt2_se=abs_fgt2
global hcpov (mean) hc_fgt0 (sem) hc_fgt0_se=hc_fgt0 (mean) hc_fgt1 (sem) hc_fgt1_se=hc_fgt1 (mean) hc_fgt2 (sem) hc_fgt2_se=hc_fgt2
global cons_exp (mean) Mean=cons (p50) Median=cons (mean) Q1= cons_qtile_1 (mean) Q2= cons_qtile_2 (mean) Q3= cons_qtile_3 (mean) Q4= cons_qtile_4 (mean) Q5= cons_qtile_5
global cons (mean) food=fcons (mean) nonfood=nfcons (mean) total=cons (mean) fperc=fprop (mean) nfperc=nfprop

local dimensions hh pop adq yrs05 yrs613 yrs1417 yrs017 yrs1835 yrs3659 yrs6069 yrs70plus
foreach d of local dimensions {
	qui mi svyset clid [pw=weight_`d'],  strata(strat) singleunit(certainty)

	preserve
	bys county: egen N=sum(weight_`d')
	collapse (first) cstring (mean) N=N ${foodpov} ${abspov} ${hcpov} ${cons_exp} ${cons_source} ${cons} [aw=weight_`d'],by(county)
	labmask county,val(cstring)
	drop cstring
	qui drop if mi(county)
	qui save "${gsdTemp}/fgts_`d'_county.dta", replace 
	restore

	preserve
	bys resid: egen N=sum(weight_`d')
	collapse (first) rstring (mean) N=N ${foodpov} ${abspov} ${hcpov} ${cons_exp} ${cons_source} ${cons} [aw=weight_`d'],by(resid)
	labmask resid,val(rstring)
	drop rstring
	qui drop if mi(resid)
	qui save "${gsdTemp}/fgts_`d'_resid.dta", replace 
	restore

	preserve
	bys nat: egen N=sum(weight_`d')
	collapse (mean) N=N ${foodpov} ${abspov} ${hcpov} ${cons_exp} ${cons_source} ${cons} [aw=weight_`d'],by(nat)
	gen national="National"
	qui drop if mi(nat)
	qui save "${gsdTemp}/fgts_`d'_nat.dta", replace 
	restore

	preserve 
	use "${gsdTemp}/fgts_`d'_nat.dta",clear
	qui append using "${gsdTemp}/fgts_`d'_resid.dta" "${gsdTemp}/fgts_`d'_county.dta"
	qui decode resid, gen(b)
	qui decode county, gen(c)
	qui gen residence_county=national if !mi(national)
	qui replace residence_county=b if !mi(b)
	qui replace residence_county=c if !mi(c)
	drop b c national nat resid county
	order residence_county, first
	replace N=N/100
	qui save "${gsdTemp}/fgts_`d'.dta", replace 
	qui export excel using "${gsdOutput}/poverty_2019.xlsx", sheet("raw_`d'") sheetmodify firstrow(variables) 
	restore 
	erase "${gsdTemp}/fgts_`d'_nat.dta"
	erase "${gsdTemp}/fgts_`d'_resid.dta"
	erase "${gsdTemp}/fgts_`d'_county.dta"
}

qui save "${gsdTemp}/consagg_adept.dta", replace 

***Get Multiple imputation consistent standard errors for the FGTs
use "${gsdDataRaw}/mi-imp-wide.dta", clear

*qui mi rename poor abs_fgt0
*qui mi rename pgi abs_fgt1
*qui mi passive: gen abs_fgt2 = abs_fgt1^2

*qui mi rename fpoor food_fgt0
*qui mi passive: gen food_fgt1 = (fpline-fcons)/fpline if !missing(fcons) & fcons<fpline
*qui mi passive: replace food_fgt1=0 if !missing(fcons) & fcons>fpline
*qui mi passive: gen food_fgt2 = food_fgt1^2

*qui mi passive: gen hc_fgt0 = cons<fpline if !mi(cons)
*qui mi passive: gen hc_fgt1= (fpline-cons)/fpline if !missing(cons) & cons<fpline
*qui mi passive: replace hc_fgt1=0 if !missing(cons) & cons>fpline
*qui mi passive: gen hc_fgt2 = hc_fgt1^2

merge m:1 clid hhid using "${gsdTemp}/child_weights.dta", keep(match) nogen assert(match) //merge child specific weights 
merge m:1 clid hhid using "${gsdDataRaw}/weight_adq.dta", keep(match) assert(match) nogen
rename weight weight_hh

qui save "${gsdDataRaw}/mi-imp-allstats.dta", replace

use "${gsdDataRaw}/mi-imp-allstats.dta", clear 

*creating level specific datasets
*Residence level
forvalues j = 1/2 {
   use "${gsdDataRaw}/mi-imp-allstats.dta", clear
   qui keep if resid == `j'
   tempfile resid_`j'
   qui save resid_`j'.dta, replace 
}
*County level
forvalues k = 1/47 {
	use "${gsdDataRaw}/mi-imp-allstats.dta", clear
    qui keep if county == `k'
    tempfile county_`k'
    qui save county_`k'.dta, replace 
}
*Save age bracket specific datasets (for MI-robust computations)
use "${gsdDataRaw}/mi-imp-allstats.dta", clear 
local agebr yrs05 yrs613 yrs1417 yrs017 yrs1835 yrs3659 yrs6069 yrs70plus
foreach a of local agebr {
	
	*National age bracket level
	qui savesome if hh_has_`a'==1 using "$gsdTemp/nat_`a'.dta", replace
	
	*Residence and age bracket level
	forval j=1/2 {
		qui savesome if resid==`j' & hh_has_`a'==1 using "$gsdTemp/resid_`j'_`a'.dta", replace
	}
	
	*County and age bracket level
	forval k=1/47 {
		qui savesome if county==`k' & hh_has_`a'==1 using "$gsdTemp/county_`k'_`a'.dta", replace
	}

}

*National level estimates
local agebr yrs05 yrs613 yrs1417 yrs017 yrs1835 yrs3659 yrs6069 yrs70plus
local typepov food abs hc 
foreach a of local agebr { //for each age bracket
	use "$gsdTemp/nat_`a'.dta", clear
	qui mi svyset clid [pw=weight_`a'], strata(strata) singleunit(centered)
	qui putexcel set "${gsdOutput}/poverty_2019.xlsx",  sheet("raw_`a'") modify
	foreach t of local typepov { //for each type of poverty
			forval m=0/0 { //for each FGT
			dis in red "Estimate `t' FGT`m' for age bracket `a' , at national level"
			qui mi estimate: svy: mean `t'_fgt`m' //compute MI robust estimates
			matrix A=r(table)
			matrix B=el(A,2,1) //store SE
			
			*Export Food FGTs //export SE into relevant sheet/colums
			if `m'==0 & "`t'"=="food" {
				qui putexcel D2=matrix(B) 
			}
			if `m'==1 & "`t'"=="food" {
				qui putexcel F2=matrix(B)
			}
			if `m'==2 & "`t'"=="food" {
				qui putexcel H2=matrix(B)
			}
			
			*Export Absolute FGTs
			if `m'==0 & "`t'"=="abs" {
				qui putexcel J2=matrix(B) 
			}
			if `m'==1 & "`t'"=="abs" {
				qui putexcel L2=matrix(B)
			}
			if `m'==2 & "`t'"=="abs"{
				qui putexcel N2=matrix(B)
			}
			
			*Export Hardcore FGTs
			if `m'==0 & "`t'"=="hc" {
				qui putexcel P2=matrix(B) 
			}
			if `m'==1 & "`t'"=="hc" {
				qui putexcel R2=matrix(B)
			}
			if `m'==2 & "`t'"=="hc"{
				qui putexcel T2=matrix(B)
			}

		}
	}
}

*Residence estimates
local rr=3
local agebr yrs05 yrs613 yrs1417 yrs017 yrs1835 yrs3659 yrs6069 yrs70plus
local typepov food abs hc 
foreach a of local agebr {
	forval r=1/2 {
		use "$gsdTemp/resid_`r'_`a'.dta", clear 
		qui mi svyset clid [pw=weight_`a'], strata(strata) singleunit(centered)
		qui putexcel set "${gsdOutput}/poverty_2019.xlsx",  sheet("raw_`a'") modify
		foreach t of local typepov {
			forval m=0/0 {
			dis in red "Estimate `t' FGT`m' for age bracket `a' within resid `c'"
			
			*As some imputations may be all 0s, identify those that are not always null (hence can be included in the MI estimate)
			qui count
			if `r(N)'<100 {
				qui set obs `=`r(N)'+(100-`r(N)')'
			}

			local i=1
			qui gen a_`t'_`m'=""
			foreach v of varlist _*_`t'_fgt`m' { //for each fgt and type of poverty
			 qui sum `v',d
			 if `r(mean)' >0 & `r(sd)'>0 { //store the variable names that have no all 0s
				qui replace a_`t'_`m' = "`v'" in `i' 
			 }
			local i=`i'+1
			}
			qui split a_`t'_`m',parse("_`t'")
			qui destring a_`t'_`m'1, replace force
			qui levelsof a_`t'_`m'1, local(imputations) //convert the names into the imputation number
			qui count if !mi(a_`t'_`m'1)
			local n_elig_m `r(N)'
			qui drop a_`t'_`m' a_`t'_`m'1
			qui drop if mi(hhid)
			if `n_elig_m' >= 2 {
				qui mi estimate, imputations(`imputations'): svy: mean `t'_fgt`m' //run the MI estimates only with 
				matrix A=r(table)
				matrix B=el(A,2,1)
				
				*Export Food FGTs
				if `m'==0 & "`t'"=="food" {
					qui putexcel D`rr'=matrix(B) 
				}
				if `m'==1 & "`t'"=="food" {
					qui putexcel F`rr'=matrix(B)
				}
				if `m'==2 & "`t'"=="food" {
					qui putexcel H`rr'=matrix(B)
				}
				
				*Export Absolute FGTs
				if `m'==0 & "`t'"=="abs" {
					qui putexcel J`rr'=matrix(B) 
				}
				if `m'==1 & "`t'"=="abs" {
					qui putexcel L`rr'=matrix(B)
				}
				if `m'==2 & "`t'"=="abs"{
					qui putexcel N`rr'=matrix(B)
				}
				
				*Export Hardcore FGTs
				if `m'==0 & "`t'"=="hc" {
					qui putexcel P`rr'=matrix(B) 
				}
				if `m'==1 & "`t'"=="hc" {
					qui putexcel R`rr'=matrix(B)
				}
				if `m'==2 & "`t'"=="hc"{
					qui putexcel T`rr'=matrix(B)
				}
	
			}

			}
		}
		local rr=`rr'+1
	}	
}
*County estimates
local cc=5
local agebr yrs05 yrs613 yrs1417 yrs017 yrs1835 yrs3659 yrs6069 yrs70plus
local typepov food abs hc 
foreach a of local agebr {
	forval c=1/47 {
		use "$gsdTemp/county_`c'_`a'.dta", clear 
		qui mi svyset clid [pw=weight_`a'], strata(strata) singleunit(centered)
		qui putexcel set "${gsdOutput}/poverty_2019.xlsx",  sheet("raw_`a'") modify
		foreach t of local typepov {
			forval m=0/0 {
			dis in red "Estimate `t' FGT`m' for age bracket `a' within county `c'"
			
			*As some imputations may be all 0s, identify those that are not always null (hence can be included in the MI estimate)
			qui count
			if `r(N)'<100 {
				qui set obs `=`r(N)'+(100-`r(N)')'
			}

			local i=1
			qui gen a_`t'_`m'=""
			foreach v of varlist _*_`t'_fgt`m' { //for each fgt and type of poverty
			 qui sum `v',d
			 if `r(mean)' >0 & `r(sd)'>0 { //store the variable names that have no all 0s
				qui replace a_`t'_`m' = "`v'" in `i' 
			 }
			local i=`i'+1
			}
			qui split a_`t'_`m',parse("_`t'")
			qui destring a_`t'_`m'1, replace force
			qui levelsof a_`t'_`m'1, local(imputations) //convert the names into the imputation number
			
			qui count if !mi(a_`t'_`m'1)
			local n_elig_m `r(N)'
			qui drop a_`t'_`m' a_`t'_`m'1
			qui drop if mi(hhid)
			if `n_elig_m' >= 2 {

				qui mi estimate, imputations(`imputations'): svy: mean `t'_fgt`m' //run the MI estimates only with 
				matrix A=r(table)
				matrix B=el(A,2,1)
				
				*Export Food FGTs
				if `m'==0 & "`t'"=="food" {
					qui putexcel D`cc'=matrix(B) 
				}
				if `m'==1 & "`t'"=="food" {
					qui putexcel F`cc'=matrix(B)
				}
				if `m'==2 & "`t'"=="food" {
					qui putexcel H`cc'=matrix(B)
				}
				
				*Export Absolute FGTs
				if `m'==0 & "`t'"=="abs" {
					qui putexcel J`cc'=matrix(B) 
				}
				if `m'==1 & "`t'"=="abs" {
					qui putexcel L`cc'=matrix(B)
				}
				if `m'==2 & "`t'"=="abs"{
					qui putexcel N`cc'=matrix(B)
				}
				
				*Export Hardcore FGTs
				if `m'==0 & "`t'"=="hc" {
					qui putexcel P`cc'=matrix(B) 
				}
				if `m'==1 & "`t'"=="hc" {
					qui putexcel R`cc'=matrix(B)
				}
				if `m'==2 & "`t'"=="hc"{
					qui putexcel T`cc'=matrix(B)
				}

				}
			}
		}
		local cc=`cc'+1
	}	
}
*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
**Absolute poverty
dis in red "Get MI consistent SE for the ABSOLUTE POVERTY FGTs"
local level hh adq pop  //for each analysis level. comment out the type of weights you don't want to use (remember: the more type of weights, the more the computational time)
local fgt 0 1 2
foreach l of local level {
	foreach m of local fgt {
		if  `m'==0 {
			local column = "J"			
		}
		else if  `m'==1 {
			local column = "L"			
		}	
		else if  `m'==2 {
			local column = "N"			
		}
		else  {
			di as error "Ensure that forvalues loop above runs for each fgt measure (0, 1 & 2)"
			error 1		
		}	
		use "${gsdDataRaw}/mi-imp-allstats.dta", clear
		qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
		qui putexcel set "${gsdOutput}/poverty_2019.xlsx",  sheet("raw_`l'") modify
		
		*National level
		dis "National level, with `l' weights"
		qui mi estimate: svy: mean abs_fgt`m' 
		matrix A=r(table)
		matrix B=el(A,2,1)
		qui putexcel `column'2=matrix(B)
		
		*Residence level
		dis "Residence level"
		local i=3
		forval j=1/2 {
			use resid_`j'.dta, clear
			mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
			dis in red "Residence level `j', with `l' weights" 
			qui mi estimate: svy: mean abs_fgt`m'
			matrix A=r(table)
			matrix B=el(A,2,1)
			qui putexcel `column'`i'=matrix(B)
			local i=`i'+1
		}
		
		*County level
		dis in green "County level"
		local i=5
		forval k=1/47 {
			use county_`k'.dta, clear
			qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
			dis in red "County level `k', with `l' weights"
			qui mi estimate: svy: mean abs_fgt`m'
			matrix A=r(table)
			matrix B=el(A,2,1)
			qui putexcel `column'`i'=matrix(B)
			local i=`i'+1
		}

	}
}

**Food poverty
dis in red "Get MI consistent SE for the FOOD POVERTY FGTs"
local level hh adq pop //for each analysis level
local fgt 0 1 2
foreach l of local level {
		foreach m of local fgt {
		if  `m'==0 {
			local column = "D"			
		}
		else if  `m'==1 {
			local column = "F"			
		}	
		else if  `m'==2 {
			local column = "H"		
		}
		else  {
			di as error "Ensure that forvalues loop above runs for each fgt measure (0, 1 & 2)"
			error 1		
		}	
	use "${gsdDataRaw}/mi-imp-allstats.dta", clear
	
	qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
	qui putexcel set "${gsdOutput}/poverty_2019.xlsx",  sheet("raw_`l'") modify
	
	*National level
	dis "National level, with `l' weights"
	qui mi estimate: svy: mean food_fgt`m' 
	matrix A=r(table)
	matrix B=el(A,2,1)
	qui putexcel `column'2=matrix(B)

	*Residence level
	dis "Residence level"
	local i=3
	forval j=1/2 {
		use resid_`j'.dta, clear
		qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
		dis in red "Residence level `j', with `l' weights"
		qui mi estimate: svy: mean food_fgt`m' 
		matrix A=r(table)
		matrix B=el(A,2,1)
		qui putexcel `column'`i'=matrix(B)
		local i=`i'+1
	}
	
	*County level
	dis in green "County level"
	local i=5
	forval k=1/47 {
		dis in red "County `k', with `l' weights"
		use county_`k'.dta, clear
		qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
		qui mi estimate: svy: mean food_fgt`m' 
		matrix A=r(table)
		matrix B=el(A,2,1)
		qui putexcel `column'`i'=matrix(B)
		local i=`i'+1
	}

}
}
**Hardcore poverty
dis in red "Get MI consistent SE for the HARCORE POVERTY FGTs"
local fgt 0 1 2
foreach l in pop adq hh {
		foreach m of local fgt {
		if  `m'==0 {
			local column = "P"			
		}
		else if  `m'==1 {
			local column = "R"			
		}	
		else if  `m'==2 {
			local column = "T"		
		}
		else  {
			di as error "Ensure that forvalues loop above runs for each fgt measure (0, 1 & 2)"
			error 1		
		}	
	use "${gsdDataRaw}/mi-imp-allstats.dta", clear
	qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
	qui putexcel set "${gsdOutput}/poverty_2019.xlsx",  sheet("raw_`l'") modify
	
	*National level
	dis "National level, with `l' weights"
	qui mi estimate: svy: mean hc_fgt`m' 
	matrix A=r(table)
	matrix B=el(A,2,1)
	qui putexcel `column'2=matrix(B)

	*Residence level
	dis "Residence level"
	local i=3
	forval j=1/2 {
		use resid_`j'.dta, clear
		qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
		dis in red "Residence level `j', with `l' weights"
		qui mi estimate: svy: mean hc_fgt`m' 
		matrix A=r(table)
		matrix B=el(A,2,1)
		qui putexcel `column'`i'=matrix(B)
		local i=`i'+1
	}
	
	*County level
	dis in green "County level"
	local i=5
	forval k=1/47 {
		dis in red "County `k', with `l' weights"
		use county_`k'.dta, clear
		qui mi svyset clid [pw=weight_`l'], strata(strata) singleunit(centered)
		*Two imputations (60 & 67 for Nairobi are constant (i.e. all zeros) these are excluded to allow for the construction of an SE)
		if inrange(`k',1,46) {
			mi estimate: svy: mean hc_fgt`m' 	
		}
		else if  `k' == 47 {
			qui mi estimate , imputations(1/59 61/66 68/100): svy: mean hc_fgt`m' if county ==47		
		}
		else {
			di in red "Ensure loop runs for each of the 47 counties"
			error 1
		}
		matrix A=r(table)
		matrix B=el(A,2,1)
		qui putexcel `column'`i'=matrix(B)
		local i=`i'+1
		}
	}
}			
*Erase temp datasets
qui filelist, dir("${gsdTemp}/") pat("*.dta") //list files 
qui levelsof filename if regexm(filename,"_yrs"),local(filestodelete) 
foreach f of local filestodelete {
	erase "$gsdTemp/`f'"	
}
qui filelist, dir("${gsdDo}/") pat("*.dta") //list files 
qui levelsof filename,local(filestodelete) 
foreach f of local filestodelete {
	erase "$gsdDo/`f'"	
}

*Contribution to poverty 
run "${swdLocal}/2-poverty-contribution.do" 

/*use "${gsdTemp}/consagg_adept.dta", clear  
 
alorenz  cons [pw=weight_hh], gl angle45
sumdist cons [aw=weight_hh]
lorenz  cons [pw=weight_hh]

*Palma ratio (PR). S(90)/S(40) ie ratio of share of income received by richest 10% and the share of income received by the poorest 40%.
qui sumdist cons [aw=weight_pop] 
dis in red "PR is " (1-`r(cush9)')/ `r(cush4)'
inineq cons,hsize(hhsize) p1(.9) p2(1) p3(0) p4(0.4) index(sr)

*Gini: Difference of 2 ppl randomly taken from distribution(i.e. mean difference delta) / mean income distribution. If =0 full equality; =100 full inequality (all goes to one person) 
fastgini cons [pw=weight_hh]
gen gini=.
forval i=1/47 {
	qui fastgini cons if county==`i' [pw=weight_hh]
	qui replace gini=`r(gini)' if county==`i'
}
assert !mi(gini)

preserve
collapse food_fgt? abs_fgt? hc_fgt? gini [pw=weight_hh],by(county)
qui save "${gsdTemp}/fgtcounty.dta", replace
restore 

*Create poverty map
cd "${gsdTemp}"
spshape2dta "${gsdDataRaw}/kenyan-counties/County.shp", replace 
use "${gsdTemp}/County.dta", clear 
replace COUNTY=upper(COUNTY)
replace COUNTY="ELGEYO MARAKWET" if regexm(COUNTY,"KEIYO-MARAKWET") 
replace COUNTY="THARAKA NITHI" if regexm(COUNTY,"THARAKA")
rename COUNTY a01
merge 1:1 a01 using "${gsdDataRaw}/county_codes.dta", keep(match) keepusing(county) assert(match) nogen
merge 1:1 county using "${gsdTemp}/fgtcounty.dta", keep(match) assert(match) nogen keepusing(food_fgt? abs_fgt? hc_fgt? gini)
foreach v of varlist food_fgt0-hc_fgt2 {
	qui replace `v'=`v'*100
}
spset 
grmap, activate
grmap gini, fcolor(Reds2) legstyle(2) cln(6) title("County distribution of Gini (2019)", size(*0.8)) 