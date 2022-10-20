*This dofile calulates the contribution to food, absolute and harcore poverty (Author: Emanuele Clemente eclemente@worldbank.org)

use "${gsdTemp}/consagg_adept.dta", clear
gen fpline=1954 if resid==1
replace fpline=2551 if resid==2
gen abspline=3252 if resid==1
replace abspline=5995 if resid==2  
*collapse survey county resid weight_hh weight_pop weight_adq strata hhsize fshare region fpline abspline fcons poor fpoor pgi poor_pline_20 cons nat (first) costring=cstring (first) restring=rstring ,by(clid hhid)
replace cstring="Murang'a" if regexm(cstring,"Muran")
replace cstring="Trans Nzoia" if regexm(cstring,"Trans")
replace cstring="THARAKA NITHI" if regexm(cstring,"Tharaka")
labmask county, val(cstring)
labmask resid, val(rstring)
gen _mi_miss=.
mi unset
***FOOD
dis in red "Contribution for FOOD poverty"
local level adq //hh pop  //for each analysis level
foreach l of local level {
	qui svyset clid [pw=weight_`l'],  strata(strat) singleunit(certainty) 
	
	forval f=0/2 { //for each indicator
		
		*Residence area
		 qui dfgtg fcons, hgroup(resid) alpha(`f') pline(fpline) xfil("${gsdTemp}/contributionresid_`l'_fgt`f'.xlsx") xshe(Table 01) dec(3)
		preserve 
		 qui import excel "${gsdTemp}/contributionresid_`l'_fgt`f'.xlsx", sheet("Table 01") cellrange(A3:J32) firstrow clear
		qui drop if mi(Relativecontribution)
		qui keep A Relativecontribution
		qui gen se=Relativecontribution if mi(A)
		qui replace se=se[_n+1] if !mi(se[_n+1]) & mi(se)
		qui drop if mi(A)
		qui gen residence=-3 if A=="Population"
		qui replace residence=-2 if A=="Rural"
		qui replace residence=-1 if A=="Urban"
		sort residence
		qui save "${gsdTemp}/contributionresid_`l'_fgt`f'.dta", replace 
		restore
	
		*County
		 qui dfgtg fcons, hgroup(county) alpha(`f') pline(fpline) xfil("${gsdTemp}/contributioncty_`l'_fgt`f'.xlsx") xshe(Table 01) dec(3)
		preserve 
		qui import excel "${gsdTemp}/contributioncty_`l'_fgt`f'.xlsx", sheet("Table 01") cellrange(A3:CV122) firstrow clear
		qui drop if mi(Relativecontribution)
		qui keep A Relativecontribution
		qui gen se=Relativecontribution if mi(A)
		qui replace se=se[_n+1] if !mi(se[_n+1]) & mi(se)
		qui drop if mi(A)
		qui save "${gsdTemp}/contributioncty_`l'_fgt`f'.dta", replace 
		restore
	}

	preserve 
	use "${gsdTemp}/contributioncty_`l'_fgt0.dta",clear 
	rename (Relativecontribution se) (cont_fgt0_food se_cont_fgt0_food)
	qui merge 1:1 A using "${gsdTemp}/contributioncty_`l'_fgt1.dta", nogen keep(match) 
	qui rename (Relativecontribution se) (cont_fgt1_food se_cont_fgt1_food)
	qui merge 1:1 A using "${gsdTemp}/contributioncty_`l'_fgt2.dta", nogen keep(match) 
	rename (Relativecontribution se A) (cont_fgt2_food se_cont_fgt2_food a01)
	qui replace a01=upper(a01)
	qui merge 1:1 a01 using "${gsdDataRaw}/county_codes.dta",  nogen keep(match) 
	sort county
	qui export excel cont_fgt0_food se_cont_fgt0_food cont_fgt1_food se_cont_fgt1_food cont_fgt2_food se_cont_fgt2_food  using "${gsdOutput}/poverty_2019.xlsx", sheet("raw_`l'") sheetmodify cell(AG5) 
	restore
	
	preserve 
	use "${gsdTemp}/contributionresid_`l'_fgt0.dta",clear 
	rename (Relativecontribution se) (cont_fgt0_food se_cont_fgt0_food)
	qui merge 1:1 A using "${gsdTemp}/contributionresid_`l'_fgt1.dta", keep(match) nogen
	qui rename (Relativecontribution se) (cont_fgt1_food se_cont_fgt1_food)
	qui merge 1:1 A using "${gsdTemp}/contributionresid_`l'_fgt2.dta", nogen keep(match)
	rename (Relativecontribution se A) (cont_fgt2_food se_cont_fgt2_food a01)
	sort residence
	qui export excel cont_fgt0_food se_cont_fgt0_food cont_fgt1_food se_cont_fgt1_food cont_fgt2_food se_cont_fgt2_food  using "${gsdOutput}/poverty_2019.xlsx", sheet("raw_`l'") sheetmodify cell(AG1) firstrow(variables)
	restore
}

***ABSOLUTE
dis in red "Contribution for ABSOLUTE poverty"
local level adq //hh pop  //for each analysis level
foreach l of local level {
	qui svyset clid [pw=weight_`l'],  strata(strat) singleunit(certainty)
	
	forval f=0/2 { //for each indicator
		
		*Residence area
		qui dfgtg cons, hgroup(resid) alpha(`f') pline(abspline) xfil("${gsdTemp}/contributionresid_`l'_fgt`f'.xlsx") xshe(Table 01) dec(3)
		preserve 
		qui import excel "${gsdTemp}/contributionresid_`l'_fgt`f'.xlsx", sheet("Table 01") cellrange(A3:J32) firstrow clear
		qui drop if mi(Relativecontribution)
		keep A Relativecontribution
		qui gen se=Relativecontribution if mi(A)
		qui replace se=se[_n+1] if !mi(se[_n+1]) & mi(se)
		qui drop if mi(A)
		qui gen residence=-3 if A=="Population"
		qui replace residence=-2 if A=="Rural"
		qui replace residence=-1 if A=="Urban"
		sort residence
		qui save "${gsdTemp}/contributionresid_`l'_fgt`f'.dta", replace 		
		restore
	
		*County
		qui dfgtg cons, hgroup(county) alpha(`f') pline(abspline) xfil("${gsdTemp}/contributioncty_`l'_fgt`f'.xlsx") xshe(Table 01) dec(3)
		preserve 
		qui import excel "${gsdTemp}/contributioncty_`l'_fgt`f'.xlsx", sheet("Table 01") cellrange(A3:CV122) firstrow clear
		qui drop if mi(Relativecontribution)
		keep A Relativecontribution
		qui gen se=Relativecontribution if mi(A)
		qui replace se=se[_n+1] if !mi(se[_n+1]) & mi(se)
		qui drop if mi(A)
		qui save "${gsdTemp}/contributioncty_`l'_fgt`f'.dta", replace 
		restore
	}

	preserve 
	use "${gsdTemp}/contributioncty_`l'_fgt0.dta",clear 
	rename (Relativecontribution se) (cont_fgt0_abs se_cont_fgt0_abs)
	qui merge 1:1 A using "${gsdTemp}/contributioncty_`l'_fgt1.dta",assert(match) nogen
	qui rename (Relativecontribution se) (cont_fgt1_abs se_cont_fgt1_abs)
	qui merge 1:1 A using "${gsdTemp}/contributioncty_`l'_fgt2.dta",assert(match) nogen
	rename (Relativecontribution se A) (cont_fgt2_abs se_cont_fgt2_abs a01)
	qui replace a01=upper(a01)
	qui merge 1:1 a01 using "${gsdDataRaw}/county_codes.dta", keep(match) keepusing(county) assert(match master) nogen
	sort county
	qui export excel cont_fgt0_abs se_cont_fgt0_abs cont_fgt1_abs se_cont_fgt1_abs cont_fgt2_abs se_cont_fgt2_abs  using "${gsdOutput}/poverty_2019.xlsx", sheet("raw_`l'") sheetmodify cell(AM5) 
	restore
	
	preserve 
	use "${gsdTemp}/contributionresid_`l'_fgt0.dta",clear 
	rename (Relativecontribution se) (cont_fgt0_abs se_cont_fgt0_abs)
	qui merge 1:1 A using "${gsdTemp}/contributionresid_`l'_fgt1.dta",assert(match) nogen
	qui rename (Relativecontribution se) (cont_fgt1_abs se_cont_fgt1_abs)
	qui merge 1:1 A using "${gsdTemp}/contributionresid_`l'_fgt2.dta",assert(match) nogen
	rename (Relativecontribution se A) (cont_fgt2_abs se_cont_fgt2_abs a01)
	sort residence
	qui export excel cont_fgt0_abs se_cont_fgt0_abs cont_fgt1_abs se_cont_fgt1_abs cont_fgt2_abs se_cont_fgt2_abs  using "${gsdOutput}/poverty_2019.xlsx", sheet("raw_`l'") sheetmodify cell(AM1) firstrow(variables)
	restore
	
}

***HARDCORE
dis in red "Contribution for HARDCORE poverty"
local level adq //hh pop  //for each analysis level
foreach l of local level {
	qui svyset clid [pw=weight_`l'],  strata(strat) singleunit(certainty)
	
	forval f=0/2 { //for each indicator
		
		*Residence area
		qui dfgtg cons, hgroup(resid) alpha(`f') pline(fpline) xfil("${gsdTemp}/contributionresid_`l'_fgt`f'.xlsx") xshe(Table 01) dec(3)
		preserve 
		qui import excel "${gsdTemp}/contributionresid_`l'_fgt`f'.xlsx", sheet("Table 01") cellrange(A3:J32) firstrow clear
		qui drop if mi(Relativecontribution)
		keep A Relativecontribution
		qui gen se=Relativecontribution if mi(A)
		qui replace se=se[_n+1] if !mi(se[_n+1]) & mi(se)
		qui drop if mi(A)
		qui gen residence=-3 if A=="Population"
		qui replace residence=-2 if A=="Rural"
		qui replace residence=-1 if A=="Urban"
		sort residence
		qui save "${gsdTemp}/contributionresid_`l'_fgt`f'.dta", replace 		
		restore
	
		*County
		qui dfgtg cons, hgroup(county) alpha(`f') pline(fpline) xfil("${gsdTemp}/contributioncty_`l'_fgt`f'.xlsx") xshe(Table 01) dec(3)
		preserve 
		qui import excel "${gsdTemp}/contributioncty_`l'_fgt`f'.xlsx", sheet("Table 01") cellrange(A3:CV122) firstrow clear
		qui drop if mi(Relativecontribution)
		keep A Relativecontribution
		qui gen se=Relativecontribution if mi(A)
		qui replace se=se[_n+1] if !mi(se[_n+1]) & mi(se)
		qui drop if mi(A)
		qui save "${gsdTemp}/contributioncty_`l'_fgt`f'.dta", replace 
		restore
	}

	preserve 
	use "${gsdTemp}/contributioncty_`l'_fgt0.dta",clear 
	rename (Relativecontribution se) (cont_fgt0_hc se_cont_fgt0_hc)
	qui merge 1:1 A using "${gsdTemp}/contributioncty_`l'_fgt1.dta",assert(match) nogen
	qui rename (Relativecontribution se) (cont_fgt1_hc se_cont_fgt1_hc)
	qui merge 1:1 A using "${gsdTemp}/contributioncty_`l'_fgt2.dta",assert(match) nogen
	rename (Relativecontribution se A) (cont_fgt2_hc se_cont_fgt2_hc a01)
	qui replace a01=upper(a01)
	qui merge 1:1 a01 using "${gsdDataRaw}/county_codes.dta", keep(match) keepusing(county) assert(match master) nogen
	sort county
	qui export excel cont_fgt0_hc se_cont_fgt0_hc cont_fgt1_hc se_cont_fgt1_hc cont_fgt2_hc se_cont_fgt2_hc  using "${gsdOutput}/poverty_2019.xlsx", sheet("raw_`l'") sheetmodify cell(AS5) 
	restore
	
	preserve 
	use "${gsdTemp}/contributionresid_`l'_fgt0.dta",clear 
	rename (Relativecontribution se) (cont_fgt0_hc se_cont_fgt0_hc)
	qui merge 1:1 A using "${gsdTemp}/contributionresid_`l'_fgt1.dta",assert(match) nogen
	qui rename (Relativecontribution se) (cont_fgt1_hc se_cont_fgt1_hc)
	qui merge 1:1 A using "${gsdTemp}/contributionresid_`l'_fgt2.dta",assert(match) nogen
	rename (Relativecontribution se A) (cont_fgt2_hc se_cont_fgt2_hc a01)
	sort residence
	qui export excel cont_fgt0_hc se_cont_fgt0_hc cont_fgt1_hc se_cont_fgt1_hc cont_fgt2_hc se_cont_fgt2_hc  using "${gsdOutput}/poverty_2019.xlsx", sheet("raw_`l'") sheetmodify cell(AS1) firstrow(variables)
	restore
	
}

*Erase temp files
qui filelist, dir("${gsdTemp}/") pat("*.xlsx") 
qui levelsof filename,local(filestoerase) 
foreach f of local filestoerase { //for each temp file
	if regexm("`f'","ontribu") {
		qui erase "${gsdTemp}/`f'" //erase it 
	}
}
qui filelist, dir("${gsdTemp}/") pat("*.dta") 
qui levelsof filename,local(filestoerase) 
foreach f of local filestoerase { //for each temp file
	if regexm("`f'","ontribution") {
		qui erase "${gsdTemp}/`f'" //erase it 
	}
}