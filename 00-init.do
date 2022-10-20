*Set paths parameters
clear all
set max_memory .
global suser = c(username)

clear all
set more off
set maxvar 10000
set seed 10051990 

* Define username
global suser = c(username)

*Emanuele
if (inlist("${suser}","wb562201","WB562201")) {
	global swdLocal = "C:\Users\wb562201\OneDrive - WBG\Countries\Kenya\KCHS_2019"
	global gsdDataRaw_imp "C:\Users\wb562201\WBG\Kenya Poverty and Statistics Team - WB Group - Data\1-CleanInput\2019\KCHS"
}
*Nduati
else if (inlist("${suser}","wb475840","WB475840")){	
	*Local directory of your checked out copy of the code
	global swdLocal = "C:\Users\wb475840\OneDrive - WBG\Desktop\KCHS2019_povtables\"
	global gsdDataRaw_imp "2022 poverty assessment folder "
}
*Pius
else if (inlist("${suser}","Pius")){	
	*Local directory of your checked out copy of the code
	global swdLocal = "Desktop location of the KCHS2019_povtables folder"
}
*Silas, or anyone else
else if (inlist("${suser}","Silas, or anyone else")){	
	*Local directory of your checked out copy of the code
	global swdLocal = "Desktop location of the KCHS2019_povtables folder"
}
*Define filepaths
global gsdDataRaw = "${swdLocal}/Rawinput"
global gsdTemp = "${swdLocal}/Temp"
global gsdOutput = "${swdLocal}/Output"
global gsdDo = "${swdLocal}/Do"

local dir Rawinput Temp Output Do
foreach d of local dir {
	confirmdir "${swdLocal}/`d'"
	 if _rc!=0 {
	 	shell mkdir "${swdLocal}/`d'"
	 }
}

if (inlist("${suser}","wb562201","WB562201")) {
	qui filelist, dir("${gsdDataRaw_imp}") pat("*.dta") //list files in folder
	drop if regexm(dirname,"Labor")
	qui levelsof filename,local(files) //store their names in local
	foreach f of local files {
		qui copy "${gsdDataRaw_imp}/`f'" "${gsdDataRaw}/`f'", replace
	}
}
