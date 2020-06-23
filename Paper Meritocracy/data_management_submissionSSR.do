******************************************
*Project: Feedback effect of meritocracy
*Data prepartion cross-lagged analysis
*March 30th 2019
******************************************

global root = "C:\Users\fjoli\Dropbox\Data\CEPS"
global output = "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Results"


*Dataset

set more off
clear all
use "${root}\cepsw2studentEN.dta", clear

merge m:1 ids using "${root}\cepsw2parentEN.dta"
drop _merge

clonevar sub2 = w2bc05

keep ids schids clsids w2clsids schids w2clsra w2bb1406 w2bb1407 w2mat w2eng w2chn sub2 w2status

save "${output}\s2t2p2.dta", replace

use "${root}\cepsw1studentEN.dta", clear

*Age 

gen birthyr = a02a

gen birthmon = a02b

gen age = 2013 - birthyr
label variable age "Age"

*Number of siblings including self

gen nsibling = stsib+1
replace nsibling = 1 if stsib==. 


egen relat1 = rowmean(b2801 b2802 b2803 b2804 b2805 b2806)
alpha b2801 b2802 b2803 b2804 b2805 b2806, item // .77

recode stmigrant (1=0) (2/3=1)
lab def stmigrant 1 "Migrant" 2 "Non-migrant", replace
lab val stmigrant stmigrant

gen hw1 = (((b14a1*60)+ b14a2)*5) + (((b14b1*60)+ b14b2)*2)

gen hw = ln(hw1)

*gen hw = (b14a1*5) + (b14b1*2)

keep ids clsids schids grade9 stdmat stdchn stdeng cog3pl steco_5c stmigrant sthktype stmedu stfedu nsibling age  stsex sweight c22 c23 c24 c14 hw relat1 b2501 b2502
merge 1:1 ids using "${root}\cepsw1parentEN"

clonevar par = ba01 // Biological parents 

*Relationship with parents

gen close = . 
replace close = 1 if ba01==1 & b2502==3
replace close = 1 if ba01==2 & b2501==3
replace close = 0 if ba01==2 & b2501<3 
replace close = 0 if ba01==1 & b2502<3 
lab var close "Relationship with parents"  


clonevar sub = bc10

keep ids clsids schids bb1206 bb1207 grade9 stdmat stdchn stdeng cog3pl steco_5c stmigrant sthktype stmedu stfedu nsibling age  stsex sweight par sub hw ba0801 ba1401 ba04 relat1 close 
gen schclassid = (schids*1000)+ clsids
drop if grade9==1
save "${output}\s1p1.dta", replace

keep clsids schids schclassid ids grade9 bb1206 bb1207 stdmat stdchn stdeng cog3pl steco_5c stmigrant sthktype stmedu stfedu nsibling age  stsex sweight par  sub hw ba0801 ba1401 ba04 relat1 close 


merge m:1 schids using "${root}\cepsw1principalEN.dta" // for random assigntment to class at 7th and 9th

gen urban = .
replace urban = 1 if pla23==1 | pla23==2
replace urban = 0 if pla23==3 | pla23==4 | pla23==5

keep clsids schids schclassid ids grade9 bb1206 bb1207 stdmat stdchn stdeng cog3pl steco_5c stmigrant sthktype stmedu stfedu nsibling age  stsex sweight par  sub hw ple1503 ple17 ba0801 ba1401 ba04 relat1 close  urban

merge m:1 clsids using "${root}\cepsw1teacherEN.dta" // for random assigntment to class at 7th 

keep clsids schids schclassid ids grade9 bb1206 bb1207 stdmat stdchn stdeng cog3pl steco_5c stmigrant sthktype stmedu stfedu nsibling age  stsex sweight par  sub hw ple1503 ple17 hra05 ba0801 ba1401 ba04 relat1 close urban

drop if grade9==1

gen w1 = 1 // for atrittion 

save "${output}\s1t1p1.dta", replace

merge 1:1 ids using "${root}\s2t2p2.dta"

keep clsids schids schclassid ids bb1206 bb1207 stdmat stdchn stdeng cog3pl steco_5c stmigrant sthktype stmedu stfedu nsibling age  stsex sweight hw w2clsids w2bb1406 w2bb1407 w2mat w2eng w2chn par  sub sub2 ple1503 ple17 hra05 w1 w2status ba0801 ba1401 ba04 relat1 close urban

save "${output}\mergemerit.dta", replace


***Export to Mplus

sort clsids stdmat
by clsids: gen rm1 = (_n - 1)/_N

sort clsids stdchn
by clsids: gen rc1 = (_n - 1)/_N

sort clsids  stdeng
by clsids: gen re1 = (_n - 1)/_N

sort schclassid w2mat
by schclassid: gen rm2 = (_n - 1)/_N

sort schclassid w2eng
by schclassid: gen re2 = (_n - 1)/_N

sort schclassid w2chn
by schclassid: gen rc2 = (_n - 1)/_N



*Grade ranks


sort schids stdmat
by schids: gen gm1 = (_n - 1)/_N

sort schids stdchn
by schids: gen gc1 = (_n - 1)/_N

sort schids  stdeng
by schids: gen ge1 = (_n - 1)/_N

sort schids w2mat
by schids: gen gm2 = (_n - 1)/_N

sort schids w2eng
by schids: gen ge2 = (_n - 1)/_N

sort schids w2chn
by schids: gen gc2 = (_n - 1)/_N


rename w2bb1406 hard2
rename w2bb1407 skill2

rename bb1206 hard1
rename bb1207 skill1

rename nsibling sib
rename cog3pl cog
rename steco_5c ses
rename stmigrant mig
rename sthktype huk 
rename stmedu emo
rename stfedu efa
rename stsex sex
rename hard1 h1
rename hard2 h2
rename skill1 s1
rename skill2 s2
rename sweight w

// code missing values consistently


tab w1 if w1==1 & par<3 // analytic sample

// Attrition alaysis

tab w2status

dis (10279-9440)*100/10279 // 8.1622726% attrition

gen attrition = .
replace attrition = 1 if w2status==2
replace attrition = 0 if w2status==1

probit attrition sex huk mig ses emo efa age sib cog

gen phat1 = normprob(_b[sex]*sex + _b[huk]*huk + _b[mig]*mig + ///
           _b[ses]*ses + _b[emo]*emo + _b[efa]*efa + _b[sib]*sib + _b[age]*age + ///
		   _b[sib]*sib + _b[cog]*cog + _b[_cons])

		   
sum huk  emo efa sib cog hw close urban mig sex age [w=w] if par<=2

replace rm1 = rm1*10
replace rm2 = rm2*10
replace rc1 = rc1*10
replace rc2 = rc2*10
replace re1 = re1*10
replace re2 = re2*10

sum rm1 rm2 rc1 rc2 re1 re2 h1 h2 s1 s2 [w=w] if par<=2
zz		   
// Dataset for Mplus



/*--------- Descriptives 

preserve

replace rm1 = rm1*10
replace rm2 = rm2*10
replace rc1 = rc1*10
replace rc2 = rc2*10
replace re1 = re1*10
replace re2 = re2*10

replace gm1 = gm1*10
replace gm2 = gm2*10
replace gc1 = gc1*10
replace gc2 = gc2*10
replace ge1 = ge1*10
replace ge2 = ge2*10
 
keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc1 re1 rc2 re2 gm1 gm2 gc1 ge1 gc2 ge2 sim2 sub sub2 w

tabstat rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc1 re1 rc2 re2 gm1 gm2 gc1 ge1 gc2 ge2 sim2 sub sub2 [w=w], stat(mean sd median N min max) c(s)

restore

*Tetrachoric correlation of meritocratic beliefs

tetrachoric h1 s1 h2 s2 

*--------- Main Model
		   
preserve

drop if par>2
// drop if ba04==2 // different measurements of closure ba0801 ba1401 ba04
replace rm1 = rm1*10
replace rm2 = rm2*10
replace rc1 = rc1*10
replace rc2 = rc2*10
replace re1 = re1*10
replace re2 = re2*10

keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc1 re1 rc2 re2 sub sub2 w phat1 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc2 re2 rc1 re1 sub sub2 w phat1
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc1 re1 rc2 re2 sub sub2 w phat1
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 rc1 re1 rc2 re2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 
     	 
   using "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Analysis\Main analysis\main1.dat", wide replace nolabel 
;
#del cr

restore

/*Effect sizes

dis exp(0.358) // hard work to hard work

dis exp(0.393) // skills to skills

dis exp(0.190) // hard work to skills 

dis exp(0.134) // skills to hard work

dis exp(0.133) // performance to hard work

dis exp(0.037) // performance to skills

*/

*-------------------------------- Robusteness checks

*--------- Subjective assessment 

preserve

drop if par>2

keep  h1 h2 s1 s2 age sex huk mig emo efa sib ses cog sub sub2 w phat1
sum   h1 h2 s1 s2 age sex huk mig emo efa sib ses cog sub sub2 w phat1
order h1 h2 s1 s2 age sex huk mig emo efa sib ses cog sub sub2 w phat1
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   h1 h2 s1 s2 age sex huk mig emo efa sib ses cog sub sub2 w phat1
     	 
   using "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Analysis\Robustness\Subjective assessment\subjective.dat", wide replace nolabel 
;
#del cr

restore

*--------- Grade rank

preserve

drop if par>2

replace gm1 = gm1*10
replace gm2 = gm2*10
replace gc1 = gc1*10
replace gc2 = gc2*10
replace ge1 = ge1*10
replace ge2 = ge2*10
 
keep gm1 gm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog gc1 ge1 gc2 ge2 w phat1
sum  gm1 gm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog gc2 ge2 gc1 ge1 w phat1
order gm1 gm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog gc1 ge1 gc2 ge2 w phat1
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   gm1 gm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog gc1 ge1 gc2 ge2 w phat1
     	 
   using "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Analysis\Robustness\Class rank\grade.dat", wide replace nolabel 
   
   ;
#del cr


restore


*--------- Random sample

preserve

keep if ple1503 == 1 & hra05 == 2 & ple17 == 5  

drop if par>2

replace rm1 = rm1*10
replace rm2 = rm2*10
replace rc1 = rc1*10
replace rc2 = rc2*10
replace re1 = re1*10
replace re2 = re2*10

keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w phat1
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w phat1
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w phat1
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w phat1 
     	 
   using "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Analysis\Robustness\Random sample\random.dat", wide replace nolabel 
;
#del cr

restore

*****Subsamples for WRMR sensitivity

preserve
drop if par>2
set seed 101
generate random = runiform()
sort random
generate insample = _n <= 9000 
drop if insample==0


keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged9000.dat", wide replace nolabel 
;
#del cr

restore

preserve
drop if par>2
set seed 102
generate random = runiform()
sort random
generate insample = _n <= 8000 
drop if insample==0


keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged8000.dat", wide replace nolabel 
;
#del cr

restore

preserve
drop if par>2
set seed 103
generate random = runiform()
sort random
generate insample = _n <= 7000 
drop if insample==0

keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged7000.dat", wide replace nolabel 
;
#del cr

restore

preserve
drop if par>2
set seed 104
generate random = runiform()
sort random
generate insample = _n <= 6000
drop if insample==0



keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged6000.dat", wide replace nolabel 
;
#del cr

restore 

preserve
drop if par>2
set seed 105
generate random = runiform()
sort random
generate insample = _n <= 5000 

drop if insample==0


keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged5000.dat", wide replace nolabel 
;
#del cr

restore


preserve
drop if par>2
set seed 106
generate random = runiform()
sort random
generate insample = _n <= 4000 

drop if insample==0


keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged4000.dat", wide replace nolabel 
;
#del cr

restore


preserve
drop if par>2
set seed 107
generate random = runiform()
sort random
generate insample = _n <= 3000 

drop if insample==0


keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged3000.dat", wide replace nolabel 
;
#del cr

restore


preserve
drop if par>2
set seed 108
generate random = runiform()
sort random
generate insample = _n <= 2000 

drop if insample==0


keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslagged2000.dat", wide replace nolabel 
;
#del cr

restore

*Results

/*
WRMR

Sample size	WRMR
9000	1.526
8000	1.432
7000	1.355
6000	1.276
5000	1.195
4000	1.067
3000	0.964
2000	0.805
*/

*Restricted sample of no tracking



**Attrition analysis

preserve
   
drop if par>2

keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w  phat1
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc2 re2 rc1 re1 w  phat1
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w  phat1
     	
mvencode _all, mv(9999)

drop if w==9999
		   
#del ; 	 
outfile 
   rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog rc1 re1 rc2 re2 w phat1 
     	 
   using "C:\Users\fjoli\Dropbox\Experiments\crosslaggedHeck.dat", wide replace nolabel 
;
#del cr

restore

*/

*****Dataset for interaction 


preserve

drop if par>2
// drop if ba04==2 // different measurements of closure ba0801 ba1401 ba04


keep rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc1 re1 rc2 re2 sub sub2 w phat1 relat1 close  par urban
sum  rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc2 re2 rc1 re1 sub sub2 w phat1 relat1 close  par urban
order rm1 rm2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw rc1 re1 rc2 re2 sub sub2 w phat1 relat1 close par urban


gen inter1 = relat1*h1
gen inter2 = relat1*s1

gen gend = .
replace gend = 1 if par==1 & sex==1
replace gend = 2 if par==1 & sex==0
replace gend = 3 if par==2 & sex==1
replace gend = 4 if par==2 & sex==0
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 rc1 re1 rc2 re2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 inter1 inter2 h1 h2 s1 s2 relat1 close par gend urban
     	 
   using "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Analysis\Main analysis\int.dat", wide replace nolabel 
;
#del cr

sum h1 h2 s1 s2 

tab h1 gend
tab h2 gend
tab s1 gend
tab s2 gend
restore

*--------- Grade rank

preserve

drop if par>2

replace gm1 = gm1*10
replace gm2 = gm2*10
replace gc1 = gc1*10
replace gc2 = gc2*10
replace ge1 = ge1*10
replace ge2 = ge2*10
 
keep gm1 gm2 gc1 ge1 gc2 ge2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 close par urban
sum  gm1 gm2 gc1 ge1 gc2 ge2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 close par urban
order gm1 gm2 gc1 ge1 gc2 ge2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 close par urban
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
  gm1 gm2 gc1 ge1 gc2 ge2 h1 h2 s1 s2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 close par urban
     	 
   using "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Analysis\Robustness\Class rank\grade.dat", wide replace nolabel 
   
   ;
#del cr


restore

*--------- Random sample

preserve

keep if ple1503 == 1 & hra05 == 2 & ple17 == 5  

drop if par>2


keep rm1 rm2 rc1 re1 rc2 re2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 h1 h2 s1 s2 close par urban
sum  rm1 rm2 rc1 re1 rc2 re2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 h1 h2 s1 s2 close par urban
order rm1 rm2 rc1 re1 rc2 re2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 h1 h2 s1 s2 close par urban
     	
mvencode _all, mv(9999)

drop if w==9999

#del ; 	 
outfile 
   rm1 rm2 rc1 re1 rc2 re2 age sex huk mig emo efa sib ses cog hw sub sub2 w phat1 h1 h2 s1 s2 close par urban
     	 
   using "C:\Users\fjoli\Dropbox\_PhD\Thesis\Paper_Meritocracy\Analysis\Robustness\Random sample\random.dat", wide replace nolabel 
;
#del cr

restore
