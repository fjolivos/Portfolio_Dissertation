********************************************************************************
*Francisco Olivos
*Forms of migrant selectivity in China
********************************************************************************

set more off

global root = "C:\Users\fjoli\Dropbox\Data\CEPS"
global output = "C:\Users\fjoli\Dropbox\_PhD\Thesis\Papers Proposal\Paper 2 - Migration Expectations\Results"


set more off
use "${root}\cepsw1parentEN.dta", clear
merge 1:1 ids using "${root}\cepsw2parentEN.dta"
drop _merge
merge 1:1 ids using "${root}\cepsw1studentEN.dta"
drop _merge
merge 1:1 ids using "${root}\cepsw2studentEN.dta"
drop _merge
merge m:1 schids using "${root}\cepsw1principalEN.dta"

drop if (fall!=. & grade9==1) | fall==. // 9th graders were not followed in wave 2

********************************************************************************
*-------------------------------DATA MANAGEMENT--------------------------------*
********************************************************************************


************DEPENDENT VARIABLE: Expectation of Migration

tab c24
tab w2b20

clonevar exliwow2 = w2b20
recode   exliwow2 (1/2=1) (3/5=2) (6=3) (7=.)
tab      exliwow2 // Check
lab def  live 1 "Rural" 2 "Urban" 3 "Abroad"
lab val exliwow2 live

clonevar exliwow1 = c24
recode   exliwow1 (1=1) (2/3=2) (4=3) (5=.) // Need of sensitivity analysis due to diff in categories 
lab val  exliwow1 live

gen exmig1 = . // Location of the school according to the principal and expectation
replace exmig1 = 0 if (schloc_3c==1 | schloc_3c==2) &  exliwow1==2
replace exmig1 = 0 if (schloc_3c==3)                &  exliwow1==1
replace exmig1 = 1 if (schloc_3c==3)                & (exliwow1==2 | exliwow1==3)
replace exmig1 = 1 if (schloc_3c==1 | schloc_3c==2) & (exliwow1==3)
replace exmig1 = 1 if (schloc_3c==1 | schloc_3c==2) & (exliwow1==1) 
tab exmig1

gen exmig2 = .
replace exmig2 = 0 if (schloc_3c==1 | schloc_3c==2) &  exliwow2==2
replace exmig2 = 0 if (schloc_3c==3)                &  exliwow2==1
replace exmig2 = 1 if (schloc_3c==3)                & (exliwow2==2 | exliwow2==3)
replace exmig2 = 1 if (schloc_3c==1 | schloc_3c==2) & (exliwow2==3)
replace exmig2 = 1 if (schloc_3c==1 | schloc_3c==2) & (exliwow2==1)
tab exmig2

gen abroad1 = .
replace abroad1 = 1 if exliwow1==3
replace abroad1 = 0 if exliwow1==1 | exliwow1==2

gen abroad2 = .
replace abroad2 = 1 if exliwow2==3
replace abroad2 = 0 if exliwow2==1 | exliwow2==2 // Binary variable expection going abroad 

clonevar abroad1a = abroad1 // save variables after reshape to exclude abroad from the group analysis 
clonevar abroad2a = abroad2

*Maintaining I don't care in 0

clonevar abroad_rob1 = abroad1
replace abroad_rob1 = 0 if c24==5
clonevar abroad_rob2 = abroad2 
replace abroad_rob2 = 0 if w2b20==5

*For analysis of I dont care

recode c24 (1/4=0) (5=1), gen(idnc1)
recode w2b20 (1/6=0) (7=1), gen(idnc2)

*Id of urban-to-rural migration for robustness check

gen robmig1 = .
replace robmig1 = 1 if (schloc_3c==1 | schloc_3c==2) & (exliwow2==1) 

gen robmig2 = .
replace robmig2 = 1 if (schloc_3c==1 | schloc_3c==2) & (exliwow2==1) 

************KEY PREDICTORS

*Cognitive skills

clonevar cog1 = cog3pl
clonevar cog2 = w2cog3pl

*Meritocratic beliefs

lab def yes 1 "Yes" 0 "No" // Labeling mistake in wave 1, english dataset
lab val bb1206 yes
lab val bb1207 yes 
clonevar bhw1 = bb1206
clonevar bs1 = bb1207

clonevar bhw2 = w2bb1406
clonevar bs2 = w2bb1407 

*Effort

clonevar effortA1 = a1201
clonevar effortB1 = a1202
clonevar effortC1 = a1203

clonevar effortA2 = w2c2401
clonevar effortB2 = w2c2402
clonevar effortC2 = w2c2403

alpha effortA1 effortB1 effortC1
alpha effortA2 effortB2 effortC2

egen effort1 = rowmean(effortA1 effortB1 effortC1)
egen effort2 = rowmean(effortA2 effortB2 effortC2)

*Psycgological well-being 

clonevar blue1 = a1801
clonevar depre1 = a1802
clonevar unhappy1 = a1803
clonevar notenjo1 = a1804
clonevar sad1 = a1805 

clonevar blue2 = w2c2501
clonevar depre2 = w2c2502 
clonevar unhappy2 = w2c2503
clonevar notenjo2 = w2c2504
clonevar sad2 =  w2c2506

foreach var of varlist blue1 depre1 unhappy1 notenjo1 sad1 blue2 depre2 unhappy2 notenjo2 sad2{
revrs `var', replace
}
egen wellbeing1 = rowmean( blue1 depre1 unhappy1 notenjo1 sad1)
egen wellbeing2 = rowmean( blue2 depre2 unhappy2 notenjo2 sad2)

alpha blue1 depre1 unhappy1 notenjo1 sad1, item
alpha blue2 depre2 unhappy2 notenjo2 sad2, item

*Self-perceived health 

clonevar health1 = a17 
clonevar health2 = w2c04


*Perceived health (by parent)

clonevar health_pa1 = bd13
clonevar health_pa2 = w2bd08

**Declared objective conditions by parents

clonevar objhealth1 = bd1500
recode objhealth1 (1=0) (0=1)

clonevar objhealth2 = w2bd1100
recode objhealth2 (1=0) (0=1)

*Educational expectations 

clonevar exp1 = ba18
recode ba18 (1=0) (2=2) (3/5=4) (6/7=8) (8=10) (9=13) (10=.), gen(paconteduexpt1)
recode ba18 (1=0) (2=1) (3/5=2) (6/7=3) (8=4) (9=5) (10=.), gen(expb1)
recode ba18 (1/2=0) (3/5=1) (6/9=2) (10=.), gen(expc1)

clonevar exp2 = w2ba29 
recode w2ba29  (1=0) (2=2) (3/5=4) (6/7=8) (8=10) (9=13) (10=.), gen(paconteduexpt2)
recode w2ba29  (1=0) (2=1) (3/5=2) (6/7=3) (8=4) (9=5) (10=.), gen(expb2)
recode w2ba29  (1/2=0) (3/5=1) (6/9=2) (10=.), gen(expc2)

recode c22 (1=0) (2=2) (3/5=4) (6/7=8) (8=10) (9=13) (10=.), gen(eduexpect1) // Two indicators of expectations for a robustness check 
lab var eduexpect1 "Student's educational expectation"

recode w2ba29 (1=0) (2=2) (3/5=4) (6/7=8) (8=10) (9=13) (10=.), gen(eduexpect2)
lab var eduexpect2 "Student's educational expectation"


clonevar openness1 = bd1704 
clonevar openness2 = w2bd1304

************TIME-VARIANT CONTROLS 

*Pocket Money // proxy of income showcks

gen lpocket1 = log(ba06+1) // +1 to avoid zero problem 
lab var lpocket "Pocket money (Log)"

gen lpocket2 = log(w2ba04+1) // +1 to avoid zero problem 
lab var lpocket2 "Pocket money (Log)"

*Parents given instructions
clonevar inst1 = b2202
clonevar inst2 = w2a19

*Caring about stuff
egen care2 = rowmean(w2a2001-w2a2006)
egen care1 = rowmean(b2301 b2302 b2305 b2306 b2307 b2308)

alpha w2a2001-w2a2006
alpha b2301 b2302 b2305 b2306 b2307 b2308

*Engagement

egen engamo2 = rowmean(w2a2101b w2a2102b w2a2103b w2a2104b)
egen engafa2 = rowmean(w2a2101a w2a2102a w2a2103a w2a2104a)
egen engamo1 = rowmean( b24a1 b24a2 b24a3 b24a5)
egen engafa1 = rowmean( b24b1 b24b2 b24b3 b24b5) 

*activities together
egen act2 = rowmean(w2a26 w2a25 w2a24)
egen act1 = rowmean( b2801 b2806 b2805)

*Confident in the future of the child

clonevar confichild1 = ba21
clonevar confichild2 = w2ba32

*Difficulty of studies

clonevar diffmath1 = c1101
clonevar diffchin1 = c1102
clonevar diffeng1  = c1103

clonevar diffmath2 = w2b02
clonevar diffchin2 = w2b03
clonevar diffeng2  = w2b04 

*Parents' subjective assessment

clonevar psubass1 = bc10
clonevar psubass2 = w2bc05

*Social interaction 

clonevar schoolathmo1 = c1710
clonevar schoolbore1  = c1711
clonevar schoolmove1  = c1712

clonevar schoolathmo2 = w2b0608
clonevar schoolbore2  = w2b0609
clonevar schoolmove2  = w2b0610


*percentile rank math (0 low and 1 high)

egen ptile1    =  ridit(tr_mat), by(clsids)
gen  ptilesqr1 = ptile1*ptile1 

egen ptile2    =  ridit(w2upmat), by(clsids)
gen  ptilesqr2 = ptile2*ptile2

clonevar rank1 = ptile1

*percentile rank Chinese (0 low and 1 high)

egen ptile_ch1    =  ridit(stdchn), by(clsids)
gen  ptilesqr_ch1 = ptile_ch1*ptile_ch1 

egen ptile_ch2    =  ridit(w2chn), by(clsids)
gen  ptilesqr_ch2 = ptile_ch2*ptile_ch2

clonevar rank_ch1 = ptile_ch1


*percentile rank English (0 low and 1 high)

egen ptile_eng1    =  ridit(stdeng), by(clsids)
gen  ptilesqr_eng1 = ptile_eng1*ptile_eng1 

egen ptile_eng2    =  ridit(w2eng), by(clsids)
gen  ptilesqr_eng2 = ptile_eng2*ptile_eng2

clonevar rank_eng1 = ptile_eng1

*Id of students with within variance

gen     idwithin = . 
replace idwithin = 1 if exmig1==exmig2
replace idwithin = 0 if exmig1!=exmig2


*Mathematics teacher praises me
clonevar matpraise1 = c1307 
clonevar matpraise2 = w2b0507 

*Chinese teacher praises me
clonevar chnpraise1 = c1308 
clonevar chnpraise2 = w2b0508 

*English teacher praises me
clonevar engpraise1 = c1309 
clonevar engpraise2 = w2b0509

*Percentile rank of key variables for comparison of effects

egen ptile_cog1          =  ridit(cog1)
egen ptile_eduexpect1    =  ridit(eduexpect1)
egen ptile_effort1       =  ridit(effort1)
egen ptile_openness1     =  ridit(openness1)

gen ptile0_cog1          =  ptile_cog1*10
gen ptile0_eduexpect1    =  ptile_eduexpect1*10
gen ptile0_effort1       =  ptile_effort1*10
gen ptile0_openness1     = ptile_openness1*10

egen ptile_cog2          =  ridit(cog2)
egen ptile_eduexpect2    =  ridit(eduexpect2)
egen ptile_effort2      =  ridit(effort2)
egen ptile_openness2     =  ridit(openness2)

gen ptile0_cog2          =  ptile_cog2*10
gen ptile0_eduexpect2    =  ptile_eduexpect2*10
gen ptile0_effort2       =  ptile_effort2*10
gen ptile0_openness2     = ptile_openness2*10

*Standarization for benchmark

foreach var of varlist cog1 bhw1 bs1 effort1 openness1 wellbeing1 health1 paconteduexpt1 eduexpect1 ///
                       cog2 bhw2 bs2 effort2 openness2 wellbeing2 health2 paconteduexpt2 eduexpect2 ///
					   {
                        egen std_`var' = std(`var')
} 

************TO LONG FORMAT

reshape long ///
        exmig abroad idnc abroad_rob cog bhw bs effort wellbeing health objhealth health_pa paconteduexpt eduexpect ///
        lpocket inst care engamo engafa act confichild diffmath diffchin diffeng psubass ///
        schoolathmo schoolbore schoolmove ptile ptilesqr openness ///
		std_cog std_bhw std_bs std_effort std_openness std_wellbeing std_paconteduexpt std_eduexpect ///
        ptile_eng ptile_ch matpraise chnpraise engpraise ptile0_cog ptile0_eduexpect ptile0_effort ptile0_openness ///
, i(ids) j(w)

lab var exmig "Expectation of migration" 
lab var abroad "Expectation of migrating abroad"
lab var cog "Cognitive skills"
lab var bhw "Parental belief in hard work"
lab var bs "Parental beliefs in talents"
lab var effort "Index of students' effort" 
lab var wellbeing "Psychological well-being" 
lab var health "Declared health status"
lab var paconteduexpt "Parental educational expectations - continous"
lab var eduexpect "Students educational expectations - continous"
lab var lpocket "Pocket money - logarithm"
lab var inst "Frequency of parents giving instruction on your homework"
lab var care "Parental strictness index" 
lab var engamo "Mother-child communication"
lab var engafa "Father-child communication"
lab var act "Parents-child activities"
lab var confichild "Parents confidence in childs' future" 
lab var diffmath "Current difficulty of Mathematics"
lab var diffchin "Current difficulty of Chinese"
lab var diffeng "Current difficulty of English"
lab var psubass "Aprental subjective assessment of student's performance"
lab var schoolathmo "I feel close to people in this school"
lab var schoolbore "I feel bored in this school"
lab var schoolmove "I hope that I could transfer to another school"
lab var ptile "Percentile rank - Mathematics"
lab var ptile_eng "Percentile rank - English" 
lab var ptile_ch "Percentile rank - Chinese"
lab var ptilesqr "Squared percentile rank - Mathematics"
lab var w "Wave"
lab var idnc "Student's indifference"
lab var abroad_rob "Alternative indicator of abroad - Includes IDNC"
lab var objhealth "Declared objetive health disorders"
lab var health_pa "Perceived child's health by parents"
lab var openness "Openess"
lab var matpraise "Teacher praises me - Mathematics" 
lab var chnpraise "Teacher praises me - Chinese"
lab var engpraise "Teacher praises me - English"

order ids w exmig idnc schloc_3c c24 w2b20 abroad abroad1a abroad2a cog bhw bs effort wellbeing health paconteduexpt eduexpect openness ///
      lpocket inst care engamo engafa act confichild diffmath diffchin diffeng psubass ///
      schoolathmo schoolbore schoolmove ptile ptile_ch ptile_eng ptilesqr ///
	  std_cog std_bhw std_bs std_effort std_openness std_wellbeing std_paconteduexpt std_eduexpect
 
********************************************************************************
*-------------------------------------ANALYSES---------------------------------*
********************************************************************************

************Declare planel data structure
global w w
global ylist exmig
global key cog bhw bs eduexpect effort openness wellbeing health
global controls paconteduexpt lpocket inst care engamo engafa act confichild diffmath diffchin ///
                diffeng psubass schoolathmo schoolbore schoolmove ptile 
sort ids $w
xtset ids $w
xtdescribe
xtsum $ylist abroad idnc $key

*Esample

clogit abroad $key $controls $w [pw=w1w2sweight], ///
       group(ids) vce(cluster schids) or
	   
gen sample = e(sample)	

xx

**********Models for proposal


*Hausman test with xtlogit

xtlogit abroad $key $controls $w, re // rho is important, variation due to individual effects
estimate store random

xtlogit abroad $key $controls $w, fe
estimate store fixed

hausman fixed random // Significant: we have to use fixed effect
 
*Fixed effects and Average (semi) elasticities of Pr(y=1|x,u)

clogit abroad $key $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)		  
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab replace ctitle(1)	   
	
clogit abroad $key $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(2)	   

clogit abroad $key $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids) or
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab append ctitle(3)	
			  
aextlogit abroad $key $controls $w [w=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(4)	

*Standardised effects to compare 

revrs std_cog // to compare


aextlogit abroad revstd_cog bhw bs std_eduexpect std_effort std_openness wellbeing health $controls $w [w=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)

test revstd_cog=std_eduexpect
test revstd_cog=std_effort
test revstd_cog=std_openness


*Alternative linear model for supplementary material

xtreg abroad $key $w [pw=w1w2sweight] if sample==1, ///
          fe vce(cluster schids)		  
outreg2 using "${output}\linear", excel dec(2) alpha(0.001, 0.01, 0.05) lab replace ctitle(1)	   
	
xtreg abroad $key $controls $w [pw=w1w2sweight] if sample==1, ///
          fe vce(cluster schids)
outreg2 using "${output}\linear", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(2)	   

xtreg abroad $key $controls $w [pw=w1w2sweight] if sample==1, ///
          fe vce(cluster schids)
outreg2 using "${output}\linear", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(3)

set matsize 3000
reg abroad $key $controls $w i.ids [aw=w1w2sweight] if sample==1, ///
estat esize



*Comparison based on deciles following Heckman

clogit abroad ptile0_cog ptile0_eduexpect ptile0_effort ptile0_openness bhw bs wellbeing health $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids) or
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab replace ctitle(3)	
			  
aextlogit abroad ptile0_cog ptile0_eduexpect ptile0_effort ptile0_openness bhw bs wellbeing health $controls $w [w=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)			  
 outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab replace ctitle(3)	
*E-val

gen cog_by = .
replace cog_by = 1 if cog>0
replace cog_by = 0 if cog<=0
gen eduexpect_by1 = . 
replace eduexpect_by1 = 1 if eduexpect>=8
replace eduexpect_by1 = 0 if eduexpect<8
gen eduexpect_by2 = . // to avoid arbitrary cut-off points
replace eduexpect_by2 = 1 if eduexpect>8
replace eduexpect_by2 = 0 if eduexpect<=8

gen effort_by = .
replace effort_by = 1 if effort>3
replace effort_by = 0 if effort<=3

gen openness_by = . 
replace openness_by = 1 if openness>3
replace openness_by = 0 if openness<=3

clogit abroad cog_by bhw bs eduexpect_by2 effort_by openness_by wellbeing health $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or
		  
evalue_estat cog_by
evalue_estat eduexpect_by2
evalue_estat effort_by
evalue_estat openness_by

clogit abroad cog_by bhw bs eduexpect_by2 effort wellbeing health $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or	
		  
clogit abroad cog_by bhw bs eduexpect_by2 effort wellbeing $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or		  
		  
evalue_estat cog_by
evalue_estat eduexpect_by2
************Pooled correlations between key preditors

pwcorr cog bhw bs eduexpect effort openness wellbeing health [w=w1w2sweight] if sample==1, ///
        star(.001) bonferroni 


***********Student's indiference

clogit idnc $key $controls $w [pw=w1w2sweight], ///
          group(ids) vce(cluster schids)		
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(5)	

clogit idnc $key $controls $w [pw=w1w2sweight], ///
          group(ids) vce(cluster schids) or		
outreg2 using "${output}\migration", excel eform dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(6)	

aextlogit idnc $key $controls $w [w=w1w2sweight], ///
          group(ids) b vce(cluster schids)		
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(7)	

***********Abroad with IDNC as 0

clogit abroad_rob $key $controls $w [pw=w1w2sweight], ///
          group(ids) vce(cluster schids)		
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(8)	

clogit abroad_rob $key $controls $w [pw=w1w2sweight], ///
          group(ids) vce(cluster schids) or		
outreg2 using "${output}\migration", excel eform dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(9)	

aextlogit abroad_rob $key $controls $w [w=w1w2sweight], ///
          group(ids) b vce(cluster schids)		
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(10)	

***********Squared terms & Interaction

clogit abroad c.cog c.cog#c.cog bhw bs eduexpect effort openness wellbeing $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids) or		  

clogit abroad c.cog bhw bs eduexpect c.eduexpect#c.eduexpect effort openness wellbeing $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids) or


clogit abroad c.cog bhw bs eduexpect c.openness#c.openness openness effort wellbeing $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or

clogit abroad c.cog bhw bs eduexpect c.effort#c.effort openness effort wellbeing $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids) or
		  
*Be cautious

margins, at(effort=(1(.5)4)) atmeans



clogit abroad c.cog bhw bs eduexpect c.cog#c.eduexpect effort openness wellbeing $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or	
		  
		  
		  
*********Alternative indicators of health 


clogit abroad cog bhw bs eduexpect effort openness wellbeing objhealth $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids) or	
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab append ctitle(11)	

clogit abroad cog bhw bs eduexpect effort openness wellbeing health_pa $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids) or	
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab append ctitle(12)	


***Ex-post analysis of reward of skills at school 

gen ptile_math = 1-ptile
replace ptile_ch = 1-ptile_ch
replace ptile_eng = 1-ptile_eng


xtreg ptile_math cog eduexpect effort openness if sample==1, fe vce(cluster schids) 
outreg2 using "${output}\expost", excel dec(2) alpha(0.001, 0.01, 0.05) lab replace ctitle(Mathematics)

xtreg ptile_ch cog eduexpect effort openness if sample==1, fe vce(cluster schids)
outreg2 using "${output}\expost", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(Chinese)

xtreg ptile_eng cog eduexpect effort openness if sample==1, fe vce(cluster schids)
outreg2 using "${output}\expost", excel dec(2) alpha(0.001, 0.01, 0.05) lab append ctitle(English)



		  
********************************************************************************
*Non-reported models
********************************************************************************		


************Logit

logit $ylist $key $controls $w if sample==1

************Population-average estimator

xtlogit $ylist $key $controls $w if sample==1, pa

************Between estimator (Random effect)

xtlogit $ylist $key $controls $w if sample==1, re // rho is important, variation due to individual effects

estimate store random

************Within estimator (Fixed effect)

xtlogit $ylist $key $controls $w if sample==1, fe
estimate store fixed

************Hausman test for FE vs RE models

hausman fixed random // Significant: we have to use fixed effect
 
************Conditional logic models

// It allows to include weights

*All the sample
clogit $ylist $key $w [pw=w1w2sweight] if sample==1, ///
       group(ids) vce(cluster schids) or

outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab replace ctitle(FE)	   
	   
clogit $ylist $key $controls $w [pw=w1w2sweight] if sample==1, ///
       group(ids) vce(cluster schids) or
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform  lab append ctitle(FE+C)
estimate store clfixed

*Only internal migration

clogit $ylist $key $w [pw=w1w2sweight] if (abroad1a==0 & abroad2a==0) & sample==1, ///
          group(ids) vce(cluster schids)or 
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab append ctitle(Internal)

clogit $ylist $key $controls $w [pw=w1w2sweight] if (abroad1a==0 & abroad2a==0) & sample==1, ///
          group(ids) vce(cluster schids)or 
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab append ctitle(Internal)

*Migration abroad

clogit abroad $key $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or 
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform  lab append ctitle(International)

clogit abroad $key $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or 
outreg2 using "${output}\migration", excel dec(2) alpha(0.001, 0.01, 0.05) eform lab append ctitle(International)


*Interactions

*clogit abroad c.cog##c.eduexpect bhw bs effort wellbeing paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or 
		  
*clogit abroad c.cog##i.bhw eduexpect bs effort wellbeing paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or 		
		  
*clogit abroad c.cog##i.bs bhw eduexpect effort wellbeing paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or 
		  
*clogit abroad c.cog##c.effort bs bhw eduexpect wellbeing paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or

*clogit abroad c.cog##c.wellbeing effort bs bhw eduexpect paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or 	
		  
*clogit abroad c.eduexpect##i.bhw cog bs effort wellbeing paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or 		
		  
*clogit abroad c.eduexpect##i.bs bhw cog effort wellbeing paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or 
		  
clogit abroad c.eduexpect##c.effort bs bhw cog wellbeing paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or
		  
*clogit abroad c.eduexpect##c.wellbeing effort bs bhw cog paconteduexpt $controls $w [pw=w1w2sweight] if sample==1, ///
*          group(ids) vce(cluster schids)or	  

************Average elasticities for fixed effects logit

aextlogit $ylist $key $controls $w [w=w1w2sweight] if sample==1, ///
          group(ids) b vce(cluster schids)

// e.g. when cognitive skills increases one point, on average the probability of being unionised goes down by -14.7%. 

************Average elasticities for fixed effects logit with standardised coefficients

aextlogit $ylist ///
          std_cog std_bhw std_bs std_effort std_wellbeing std_paconteduexpt std_eduexpect ///
		  $controls $w [w=w1w2sweight] if sample==1, ///
          group(ids) b vce(cluster schids)

aextlogit $ylist ///
          std_cog std_bhw std_bs std_effort std_wellbeing std_paconteduexpt std_eduexpect ///
		  $controls $w [w=w1w2sweight] if sample==1, ///
          group(ids) b vce(cluster schids)		  
		  
***********Expectation abroad

aextlogit abroad $key $controls $w [w=w1w2sweight] if sample==1, ///
          group(ids) b vce(cluster schids)

***********Expectation internal migration 

aextlogit $ylist $key $controls $w [w=w1w2sweight] if (abroad1a==0 | abroad2a==0) & sample==1, ///
          group(ids) b vce(cluster schids) 
		  
***********Robustness check	1: Unmeasured confounding

quietly: clogit $ylist $key $controls $w [pw=w1w2sweight] if sample==1, ///
       group(ids) vce(cluster schids) or 
evalue_estat cog 
evalue_estat effort 
evalue_estat eduexpect

// treatments are required to be binary

gen cog_by = .
replace cog_by = 1 if cog>0
replace cog_by = 0 if cog<=0
gen eduexpect_by1 = . 
replace eduexpect_by1 = 1 if eduexpect>=8
replace eduexpect_by1 = 0 if eduexpect<8
gen eduexpect_by2 = . // to avoid arbitrary cut-off points
replace eduexpect_by1 = 1 if eduexpect>8
replace eduexpect_by1 = 0 if eduexpect<=8

clogit abroad cog_by bhw bs effort wellbeing paconteduexpt eduexpect_by2 $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or
		  
evalue_estat cog_by
evalue_estat eduexpect_by2

clogit abroad cog_by bhw bs effort wellbeing paconteduexpt eduexpect_by1 $controls $w [pw=w1w2sweight] if sample==1, ///
          group(ids) vce(cluster schids)or			  
evalue_estat eduexpect_by1
		  
/*minimum strength of association on the risk ratio scale that an unmeasured 
confounder would need to have with both the treatment and the outcome to fully 
explain away a specific treatment-outcome association*/

***********Robustness check	2: Results without urban-to-rural migration
	  
clogit $ylist $key $controls $w [pw=w1w2sweight] if sample==1 & (robmig1==. & robmig1==.), ///
       group(ids) vce(cluster schids) or

	   
***********Robustness check	3: Don't care
