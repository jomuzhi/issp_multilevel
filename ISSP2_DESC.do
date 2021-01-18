 cd "C:\Users\Muzhi\OneDrive - Nexus365\GenTime_MYK\Team working papers\ISSP_kamila\stata_mz"

 use "issp_religion_sample_mz.dta", clear
 
    ta C_ALPHAN Year
   gen Male=[Female==0]
   ta Male Female
   la def sex 1 "Men" 0 "Women"
   la val Male sex
   
****************************************************

*Factor analysis to create key variables of interest

****************************************************
/*
*1. Gender attitudes
    *Principle Factor Analysis:	
	polychoric ga1 ga2 ga3 ga4 ga5 ga6 ga7 
	display r(sum_w)
	matrix r = r(R)	
	
	*factormat r, n(10619) pcf //principle component factor analysis 
	factormat r, n(36420) pf //principal factor; the default
	*factormat r, n(17072) ipf // iterated principal factor
	*factormat r, n(17072) ml //maximum-likelihood	
	
 *--Table 1: dependent variable - results from principle factor analysis:-----
	rotate, promax horst  
	estat common //0.6012 correlation between factor 1 and 2
	*gender1,2,3 should belong to one
	*gender4,5,7 should belong to one	
	
**Consistency Check:
    alpha ga1 ga2 ga3 ga4 ga5 ga7, std item l
	alpha ga1 ga2 ga3 , std item l //alpha=0.728 
	bysort Year: alpha ga1 ga2 ga3, std item l //from 0.712 to 0.727
    bysort Region: alpha ga1 ga2 ga3, std item l
    //0.789, 0.781, 0.746, 0.656, 0.676, 0.595
	
	alpha ga4 ga5 ga7 , std item l //alpha=0.642
	bysort Year: alpha ga4 ga5 ga7 , std item l //from 0.605 to 0.642
    bysort Region: alpha ga4 ga5 ga7 , std item l 
    //0.586, 0.568, 0.629, 0.556, 0.640, 0.343
    alpha ga4 ga5 ga7 if C_ALPHAN=="JP", std item l 
    alpha ga4 ga5 ga7 if C_ALPHAN=="TW", std item l 
    alpha ga4 ga5 ga7 if Region==5, std item l 

*2. Marital Attitudes
    *Principle Factor Analysis:	
	polychoric ma1 ma2 ma3 ma4 ma5
	display r(sum_w)
	matrix r = r(R)	
	
	*factormat r, n(10619) pcf //principle component factor analysis 
	factormat r, n(36002) pf //principal factor; the default
	*factormat r, n(17072) ipf // iterated principal factor
	*factormat r, n(17072) ml //maximum-likelihood	
	
 *--Table 1: dependent variable - results from principle factor analysis:-----
	rotate, promax horst  
	estat common 
	
**Consistency Check:
    alpha ma1 ma2 ma4, std item l //0.674 
	alpha ma1 ma2 ma3 ma4 ma5, std item l //alpha=0.604 
	bysort Year: alpha ma1 ma2 ma4, std item l // from 0.649 to 0.694
    bysort Region: alpha ma1 ma2 ma4, std item l
    //0.616, 0.654, 0.663, 0.635, 0.604, 0.455
*/
    
    capture drop marcent
    gen marcent=ma1+ma2+ma4
    sum marcent
    gen marcent_raw=ma1+ma2+ma4+ma3+ma5
    
    gen ega1=ga1+ga2+ga3
    gen ega2=ga4+ga5+ga7
    corr ega1 ega2
    
    capture drop ega
    gen ega=ga1+ga2+ga3+ga4+ga5+ga7
    sum ega
    
  *Standardizing continuous variables of the whole sample:
  //Comparing values across the whole sample, individual-level variables
    egen z2marcent = std(marcent)
    egen z2marcent_raw = std(marcent_raw)
    egen z2ega1 = std(ega1)
    egen z2ega2 = std(ega2)
    egen z2ega= std(ega)
    
    egen z2age=std(Age)   
    gen agesq=z2age*z2age
    
    sum z2marcent
    
  la var z2marcent "Std. Marital centrality"
  la var z2ega1 "Std. EGA1"
  la var z2ega2 "Std. EGA2"
  la var z2ega "Std. EGA"
  la var z2age "Std. age"
  la var agesq "Std. age squared"
    
  *Dummy variables:
     tabulate Married, gen(married)
     tabulate nchild, gen(nchildc)
     tabulate Working, gen(working)
     replace Edulevel=1 if Edulevel==0
     ta C_ALPHAN Edulevel, nof row
     recode Edulevel 0/2=1 3=2 4/5=3, gen(Edu3c)
     la def Edu3c 1 "<Secondary" 2 "Secondary" 3 ">Secondary"
      ta C_ALPHAN Edu3c, nof row
     la val Edu3c Edu3c      
     *tabulate Edulevel, gen(educ)
     tabulate Edu3c, gen(educ)
     tabulate urban, gen(urbanc)
     
     
  encode C_ALPHAN, gen(country)
  la list country  
  
  
  merge n:1 country Yeardum using bycountry_gender_ineq_index_y02
  ta Yeardum _merge
  drop _merge
  
  merge n:1 country Yeardum using bycountry_gender_ineq_index_y12
  ta Yeardum _merge
  drop _merge
  
  gen country_gii=y2002
  replace country_gii=y2012 if country_gii==.
  la var country_gii "Gender Inequality Index"
  
  gen stdcountry_gii=stdy2002
  replace stdcountry_gii=stdy2012 if stdcountry_gii==.
  la var stdcountry_gii "Std. Gender Inequality Index (GII)"
  
  drop y2002 y2012 stdy2002 stdy2012
  
  merge n:1 country Yeardum Female using bycountry_evermarried
  drop _merge
  
  merge n:1 country Yeardum using bycountry_tfr
  drop _merge
  
  egen country_religion =mean(religion), by (country)
  
 *Standardize GII - this is wrong bc considered # of cases in each country:
 //values are reweighted, rather than comparing across countries
 //these values are comparing across all individuals in this dataset
 //what we need is a country-level variable, this go back to the country data
  foreach v in country_gii  {
  	  egen z2`v' = std(`v')
  }
  **
  la var z2country_gii "Std. Gender Inequality Index"
  la var country_evermarr "Proportion of people ever married"
  la var country_religion "Proportion of religious people"
  la var country_tfr "Total fertility rate"
  
  order country name Yeardum country_gii z2country_gii stdcountry_gii country_* 
  drop z2country_gii
  rename stdcountry_gii z2country_gii
  sort country Yeardum
  
   global country z2country_gii  country_evermarr country_tfr country_religion
   global y z2marcent z2marcent_raw
   global x  z2ega1 z2ega2 z2ega
   
   foreach v in $country {
    corr z2marcent `v'
    }
    
   foreach v in $country {
    corr z2marcent_raw `v'
    }
    
 save "issp_religion_sample2_mz.dta", replace    
  
*===========================================

*Sample Statistics

*===========================================
use "issp_religion_sample2_mz.dta",clear

*App Table A1:
    ta C_ALPHAN Year
    ta name Year
 
*Key variables:
    est clear
        global keyvar marcent ega 
        
    foreach i in $keyvar {
        forval s=0/1{
    estpost tabstat `i' if Female==1&Yeardum==`s' , ///
        by(C_ALPHAN) statistics(mean sd count) ///
        columns(statistics) listwise
    eststo fem`i'_`s'
    }
    }
    foreach i in $keyvar {
        forval s=0/1{
    estpost tabstat `i' if Female==0&Yeardum==`s' , ///
        by(C_ALPHAN) statistics(mean sd count) ///
        columns(statistics) listwise
    eststo men`i'_`s'
    }
    }
    **

    esttab fem*_0 fem*_1 using AppTA2.rtf, ///
        modelwidth(10) varwidth(16)  ///
        cells(mean(fmt(%9.3f))sd(par)) ///
        nostar nonote ///
        mtitle ("2002: Marriage Attitudes" "2002: EGA" ///
        "2012: Marriage Attitudes" "2012: EGA") ///
        title("Table A1a. Women: Mean (SD) of key individual-level variables (not standardized)") ///
        collabels(,none) nonumbers replace
        
     esttab men*_0 men*_1 using AppTA2.rtf, ///
        modelwidth(10) varwidth(10)  ///
        cells(mean(fmt(%9.3f))sd(par)) ///
        nostar nonote ///
        mtitle ("2002: Marriage Attitudes" "2002: EGA" ///
        "2012: Marriage Attitudes" "2012: EGA") ///
        title("Table A1b. Men: Mean (SD) of key individual-level variables (not standardized)") ///
        collabels(,none) nonumbers app  
  
 *2. Control variables:
    global clist Married nchild Working Edu3c
    
    foreach v in $clist{
    ta `v' C_ALPHAN , nof col
    }
    *net install http://www.stata.com/users/kcrow/tab2xl, replace
    foreach v in $clist{
    	forval s=0/1{
    tab2xl `v' C_ALPHAN using categoryv`s'_`v' if Yeardum==`s', ///
    col(1) row(1) perc replace
     }
    }
     **    
    
    est clear	
    global mylist  ///
            married1 married2 married3 ///
            nchildc1 nchildc2 nchildc3 nchildc4   ///
            working1 working2 working3 ///
            educ1 educ2 educ3  ///
            religion Age
            
    foreach i in $mylist {
        forval s=0/1{
    estpost tabstat `i' if Yeardum==`s' , ///
        by(C_ALPHAN) statistics(mean sd count) ///
        columns(statistics) listwise
    eststo desc`i'_`s'
    }
    }
    
    esttab descm*_0 descn*_0 using AppTA3.rtf, ///
        modelwidth(8) varwidth(8)  ///
        cells(mean(fmt(%9.2f))) ///
        nostar nonote ///
        mtitle ("Single" "Married" "Sep/Div/Wid"  ///
        "No child" "1 child" "2 children" ">2 children" ///
        "Full-time working" "Part-time working" ///
        "Not working" ///
        "<Sec. education" "Secondary" ///
        ">Sec. edu" ///
        "Has religion" "Age") ///
        title("Table A2a. Mean of control variables in 2002") ///
        collabels(,none) nonumbers replace 
        
    esttab descw*_0 desce*_0 descreligion_0 descAge_0 using AppTA3.rtf, ///
        modelwidth(8) varwidth(8)  ///
        cells(mean(fmt(%9.2f))) ///
        nostar nonote ///
        mtitle ("Full-time working" "Part-time working" ///
        "Not working" ///
        "<Sec. education" "Secondary" ///
        ">Sec. edu" ///
        "Has religion" "Age") ///
        collabels(,none) nonumbers app
        
        
       esttab descm*_1 descn*_1 using AppTA3.rtf, ///
        modelwidth(8) varwidth(8)  ///
        cells(mean(fmt(%9.2f))) ///
        nostar nonote ///
        mtitle ("Single" "Married" "Sep/Div/Wid"  ///
        "No child" "1 child" "2 children" ">2 children" ///
        "Full-time working" "Part-time working" ///
        "Not working" ///
        "<Sec. education" "Secondary" ///
         ">Sec. edu" ///
        "Has religion" "Age") ///
        title("Table A2b. Mean of control variables in 2012") ///
        collabels(,none) nonumbers app 
        
    esttab descw*_1 desce*_1 descreligion_1 descAge_1 using AppTA3.rtf, ///
        modelwidth(8) varwidth(8)  ///
        cells(mean(fmt(%9.2f))) ///
        nostar nonote ///
        mtitle ("Full-time working" "Part-time working" ///
        "Not working" ///
        "<Sec. education" "Secondary" ///
         ">Sec. edu" ///
        "Has religion" "Age") ///
        collabels(,none) nonumbers app
        
    
