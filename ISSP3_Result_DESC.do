*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

*Macro-level/Country-leve variable Correlations

*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

cd "C:\Users\Muzhi\OneDrive - Nexus365\GenTime_MYK\Team working papers\ISSP_kamila\stata_mz"   
    
****************************************************************************
* 2. Graphs showing macro-level measures in each country
*All variables are now set at country level
****************************************************************************

use "issp_religion_sample2_mz.dta", clear

*1. original individual attitude variables go back to original values
*2. after collapsed by country, recalculate the std. values to make them become country-level std.
*3. other contry-level index variables have already been at country-level, so no need to change

  collapse (mean) marcent (mean) marcent_raw ///
    (mean) ega1 (mean) ega2 (mean) ega ///
    (mean) z2country_gii (mean) country_evermarr (mean) country_tfr ///
    (mean) country_religion (mean)country_gii , ///
    by (C_ALPHAN name Male Yeardum)
    
  sort C_ALPHAN Yeardum
  
  bysort C_ALPHAN: replace name=name[_n-1] if name=="."
    
    
 *Country-level standardization:
    egen z2marcent = std(marcent)
    egen z2marcent_raw = std(marcent_raw)
    egen z2ega1 = std(ega1)
    egen z2ega2 = std(ega2)
    egen z2ega= std(ega)
    
      la var z2marcent "Std. Marital centrality"
      la var z2marcent_raw "Std. Marital centrality (5 items)"
      la var z2ega1 "Std. EGA1"
      la var z2ega2 "Std. EGA2"
      la var z2ega "Std. EGA (6 items)"
      la var z2country_gii "Std. Gender Inequality Index"
      la var country_evermarr "Proportion of people ever married (age 45-59)"
      la var country_religion "Proportion of religous people"
      la var country_tfr "Total fertility rate"
   
   encode C_ALPHAN, gen(country)
   la list country 
   
   gen Region=.
    replace Region=0 if C_ALPHAN=="DK"|C_ALPHAN=="NO"|C_ALPHAN=="SE"|C_ALPHAN=="FI"
    //Socio-democratic
    replace Region=1 if C_ALPHAN=="AU"|C_ALPHAN=="GB"|C_ALPHAN=="US"|C_ALPHAN=="CA"
    replace Region=2 if C_ALPHAN=="FR"|C_ALPHAN=="DE"|C_ALPHAN=="CH"|C_ALPHAN=="DE"|C_ALPHAN=="BE"|C_ALPHAN=="NL"|C_ALPHAN=="AT"
    replace Region=3 if C_ALPHAN=="PT"|C_ALPHAN=="ES"
    replace Region=4 if C_ALPHAN=="CZ"|C_ALPHAN=="HU"|C_ALPHAN=="LV"||C_ALPHAN=="PL"|C_ALPHAN=="RU"|C_ALPHAN=="SK"|C_ALPHAN=="SI"|C_ALPHAN=="LT"
    replace Region=5 if C_ALPHAN=="JP"|C_ALPHAN=="TW"|C_ALPHAN=="KR"
    la val Region Region      
    
  capture drop obs
  gen long obs = _n  
  
  *Make Year a string variable
    gen Year="2002" if Yeardum==0
    replace Year="2012" if Yeardum==1
    labmask Yeardum, values(Year)  
    
  global y  z2marcent z2marcent_raw  
  
  foreach v in country_gii country_tfr country_evermarr country_religion {
      sum `v'
  }
  
  
*----------Plot 1: Macro-level dots and fitted lines ------------

global x  z2ega1 z2ega2 z2ega
global country z2country_gii  country_evermarr country_tfr country_religion

foreach v in $x $country {
    /*
     su obs if Yeardum == `i', meanonly
     local t = Year[r(min)]  
    */
  #delimit ;   
  twoway scatter z2marcent `v' if Region==0,
  msymbol(s) mcolor(red) mlabel(country) || 
  scatter z2marcent `v' if Region==1,
  msymbol(Oh) mcolor(blue) mlabel(country) || 
  scatter z2marcent `v' if Region==2,
  msymbol(Oh) mcolor(green) mlabel(country) || 
  scatter z2marcent `v' if Region==3,
  msymbol(Oh) mcolor(yellow) mlabel(country) || 
  scatter z2marcent `v' if Region==4,
  msymbol(T) mcolor(pink) mlabel(country) || 
  scatter z2marcent `v' if Region==5,
  msymbol(x) mcolor(gs1) mlabel(country) || 
  lfit z2marcent `v' ,
  by(Male Yeardum, note("Data source: ISSP 2002&2012", size(vsmall)) 
  graphregion(color(white)) bgcolor(white)) 
  legend(order(1 "Socio-democratic" 
        2 "Liberal" 
        3 "Conservative" 
        4 "Mediterranean" 
        5 "Eastern Europe" 
        6 "East Asia"))legend(row(1) size(small))   
  ytitle("`: var label z2marcent'") 
  xtitle("`: var label `v''") 
  name(y_`v', replace) 
   ;
  #delimit cr 
  graph export Figure1_`v'.png, replace
}
  **  
*1. GII higher, more support to marital centrality
*2. More married, more support to marital centrality
*3. Higher TFR, less support to marital centrality
*4. Religious, flat line