cd "C:\Users\Administrator\OneDrive - HKUST\Supervision\LUO_ys\Intergen_xcountry\Stata_issp_mz"

*ssc install mltcooksd //new tool for multileve modelling
*ssc install aaplot

    global y  z2marcent /*z2marcent_raw*/
    global x  z2ega1 z2ega2 z2ega
    global country z2country_gii  country_tfr /*country_evermarr  country_religion*/
    global c  married2 married3 nchildc2 nchildc3 nchildc4 working2 working3 ///
              educ2 educ3 religion ///
              z2age agesq

    global weight regionweight
    
 
****************************************************************************

* Models - 1. Country-fixed: check the relationship between EGA and MA
 //No country-level variables

**************************************************************************** 
use "issp_religion_sample3_mz50.dta",clear

    xtset country pid
    xtdes

est clear

forval i=0/1{
    xtreg $y  c.z2ega i.Male $c if Yeardum==`i' ,fe
    eststo m1_yr`i'
    
	xtreg $y  c.z2ega##i.Male $c if Yeardum==`i',fe
    eststo m2_yr`i'
}
  **
  
    est clear 
  *------------2-factor gender-role attitdes:--------------
    forval i=0/1{
        xtreg $y  c.z2ega1 i.Male $c if Yeardum==`i' ,fe
        eststo m1_yr`i'
        
        xtreg $y  c.z2ega1##i.Male $c if Yeardum==`i',fe
        eststo m2_yr`i'
    }
      **
        
    est clear 
    forval i=0/1{
        xtreg $y  c.z2ega2 i.Male $c if Yeardum==`i' ,fe
        eststo m1_yr`i'
        
        xtreg $y  c.z2ega2##i.Male $c if Yeardum==`i',fe
        eststo m2_yr`i'
    }
      **
      
    est clear 
    forval i=0/1{
        xtreg $y  c.z2ega1 c.z2ega2 i.Male $c if Yeardum==`i' ,fe
        eststo m1_yr`i'
        
        xtreg $y  c.z2ega1##i.Male c.z2ega2##i.Male $c if Yeardum==`i',fe
        eststo m2_yr`i'
    }
      **

esttab m1_yr0 m2_yr0 m1_yr1 m2_yr1 using reg1_CountryFE.rtf, ///
	b(3) se r2 ar2 title ///
	("Table 1. Predicting Marital Centrality, ISSP 2002&2012") ///
	mtitle("Baseline 2002" "Interaction 2002" ///
	"Baseline 2012" "Interaction 2012") ///
	nonumber coeflabels( _cons "Constant")  ///
	nonumber ///
	modelwidth(8) star(* 0.05 ** 0.01 *** 0.001) ///
	varwidth(20) onecell label nogap nobase replace
 
 //Country-fixed effect models:
 //2002: -0.250, 2012: -0.287
 //Gender interaction: no difference between women and men
    
*================================================

*Mixed effect models

*1. Hierarchical data structures: individuals nested in countries - 2 levels
*Q: Extend a linear regression model to a multilevel setting
*Q: Show the relative importance of level 1 and level 2 covariates? [random intercept]
  //1. level 1 - individual gender attitude; 2. level 2 - macro level gender attitude
*Q: the effect of covariates might therefore vary across level-2 units [random slope]
    //do it with a one-by-one case
    //do it with all 2nd level together
*================================================

*-------------------------------------------------------

*OLS in each country:

*-------------------------------------------------------

use "issp_religion_sample3_mz.dta",clear

est clear 

 *Test it by country -  
 *specific slope in each country, it is actually a country-fixed effects: 
   sort country Yeardum
    statsby inter=_b[_cons] slope=_b[z2ega], ///
    by (country Yeardum) saving(ols_ega, replace): ///
    reg $y z2ega Male $c 

    sort country Yeardum
    capture drop _merge
    merge country Yeardum using ols_ega
    drop _merge
    
    sum slope inter
    
    twoway scatter slope inter, by(Yeardum,title(`t')) ///
    mlabel(country) xtitle(Intercept) ytitle(Slope)
    //this one somehow shows a U shape!
    *graph export Figure2.png, replace  
    
  /*  
    aaplot slope inter if Yeardum==0, ///
    mlabel(country) xtitle(Intercept) ytitle(Slope) title(2002)
    //slope=-0.211 in 2002 --> FE results is not a reweight of these country-specific slope?
  */
    
*Color markers BY Region groups  
  #delimit ;   
  twoway scatter slope inter if Region==0,
  msymbol(s) mcolor(red) mlabel(country) || 
  scatter slope inter if Region==1,
  msymbol(Oh) mcolor(blue) mlabel(country) || 
  scatter slope inter if Region==2,
  msymbol(Oh) mcolor(green) mlabel(country) || 
  scatter slope inter if Region==3,
  msymbol(Oh) mcolor(gs7) mlabel(country) || 
  scatter slope inter if Region==4,
  msymbol(T) mcolor(pink) mlabel(country) || 
  scatter slope inter if Region==5,
  msymbol(x) mcolor(gs1) mlabel(country)|| 
  lfit slope inter,
  by(Yeardum, note("Data source: ISSP 2002&2012", size(vsmall)) 
  graphregion(color(white)) bgcolor(white)) 
  legend(order(1 "Socio-democratic" 
        2 "Liberal" 
        3 "Conservative" 
        4 "Mediterranean" 
        5 "Eastern Europe" 
        6 "East Asia"))legend(row(1) size(small))   
  ytitle(Slope (Est. of EGA)) 
  xtitle(Intercept (Marital centrality)) 
  name(Slope_intercept, replace) 
   ;
  #delimit cr 
  graph export Figure2_SlopebyCountry.png, replace
  //Overall, esp in later years, the est of EGA is weaker in countries with higher level of marital centrality
  
**  
    *Test it by Country x Sex -  country-fixed effects:
 est clear
    sort country Yeardum Male 
    statsby inter=_b[_cons] slope=_b[z2ega], ///
    by (country Male Yeardum) saving(ols_ega, replace): ///
    reg $y z2ega $c 

    sort country Yeardum Male
    capture drop _merge
    merge n:1 country Yeardum Male using ols_ega
    drop _merge
    
    order country Yeardum Male inter slope
    sort country Yeardum Male
    twoway scatter slope inter, by(Yeardum Male) ///
    mlabel(country) xtitle(Intercept) ytitle(Slope)
    //this one somehow shows a U shape!
    graph export Figure2a.png, replace 
    
  #delimit ;   
  twoway scatter slope inter if Region==0,
  msymbol(s) mcolor(red) mlabel(country) || 
  scatter slope inter if Region==1,
  msymbol(Oh) mcolor(blue) mlabel(country) || 
  scatter slope inter if Region==2,
  msymbol(Oh) mcolor(green) mlabel(country) || 
  scatter slope inter if Region==3,
  msymbol(Oh) mcolor(gs7) mlabel(country) || 
  scatter slope inter if Region==4,
  msymbol(T) mcolor(pink) mlabel(country) || 
  scatter slope inter if Region==5,
  msymbol(x) mcolor(gs1) mlabel(country)|| 
  /*lfit slope inter*/,
  by(Yeardum Male, note("Data source: ISSP 2002&2012") 
  graphregion(color(white)) bgcolor(white)) 
  legend(order(1 "Socio-democratic" 
        2 "Liberal" 
        3 "Conservative" 
        4 "Mediterranean" 
        5 "Eastern Europe" 
        6 "East Asia")) legend(row(1) size(small)) 
  ytitle(Slope) 
  xtitle(Intercept)  /*aspect(0.8)*/
  name(Slope_intercept, replace) 
   ;
  #delimit cr 
  graph export Figure2a_SlopebyCountrySex.png, replace 

 
*-------------------------------------------------------

*Multilevel modelling:

*-------------------------------------------------------
use "issp_religion_sample3_mz.dta",clear

est clear

    xtmixed $y c.z2ega i.Male $c if Yeardum==1 || ///
    country: ,cov(unstructured) mle
    eststo randint    

    xtmixed $y c.z2ega i.Male $c if Yeardum==1 || ///
    country: z2ega , mle
    eststo randslope
    
     lrtest randslope randint
    
     xtmixed $y c.z2ega i.Male $c if Yeardum==1 || ///
    country: z2ega ,cov(unstructured) mle
    
    lrtest . randslope
    
    
  forval i=0/1{
   * xtsum $y $x $country, i(country)  
   
   *1.m0: only random intercepts
    xtmixed $y z2ega Male $c if Yeardum==`i' || ///
    country: ,cov(unstructured) mle
    //there is a change in est of gender attitude along with country
    eststo m0_y`i' //Model without macro-level factors
   //slope=-0.250 in 2002: this is the same as the FE output, which fix the slope to be the same across countries, I see!
   
   *2. m1: random intercepts and slopes
    xtmixed $y c.z2ega i.Male $c if Yeardum==`i' || ///
    country: z2ega ,cov(unstructured) mle
    //there is a change in est of gender attitude along with country
    eststo m1_y`i' //Model without macro-level factors
    //slope=-0.248 in 2002  
    
   *3. Interaction btw EHA and Male
    xtmixed $y c.z2ega##i.Male $c if Yeardum==`i' || ///
    country: z2ega ,cov(unstructured) mle
    //there is a change in est of gender attitude along with country
    eststo m2_y`i' //Model without macro-level factors
  }
  **
    
   esttab /*m0_y0*/ m1_y0 m2_y0 /*m0_y1*/ m1_y1 m2_y1 using reg1_testgender.rtf, ///
	b(3) se transform(ln*: exp(@) exp(@) at*: tanh(@) (1-tanh(@)^2)) ///
    title ("Table x. Predicting marital centrality and test gender differences, ISSP 2002&2012") ///
	mtitle("Baseline 2002" "EGA x Gender 2002" ///
	"Baseline 2012" "EGA x Gender 2012") ///
	nonumber coeflabels( _cons "Intercept" lns1_1_1_cons "sd(EGA)" ///
    lns1_1_2_cons "sd(Intercept)" lnsig_e_cons "sd(Residual)")  ///
	nonumber ///
	modelwidth(8) star(* 0.05 ** 0.01 *** 0.001) ///
	varwidth(20) onecell label nogap nobase replace 
  
  
       
 *----------------2. Test it with Macro-level factors in:---------------------
    use "issp_religion_sample3_mz.dta",clear
 
    global country z2country_gii  country_tfr /*country_evermarr  country_religion */
   
    gen z2egasq=z2ega*z2ega //no non-linear relationship was found
    
    
 *1. Full sample - actually 3 levels - individuals nested in countries and in two years
 *But I used dummy to represent 3rd level
    xtmixed $y c.z2ega c.country_gii  c.country_tfr /*c.country_evermarr  c.country_religion*/ ///
        i.Male i.Yeardum $c ,|| ///
    country: z2ega,cov(unstructured) mle
    eststo m1_yFull //pool together ONLY TFR matters
    
    xtmixed $y c.z2ega##i.Yeardum ///
     c.country_gii c.country_tfr /*country_evermarr country_religion*/  ///
     Male $c, || ///
    country: z2ega,cov(unstructured) mle
    eststo m2_yFull
    //2012, weaker MC; 2012, stronger negative EGA
    
    xtmixed $y c.z2ega##c.c.country_gii c.z2ega##c.country_tfr /*country_evermarr country_religion*/ ///
    Male c.z2ega##i.Yeardum $c, || ///
    country: z2ega,cov(unstructured) mle
    eststo m3_yFull
    //higher gii, higher MC, higher GII, weaker negative EGA！
    //higher TFR, higher MC, higher TFR, more negative EGA!
    
     xtmixed $y c.z2ega##c.country_gii c.z2ega##c.country_tfr ///
        /*c.z2ega##c.country_evermarr c.z2ega##c.country_religion*/ ///
        c.z2ega##i.Yeardum Male $c, || ///
    country: z2ega,cov(unstructured) mle
    eststo m4_yFull
    
    esttab m1_yFull m2_yFull m3_yFull m4_yFull using reg2a_Full.rtf, ///
	b(3) se transform(ln*: exp(@) exp(@) at*: tanh(@) (1-tanh(@)^2)) ///
    title ("Table x. Predicting Marital Centrality, ISSP 2002&2012") ///
	mtitle("Baseline" "EGA*Country" ///
	"EGA*GII & EGA*TFR" "Full Interaction") ///
	nonumber coeflabels( _cons "Intercept" lns1_1_1_cons "sd(EGA)" ///
    lns1_1_2_cons "sd(Intercept)" lnsig_e_cons "sd(Residual)")  ///
	nonumber ///
	modelwidth(8) star(* 0.05 ** 0.01 *** 0.001) ///
	varwidth(20) onecell label nogap replace
    

   *2. By Year - 2 level model: individuals nested in countries
  forval i=0/1{
    xtmixed $y c.z2ega c.country_gii c.country_tfr /*c.country_evermarr  c.country_religion*/ ///
        i.Male $c if Yeardum==`i' , /*vce(cluster country)*/ || ///
    country: c.z2ega,cov(unstructured) mle
    eststo m1_y`i'
  }
  **higher GII, higher MC, every married, tfr, and religious - no effect
  
  /*
   forval i=0/1{
    xtmixed $y c.z2ega##i.Male  $country $c if Yeardum==`i' , vce(cluster country) || ///
    country: z2ega,cov(unstructured) mle
  }
  //Confirmed, no gender difference in EGA effect
  */
  
  forval i=0/1{  
    xtmixed $y c.z2ega##c.country_gii c.country_tfr ///
        i.Male $c if Yeardum==`i' , || ///
    country: c.z2ega,cov(unstructured) mle
    eststo m2_y`i' 
    
    xtmixed $y c.z2ega##c.country_tfr c.country_gii  ///
        i.Male $c if Yeardum==`i' , || ///
    country: c.z2ega,cov(unstructured) mle
    eststo m3_y`i' 
    
    xtmixed $y c.z2ega##c.country_gii c.z2ega##c.country_tfr  ///
        i.Male $c if Yeardum==`i' , || ///
    country: c.z2ega,cov(unstructured) mle
    eststo m4_y`i'
    /*
    xtmixed $y c.z2ega##c.country_gii c.z2ega##c.country_tfr ///
        c.z2ega##c.country_evermarr c.z2ega##c.country_religion  ///
        i.Male $c if Yeardum==`i' , || ///
    country: c.z2ega,cov(unstructured) mle
    eststo m5_y`i'
    */
  }
  **
  
    esttab m1_y0 m2_y0 m3_y0 m4_y0 /*m5_y0*/ using reg2_testCountry.rtf, ///
	b(3) se transform(ln*: exp(@) exp(@) at*: tanh(@) (1-tanh(@)^2)) ///
    title ("Table x. Predicting marital centrality with country-level indicators, ISSP 2002&2012") ///
	mtitle("Baseline 2002" "EGA x GII 2002" "EGA x TFR 2002" ///
	"EGA x GII & EGA x TFR 2002" "Full Interaction 2002") ///
	nonumber coeflabels( _cons "Intercept" lns1_1_1_cons "sd(EGA)" ///
    lns1_1_2_cons "sd(Intercept)" lnsig_e_cons "sd(Residual)")  ///
	nonumber ///
	modelwidth(7) star(* 0.05 ** 0.01 *** 0.001) ///
	varwidth(16) onecell label nogap nobase replace
    
    esttab m1_y1 m2_y1 m3_y1 m4_y1 /*m5_y1*/ using reg2_testCountry.rtf, ///
	b(3) se transform(ln*: exp(@) exp(@) at*: tanh(@) (1-tanh(@)^2)) ///
	mtitle("Baseline 2012" "EGA x GII 2012" "EGA x TFR 2012" ///
	"EGA x GII & EGA x TFR 2012" "Full Interaction 2012") ///
	nonumber coeflabels( _cons "Intercept" lns1_1_1_cons "sd(EGA)" ///
    lns1_1_2_cons "sd(Intercept)" lnsig_e_cons "sd(Residual)")  ///
	nonumber ///
	modelwidth(7) star(* 0.05 ** 0.01 *** 0.001) ///
	varwidth(16) onecell label nogap nobase app
    
    
    
 *------------------------------------------------- 
 
 *Sensitivity analysis - whether one case/country is particularly influential:
 
 *-------------------------------------------------
 use "issp_religion_sample3_mz.dta",clear
 est clear
 gen int1=z2ega*country_gii
 gen int2=z2ega*country_tfr
    
  forval i=0/1 {
    xtmixed $y z2ega country_gii country_tfr /*country_evermarr  country_religion*/ ///
        int1 int2 ///
        Male $c if Yeardum==`i' || ///
    country: z2ega,cov(unstructured) mle
    //which observations have the greatest impact on the determination of the coefficient on EGA
    
    mltcooksd, fixed random graph  
    graph export Figure3a_mltcooksd_Year`i'.png, replace
    //EGA: SE outlier -2012 very low EGA, 
    //country_gii: HU outlier, TW, SK, RU also outlier but within cutoff values
    //interaction: US outlier
  }
  **
  
  use "issp_religion_sample3_mz.dta",clear
    est clear
    gen int1=z2ega*country_gii
    gen int2=z2ega*country_tfr
    la var int1 "EGA*GII"
    la var int2 "EGA*TFR"
    
    keep if Yeardum==0
    drop if C_ALPHAN=="SK"|C_ALPHAN=="DK" |C_ALPHAN=="US"|C_ALPHAN=="RU"
    xtmixed $y z2ega country_gii country_tfr /*country_evermarr  country_religion*/ ///
        int1 int2 ///
        Male $c  || ///
    country: z2ega,cov(unstructured) mle
    eststo m2r_y0   
    
  use "issp_religion_sample3_mz.dta",clear
    gen int1=z2ega*country_gii
    gen int2=z2ega*country_tfr
    la var int1 "Std. EGA * Gender Inequality Index"
    la var int2 "Std. EGA * Total Fertility Rate"
    
    keep if Yeardum==1
    drop if C_ALPHAN=="CZ"|C_ALPHAN=="RU"|C_ALPHAN=="HU"|C_ALPHAN=="TW"
   
    xtmixed $y z2ega country_gii country_tfr /*country_evermarr  country_religion*/ ///
        int1 int2 ///
        Male $c || ///
    country: z2ega,cov(unstructured) mle
    eststo m2r_y1
  
  **
  esttab m2r_y0 m2r_y1 using reg3_outlier.rtf, ///
	b(3) se transform(ln*: exp(@) exp(@) at*: tanh(@) (1-tanh(@)^2)) ///
    title ("Table x. Predicting Marital Centrality (influencing cases excluded), ISSP 2002&2012") ///
	mtitle("Interaction 2002" "Interaction 2012") ///
	nonumber coeflabels( _cons "Intercept" lns1_1_1_cons "sd(EGA)" ///
    lns1_1_2_cons "sd(Intercept)" lnsig_e_cons "sd(Residual)")  ///
	nonumber ///
	modelwidth(8) star(* 0.05 ** 0.01 *** 0.001) ///
	varwidth(35) onecell label nogap nobase replace
    
 
************************************************
*
*Sensitivity - check Nonlinear relationship

************************************************
    
  use "issp_religion_sample3_mz.dta",clear
    est clear
    gen int1=z2ega*country_gii
    gen int2=z2ega*country_tfr
    la var int1 "EGA*GII"
    la var int2 "EGA*TFR"
    
    gen giisq=country_gii*country_gii
    gen tfrsq=country_tfr*country_tfr
    la var tfrsq "TFR*TFR"
    
    gen egasq=z2ega*z2ega
    
    gen int1sq=z2ega*giisq
    la var int1sq "Std. EGA*GII*GII"  
    
    gen int2sq=z2ega*tfrsq
    la var int2sq "Std. EGA*TFR*TFR"  
    
    
    keep if Yeardum==1
    
  *1. Country-level indicator non-linear
   xtmixed $y z2ega egasq country_gii country_tfr country_evermarr  country_religion ///
        giisq tfrsq ///
        Male $c  || ///
   country: z2ega,cov(unstructured) mle
   //2002: ega - sig negative (-0.247); egasq - not sig and close to zero; tfr- is nonlinear: open up,  di 11.5/(-2*3.51)=-1.6; above 1.6, positive, below 1.6 - negative
   //2012: ega - sig negative; egasq - not sig and close to zero; non of the two is curvelinear 
   
    
 *2. EGA varies nonlinearly with GII and TFR
   xtmixed $y z2ega country_gii country_tfr country_evermarr  country_religion ///
        giisq tfrsq ///
        int1 int1sq int2 int2sq ///
        Male $c  || ///
   country: z2ega,cov(unstructured) mle
   //2002: int1 - not sig, int1sq- not sig; int2 - not sig, int2sq - not sig
   //2012: int1 - not sig, int1sq- not sig; int2 - not sig, int2sq - not sig
   
   
 *3. Final model - Not dropping Years:
   forval i=0/1{
   sum country_tfr  if Yeardum==`i'
   } //2002-1.18 to 2.02; 2012 - 1.27 to 2; Mean increases a bit, and SD decrease a bit
   
   xtmixed $y z2ega country_gii country_tfr country_evermarr  country_religion ///
        tfrsq ///
        Male $c  if Yeardum==0 || ///
   country: z2ega,cov(unstructured) mle
   eststo regs_y0
   di -12.2/(-2*3.74) //open up, change signs at 1.63
   
   xtmixed $y z2ega country_gii country_tfr country_evermarr  country_religion ///
        Male $c  if Yeardum==1 || ///
   country: z2ega,cov(unstructured) mle
   eststo regs_y1
   
    esttab regs_y0 regs_y1 using reg4_nonlinear.rtf, ///
	b(3) se transform(ln*: exp(@) exp(@) at*: tanh(@) (1-tanh(@)^2)) ///
    title ("Table Ax. Predicting Marital Centrality (influencing cases excluded), ISSP 2002&2012") ///
	mtitle("2002" "2012") ///
	nonumber coeflabels( _cons "Intercept" lns1_1_1_cons "sd(EGA)" ///
    lns1_1_2_cons "sd(Intercept)" lnsig_e_cons "sd(Residual)")  ///
	nonumber ///
	modelwidth(8) star(* 0.05 ** 0.01 *** 0.001) ///
	varwidth(20) onecell label nogap replace    
    
  corr z2ega country_gii country_tfr country_evermarr  country_religion
   
   
   