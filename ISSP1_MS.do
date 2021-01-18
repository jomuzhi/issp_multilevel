cd "C:\Users\Muzhi\OneDrive - Nexus365\GenTime_MYK\Team working papers\ISSP_kamila\stata_mz"

use "issp_religion_mz.dta", clear
    drop if Year==.
    ta C_ALPHAN Year
    
    gen c_dk=.
    replace c_dk=1 if Year==2012&C_ALPHAN=="CA"
    replace c_dk=1 if Year==2012&C_ALPHAN=="KR"
    replace c_dk=1 if Year==2012&C_ALPHAN=="ES"
    replace c_dk=1 if Year==2012&C_ALPHAN=="NL"
    //CA KR and NL and ES - dont know for attitude questions
    la var c_dk "Country with dontknow as an option in 2012"
    
   *Countries in High Income group in Both Years:
    gen Region=.
    replace Region=0 if C_ALPHAN=="DK"|C_ALPHAN=="NO"|C_ALPHAN=="SE"|C_ALPHAN=="FI"
    //Socio-democratic
    replace Region=1 if C_ALPHAN=="AU"|C_ALPHAN=="GB-GBN"|C_ALPHAN=="US"|C_ALPHAN=="CA"
    replace Region=2 if C_ALPHAN=="FR"|C_ALPHAN=="DE"|C_ALPHAN=="CH"|C_ALPHAN=="DE-E"|C_ALPHAN=="DE-W"|C_ALPHAN=="BE"|C_ALPHAN=="BE-FLA"|C_ALPHAN=="NL"|C_ALPHAN=="AT"
    replace Region=3 if C_ALPHAN=="PT"|C_ALPHAN=="ES"
    replace Region=4 if C_ALPHAN=="CZ"|C_ALPHAN=="HU"|C_ALPHAN=="LV"||C_ALPHAN=="PL"|C_ALPHAN=="RU"|C_ALPHAN=="SK"|C_ALPHAN=="SI"|C_ALPHAN=="LT"
    replace Region=5 if C_ALPHAN=="JP"|C_ALPHAN=="TW"|C_ALPHAN=="KR"
    la def Region 0 "Socio-democratic" 1 "Liberal" 2 "Conservative" ///
        3 "Mediterranean" 4 "Eastern European" 5 "East Asian"
        la val Region Region
    ta C_ALPHAN Region,m
    ta Year Region,m
    ta Region Year
    drop if Region==.
    
    replace C_ALPHAN="BE" if C_ALPHAN=="BE-FLA"
    replace C_ALPHAN="DE" if C_ALPHAN=="DE-E"|C_ALPHAN=="DE-W"
    replace C_ALPHAN="GB" if C_ALPHAN=="GB-GBN"  
    ta C_ALPHAN
    ta Region 
    ta Region [aw=weight]   
    
    gen Yeardum=0 if Year==2002
    replace Yeardum=1 if Year==2012    
    
************************************************************

***** Sample Selection

************************************************************
    gen marcent=ma1+ma2+ma3+ma4+ma5
    gen ega=ga1+ga2+ga3+ga4+ga5+ga6+ga7
    
    table Region , c(mean ega)
    table Region [aw=weight] , c(mean ega)
    table Region , c(mean marcent)

    gen no_miss = !missing(Year, ///
        Married, nchild, ///
        Working, Edulevel,  ///
        Age, Female, ///
        ga1, ga2, ga3, ga4, ga5, ga7, ///
        ma1, ma2, ma4, ///
        religion)
    *	HWshare, PaidWork_SP, QuantilesHI,    

    global mylist marcent ega Year ///
        Married nchild  Age ///
        Working Edulevel religion       
 
     sort Region Year 
     order Region Year $mylist
   
   *replace nchild=0 if nchild==.
    
*Sample
    count
    sum Age
    keep if Age>19&Age<60
    count //70% of the original age 15 to 102
    drop if C_ALPHAN=="CA"
          
    ta Region Married , row nof
    
    foreach v in $mylist {
    	count if `v'==.
    }
    **only the two attitude variables missing the most
    *urban missing a lot - but little effect so drop
   
     foreach v in $mylist {
        ta C_ALPHAN if `v'==.
    }
    
     ta C_ALPHAN Year 
    * drop if C_ALPHAN=="AT" //no years of education and missing too many
    
    * drop if C_ALPHAN=="SI"&Year==2002
     
*Same country in both years
    ta C_ALPHAN Year
    drop if C_ALPHAN=="KR"
    drop if C_ALPHAN=="LT"
    
    
    ta Year    
    drop if no_miss==0
    ta Year //83% kept
    
    ta C_ALPHAN Region    

    
*Calculate Region-Year weight so each region occupy similar percentage
       bysort Region Year: gen regionn=_N 
       order regionn Region Year C_ALPHAN
       gen regionweight=1000/regionn
       
       ta Region if Year==2002 [aw=regionweight]
       ta Region if Year==2012 [aw=regionweight]
       drop regionn

       
  save "issp_religion_sample_mz.dta", replace  
  
  
    
   
