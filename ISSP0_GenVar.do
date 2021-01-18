cd "C:\Users\Muzhi\Dropbox\MZ and KK\ISSP original data" //change directory to your own one


*Variable generation - 2002 and 2012 data

use "ZA3880_v1-1-0.dta",clear //change data name to "ZA3880_v1-1-0_sample.dta" when using the sample data
    rename COUNTRY Country
    *keep v3 C_ALPHAN v288 v289 v290 v358 v202
    rename v3 pid
    order C_ALPHAN v204 v205
    
 *Control variables: religion, urban type, marital status, sex, education, employment status, number of children
    ta v288,m
    gen religion=[v288~=990]
    replace religion=. if v288>990
    la var religion "Has religion"
    ta v288 religion,m
    ta v289 religion

    recode v358 4/6=4 .a=. .n=., gen(urban)
    la var urban "Urban type"
    la val urban V358

    gen Age=v201
    recode v200 1=0 2=1, gen(Female)

    recode v202 2=2 3/4=2 5=0 1=1, gen(Married)
    la def Married 0 "Single" 1 "Married" 2 "Sep/Div/Widowed"
    la val Married Married

    capture drop EduInYears_1
    gen EduInYears_1=v204
    replace EduInYears_1=Age-6 if  v204==93|v204==95|v204==96
    replace EduInYears_1=. if v204==98|v204==99|v204==94
    replace EduInYears_1=1 if v204==97
    ta EduInYears_1,m
    replace EduInYears_1=. if EduInYears_1>22
    
    rename v205 Edulevel

    recode v239 1=0 2/3=1 4/10=2, gen(Working)
    la def working 0 "Working full time" 1 "Part-time working" 2 "Not working"
    la val Working working
    ta v239 Working ,m
    
    recode v239 1=0 2/3=0 4/10=1, gen(workdum)
    la def workdum 0 "Working" 1 "Not working"
    la val workdum workdum

    capture drop nchild
    gen nchild=v66
    replace nchild=. if nchild>10
    replace nchild=0 if v252==1&nchild==.|v252==5&nchild==.|v252==9&nchild==.|v252==11&nchild==.|v252==13&nchild==.|v252==15&nchild==.|v252==17&nchild==.|v252==19&nchild==.|v252==21&nchild==.|v252==23&nchild==.|v252==25&nchild==.|v252==27&nchild==.|v252==95&nchild==.
    replace nchild=1 if v252==2&nchild==.|v252==6&nchild==.
    replace nchild=2 if v252==3&nchild==.|v252==7&nchild==.
    replace nchild=3 if v252==4&nchild==.|v252==8&nchild==.
    replace nchild=3 if nchild>3&nchild<11
    la def nchild 0 "No child" 1 "1 child" 2 "2 children"  3 ">=3 children"
    la val nchild nchild
    ta nchild,m 
    
    ta v252 nchild,m 
    ta v69 nchild,m
    replace nchild=v69 if v252==10&nchild==.|v252==12&nchild==.|v252==14&nchild==.|v252==16&nchild==.|v252==20&nchild==.|v252==22&nchild==.|v252==24&nchild==.|v252==26&nchild==.|v252==28&nchild==.
    replace nchild=0 if nchild==.a|nchild==.n
    replace nchild=3 if nchild>3&nchild<12
    
    order Age v252 v66 v69 nchild
    
    ta C_ALPHAN nchild,m //SI missing a lot still
    
  *Key variables - marital and gender attitudes
        /*(1) ‘Q1a Working mom: warm relationship with children as a not working mom’ (2) ‘Q1b Working mom: Preschool child is likely to suffer’ (3) ‘Q1c Working woman: Family life suffers when the woman has full-time job’ (4) ‘Q1d Working woman: What women really want is home and kids’ (5) ‘Q1e Working woman: Being a housewife is as fulfilling as working for pay’ (6) ‘Q2a Both should contribute to household income’ (7) Q2b ‘Men’s job earn money, women’s job look after the home.’    
        */
    ta v4 //1 - strongly agree and 5 strongly disagree
    *Higher score more liberal
    *help revrs
    
    revrs v4 v9 v10 v12 v13
    ta v4 revv4    
    ta v10 revv10 if Country==25 //Spain      
    
    /*(1) ‘Married people are generally happier than unmarried people’; (2) ‘People who want children ought to get married’; (3) ‘It is all right for a couple to live together without intending to get married’; (4) ‘Divorce is usually the best solution when a couple cannot seem to work out their marriage problems’; and (5) ‘One parent can bring up a child as well as two parents together.’.*/
    *Higher score showing more supporitve of marriage
    ta v18
    revrs v18 v19 v20   
   
    rename v361 weight
    
    keep C_ALPHAN pid religion urban ///
        Female Age Married EduInYears_1 Edulevel Working workdum ///
        nchild  ///
        revv4 v5 v6 v7 v8 revv9 revv10 v11 revv12 revv13 ///
        revv18 revv19 revv20 v21 v22 v23 v24 ///
        weight
        
    rename (revv4 v5 v6 v7 v8 revv10 v11 ) (ga1 ga2 ga3 ga4 ga5 ga6 ga7)
    rename (revv18 revv20 v21 v22 v24) (ma1 ma2 ma3 ma4 ma5)
        
    gen Year=2002
    order C_ALPHAN Year
    
    la var Female "Female"
    la var Married "Marital status"
    la var EduInYears_1 "Years of education"
    la var Working "Working status"
    la var nchild "Number of children"
    
    /*           0 No formal qualification
           1 Lowest formal qualification
           2 Above lowest qualification
           3 Higher secondary completed
           4 Above higher sec level,below full uni
           5 University degree completed
          .n Na, Dk
*/
    
    desc
    order C_ALPHAN Year ga* ma*
save "religion2002.dta", replace


*=================================2012=======================================*
use "ZA5900_v4-0-0.dta",clear //change data name to "ZA5900_v4-0-0_sample.dta" when using the sample data
    rename V4 Country
    rename CASEID pid
    
    rename WEIGHT weight
    rename DEGREE Edulevel
    
    replace Edulevel=5 if Edulevel==6
    replace Edulevel=. if Edulevel==9

 *Control variables: religion, urban type, marital status, sex, education, employment status, number of children
    recode RELIGGRP 1/10=1 97/99=., gen(religion)
    recode URBRURAL 4/7=4 9=., gen(urban)
    ta MARITAL
    recode MARITAL 1/2=1 3/5=2 6=0 7/9=., gen(Married)
    la def Married 0 "Single" 1 "Married" 2 "Sep/Div/Widowed"
    la val Married Married 
    ta MARITAL Married
    
    recode SEX 1=0 2=1 9=., gen(Female)
    
    rename AGE Age
    replace Age=. if Age>101
    capture drop EduInYears_1
    gen EduInYears_1=EDUCYRS
    replace EduInYears_1=Age-6 if  EDUCYRS==95|EDUCYRS==96
    replace EduInYears_1=. if EDUCYRS==98|EDUCYRS==99
    ta EduInYears_1,m
    replace EduInYears_1=. if EduInYears_1>22
    
    la list WORK 
    
    recode WRKHRS 1/36=1 37/96=0 98/99=. 97=1, gen(Working)
    replace Working=2 if WORK==3|WORK==2
    replace Working=. if WORK==9
    la def working 0 "Working full time" 1 "Part-time working" 2 "Not working"
    la val Working working
    ta Working WORK,m
    ta WRKHRS Working,m
    
    recode WORK 1=0 2/3=1 9=., gen(workdum)
    la def workdum 0 "Working" 1 "Not working"
    la val workdum workdum
    ta Working workdum,m
    
   la list HHCHILDR HHTODD
   
   recode HHCHILDR 96=0 97/99=., gen(hhchildr)
   recode HHTODD 96=0 97/99=., gen(hhtodd)
   gen childabs=hhchildr+hhtodd
   recode childabs 0=0 1=1 2=2 3/23=3, gen(nchild)
   la def nchild 0 "No child" 1 "1 child" 2 "2 children"  3 ">=3 children"
   la val nchild nchild
   ta nchild,m 
   
   ta C_ALPHAN nchild,m
    
 *Key variables - marital and gender attitudes
        /*(1) ‘Q1a Working mom: warm relationship with children as a not working mom’ (2) ‘Q1b Working mom: Preschool child is likely to suffer’ (3) ‘Q1c Working woman: Family life suffers when the woman has full-time job’ (4) ‘Q1d Working woman: What women really want is home and kids’ (5) ‘Q1e Working woman: Being a housewife is as fulfilling as working for pay’ (6) ‘Q2a Both should contribute to household income’ (7) Q2b ‘Men’s job earn money, women’s job look after the home.’    
        */   
    ta V5 //5,6,7,8, 9, 10, 11
    forval i=5/11{
    capture drop newV`i'
    gen newV`i'=V`i'
    replace newV`i'=. if V`i'==9
    replace newV`i'=3 if V`i'==8 //dont know option
    replace newV`i'=ES_V`i' if ES_V`i'>0&ES_V`i'<3
    replace newV`i'=3 if ES_V`i'==8
    replace newV`i'=4 if ES_V`i'==3
    replace newV`i'=5 if ES_V`i'==4
    replace newV`i'=. if ES_V`i'==9
    ta newV`i' ES_V`i',m
    }
    **
    
    revrs newV5 newV10
    
    /*(1) ‘Married people are generally happier than unmarried people’; (2) ‘People who want children ought to get married’; (3) ‘It is all right for a couple to live together without intending to get married’; (4) ‘Divorce is usually the best solution when a couple cannot seem to work out their marriage problems’; and (5) ‘One parent can bring up a child as well as two parents together.’.*/
    *Higher score showing more supporitve of marriage
    ta V14 //1 - strongly agree and 5 strongly disagree
    *14，15，16，17，18
    *help revrs
    forval i=14/18{
    capture drop newV`i'
    gen newV`i'=V`i'
    replace newV`i'=3 if V`i'==8 //CA KR and NL - exceptions
    replace newV`i'=. if V`i'==9
    replace newV`i'=ES_V`i' if ES_V`i'<3&ES_V`i'>0
    replace newV`i'=3 if ES_V`i'==8 //ES - exception dont know option
    replace newV`i'=4 if ES_V`i'==3
    replace newV`i'=5 if ES_V`i'==4
    replace newV`i'=. if ES_V`i'==9
    
    ta newV14 ES_V14,m
    }
    **
    revrs newV14 newV15     
   
    keep C_ALPHAN pid religion urban ///
        Female Age Married EduInYears_1 Working workdum ///
        nchild Edulevel ///
        revnewV5 revnewV10 newV6 newV7 newV8 newV9 newV11 ///
        revnewV14 revnewV15 newV16 newV17 newV18  ///
        weight
        
    rename (revnewV5 newV6 newV7 newV8 newV9 revnewV10 newV11) ///
    (ga1 ga2 ga3 ga4 ga5 ga6 ga7)
    rename (revnewV14 revnewV15 newV18 newV16 newV17 ) ///
        (ma1 ma2 ma3 ma4 ma5)    
    
    gen Year=2012
    order C_ALPHAN Year
    
    la var Female "Female"
    la var Married "Marital status"
    la var EduInYears_1 "Years of education"
    la var Working "Working status"
    la var nchild "Number of children"
    
    desc   
save "religion2012.dta", replace

use "religion2002.dta",clear
append using "religion2012.dta"
sort C_ALPHAN Year
order pid C_ALPHAN Year
format pid %20.0f
save "C:\Users\Muzhi\OneDrive - Nexus365\GenTime_MYK\Team working papers\ISSP_kamila\stata_mz/issp_religion_mz", replace

erase "religion2002.dta"
erase "religion2012.dta"