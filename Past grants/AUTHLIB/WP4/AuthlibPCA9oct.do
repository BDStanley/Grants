


/////////////////////////////////////////////////////////////////////

////// Authlib PCA Oct 9, v2.1

* load data


* POPULISM 2.1

* Reverse scales
tab P11_04
gen P11_04r=(P11_04-6)*(-1) if P11_04!=99
la def P11_04r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_04r P11_04r
la var P11_04r "Politics is ultimately a struggle between good and evil"
tab P11_04r P11_04 

tab P11_05
gen P11_05r=(P11_05-6)*(-1) if P11_05!=99
la def P11_05r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_05r P11_05r
la var P11_05r "The will of the majority should always prevail, even over the rights of minorities"
tab P11_05r P11_05 

tab P11_06
gen P11_06r=(P11_06-6)*(-1) if P11_06!=99
la def P11_06r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_06r P11_06r
la var P11_06r "[Country] is pretty much run by a few big interests looking out for themselves"
tab P11_06r P11_06 

tab P11_07
gen P11_07r=(P11_07-6)*(-1) if P11_07!=99
la def P11_07r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_07r P11_07r
la var P11_07r "Politicians constantly over-complicate simple matters instead of effectively solving them"
tab P11_07r P11_07 


* Standarization
egen std_P2_02 = std(P2_02)
egen std_P2_03 = std(P2_03)
egen std_P11_04 = std(P11_04r)
egen std_P11_05 = std(P11_05r)
egen std_P11_06 = std(P11_06r)
egen std_P11_07 = std(P11_07r)

* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99, pcf
rotate
predict f1_POP_antielite_simple f2_POP_manichean_majoritarian
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight]  if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99


** BY COUNTRIES

tab country
/*
1.	IT	1,008	14.23	14.23
2.	CZ	1,010	14.26	28.49
3.	AT	1,005	14.19	42.69
4.	FR	1,010	14.26	56.95
5.	GB	1,014	14.32	71.27
6.	HU	1,029	14.53	85.79
7.	PL	1,006	14.21	100.00
*/
* Italy
* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==1, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==1, pcf
rotate
predict f1_POP_a1_it f2_POP_a1_it
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight]  if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==1, generate(pop2stds1it1)

*Czechia
* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==2, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==2, pcf
rotate
predict f1_POP_a1_cz 
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==2, generate(pop2stds1cz1)

* Austria
* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==3, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==3, pcf
rotate
predict f1_POP_a1_at 
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==3, generate(pop2stds1at1)

* France
* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==4, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==4, pcf
rotate
predict f1_POP_a1_fr f2_POP_a1_fr
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==4, generate(pop2stds1fr1)

* GB
* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==5, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==5, pcf
rotate
predict f1_POP_a1_gb 
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==5, generate(pop2stds1gb1)

* HU
* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==6, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==6, pcf
rotate
predict f1_POP_a2_hu f2_POP_a2_hu
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==6, generate(pop2stds1hua1)


* PL
* Correlations
pwcorr /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==7, st(0.05)

* PCF
factor /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==7, pcf
rotate
predict f1_POP_a2_pl f2_POP_a2_pl
alphawgt  /*std_P2_02 std_P2_03*/ std_P11_04 std_P11_05 std_P11_06 std_P11_07 [aweight=weight] if /*P2_02!=99 & P2_03!=99 &*/ P11_04!=99 & P11_05!=99 & P11_06!=99 & P11_07!=99 & country==7, generate(pop2stds1pl1)





* ILLIBERALISM 2.1

* Reverse scales
tab P2_04
gen P2_04r=(P2_04-10)*(-1) if P2_04!=99
la def P2_04r 0"Extremely important for democracy" 10"Not at all important for democracy", replace
la val P2_04r P2_04r
la var P2_04r "Media freedom not important for democracy"
tab P2_04r P2_04 

tab P2_06
gen P2_06r=(P2_06-10)*(-1) if P2_06!=99
la def P2_06r 0"Extremely important for democracy" 10"Not at all important for democracy", replace
la val P2_06r P2_06r
la var P2_06r "Courts treat equally not important for democracy"
tab P2_06r P2_06 

tab P4_03
gen P4_03r=(P4_03-6)*(-1) if P4_03!=99
la def P4_03r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_03r P4_03r
la var P4_03r "Women should be prepared to cut down on paid work for the sake of the family"
tab P4_03r P4_03 

tab P4_04
gen P4_04r=(P4_04-6)*(-1) if P4_04!=99
la def P4_04r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_04r P4_04r
la var P4_04r "Same‐sex marriages should be prohibited by law"
tab P4_04r P4_04 

tab P4_06
gen P4_06r=(P4_06-6)*(-1) if P4_06!=99
la def P4_06r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_06r P4_06r
la var P4_06r "Teaching teenagers about tolerance towards transgender and homosexual minorities in schools can cause harm"
tab P4_06r P4_06 

tab P4_07
gen P4_07r=(P4_07-6)*(-1) if P4_07!=99
la def P4_07r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_07r P4_07r
la var P4_07r "These days you cannot even say what you think about immigrants, women or gays without being called prejudiced"
tab P4_07r P4_07 

tab P4_08
gen P4_08r=(P4_08-6)*(-1) if P4_08!=99
la def P4_08r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_08r P4_08r
la var P4_08r "Those who don't believe in God are not suitable for governing this country"
tab P4_08r P4_08 

tab P11_08
gen P11_08r=(P11_08-6)*(-1) if P11_08!=99
la def P11_08r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_08r P11_08r
la var P11_08r "The government should be able to bend the law in order to solve pressing social and political problems"
tab P11_08r P11_08 

tab P12_01
gen P12_01r=(P12_01-6)*(-1) if P12_01!=99
la def P12_01r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P12_01r P12_01r
la var P12_01r "What our society needs is protection against international influences, not further opening up"
tab P12_01r P12_01 

tab P12_02
gen P12_02r=(P12_02-6)*(-1) if P12_02!=99
la def P12_02r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P12_02r P12_02r
la var P12_02r "Leaders should always follow the interests of their country, even if this means going against international rules"
tab P12_02r P12_02 

tab P12_03
gen P12_03r=(P12_03-6)*(-1) if P12_03!=99
la def P12_03r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P12_03r P12_03r
la var P12_03r "It is impossible for an organisation to serve the interests of our nation if most of its funding comes from foreign sources"
tab P12_03r P12_03 

tab P12_04
gen P12_04r=(P12_04-6)*(-1) if P12_04!=99
la def P12_04r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P12_04r P12_04r
la var P12_04r "Those who claim that there are powerful organisations in the world which conspire against [Country] have a good point"
tab P12_04r P12_04 

tab P12_05
gen P12_05r=(P12_05-6)*(-1) if P12_05!=99
la def P12_05r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P12_05r P12_05r
la var P12_05r "These days the leading contribution white people have made to human culture is being downplayed"
tab P12_05r P12_05 

tab P6 

gen P6r=(P6-10)*(-1) if P6!=99
la def P6r 0"Good for the economy" 10"Bad for the economy", replace
la val P6r P6r
la var P6r "Are immigrants good or bad for the economy"
tab P6r P6 


tab P7
gen P7r=(P7-10)*(-1) if P7!=99
la def P7r 0"Good for culture" 10"Bad for culture", replace
la val P7r P7r
la var P7r "Are immigrants good or bad for culture"
tab P7r P7 


egen std_P6 = std(P6r)
egen std_P7 = std(P7r)



egen std_P2_04 = std(P2_04r)
egen std_P2_06 = std(P2_06r)
egen std_P4_03 = std(P4_03r)
egen std_P4_04 = std(P4_04r)
egen std_P4_05 = std(P4_05) if P4_05!=99
egen std_P4_06 = std(P4_06r)
egen std_P4_07 = std(P4_07r)
egen std_P4_08 = std(P4_08r)
egen std_P12_01 = std(P12_01r)
egen std_P12_02 = std(P12_02r)
egen std_P12_03 = std(P12_03r)
egen std_P12_04 = std(P12_04r)
egen std_P12_05 = std(P12_05r)


*std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 

pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight], st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight], pcf
rotate

predict f1_ILL_Parochialism f2_ILL_Trad_Cons_Intoler f3_ILL_Nativism

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight]


* ITALY

pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==1, st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==1, pcf
rotate

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==1


* CZECHIA

pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==2, st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==2, pcf
rotate

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==2

* AUSTRIA

pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==3, st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==3, pcf
rotate

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==3

* FRANCE

pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==4, st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==4, pcf
rotate

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==4

* GB

pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==5, st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==5, pcf
rotate

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==5

* HU
pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==6, st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==6, pcf
rotate

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==6

* PL

pwcorr  std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==7, st(0.05)

factor   std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==7, pcf
rotate

alphawgt std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07  std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 [aweight=weight] if country==7





* AUTHORITARIANISM 2.1

* Reverse scales
tab P4_01
gen P4_01r=(P4_01-6)*(-1) if P4_01!=99
la def P4_01r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_01r P4_01r
la var P4_01r "Obedience and respect for authority are the most important virtues children should learn"
tab P4_01r P4_01

tab P4_02
gen P4_02r=(P4_02-6)*(-1) if P4_02!=99
la def P4_02r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_02r P4_02r
la var P4_02r "People who break the law should be given much harsher sentences than they are these days"
tab P4_02r P4_02



tab P2_10
tab P2_11

* Standarization
egen std_P3 = std(P3)
egen std_P4_01 = std(P4_01r)
egen std_P4_02 = std(P4_02r)
egen std_P11_08 = std(P11_08r)


egen std_P2_10 = std(P2_10)
egen std_P2_11 = std(P2_11)

* Correlations
pwcorr  std_P3 std_P11_08 std_P4_01 std_P4_02    [aweight=weight]if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99, st(0.05)

* PCF
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99, pcf
rotate

predict f1_AUTH

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99


* BY COUNTRY

* ITALY
pwcorr  std_P3 std_P11_08 std_P4_01 std_P4_02    [aweight=weight]if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==1, st(0.05)

* PCF
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==1, pcf
rotate

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==1


* Czechia
pwcorr  std_P3 std_P11_08 std_P4_01 std_P4_02    [aweight=weight]if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==2, st(0.05)

* PCF
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==2, pcf
rotate

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==2


* Austria
pwcorr  std_P3 std_P11_08 std_P4_01 std_P4_02    [aweight=weight]if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==3, st(0.05)

* PCF
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==3, pcf
rotate

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==3

* France
pwcorr  std_P3 std_P11_08 std_P4_01 std_P4_02    [aweight=weight]if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==4, st(0.05)

* PCF
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==4, pcf
rotate

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==4


* GB
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==5, pcf
rotate

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==5


* HU
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==6, pcf
rotate

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==6


* PL
factor  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==7, pcf
rotate

alphawgt  std_P3 std_P11_08 std_P4_01 std_P4_02  [aweight=weight] if P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & country==7



/////////////////////////////////////////////////////////
* TABLE 1 PIA POOLED - ALL IN ONE BASKET 
/////////////////////////////////////////////////////////

/** ALL
*P
std_P11_04 std_P11_05 std_P11_06 std_P11_07
*I
std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P4_08 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 std_P5_01 std_P5_02 std_P5_04 std_P5_06
*A
std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  
*/



tab P5_01 //Muslim
gen P5_01r=P5_01
replace P5_01r=0 if P5_01==2
replace P5_01r=0 if P5_01==3
replace P5_01r=. if P5_01==99
la def P5_01r 1 "No Muslim neighbours" 0 "Doesn't care or wants", replace
la var P5_01r "No Muslim neighbours"
la val P5_01r P5_01r
tab P5_01r

tab P5_02 //Jews
gen P5_02r=P5_02
replace P5_02r=0 if P5_02==2
replace P5_02r=0 if P5_02==3
replace P5_02r=. if P5_02==99
la def P5_02r 1 "No Jewish neighbours" 0 "Doesn't care or wants", replace
la var P5_02r "No Jewish neighbours"
la val P5_02r P5_02r
tab P5_02r

tab P5_04 //Homo
gen P5_04r=P5_04
replace P5_04r=0 if P5_04==2
replace P5_04r=0 if P5_04==3
replace P5_04r=. if P5_04==99
la def P5_04r 1 "No homosexual neighbours" 0 "Doesn't care or wants", replace
la var P5_04r "No homosexual neighbours"
la val P5_04r P5_04r
tab P5_04r


tab P5_06 //Race
gen P5_06r=P5_06
replace P5_06r=0 if P5_06==2
replace P5_06r=0 if P5_06==3
replace P5_06r=. if P5_06==99
la def P5_06r 1 "No other race neighbours" 0 "Doesn't care or wants", replace
la var P5_06r "No other race neighbours"
la val P5_06r P5_06r
tab P5_06r



egen std_P5_01 = std(P5_01r)
egen std_P5_02 = std(P5_02r)
egen std_P5_04 = std(P5_04r)
egen std_P5_06 = std(P5_06r)

pwcorr std_P11_04 std_P11_05 std_P11_06 std_P11_07 std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 std_P5_01    std_P5_02 std_P5_04 std_P5_06 std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02 std_P4_08 [aweight=weight], st(0.05)

* PCF
factor std_P11_04 std_P11_05 std_P11_06 std_P11_07 std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 std_P5_01    std_P5_02 std_P5_04 std_P5_06  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08 [aweight=weight], pcf
rotate

alphawgt  std_P11_04 std_P11_05 std_P11_06 std_P11_07 std_P4_03 std_P4_04 std_P4_05 std_P4_06 std_P4_07 std_P12_01 std_P12_02 std_P12_03 std_P12_04 std_P12_05 std_P6 std_P7 std_P5_01    std_P5_02 std_P5_04 std_P5_06  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight]




//////
* Correlations between factors
pwcorr f1_POP_antielite_simple f2_POP_manichean_majoritarian f1_ILL_Parochialism f2_ILL_Trad_Cons_Intoler f3_ILL_Nativism f1_AUTH, st (0.05)




//// 
* Correlations with P2

tab P2_01 
tab P2_02 
tab P2_03 
tab P2_04 
tab P2_05 
tab P2_06 
tab P2_07 
tab P2_08 
tab P2_09 
tab P2_10 
tab P2_11

pwcorr f1_POP_antielite_simple f2_POP_manichean_majoritarian f1_ILL_Parochialism f2_ILL_Trad_Cons_Intoler f3_ILL_Nativism f1_AUTH P2_01 P2_02 P2_03 P2_04 P2_05 P2_06 P2_07 P2_08 P2_09 P2_10 P2_11 if P2_01!=99 & P2_02!=99 & P2_03!=99 & P2_04!=99 & P2_05!=99 & P2_06!=99 & P2_07!=99 & P2_08!=99 & P2_09!=99 & P2_10!=99 & P2_11!=99, st (0.05)



//////
* Correlations with other important ones

* Neighbours
* std_P5_01 std_P5_02 std_P5_04 std_P5_06

*Radek's list
/*
--  -->...reduce income differences (WAZNE, bo jedyne ekonomiczne)
-- P11_02 --> Believers in family & nation more supporte...
-- P11_03 --> resentful with people like me wealthy...
-- P11_03 --> Experts...make decisions whats good for country (WAŻNE, bo jedyny wskaźnik "technokratycznego" wymaiaru np. populizmu)
-- P12_06 --> COUNTRY better than other Ecountries (Nationalism?)
-- P16 --> ...elections represent voters... (element populizmu, jesli negatywny
-- P17 --> ...gov'ts change/don't change policies in response to people (POPULAR democracy)
*/

* Reverse scales
tab P11_01
gen P11_01r=(P11_01-6)*(-1) if P11_01!=99
la def P11_01r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_01r P11_01r
la var P11_01r "Gov should reduce income differences"
tab P11_01r P11_01

tab P11_02
gen P11_02r=(P11_02-6)*(-1) if P11_02!=99
la def P11_02r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_02r P11_02r
la var P11_02r "Believers in family & nation more support"
tab P11_02r P11_02

tab P11_03
gen P11_03r=(P11_03-6)*(-1) if P11_03!=99
la def P11_03r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_03r P11_03r
la var P11_03r "I feel resentful when I see how wealthy other people like me seem to be"
tab P11_03r P11_03

tab P11_09
gen P11_09r=(P11_09-6)*(-1) if P11_09!=99
la def P11_09r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P11_09r P11_09r
la var P11_09r "Experts...make decisions whats good for country"
tab P11_09r P11_09

tab P12_06
gen P12_06r=(P12_06-6)*(-1) if P12_06!=99
la def P12_06r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P12_06r P12_06r
la var P12_06r "COUNTRY better than other Ecountries"
tab P12_06r P12_06


tab P16
gen P16r=(P16-5)*(-1) if P16!=99
la def P16r 1"Not well at all" 2"Not very well" 3"Quite well" 4"Very well" , replace
la val P16r P16r
la var P16r "...elections represent voters..."
tab P16r P16


tab P17
gen P17r=(P17-10)*(-1) if P17!=99
la def P17r 0"Gov should not change" 10"Gov should change", replace
la val P17r P17r
la var P17r "...gov'ts change/don't change policies in response to people"
tab P17r P17 

*Generalized trust
tab P9
clonevar P9r=P9 if P9!=99

* Trust in parliament
tab P10_01
clonevar P10_01r=P10_01 if P10_01!=99

* Sat demo
tab P14_02
clonevar P14_02r=P14_02 if P14_02!=99

* Support for unification
tab P20
clonevar P20r=P20 if P20!=99


* Standarization
egen std_P11_01 = std(P11_01r)
egen std_P11_02 = std(P11_02r)
egen std_P11_03 = std(P11_03r)
egen std_P11_09 = std(P11_09r)
egen std_P12_06 = std(P12_06r)
egen std_P16 = std(P16r)
egen std_P17 = std(P17r)
egen std_P9 = std(P9r)
egen std_P10_01 = std(P10_01r)
egen std_P14_02 = std(P14_02r)
egen std_P20 = std(P20r)



pwcorr f1_POP_antielite_simple f2_POP_manichean_majoritarian f1_ILL_Parochialism f2_ILL_Trad_Cons_Intoler f3_ILL_Nativism f1_AUTH std_P5_01 std_P5_02 std_P5_04 std_P5_06 std_P11_01 std_P11_02 std_P11_03 std_P11_09 std_P12_06 std_P16 std_P17 std_P9 std_P10_01 std_P14_02 std_P20, st(0.05)




////////////////////////////////////
***
* ALTERNATIVE MEASURE OF AUTHORITARIANISM - 


/*
* Reverse scales
tab P4_01
gen P4_01r=(P4_01-6)*(-1) if P4_01!=99
la def P4_01r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_01r P4_01r
la var P4_01r "Obedience and respect for authority are the most important virtues children should learn"
tab P4_01r P4_01

tab P4_02
gen P4_02r=(P4_02-6)*(-1) if P4_02!=99
la def P4_02r 1"Strongly disagree" 2"Somewhat disagree" 3"Neither agree nor disagree" 4"Somewhat agree" 5"Strongly agree", replace
la val P4_02r P4_02r
la var P4_02r "People who break the law should be given much harsher sentences than they are these days"
tab P4_02r P4_02



tab P2_10
tab P2_11

* Standarization
egen std_P3 = std(P3)
egen std_P4_01 = std(P4_01r)
egen std_P4_02 = std(P4_02r)
egen std_P4_08 = std(P4_08r)
egen std_P11_08 = std(P11_08r)


egen std_P2_10 = std(P2_10)
egen std_P2_11 = std(P2_11)
*/

* Correlations
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08, st(0.05)

* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02 std_P4_08 [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08, pcf
rotate
predict f1_AUT_a1111 f2_AUT_a1111
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02 std_P4_08 [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08


* BY COUNTRY

* ITALY
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==1, st(0.05)
* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==1, pcf
rotate
predict f1_AUT_it f2_AUT_it
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==1

* Czechia
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==2, st(0.05)
* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==2, pcf
rotate
predict f1_AUT_cz f2_AUT_cz
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==2

* Austria
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==3, st(0.05)
* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==3, pcf
rotate
predict f1_AUT_at f2_AUT_at
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==3

* France
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==4, st(0.05)
* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==4, pcf
rotate
predict f1_AUT_fr f2_AUT_fr
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==4

* GB
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==5, st(0.05)
* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==5, pcf
rotate
predict f1_AUT_gb f2_AUT_gb
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==5

* HU
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==6, st(0.05)
* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==6, pcf
rotate
predict f1_AUT_hu f2_AUT_hu
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==6

* PL
pwcorr std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==7, st(0.05)
* PCF
factor std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==7, pcf
rotate
predict f1_AUT_pl f2_AUT_pl
alphawgt  std_P2_10 std_P2_11 std_P3 std_P11_08 std_P4_01 std_P4_02  std_P4_08  [aweight=weight] if P2_10!=99 & P2_11!=99 & P3!=99 & P11_08!=99 & P4_01!=99 & P4_02!=99 & std_P4_08 & country==7



