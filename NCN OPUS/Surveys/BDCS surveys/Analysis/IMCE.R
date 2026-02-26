##################################################################################################################
#Analysis - IMCE (binary analysis for IMCE as DV)
##################################################################################################################

#extract only needed variables for IMCEs 
long.imce<- long %>%
  dplyr::select(RID, scale, traits_sex, traits_occ, traits_corr, traits_past, traits_ir, traits_econ, traits_mig, traits_court, traits_party)
#note that scale is the continuous rating of the particular candidate profile
#when the above line does not work, the following does. 
#long.imce<-long[, c("RID", "scale", "traits_sex", "traits_occ", "traits_corr", 
#     "traits_past", "traits_ir", "traits_econ", "traits_mig", 
#     "traits_court", "traits_party")]


#turn traits into binaries 
long.imce$traits_sex.b<-long.imce$traits_sex 
long.imce$traits_occ.b<-ifelse(long.imce$traits_occ == "member of parliament", 1, 0) #turns member of parliament into 1, otherwise 0 
long.imce$traits_corr.b<-ifelse(long.imce$traits_corr == "convicted of corruption", 1, 0) #turns convicted of corruption into 1, otherwise 0 
long.imce$traits_past.b<-ifelse(long.imce$traits_past == "not a member of Communist party", 1, 0) #turns not a member of Communist party into 1, otherwise 0 
long.imce$traits_ir.b<-ifelse(long.imce$traits_ir == "favors strengthening relations with Russia", 1, 0) #turns favors strengthening relations with Russia into 1, otherwise 0 
long.imce$traits_econ.b<-ifelse(long.imce$traits_econ == "favors high progressive taxes and generous welfare", 1, 0) #turns favors high progressive taxes and generous welfare into 1, otherwise 0 
long.imce$traits_mig.b<-ifelse(long.imce$traits_mig == "opposed to accepting any refugees", 1, 0) #turns fopposed to accepting any refugees into 1, otherwise 0
long.imce$traits_court.b<-ifelse(long.imce$traits_court == "believes courts need to be independent", 1, 0) #turns believes courts need to be independent into 1, otherwise 0
long.imce$traits_party.b<-ifelse(long.imce$traits_party == "no party membership", 1, 0) #turns no party membership into 1, otherwise 0


#factor respondent id 
long.imce$RID = as.factor(long.imce$RID)
n = length(levels(long.imce$RID)) #create n as counter of individuals 

#Estimate IMCEs for each respondent/attribute
coef = matrix(0, nrow=n, ncol=9) #create empty matrix of coefficients 
se = matrix(0, nrow=n, ncol=9) #create empty matrix for SEs 
mul_coef = matrix(0, nrow=n, ncol=9) #empty matrix for coefficients from multiple regression model  
mul_se = matrix(0, nrow=n, ncol=9) #empty matrix for SEs from multiple regression model  

#loop estimation 
for (i in 1:n) {
  if(i==10000){ #skip RID==10000 because of non-variation in econ trait
    next}
  if(i==11386){ #skip RID==11386 because of non-variation in econ trait
    next}
  mod = lm(
    scale ~ traits_sex.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,1] = summ$coefficients[2,1]
  se[i,1] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_occ.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,2] = summ$coefficients[2,1]
  se[i,2] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_corr.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,3] = summ$coefficients[2,1]
  se[i,3] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_past.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,4] = summ$coefficients[2,1]
  se[i,4] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_ir.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,5] = summ$coefficients[2,1]
  se[i,5] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_econ.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,6] = summ$coefficients[2,1]
  se[i,6] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_mig.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,7] = summ$coefficients[2,1]
  se[i,7] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_court.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,8] = summ$coefficients[2,1]
  se[i,8] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ long.imce$traits_party.b,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  coef[i,9] = summ$coefficients[2,1]
  se[i,9] = summ$coefficients[2,2]
  ###new April 17, 2024
  #add multiple regression (not in Zhirkov script)
  mod =  lm(
    scale ~ traits_sex.b + traits_occ.b + traits_corr.b + traits_past.b + traits_ir.b + traits_econ.b + traits_mig.b + traits_court.b + traits_party.b ,
    data = long.imce,
    subset = (RID==levels(long.imce$RID)[i])
  )
  summ = summary(mod)
  mul_coef[i,] = summ$coefficients[-1, 1] #take all coefficients minus the first (which is the constant)
  mul_se[i,] = summ$coefficients[-1, 2] #take all SEs minus the first (which is for the constant) 
  ###end new 
}

#saveRDS(mul_coef, "/Users/rovny/Dropbox/2023_Presidential_survey_5_countries/Conjoint Analysis/mul_coef.rds")
#saveRDS(mul_se, "/Users/rovny/Dropbox/2023_Presidential_survey_5_countries/Conjoint Analysis/mul_se.rds")

#Combine estimates 
coef.exp = cbind(levels(long.imce$RID), coef, se, mul_coef, mul_se)
colnames(coef.exp) = c(
  'RID',
  'imce_female',
  'imce_MP',
  'imce_convicted',
  'imce_not_comm',
  'imce_favor_RU',
  'imce_favor_left',
  'imce_anti_mig',
  'imce_indep_courts',
  'imce_non_party',
  'imce_female_se',
  'imce_MP_se',
  'imce_convicted_se',
  'imce_not_comm_se',
  'imce_favor_RU_se',
  'imce_favor_left_se',
  'imce_anti_mig_se',
  'imce_indep_courts_se',
  'imce_non_party_se',
  'imce_mul_female',
  'imce_mul_MP',
  'imce_mul_convicted',
  'imce_mul_not_comm',
  'imce_mul_favor_RU',
  'imce_mul_favor_left',
  'imce_mul_anti_mig',
  'imce_mul_indep_courts',
  'imce_mul_non_party',
  'imce_mul_female_se',
  'imce_mul_MP_se',
  'imce_mul_convicted_se',
  'imce_mul_not_comm_se',
  'imce_mul_favor_RU_se',
  'imce_mul_favor_left_se',
  'imce_mul_anti_mig_se',
  'imce_mul_indep_courts_se',
  'imce_mul_non_party_se'
)


##
#Predict IMCEs of individuals (use IMCEs as dependent variables)
##

#merge IMCE estimates with full individual level data 
imce.individual<-as.data.frame(coef.exp)
imce.individual$RID<-as.factor(imce.individual$RID)

#NOTE! Need to load initial individual-level (not long) data
d<-import("/Users/rovny/Dropbox/2023_Presidential_survey_5_countries/Data/Datasets/Preparation/aggregated_dataset_v2.dta") 
d$RID <- seq_along(d$Country)

#merge IMCE predictions with original 'd' dataframe
d.imce<-merge(d,imce.individual, by = "RID")

#prepare individual level variables 
d.imce$yob<-d.imce$V4 #year of birth 
d.imce$educ<-as.factor(d.imce$V5) #education categories 

#rural-urban categories
d.imce$urban[d.imce$V7==1]<-1
d.imce$urban[d.imce$V7==2 | d.imce$V7==3]<-2
d.imce$urban[d.imce$V7==4 | d.imce$V7==5]<-3
d.imce$urban[d.imce$V7==6]<-4
d.imce$urban<-as.factor(d.imce$urban)

d.imce$sub.inc<-as.factor(d.imce$V68) #subjective income difficulty (ordered comfortable - very difficult)

#attendance of relig services reversed (more than 1/week - never)
d.imce$V64<-as.numeric(d.imce$V64)
d.imce$secular[d.imce$V64==1 | d.imce$V64==2 | d.imce$V64==3 | d.imce$V64==4]<-1 #2-3 months or more
d.imce$secular[d.imce$V64==5 | d.imce$V64==6 | d.imce$V64==7]<-2 #special holy days / once a year / less often
d.imce$secular[d.imce$V64==8]<-3 #never 
d.imce$secular<-as.factor(d.imce$secular)

d.imce$int.pol<-d.imce$V11 #interest in politics 

#create factors
#economic 
fa.ec <- fa(d.imce[, c("V21", "V22")]) #regulation, public spending 
# Extract factor scores for the first factor -- ordered left-right
f.econ <- as.data.frame(fa.ec$scores)
econ.right<- f.econ %>% rename(econ.right=MR1) #rename collumn 
# Attach factor scores to the original dataframe
d.imce<-cbind(d.imce,econ.right)

#cultural 
fa.cu <- fa(d.imce[, c("V30_2", "V37")]) #gay rights, immigration 
# Extract factor scores for the first factor -- ordered conservative-liberal 
f.cult <- as.data.frame(fa.cu$scores)
cult.lib<- f.cult %>% rename(cult.lib=MR1) #rename collumn 
# Attach factor scores to the original dataframe
d.imce<-cbind(d.imce,cult.lib)

#symbolic
fa.sy <- fa(d.imce[, c("V25", "V26")]) #fall of comm regime, better before 1989  
# Extract factor scores for the first factor -- ordered  communist-anti.communist
f.symb <- as.data.frame(fa.sy$scores)
symb.anti.com<- f.symb %>% rename(symb.anti.com=MR1) #rename column 
# Attach factor scores to the original dataframe
d.imce<-cbind(d.imce, symb.anti.com)

#predict IMCEs with individual level variables - considering DV (individual IMCEs) as estimate with uncertainty
#using EDVreg program to weight estimates 
#source EDVreg
source("/Users/rovny/Dropbox/2023_Presidential_survey_5_countries/Conjoint Analysis/EDVreg.R")
#export(d.imce, "/Users/rovny/Dropbox/2023_Presidential_survey_5_countries/Conjoint Analysis/d_imce.rds")
d.imce<-d.imce %>%
  filter(!is.na(symb.anti.com))

#####RUN ONE OF THE FOLLOWING BLOCS (either using betas from simple or from multiple regression to calculate IMCEs) !!! NOTE result differences are trivial
#USING *SIMPLE* REGRESSION BETAs 
#predicting IMCEs with individual level variables, using EDV reg (weighting estimated DV with its w=1/(omega^2 + sigma^2) 
#Here omega^2 is the variance of the DV sampling error estimated from the IMCE regressions ***should omega^2 be the se of the betas, or rather variance of the residuals???
I1<-edvreg(d.imce$imce_not_comm~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_not_comm_se)
)
I2<-edvreg(d.imce$imce_favor_RU~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_favor_RU_se)
)
I3<-edvreg(d.imce$imce_favor_left~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_favor_left_se)
)
I4<-edvreg(d.imce$imce_anti_mig~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_anti_mig_se)
)
I5<-edvreg(d.imce$imce_indep_courts~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_indep_courts_se)
)
I6<-edvreg(d.imce$imce_convicted~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_indep_courts_se)
)


#USING *MULTIPLE* REGRESSION BETAs 
#predicting IMCEs with individual level variables, using EDV reg (weighting estimated DV with its w=1/(omega^2 + sigma^2) 
#Here omega^2 is the variance of the DV sampling error estimated from the IMCE regressions ***should omega^2 be the se of the betas, or rather variance of the residuals???
I1<-edvreg(d.imce$imce_mul_not_comm~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_mul_not_comm_se)
)
I2<-edvreg(d.imce$imce_mul_favor_RU~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_mul_favor_RU_se)
)
I3<-edvreg(d.imce$imce_mul_favor_left~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_mul_favor_left_se)
)
I4<-edvreg(d.imce$imce_mul_anti_mig~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_mul_anti_mig_se)
)
I5<-edvreg(d.imce$imce_mul_indep_courts~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_mul_indep_courts_se)
)
I6<-edvreg(d.imce$imce_mul_convicted~d.imce$yob+d.imce$educ+d.imce$urban+d.imce$sub.inc+d.imce$secular+d.imce$int.pol+d.imce$econ.right+d.imce$cult.lib+d.imce$symb.anti.com, 
           omegasq = as.numeric(d.imce$imce_mul_indep_courts_se)
)

####
#Plotting
#Yuma Ando's Reverse modelplot : YA 20240521 
source("rev_modelplot.R") #bring in source code for reversed model plot written by Yuma Ando, 21.5.2024

##load library
library(modelsummary)
library(arm)
library(ggplot2)
library(gridExtra)

#Change the name of coefficients : this is due to a slight difference between lm and edvreg functions
library(stringr)
for (o in paste0("I",1:6)){
  tmp<-get(o)
  names(tmp$coefficients)<-str_remove_all(names(tmp$coefficients),"d\\.imce\\$")
  assign(o, tmp)
}

#plot models 
model_list1 <- list(
  "No communist past" = I1, 
  "Favors Russia" = I2, 
  "Favors progressive tax + welfare" = I3,
  "Opposes all migration" = I4, 
  "Supports independent courts" = I5,
  "Convicted of corruption" = I6
)


cm <- c('econ.right' = 'Economic right',
        'cult.lib' = 'Cultural liberal', 
        'symb.anti.com' = 'Symbolic anti-communist')

#old model plot
m1<-modelplot(model_list1, coef_map = cm, background = list(geom_vline(xintercept = 0, color = 'black')) )+xlab("coefficient")

#reversed model plot (based on Yuma's script)
rev_m1<-rev_modelplot(model_list1, coef_map = cm, background = list(geom_vline(xintercept = 0, color = 'black')) )+xlab("coefficient")
rev_m1
#save
ggsave("rev_combined_plot_imce.jpg", rev_m1, width = 12, height = 6, dpi = 600)



##################################################################################################################
#Analysis - IMCE - for effect size calculation (using all categories of traits)
##################################################################################################################

#extract only needed variables for IMCEs (using three category factor attributes)
long.imce.eff<- long %>%
  dplyr::select(RID, scale, traits_sex_re, traits_occ_re, traits_corr_re, traits_past_re, traits_ir_re, traits_econ_re, traits_mig_re, traits_court_re, traits_party_re)
#note that scale is the continuous rating of the particular candidate profile
#when the above line does not work, the following does. 
#long.imce<-long[, c("RID", "scale", "traits_sex", "traits_occ", "traits_corr", 
#     "traits_past", "traits_ir", "traits_econ", "traits_mig", 
#     "traits_court", "traits_party")]

saveRDS(long.imce.eff,
        "Long_export.RDS")

#factor respondent id 
long.imce.eff$RID = as.factor(long.imce.eff$RID)
n = length(levels(long.imce.eff$RID)) #create n as counter of individuals 

#Estimate IMCEs for each respondent/attribute
coef1 = matrix(0, nrow=n, ncol=9) #create empty matrix of coefficient1 
coef2 = matrix(0, nrow=n, ncol=9) #create empty matrix of coefficient2 
se1 = matrix(0, nrow=n, ncol=9) #create empty matrix for SE1s 
se2 = matrix(0, nrow=n, ncol=9) #create empty matrix for SE2s 

##vectors for multiple regression 
coef_m1 = matrix(0, nrow=n, ncol=9) #create empty matrix of coefficients1 
coef_m2 = matrix(0, nrow=n, ncol=9) #create empty matrix of coefficients2 
se_m1 = matrix(0, nrow=n, ncol=9) #create empty matrix for SE1s 
se_m2 = matrix(0, nrow=n, ncol=9) #create empty matrix for SE2s 


#loop estimation
for (i in 1:n) {
  if(i==221){ #skip RID==221 because of non-variation 
    next}
  if(i==412){ #skip RID==412 because of non-variation 
    next}
  if(i==427){ 
    next}
  if(i==1001){ 
    next}
  if(i==2056){ 
    next}
  if(i==2733){ 
    next}
  if(i==2755){ 
    next}
  if(i==2846){ 
    next}
  if(i==3044){ 
    next}
  if(i==3477){ 
    next}
  if(i==3491){ 
    next}
  if(i==3972){ 
    next}
  if(i==4009){ 
    next}
  if(i==4677){ 
    next}
  if(i==5043){ 
    next}
  if(i==5100){ 
    next}
  if(i==5116){ 
    next}
  if(i==5400){ 
    next}
  if(i==5556){ 
    next}
  if(i==5591){ 
    next}
  if(i==5671){ 
    next}
  if(i==6030){ 
    next}
  if(i==6479){ 
    next}
  if(i==6948){ 
    next}
  if(i==6969){ 
    next}
  if(i==7142){ 
    next}
  if(i==7650){ 
    next}
  if(i==8874){ 
    next}
  if(i==9138){ 
    next}
  if(i==9195){ 
    next}
  if(i==9595){ 
    next}
  if(i==9673){ 
    next}
  if(i==9686){ 
    next}
  if(i==9708){ 
    next}
  if(i==9845){ 
    next}
  if(i==9866){ 
    next}
  if(i==10000){ 
    next}
  if(i==10035){ 
    next}
  if(i==10122){ 
    next}
  if(i==10234){ 
    next}
  if(i==10409){ 
    next}
  if(i==10534){ 
    next}
  if(i==10820){ 
    next}
  if(i==11281){ 
    next}
  if(i==11372){ 
    next}
  if(i==11386){ 
    next}
  if(i==11425){ 
    next}
  if(i==11814){ 
    next}
  if(i==11930){ 
    next}
  if(i==12184){ 
    next}
  mod = lm(
    scale ~ as.factor(traits_sex_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,1] = summ$coefficients[2,1]
  se1[i,1] = summ$coefficients[2,2]
  
  mod =  lm(
    scale ~ as.factor(traits_occ_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,2] = summ$coefficients[2,1]
  se1[i,2] = summ$coefficients[2,2]
  coef2[i,2] = summ$coefficients[3,1]
  se2[i,2] = summ$coefficients[3,2]
  
  mod =  lm(
    scale ~ as.factor(traits_corr_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,3] = summ$coefficients[2,1]
  se1[i,3] = summ$coefficients[2,2]
  coef2[i,3] = summ$coefficients[3,1]
  se2[i,3] = summ$coefficients[3,2]
  
  mod =  lm(
    scale ~ as.factor(traits_past_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,4] = summ$coefficients[2,1]
  se1[i,4] = summ$coefficients[2,2]
  coef2[i,4] = summ$coefficients[3,1]
  se2[i,4] = summ$coefficients[3,2]
  
  mod =  lm(
    scale ~ as.factor(traits_ir_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,5] = summ$coefficients[2,1]
  se1[i,5] = summ$coefficients[2,2]
  coef2[i,5] = summ$coefficients[3,1]
  se2[i,5] = summ$coefficients[3,2]
  
  
  mod =  lm(
    scale ~ as.factor(traits_econ_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,6] = summ$coefficients[2,1]
  se1[i,6] = summ$coefficients[2,2]
  coef2[i,6] = summ$coefficients[3,1]
  se2[i,6] = summ$coefficients[3,2]
  
  mod =  lm(
    scale ~ as.factor(traits_mig_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,7] = summ$coefficients[2,1]
  se1[i,7] = summ$coefficients[2,2]
  coef2[i,7] = summ$coefficients[3,1]
  se2[i,7] = summ$coefficients[3,2]
  
  mod =  lm(
    scale ~ as.factor(traits_court_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,8] = summ$coefficients[2,1]
  se1[i,8] = summ$coefficients[2,2]
  coef2[i,8] = summ$coefficients[3,1]
  se2[i,8] = summ$coefficients[3,2]
  
  
  mod =  lm(
    scale ~ as.factor(traits_party_re),
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef1[i,9] = summ$coefficients[2,1]
  se1[i,9] = summ$coefficients[2,2]
  coef2[i,9] = summ$coefficients[3,1]
  se2[i,9] = summ$coefficients[3,2]
}

#loop estimation for multiple regression
for (i in 1:n) {  
  if(i==221){ #skip RID==221 because of non-variation 
    next}
  if(i==412){ #skip RID==412 because of non-variation 
    next}
  if(i==427){ 
    next}
  if(i==660){ 
    next}
  if(i==1001){ 
    next}
  if(i==2056){ 
    next}
  if(i==2733){ 
    next}
  if(i==2755){ 
    next}
  if(i==2846){ 
    next}
  if(i==3044){ 
    next}
  if(i==3477){ 
    next}
  if(i==3491){ 
    next}
  if(i==3972){ 
    next}
  if(i==4009){ 
    next}
  if(i==4677){ 
    next}
  if(i==5043){ 
    next}
  if(i==5100){ 
    next}
  if(i==5116){ 
    next}
  if(i==5400){ 
    next}
  if(i==5556){ 
    next}
  if(i==5591){ 
    next}
  if(i==5671){ 
    next}
  if(i==6030){ 
    next}
  if(i==6479){ 
    next}
  if(i==6948){ 
    next}
  if(i==6969){ 
    next}
  if(i==7142){ 
    next}
  if(i==7650){ 
    next}
  if(i==8874){ 
    next}
  if(i==9138){ 
    next}
  if(i==9195){ 
    next}
  if(i==9595){ 
    next}
  if(i==9673){ 
    next}
  if(i==9686){ 
    next}
  if(i==9708){ 
    next}
  if(i==9845){ 
    next}
  if(i==9866){ 
    next}
  if(i==10000){ 
    next}
  if(i==10035){ 
    next}
  if(i==10122){ 
    next}
  if(i==10234){ 
    next}
  if(i==10409){ 
    next}
  if(i==10534){ 
    next}
  if(i==10820){ 
    next}
  if(i==11281){ 
    next}
  if(i==11372){ 
    next}
  if(i==11386){ 
    next}
  if(i==11425){ 
    next}
  if(i==11814){ 
    next}
  if(i==11930){ 
    next}
  if(i==12184){ 
    next}
  ##multiple regression 
  mod =  lm(
    scale ~ as.factor(traits_sex_re) + as.factor(traits_occ_re) + as.factor(traits_corr_re) + as.factor(traits_past_re) + as.factor(traits_ir_re) + 
      as.factor(traits_econ_re) + as.factor(traits_mig_re) + as.factor(traits_court_re) + as.factor(traits_party_re) ,
    data = long.imce.eff,
    subset = (RID==levels(long.imce.eff$RID)[i])
  )
  summ = summary(mod)
  coef_m1[i,1] = summ$coefficients[2,1]
  coef_m1[i,2] = summ$coefficients[3,1]
  coef_m1[i,3] = summ$coefficients[5,1]
  coef_m1[i,4] = summ$coefficients[7,1]
  coef_m1[i,5] = summ$coefficients[9,1]
  coef_m1[i,6] = summ$coefficients[11,1]
  coef_m1[i,7] = summ$coefficients[13,1]
  coef_m1[i,8] = summ$coefficients[15,1]
  coef_m1[i,9] = summ$coefficients[17,1]
  
  se_m1[i,1] = summ$coefficients[2,2]
  se_m1[i,2] = summ$coefficients[3,2]
  se_m1[i,3] = summ$coefficients[5,2]
  se_m1[i,4] = summ$coefficients[7,2]
  se_m1[i,5] = summ$coefficients[9,2]
  se_m1[i,6] = summ$coefficients[11,2]
  se_m1[i,7] = summ$coefficients[13,2]
  se_m1[i,8] = summ$coefficients[15,2]
  se_m1[i,9] = summ$coefficients[17,2]
  
  coef_m2[i,2] = summ$coefficients[4,1]
  coef_m2[i,3] = summ$coefficients[6,1]
  coef_m2[i,4] = summ$coefficients[8,1]
  coef_m2[i,5] = summ$coefficients[10,1]
  coef_m2[i,6] = summ$coefficients[12,1]
  coef_m2[i,7] = summ$coefficients[14,1]
  coef_m2[i,8] = summ$coefficients[16,1]
  coef_m2[i,9] = summ$coefficients[18,1]
  
  se_m2[i,2] = summ$coefficients[4,2]
  se_m2[i,3] = summ$coefficients[6,2]
  se_m2[i,4] = summ$coefficients[8,2]
  se_m2[i,5] = summ$coefficients[10,2]
  se_m2[i,6] = summ$coefficients[12,2]
  se_m2[i,7] = summ$coefficients[14,2]
  se_m2[i,8] = summ$coefficients[16,2]
  se_m2[i,9] = summ$coefficients[18,2]
  
}

###SIMPLE OR MULTIPLE REGRESSION for calculating IMCE betas 
##!!!NOTE, the following 2 lines lead to the use of multiple regression results for graph. If skip these 2 lines, then use simple reg results. 
coef1<-coef_m1
coef2<-coef_m2

colnames(coef1) = c(
  'sex1',
  'occ1',
  'corr1',
  'past1',
  'ir1',
  'econ1',
  'mig1',
  'court1',
  'party1')
colnames(coef2) = c(
  'sex2',
  'occ2',
  'corr2',
  'past2',
  'ir2',
  'econ2',
  'mig2',
  'court2',
  'party2'
)
abs.coef1<-as.tibble(abs(coef1))
abs.coef2<-as.tibble(abs(coef2))


#calculate sd 
coef1<-as.tibble(coef1)
std1 <- c(sd(coef1$sex1), sd(coef1$occ1), sd(coef1$corr1), 
          sd(coef1$past1), sd(coef1$ir1), sd(coef1$econ1), 
          sd(coef1$mig1), sd(coef1$court1), sd(coef1$party1)
)

coef2<-as.tibble(coef2)
std2 <- c(sd(coef2$sex2), sd(coef2$occ2), sd(coef2$corr2), 
          sd(coef2$past2), sd(coef2$ir2), sd(coef2$econ2), 
          sd(coef2$mig2), sd(coef2$court2), sd(coef2$party2)
)
#calculate standard errors 
standard_errors1 <- std1 / sqrt(nrow(coef1))
standard_errors2 <- std2 / sqrt(nrow(coef2))

# collect means and SEs ABSOLUTE VALUES 
D <- data.frame(variable = c("gender", 
                             "occupation",
                             "corruption",
                             "communist_past",
                             "foreign_policy",
                             "economic_policy",
                             "migration_policy",
                             "judiciary",
                             "partisanship"), 
                beta1 = colMeans(abs.coef1), 
                beta2 = colMeans(abs.coef2),
                se1 = standard_errors1,
                se2 = standard_errors2
)

# collect means and SEs NOT USING ABSOLUTE VALUES (this next function is NOT used in paper, skip!)
D <- data.frame(variable = c("gender", 
                             "occupation",
                             "corruption",
                             "communist_past",
                             "foreign_policy",
                             "economic_policy",
                             "migration_policy",
                             "judiciary",
                             "partisanship"), 
                beta1 = colMeans(coef1), 
                beta2 = colMeans(coef2),
                se1 = standard_errors1,
                se2 = standard_errors2
)

D$lower1 <- D$beta1 - 1.96 * D$se1
D$upper1 <- D$beta1 + 1.96 * D$se1
D$lower2 <- D$beta2 - 1.96 * D$se2
D$upper2 <- D$beta2 + 1.96 * D$se2
D$mean<-(D$beta1+D$beta2)/2 #create mean of both betas 
D$mean[D$variable=="gender"]<-D$beta1[D$variable=="gender"] #replace mean of gender with beta1 (beta2 on gender=0 because there is no third level on gender, which biases the mean calculation)
D$beta2[D$variable=="gender"]<-NA
D$se2[D$variable=="gender"]<-NA
D$lower2[D$variable=="gender"]<-NA
D$upper2[D$variable=="gender"]<-NA


#add labels to coefficients 
D$label1<-c("woman", "business exec", "investigated", "CP member", "for NATO" , 
            "for high progressive tax", "for UA refugees", "people recall judges", 
            "govt party") 
D$label2<-c( "", "MP" , "convicted", "secret police",  "for Russia", 
             "for low flat tax", "for African refugees",  "govt recall judges", 
             "opposition party")  

# order of bars as descending values of mean
D <- D[order(-D$mean),]

# Create ggplot with horizontal bars
IMCE_abs<-ggplot(D, aes(x = mean, y = reorder(variable, mean))) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.5) +
  geom_point(aes(y = variable, x=beta1), color = "black", position = position_nudge(y = -0.05) ) +
  geom_point(aes(y = variable, x=beta2), color = "gray45", position = position_nudge(y = 0.05) ) +
  geom_linerange(aes(y = variable, xmin = lower1, xmax = upper1), color = "black", linewidth = 0.5, position = position_nudge(y = -0.05)) +
  geom_linerange(aes(y = variable, xmin = lower2, xmax = upper2), color = "gray45", linewidth = 0.5, position = position_nudge(y = 0.05)) + 
  geom_text(aes(y = variable, x = beta1, label = label1, vjust = 2), color = "black", size=3) +  # Text for black bars
  geom_text(aes(y = variable, x = beta2, label = label2, vjust = -1.5), color = "gray45", size=3) +   # Text for gray bars
  labs(x = "Mean absolute IMCE", y = "Attribute") +
  coord_cartesian(xlim = c(-0.2, 1.7)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", size = 1) +
  theme_minimal() 
IMCE_abs
ggsave("plot_imce_abs.jpg", IMCE_abs, width = 7.5, height = 10, dpi = 600)

#IF DID NOT USE ABSOLUTE VALUES in Calculation (line 1502 above)
# Create ggplot with horizontal bars
IMCE_non_abs<-ggplot(D, aes(x = mean, y = reorder(variable, -mean))) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.5) +
  geom_point(aes(y = variable, x=beta1), color = "black", position = position_nudge(y = -0.05) ) +
  geom_point(aes(y = variable, x=beta2), color = "gray45", position = position_nudge(y = 0.05) ) +
  geom_linerange(aes(y = variable, xmin = lower1, xmax = upper1), color = "black", linewidth = 0.5, position = position_nudge(y = -0.05)) +
  geom_linerange(aes(y = variable, xmin = lower2, xmax = upper2), color = "gray45", linewidth = 0.5, position = position_nudge(y = 0.05)) + 
  geom_text(aes(y = variable, x = beta1, label = label1, vjust = 2), color = "black", size=3) +  # Text for black bars
  geom_text(aes(y = variable, x = beta2, label = label2, vjust = -1.5), color = "gray45", size=3) +   # Text for gray bars
  labs(x = "Mean IMCE", y = "Attribute") +
  coord_cartesian(xlim = c(-0.7, 1.5)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", size = 1) +
  theme_minimal()
IMCE_non_abs
ggsave("plot_imce_non_abs.jpg", IMCE_non_abs, width = 7.5, height = 10, dpi = 600)

library(patchwork)
combined_IMCE <- IMCE_non_abs + IMCE_abs
combined_IMCE 
