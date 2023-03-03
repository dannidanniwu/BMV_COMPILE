#use the original code in simulation
library(dplyr)
library(data.table)
library(cmdstanr)
library(posterior)
#---compile stan model---#
set_cmdstan_path(path = "/gpfs/share/apps/cmdstan/2.25.0")
mod <- cmdstan_model("/gpfs/home/dw2625/r/BSIM/mvo_original.stan");
# 
# #---Data preprocess---#
# dat <- read.csv(file="./COMPILE_merged_20210525.csv", na.strings = '-999')
# dat$ID = dat$STUDY_ID <- paste0(dat$tr_rct_id,'_',dat$site_id,'_',dat$pt_id)
# 
# dat$out_death14[!is.na(dat$out_death14) & dat$out_death14 == 1 & dat$out_deathday > 15] <- 0
# dat$out_death[!is.na(dat$out_death) & dat$out_death == 1 & dat$out_deathday > 30] <- 0
# dat$out_death14[dat$out_deathday <= 15 &!is.na(dat$out_deathday)] <- 1
# dat$out_death[dat$out_deathday <= 30 &!is.na(dat$out_deathday)] <- 1
# dat$out_who0[is.na(dat$out_who0)] <- dat$out_who1[is.na(dat$out_who0)]
# # baseline WHO
# dat$who_enroll <- factor(dat$out_who0)
# 
# 
# dat$WHO_0.tmp <- dat$out_who0
# dat$WHO_0.tmp[is.na(dat$WHO_0.tmp)] <- 99
# dat$WHO_enroll.factor <- factor(dat$WHO_0.tmp,c(4,5,6,99),c(4,5,6,'NA'))
# 
# dat$WHO_14 <- ifelse(dat$tr_rct_id %in% c('DD','EE','RR'),dat$out_who15,ifelse(dat$tr_rct_id == 'BB',dat$out_who13,dat$out_who14))
# dat[is.na(dat$WHO_14) & !is.na(dat$out_who14),]$WHO_14 <- dat[is.na(dat$WHO_14) & !is.na(dat$out_who14),]$out_who14 # patients who did not have WHO 14 but had WHO 14
# dat[is.na(dat$WHO_14) & !is.na(dat$out_who13),]$WHO_14 <- dat[is.na(dat$WHO_14) & !is.na(dat$out_who13),]$out_who13 # patients who did not have WHO 14 but had WHO 13
# dat[is.na(dat$WHO_14) & !is.na(dat$out_who15),]$WHO_14 <- dat[is.na(dat$WHO_14) & !is.na(dat$out_who15),]$out_who15 # patients who did not have WHO 14 but had WHO 15
# #dat[is.na(dat$WHO_14) & is.na(dat$out_who13) & !is.na(dat$out_who15),]$WHO_14 <- dat[is.na(dat$WHO_14) & is.na(dat$out_who13) & !is.na(dat$out_who15),]$out_who15 
# 
# for (i in dat[dat$tr_rct_id == 'EE',]$ID) {
#   tmp <- dat[dat$ID == i,]
#   day_discharge <- tmp$out_discharge
#   if (!is.na(day_discharge) & day_discharge > 0 & day_discharge < 30) {
#     if (!is.na(tmp[,paste0('out_who',day_discharge + 1)])) {
#       dat[dat$ID == i,paste0('out_who',day_discharge)] <- tmp[,paste0('out_who',day_discharge + 1)]
#     }
#   }
# } # EE discharge scores collected at 1 day after discharge
# 
# dat$WHO_14_imp <- sapply(1:nrow(dat), function(i) {
#   day_discharge <- dat[i,]$out_discharge
#   outcome <- dat[i,]$WHO_14
#   outcome_day <- ifelse(dat[i,]$tr_rct_id %in% c('DD','EE','RR'),15,ifelse(dat[i,]$tr_rct_id == 'BB',13,14))
#   if (!is.na(outcome))     
#   {return(outcome)} else 
#   {if (!is.na(day_discharge) & day_discharge < outcome_day & day_discharge >= 0) {
#     return (dat[i,paste0('out_who',day_discharge)])
#   } else {
#     return(outcome)
#   } 
#   }
# }) %>% as.numeric(.)
# dat$WHO_14_imp[dat$ID %in% c('AA_2_25','AA_2_72','AA_15_16','AA_15_3','AA_10_33','AA_10_16','AA_14_18','AA_9_7','AA_8_25','AA_9_51','AA_8_41','AA_10_15','AA_8_6','AA_21_3','AA_21_1','BB_1_46','BB_1_47','CC_1_65',"CC_5_9","CC_8_3",'CC_29_2','EE_10_18')] <- NA # withdraw by day 14
# dat$WHO_14_imp[dat$out_deathday %in% 0:15] <- 10
# 
# dat$WHO_28 <- ifelse(dat$tr_rct_id == c('BB'),dat$out_who27,ifelse(dat$tr_rct_id == 'DD', dat$out_who29,
#                                                                    ifelse(dat$tr_rct_id %in% c('EE','RR'),dat$out_who30,dat$out_who28)))
# dat[is.na(dat$WHO_28) & !is.na(dat$out_who29),]$WHO_28 <- dat[is.na(dat$WHO_28) & !is.na(dat$out_who29),]$out_who29 # patients who did not have WHO 28 but had WHO 29
# dat[is.na(dat$WHO_28) & !is.na(dat$out_who28),]$WHO_28 <- dat[is.na(dat$WHO_28) & !is.na(dat$out_who28),]$out_who28 # patients who did not have WHO 28 but had WHO 28
# dat[is.na(dat$WHO_28) & !is.na(dat$out_who27),]$WHO_28 <- dat[is.na(dat$WHO_28) & !is.na(dat$out_who27),]$out_who27 # patients who did not have WHO 28 but had WHO 27
# dat[is.na(dat$WHO_28) & !is.na(dat$out_who26),]$WHO_28 <- dat[is.na(dat$WHO_28) & !is.na(dat$out_who26),]$out_who26 # patients who did not have WHO 28 but had WHO 26
# dat[is.na(dat$WHO_28) & !is.na(dat$out_who30),]$WHO_28 <- dat[is.na(dat$WHO_28) & !is.na(dat$out_who30),]$out_who30 # patients who did not have WHO 28 but had WHO 30
# 
# dat$WHO_28_imp <- sapply(1:nrow(dat), function(i) {
#   day_discharge <- dat[i,]$out_discharge
#   outcome_14 <- dat[i,]$WHO_14
#   outcome <- dat[i,]$WHO_28
#   outcome_day_14 <- ifelse(dat[i,]$tr_rct_id %in% c('DD','EE','RR'),15,ifelse(dat[i,]$tr_rct_id == 'BB',13,14))
#   outcome_day <- ifelse(dat[i,]$tr_rct_id == 'BB',27,ifelse(dat[i,]$tr_rct_id == 'DD', 29,
#                                                             ifelse(dat[i,]$tr_rct_id %in% c('EE','RR'),30, 28)))
#   if (!is.na(outcome))     
#   {return(outcome)} else 
#   {if (!is.na(day_discharge) & day_discharge < outcome_day & day_discharge > outcome_day_14) {
#     return (dat[i,paste0('out_who',day_discharge)])
#   } else if (!is.na(day_discharge) & day_discharge <= outcome_day_14 & day_discharge >=0 & !is.na(outcome_14)) {
#     return(outcome_14)
#   } else if (!is.na(day_discharge) & day_discharge <= outcome_day_14 & day_discharge >=0 & is.na(outcome_14)) {
#     return(dat[i,paste0('out_who',day_discharge)])
#   } else {
#     return(outcome)
#   }
#   }
# }) %>% as.numeric(.)
# dat$WHO_28_imp[dat$ID %in% c('AA_2_25','AA_2_72','AA_15_16','AA_15_3','AA_10_33','AA_10_16','AA_14_18','AA_9_7','AA_8_25','AA_9_51','AA_8_41','AA_10_15','AA_8_6','AA_21_3','AA_21_1','AA_1_103','AA_1_80','BB_1_46','BB_1_47','CC_1_14','CC_1_65',"CC_5_9","CC_8_3",'CC_16_6','CC_29_2','EE_10_18')] <- NA # withdraw by day 28
# dat$WHO_28_imp[dat$out_deathday %in% 0:30] <- 10
# 
# sum(!is.na(dat$WHO_14_imp))
# #2341
# sum(!is.na(dat$WHO_28_imp))
# #outcomes:The primary outcome: WHO_14
# #secondary outcomes: WHO_28>=7; a binary indicator of ventilation or worse at day 28, a binary indicator of hospitalized or
# #not at day 28 (WHO 4~10), and a binary indicator of death or not at day 28. 
# 
# #---variables in data ---#
# #check if binary vairable is more easily to collect compared to ordinal variable
# sum(!is.na(dat$WHO_28))#2266
# sum(!is.na(dat$out_death))#2337
# 
# sum(!is.na(dat$WHO_28))#2266
# sum(!is.na(dat$out_discharge))#2267

#save to rda file
#save(dat, file = "data_after_process.rda")
load("/gpfs/home/dw2625/r/BSIM/data_after_process.rda")
dind <- na.omit(dat[,c("WHO_14_imp","WHO_28_imp","who_enroll","tr_rct_id",
                       "bl_age","bl_sex","bl_blood",'bl_cardio','bl_pulm',
                       'bl_diabetes',"bl_treatment",
                       "bl_symp_days","bl_enrollqtr")])
dind <- dind%>% as.data.table()

#outcome
dind$y_hospt <- as.numeric(dind$WHO_28_imp > 3)
dind$y_venti <- as.numeric(dind$WHO_28_imp > 6)
dind$y_death <- as.numeric(dind$WHO_28_imp %in% 10)
dind$ordY <- dind$WHO_14_imp + 1  #ordY:1-11

dind$cat_age <- cut(dind$bl_age, c(0,50,67,120))
dind$age <- factor(dind$cat_age,c("(0,50]","(50,67]","(67,120]"),1:3)
dind$blood <- factor(dind$bl_blood,  0:3, c('O', 'A', 'B', 'AB'))
#covariates in TBI:
#Indicator for baseline WHO=5, Indicator for baseline WHO=6, Indicator for baseline WHO=5 & age >67, 
#Indicator for baseline WHO=6 & age >67, Indicator for blood type A or AB, Indicator for presence of CVD
#Indicator for comorbid Diabetes Mellitus  & Pulmonary Disease  
dind$who_5 <- as.numeric(dind$who_enroll == 5)
dind$who_6 <- as.numeric(dind$who_enroll == 6)
dind$who5_age <-  as.numeric(dind$who_enroll == 5 & dind$bl_age >=67)
dind$who6_age <-  as.numeric(dind$who_enroll == 6 & dind$bl_age >=67)
dind$bloodAAB <- as.numeric(dind$blood == "A" | dind$blood == "AB")
dind$DMPD <- as.numeric(dind$bl_diabetes & dind$bl_pulm)
#process some covariates in main effect term
dind$DMCVD <- as.numeric(dind$bl_diabetes & dind$bl_cardio)
dind$age_sq <- dind$bl_age^2
dind$ss_le7 <- as.numeric(dind$bl_symp_days %in% c(1,2)) #indicator of duration of symptoms 0-6 days
### Data for stan model
x_main <- model.matrix( ~ factor(tr_rct_id)+bl_age + age_sq + factor(bl_sex) + factor(who_enroll) +
                          bl_age* factor(who_enroll) +factor(bloodAAB) + factor(DMCVD) 
                        + factor(DMPD) + factor(ss_le7) + factor(bl_enrollqtr),data=dind)[, -1]
x_inter <- dind[,.(who_5,who_6,who5_age,who6_age,bloodAAB,bl_cardio,DMPD)]
y_ord <- dind$ordY
y_b <- dind[,.(y_hospt,y_venti,y_death)]

D <- ncol(y_b) 
P_main <- ncol(x_main)
P_inter <- ncol(x_inter)
L <- length(unique(y_ord))


studydata <- list(
  N = nrow(dind), y_ord=y_ord, y_b=y_b,
  x_main=x_main, x_inter=x_inter, P_main = P_main, P_inter= P_inter,
  A=dind$bl_treatment, D=D, L=L)
#J: #outcome
#D: # of cov
#---model fitting---#
fit <- mod$sample(
  data = studydata,
  refresh = 0,
  chains = 4L,
  parallel_chains = 4L,
  iter_warmup = 500,
  iter_sampling = 2500,
  show_messages = FALSE)


fit_sumry <- fit$summary(c("beta_1","beta_int","sigma_beta","beta_star"))

date_stamp <- gsub("-", "", Sys.Date())
dir.create(file.path("/gpfs/home/dw2625/r/BSIM/", date_stamp), showWarnings = FALSE)
save(fit_sumry, file = paste0("/gpfs/home/dw2625/r/BSIM/", date_stamp, "/fit_sumry_original_withrct.rda"))

# beta_name<- expand.grid(variable=c("who_5","who_6","who5_age","who6_age","bloodAAB","bl_cardio","DMPD"),
#             outcome=c("binary_outcome1","binary_outcome2","binary_outcome3","ordinal_outcome"))
# 
# beta_inter_names <- expand.grid(variable=c("trt","who_5*trt","who_6*trt","who5_age*trt","who6_age*trt","bloodAAB*trt","bl_cardio*trt","DMPD*trt"),
#                                 outcome=c("binary_outcome1","binary_outcome2","binary_outcome3","ordinal_outcome"))
# 
# sigma_beta_names <- paste0("sigma(",c("trt","who_5*trt","who_6*trt","who5_age*trt","who6_age*trt","bloodAAB*trt","bl_cardio*trt","DMPD*trt"),")")
# rownames(fit_sumry) <- c(paste0(beta_name[1:28,1],"_",beta_name[1:28,2]), paste0(beta_inter_names[1:32,1],"_",beta_inter_names[1:32,2]),
#   sigma_beta_names)
# 
# save(fit_sumry ,file="./fit_sumry_withnames_v2.rda")
# 
# 
# write.csv(fit_sumry,'posterior_sumamry.csv')
