# let's draw adjusted survival curve after psmatching
library(data.table); library(dplyr);
library(magrittr);library(parallel);library(ggplot2);library(tableone)
library(survival);library(survminer);library(cmprsk)
library(tidycmprsk);library(ggsurvfit);
library(gridExtra); library(ggplot2)
library(tableone) ;library(moonBook)
library(ggpubr)
library(survey);library(scales) ; library(testhat); library(knitr); library(rmarkdown) ;
library(lubridate) 
library(survplot)

library(dplyr)
library(tidyr)
library(survival)
library(cmprsk)
library(data.table)
library(labelled)
library(flextable)
library(moonBook)
library(survminer)
library(magrittr);library(parallel);library(ggplot2);library(tableone)

setwd("/vol/userdata13/sta_room417/")
library(sas7bdat)
library(dplyr)
library(haven)
library(officer)
options(scipen = 999)
library(MatchIt)

# install.packages('VGAM')
library(VGAM)



# Case 1: Mgus to MM ------
# 희주샘 코드 이용
# read data --------------------------------------
mgus_surv <- read_sas("mgus_surv_mm.sas7bdat") # finalized dataset for survival anaylsis

mgus_surv_v193<-read_sas("mgus_v_ex.sas7bdat") # 462
mgus_surv_v193$c90_v193_yn<-1

mgus_surv <- merge(mgus_surv, mgus_surv_v193[,c("JID","c90_v193_yn")],by="JID",all.x=T)
mgus_surv$c90_v193_yn <- ifelse(is.na(mgus_surv$c90_v193_yn), 0, 1)


# competing risk
# status: 1 = mm (n = ), 2 = death (n = ), 0 = censor (n = )
mgus_surv <- mgus_surv %>% 
  mutate(first_c90_date = as.Date(first_c90_date, origin = "1960-01-01"),  # SAS origin date = 1960-01-01, R = 1970-01-01
         c90_medi_date = as.Date(c90_medi_date, origin = "1960-01-01"))

mgus_surv <- as.data.table(mgus_surv)

#c90_yn==1, n=483
mgus_surv[!is.na(first_c90_date),c90_yn:=1,]
mgus_surv[is.na(c90_yn),c90_yn:=0,]
mgus_surv$c90_yn %>% as.factor %>% summary

#c90_v193_yn==1, n=462
mgus_surv$c90_v193_yn %>% as.factor %>% summary


#prescription within 60days after c90
#drug_60days_c90==1, n=226
mgus_surv[c90_v193_yn==1, drug_60days_c90:=ifelse(as.numeric(first_mm_date-first_c90_date)<=60 & c90_v193_yn==1,1,0)]
mgus_surv[is.na(drug_60days_c90),drug_60days_c90:=0]
mgus_surv$drug_60days_c90 %>% as.factor %>% summary


#death within 6months after c90
#death_6months_c90==1, n=43(when <=180)
mgus_surv[c90_v193_yn==1,death_6months_c90:=ifelse(death_yn==1 & (death_day-as.numeric(first_c90_date-mgus_index_date))<=180,1,0)]
mgus_surv[is.na(death_6months_c90),death_6months_c90:=0]
mgus_surv$death_6months_c90 %>% as.factor %>% summary

### (240414) drug_60days_c90!=1 & death_6months_c90==1을 세기------
# mgus_surv 복제
mgus_surv_dup = copy(mgus_surv)

# drug_60days_c90!=1 & death_6months_c90==1을 세기, n=14
mgus_surv_dup[c90_v193_yn==1 & (drug_60days_c90!=1 & death_6months_c90==1),c90_directly_MM:=1]
mgus_surv_dup[is.na(c90_directly_MM),c90_directly_MM:=0]
mgus_surv_dup$c90_directly_MM %>% as.factor %>% summary


### (240414) drug_60days_c90만 취급한다.------
#c90 but not sMM, directly to MM
#c90_directly_MM==1, n=226
mgus_surv[c90_v193_yn==1 & (drug_60days_c90==1),c90_directly_MM:=1]
mgus_surv[is.na(c90_directly_MM),c90_directly_MM:=0]
mgus_surv$c90_directly_MM %>% as.factor %>% summary


#final sMM (among MGUS)
mgus_surv[c90_v193_yn==1 & c90_directly_MM==0,sMM_yn:=1]
mgus_surv[is.na(sMM_yn),sMM_yn:=0]
mgus_surv$sMM_yn %>% as.factor %>% summary

#no prescription among sMM: final sMM (no MM, sMM only): n=181
mgus_surv[sMM_yn==1 & is.na(first_mm_date),sMM_final_sMM:=1]
mgus_surv[is.na(sMM_final_sMM),sMM_final_sMM:=0]
mgus_surv$sMM_final_sMM %>% as.factor %>% summary

#prescription among sMM: final MM (sMM to MM): n=55
mgus_surv[sMM_yn==1 & !is.na(first_mm_date),sMM_to_MM:=1]
mgus_surv[is.na(sMM_to_MM),sMM_to_MM:=0]
mgus_surv$sMM_to_MM %>% as.factor %>% summary


mgus_surv[,status:=ifelse(c90_directly_MM==1 | sMM_to_MM==1,1,
                          ifelse(death_yn==1, 2, 0))]
mgus_surv$status %>% as.factor %>% summary #0:4579 / 1:281 / 2:813



mgus_surv[,mm_time:=ifelse(status==1 & c90_directly_MM==1, as.numeric(first_c90_date-mgus_index_date),
                           ifelse(status==1 & sMM_to_MM==1, as.numeric(first_mm_date-mgus_index_date),NA))]
mgus_surv[,time:=ifelse(status==1,mm_time,death_day)]
mgus_surv[,time_yr:=time/365.25]
# mgus_surv[,ld_yes:=as.factor(ifelse(mld_yes==1 | sld_yes==1,1,0))]

mgus_surv_tidy<-mgus_surv[,.SD,.SDcols=c("JID","status","time","time_yr","SEX_TP_CD","index_age",
                                         "mi_yes", "chf_yes", "pvd_yes", "cvd_yes", "dem_yes", "cpd_yes", 
                                         "rhe_yes", "pud_yes", "mld_yes", "dwoc_yes", "dwcc_yes", "hp_yes", 
                                         "rd_yes", "cancer_yes", "sld_yes", "mst_yes", "aids_yes", "first_mm_age","first_mm_date",
                                         "mp", "vd", "thali_only", "lenal_only", "vmp", "vtd", "other")]
mgus_surv_tidy[,status_01:=ifelse(status==1,1,0)]


# table 1 ---------------------------------

# mgus_surv_tidy <- merge(mgus_surv_tidy, mgus_surv[,c("JID","death_yn","death_year","death_day")],by="JID")
mgus_surv_tidy <- merge(mgus_surv_tidy, mgus_surv[,c("JID","death_yn","death_year_MM","death_day_MM")],by="JID")



# mgus setting
mgus_to_mm = mgus_surv_tidy


# insert #
# nrow(mgus_to_mm)
# mgus_to_mm$status %>% as.factor %>% summary
# ?write.csv
# write.csv(mgus_to_mm[mgus_to_mm$status==1,],'mgus_to_mm.csv',row.names = FALSE)
# insert end #

mgus_to_mm = mgus_to_mm %>% rename(death_year = death_year_MM, death_day = death_day_MM)

mgus_to_mm$event<-as.numeric(mgus_to_mm$death_yn)
table(mgus_to_mm$event)
# 0    1 
# 4754  919 

mgus_to_mm$group <- factor(ifelse(mgus_to_mm$status_01==0,'no-mm','mm'))
table(mgus_to_mm$group)
# mm no-mm 
# 281  5392  

# survival curve plotting #
mgus_to_mm_sub <- mgus_to_mm[mgus_to_mm$group == 'mm',] 
table(mgus_to_mm_sub$group)
# mm no-mm 
# 281     0 
# 
# # 6개월 랜드마크 #
fit_mgus_to_mm_sub <- survfit(Surv(death_year, event) ~ group, data=mgus_to_mm_sub)
dev.new()
jskm(fit_mgus_to_mm_sub, ci = T, cumhaz = F, mark = F, ystrataname = "Group", surv.scale = "percent",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - mgus_to_mm | 6mths", cut.landmark = 0.5)
ggsave("0530_Mgus_to_MM_6mths.pdf",height=7,width=10,dpi=300)
dev.off()

# 1년 랜드마크 #
dev.new()
jskm(fit_mgus_to_mm_sub, ci = T, cumhaz = F, mark = F, ystrataname = "Group", surv.scale = "percent",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - mgus_to_mm | 1yr", cut.landmark = 1)
ggsave("0530_Mgus_to_MM_1yr.pdf",height=7,width=10,dpi=300)
dev.off()

# 필요한 열만 select
mgus_to_mm_sub2 = mgus_to_mm_sub %>% dplyr::select(event, death_year, SEX_TP_CD, first_mm_age, c(ends_with('_yes')), mp, vd, thali_only, lenal_only, vmp, vtd, other)



# Case 2: sMM to MM ------
smm_to_mm <- read_sas("final_smm_cohort.sas7bdat") 

# survival curve plotting #
# smm setting
smm_to_mm$event<-as.numeric(smm_to_mm$death_yn)
table(smm_to_mm$event)
# 0    1 
# 1936 2171 

smm_to_mm$group <- factor(ifelse(smm_to_mm$mm_outcome==0,'no-mm','mm'))
table(smm_to_mm$group)
# mm no-mm 
# 1884  2223 

smm_to_mm_sub <- smm_to_mm[smm_to_mm$group == 'mm',] 
table(smm_to_mm_sub$group)
# mm no-mm 
# 1884     0 



# # 6개월 랜드마크 #
fit_smm_to_mm_sub <- survfit(Surv(death_year, event) ~ group, data=smm_to_mm_sub)
dev.new()
jskm(fit_smm_to_mm_sub, ci = T, cumhaz = F, mark = F, ystrataname = "Group", surv.scale = "percent",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - smm_to_mm | 6mths", cut.landmark = 0.5)
ggsave("0530_sMM_to_MM_6mths.pdf",height=7,width=10,dpi=300)
dev.off()

# 1년 랜드마크 #
dev.new()
jskm(fit_smm_to_mm_sub, ci = T, cumhaz = F, mark = F, ystrataname = "Group", surv.scale = "percent",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - smm_to_mm | 1yr", cut.landmark = 1)
ggsave("0530_sMM_to_MM_1yr.pdf",height=7,width=10,dpi=300)
dev.off()

smm_to_mm_sub2 = smm_to_mm_sub %>% dplyr::select(event, death_year, SEX_TP_CD, first_mm_age, c(ends_with("_yes")),mp, vd, thali_only, lenal_only, vmp, vtd, other)



# Case 3-2: denovo MM------
smm_cohort <- read_sas("denovomm.sas7bdat") 

# survival curve plotting #

# smm setting
smm_cohort$event<-as.numeric(smm_cohort$death_yn)
table(smm_cohort$event)
  


smm_cohort$group <- factor(ifelse(smm_cohort$mm_outcome==0,'no-mm','mm'))
table(smm_cohort$group)


smm_cohort_sub <- smm_cohort[smm_cohort$group == 'mm',] 
table(smm_cohort_sub$group)




# 6개월 랜드마크 #
fit_smm_cohort_sub <- survfit(Surv(death_year, event) ~ group, data=smm_cohort_sub)
dev.new()
jskm(fit_smm_cohort_sub, ci = T, cumhaz = F, mark = F, ystrataname = "Group", surv.scale = "percent",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - denovo MM | 6mths", cut.landmark = 0.5)
ggsave("0530_denovoMM_6mths.pdf",height=7,width=10,dpi=300)
dev.off()
# 
# # 1년 랜드마크 #
dev.new()
jskm(fit_smm_cohort_sub, ci = T, cumhaz = F, mark = F, ystrataname = "Group", surv.scale = "percent",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - denovo MM | 1yr", cut.landmark = 1)
ggsave("0530_denovoMM_1yr.pdf",height=7,width=10,dpi=300)
dev.off()

# select some columns #
smm_cohort_sub2 = smm_cohort_sub %>% dplyr::select(event, death_year, SEX_TP_CD, first_mm_age, c(ends_with("_yes")),mp, vd, thali_only, lenal_only, vmp, vtd, other)



# Case 4-1: Concatenate all groups ------
to_MM <- mgus_to_mm_sub2 %>% 
  mutate(group='1:Mgus_to_MM') %>% 
  bind_rows(smm_to_mm_sub2 %>% mutate(group='2:sMM_to_MM')) %>% 
  bind_rows(smm_cohort_sub2 %>% mutate(group='3:denovoMM'))

# baseline characteristics #
to_MM$ageg4 <- ifelse(to_MM$first_mm_age<60,"< 60",
                      ifelse(to_MM$first_mm_age<70,"60 - 69",
                             ifelse(to_MM$first_mm_age<80,"70 - 79",">= 80")))
to_MM$ageg4 <- factor(to_MM$ageg4, levels=c("< 60","60 - 69","70 - 79",">= 80"))

mytable(group ~ ., data=to_MM, method=3, catMethod=0)


### survival curve ###
fit_MM <- survfit(Surv(death_year, event) ~ group, data=to_MM)


# 6개월 랜드마크 #
dev.new()
jskm(fit_MM, ci = T, cumhaz = F, mark = F, surv.scale = "percent", ystrataname = "Group",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - toMM | 6mths", cut.landmark = 0.5)
ggsave("0530_toMM_6mths.pdf",height=7,width=10,dpi=300)
dev.off()

# 1년 랜드마크 #
dev.new()
jskm(fit_MM, ci = T, cumhaz = F, mark = F, surv.scale = "percent", ystrataname = "Group",
     table = T, pval =T, showpercent=T, timeby=1, main="Survival curve - toMM | 1yr", cut.landmark = 1)
ggsave("0530_toMM_1yr.pdf",height=7,width=10,dpi=300)
dev.off()


# ------

### Score 계산 ------
# Case 1: Mgus to MM ------
# SEX_TP_CD = 1 (Man) / 2 (Woman)
cci_score_mgus_to_mm <- mgus_to_mm_sub2 %>% 
  mutate(score = if_else(SEX_TP_CD == 1, 1, 0) +
           case_when(is.na(first_mm_age) ~ NA_real_,
                     first_mm_age < 60 ~ 0,
                     first_mm_age >= 60 & first_mm_age < 70 ~ 4,
                     first_mm_age >= 70 & first_mm_age < 80 ~ 8,
                     first_mm_age >= 80 ~ 12) +
           if_else(chf_yes == 1, 1, 0) +
           if_else(dem_yes == 1, 2, 0))

cci_score_mgus_to_mm %>% 
  group_by(score) %>% 
  summarise(count = n()) 
print(nrow(cci_score_mgus_to_mm))

# 기초통계량
mean(cci_score_mgus_to_mm$score);
median(cci_score_mgus_to_mm$score);
sd(cci_score_mgus_to_mm$score);
max(cci_score_mgus_to_mm$score);
min(cci_score_mgus_to_mm$score)


# Case 2: sMM to MM ------
# SEX_TP_CD = 1 (Man) / 2 (Woman)
cci_score_smm_to_mm <- smm_to_mm_sub2 %>% 
  mutate(score = if_else(SEX_TP_CD == 1, 1, 0) +
           case_when(is.na(first_mm_age) ~ NA_real_,
                     first_mm_age < 60 ~ 0,
                     first_mm_age >= 60 & first_mm_age < 70 ~ 4,
                     first_mm_age >= 70 & first_mm_age < 80 ~ 8,
                     first_mm_age >= 80 ~ 12) +
           if_else(chf_yes == 1, 1, 0) +
           if_else(dem_yes == 1, 2, 0))

cci_score_smm_to_mm %>% 
  group_by(score) %>% 
  summarise(count = n())
print(nrow(cci_score_smm_to_mm))

# 기초통계량
mean(cci_score_smm_to_mm$score);
median(cci_score_smm_to_mm$score);
sd(cci_score_smm_to_mm$score);
max(cci_score_smm_to_mm$score);
min(cci_score_smm_to_mm$score)



# Case 3-2: denovo MM------
# SEX_TP_CD = 1 (Man) / 2 (Woman)
cci_score_denovoMM <- smm_cohort_sub2 %>% 
  mutate(score = if_else(SEX_TP_CD == 1, 1, 0) +
           case_when(is.na(first_mm_age) ~ NA_real_,
                     first_mm_age < 60 ~ 0,
                     first_mm_age >= 60 & first_mm_age < 70 ~ 4,
                     first_mm_age >= 70 & first_mm_age < 80 ~ 8,
                     first_mm_age >= 80 ~ 12) +
           if_else(chf_yes == 1, 1, 0) +
           if_else(dem_yes == 1, 2, 0))

cci_score_denovoMM %>% 
  group_by(score) %>% 
  summarise(count = n())
print(nrow(cci_score_denovoMM))

# 기초통계량
mean(cci_score_denovoMM$score);
median(cci_score_denovoMM$score);
sd(cci_score_denovoMM$score);
max(cci_score_denovoMM$score);
min(cci_score_denovoMM$score)




# Case 4: when to_MM ------
# SEX_TP_CD = 1 (Man) / 2 (Woman)
cci_score_toMM <- to_MM %>% 
  mutate(score = if_else(SEX_TP_CD == 1, 1, 0) +
           case_when(is.na(first_mm_age) ~ NA_real_,
                     first_mm_age < 60 ~ 0,
                     first_mm_age >= 60 & first_mm_age < 70 ~ 4,
                     first_mm_age >= 70 & first_mm_age < 80 ~ 8,
                     first_mm_age >= 80 ~ 12) +
           if_else(chf_yes == 1, 1, 0) +
           if_else(dem_yes == 1, 2, 0))

cci_score_toMM %>% 
  group_by(score) %>% 
  summarise(count = n())
print(nrow(cci_score_toMM))

# 기초통계량
mean(cci_score_toMM$score, na.rm=TRUE);
median(cci_score_toMM$score, na.rm=TRUE);
sd(cci_score_toMM$score, na.rm=TRUE);
max(cci_score_toMM$score, na.rm=TRUE);
min(cci_score_toMM$score, na.rm=TRUE)


# ------
library(ggplot2)
library(dplyr)

### print scoring histogram ------
labels = c("mgus_to_mm","smm_to_mm","denovoMM","toMM")


i=1;i=2;i=3;i=4
for (i in 1:4){
  dev.new()
  png(paste0("0530_ScoringHist_", labels[i], ".png"),res=300, width=3000, height=1800)
  ggplot(get(paste0('cci_score_', labels[i])), aes(x= score)) +
    geom_histogram(aes(y = ..density.. ), binwidth = 1, fill='skyblue', color='#1f2d86') + 
    geom_density(color = '#fc4e07', size=1, linetype = 'dashed') +
    geom_vline(aes(xintercept = mean(score, na.rm=TRUE)), color='red', linetype = 'dotted', size=1) +
    labs(title = 'The distribution of scores', x='Scores', y='Density') +
    theme_minimal() +
    theme(plot.title = element_text(size=20, face='bold', hjust=0.5),
          axis.title.x = element_text(size=18, face='bold'),
          axis.title.y = element_text(size=18, face='bold'),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18))
  # ggsave(paste0("0530_ScoringHist_", labels[i], ".pdf"),height=7,width=10,dpi=300)
  
  dev.off()
}

#####################
### matching part using weighting ------
# mathcing using scores------
# install.packages('WeightIt')
library(WeightIt)
library(cobalt)
library(ebal)
library(CBPS)
to_MM

# before matching ------
# use to_MM ------
to_MM <- mgus_to_mm_sub2 %>% 
  mutate(group='1:Mgus_to_MM') %>% 
  bind_rows(smm_to_mm_sub2 %>% mutate(group='2:sMM_to_MM')) %>% 
  bind_rows(smm_cohort_sub2 %>% mutate(group='3:denovoMM'))

to_MM$SEX_TP_CD = as.numeric(to_MM$SEX_TP_CD)
to_MM$group <- factor(to_MM$group, levels=c('1:Mgus_to_MM','2:sMM_to_MM','3:denovoMM'), labels=c(1:3))
str(to_MM)

fit_MM <- survfit(Surv(death_year, event) ~ group, data=to_MM)
# dev.new()
# ggsurvplot(fit_MM, data=to_MM)



to_MM$group = relevel(to_MM$group, ref=3)

# use cox to compare
cox_fit <- coxph(Surv(time=death_year, event)~group, data=to_MM, robust=TRUE)
sum_cox_fit <- summary(cox_fit)
score_test <- sum_cox_fit[[13]][[1]]
score_test <- round(score_test,3)
pvalue <- sum_cox_fit[[13]][[3]]
pvalue <- if(pvalue<0.001){'<0.001'} else{round(pvalue,3)}
pvalue

# use log-rank test for p-value
surv_diff <- survdiff(Surv(death_year, event) ~ group, data=to_MM)
surv_diff

to_MM$group = factor(to_MM$group, levels=c(1:3))

# plot using 6mths landmark ------

to_MM_6mths <- subset(to_MM, !(death_year <= 0.5 & event==1))
to_MM_6mths$death_year = to_MM_6mths$death_year - 0.5
# # remove negative values
to_MM_6mths_filtered <- to_MM_6mths[to_MM_6mths$death_year >=0,]

fit_to_MM_6mths <- survfit(Surv(death_year, event) ~ group, data=to_MM_6mths_filtered)

last_times = c()
last_surv_probs = c()

for (i in 1:3){
  
  time = fit_to_MM_6mths$time[to_MM_6mths_filtered$group==i]
  n.event = fit_to_MM_6mths$n.event[to_MM_6mths_filtered$group==i]
  surv = fit_to_MM_6mths$surv[to_MM_6mths_filtered$group==i]
  
  last_time <- max(time[n.event > 0], na.rm=TRUE)
  last_surv_prob <- surv[which.max(time == last_time)]
  
  last_times <- c(last_times, last_time)
  last_surv_probs <- c(last_surv_probs, last_surv_prob)
}
last_times; last_surv_probs
last_points <- data.frame(time = last_times, surv=last_surv_probs, group=c(1,2,3))
last_points


### plotting

dev.new()
pdf('0530_ggsurvplot_6mths.pdf',height=7,width=10)
# png('0527_ggsurvplot_6mths.png',res=300, width=3000, height=1800)
?ggsurvplot
new_breaks <- c(-0.5,0, seq(0.5,7.5,1))
new_labels <- c(0,0.5,1:8)

p <- ggsurvplot(fit_to_MM_6mths,
                data=to_MM_6mths_filtered,
                surv.median.line = "hv",
                # pval=TRUE,
                risk.table=TRUE,
                risk.table.col = "strata",
                risk.table.y.text.col=FALSE,
                risk.table.y.text=TRUE,
                conf.int=TRUE,
                xlim=c(0,7.5),
                xlab="Time (years)",
                ylab="Survival Probability",
                legend.labs=c("MGUS to MM","sMM to MM","de novo MM"),
                legend.title = "Cohort",
                tables.height= 0.2,
                surv.scale="percent",
                break.time.by = 0.5)

p$plot <- p$plot +
  geom_segment(aes(x=-0.5, xend=0, y=1, yend=1), color="#619CFF",size=1) +
  scale_x_continuous(breaks=new_breaks, labels=new_labels)

p$table <- p$table + 
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

p$plot <- p$plot + ggplot2::annotate("text",
                                     x=4.8,
                                     y=0.85,
                                     label = paste0("HR = 0.6324 (MGUS to MM, p <.0001)\n",
                                                    "HR = 1.0699 (sMM to MM, p = 0.028)\n",
                                                    "(reference: de novo MM)"),
                                     hjust=0,
                                     size=5,
                                     color="black")
p
dev.off()


### Survival probability------
at_times = c(); surv_at_times = c()
Surv_Prov = function(fit, data, time_point){
  at_times = c(); surv_at_times = c(); groups = c()
  
  for (i in 1:3){
  time = fit$time[data$group==i]
  surv = fit$surv[data$group==i]
  group = paste0(i,'th group') 
  
  at_time <- max(time[time <= time_point], na.rm=TRUE)
  surv_at_time <- surv[which.max(time == at_time)]
  
  at_times <- c(at_times, at_time)
  surv_at_times <- c(surv_at_times, surv_at_time)
  groups <- c(groups, group)
}
  return(data.frame(group_var =groups, time_var = at_times, surv_var = surv_at_times))
}


Surv_Prov(fit=fit_to_MM_6mths, data=to_MM_6mths_filtered, time_point=3)
Surv_Prov(fit=fit_to_MM_6mths, data=to_MM_6mths_filtered, time_point=5)
Surv_Prov(fit=fit_to_MM_6mths, data=to_MM_6mths_filtered, time_point=10)


####################################
###### mathcing using scores------
####################################
# install.packages('WeightIt')
cci_score_toMM # <- 이거 사용
factor(cci_score_toMM$group)
library(dplyr)

scoring_MM <- copy(cci_score_toMM)


# use scoring_MM ------
scoring_MM$SEX_TP_CD = as.numeric(scoring_MM$SEX_TP_CD)
scoring_MM$group <- factor(scoring_MM$group, levels=c('1:Mgus_to_MM','2:sMM_to_MM','3:denovoMM'), labels=c(1,2,3))
str(scoring_MM)

# bind columns
scoring_MM <- scoring_MM %>% 
  rowwise() %>% 
  mutate(
    mild = if_else(mp==1 | vd==1 | thali_only==1 | lenal_only==1, 1, 0),
    intermediate = vmp,
    severe = vtd
  ) %>% 
  ungroup()



### using method=cbps ------
set.seed(1)
w.out <- weightit(group ~ score + mild + intermediate + severe, data = scoring_MM, method="cbps") # estimand='ATT';

adj_curv <- survfit(Surv(time=death_year, event)~group, data=scoring_MM, weights = w.out$weights) # robust=TRUE, <- 사용 안된다.
scoring_MM$weights = w.out$weights


# after matching ------
# dev.new()
# ggsurvplot(adj_curv, data=scoring_MM)


scoring_MM$group <- relevel(scoring_MM$group, ref=3)
# use cox to compare
?coxph
cox_fit <- coxph(Surv(time=death_year, event)~group, data=scoring_MM, robust=TRUE, weights=weights)
sum_cox_fit <- summary(cox_fit)
score_test <- sum_cox_fit[[13]][[1]]
score_test <- round(score_test,3)
pvalue <- sum_cox_fit[[13]][[3]]
pvalue <- if(pvalue<0.001){'<0.001'} else{round(pvalue,3)}
pvalue

# use log-rank test for p-value
library(survey)
design_weights <- svydesign(id=~1, strata=NULL, weights=~weights, data=scoring_MM)

log_rank <- svylogrank(Surv(time=death_year, event==1)~group, design=design_weights, rho=0)
log_rank[[2]][2] 
# p 

scoring_MM$group = factor(scoring_MM$group, levels=c(1:3))

### balance check ------
# when method = cbps ------
bal.tab(w.out, m.threshold = 0.1, un=TRUE)
bal.tab(w.out, stats=c('m','v'), thresholds=c(m=.05))

w.out$treat = factor(w.out$treat,labels=c("1:MGUS to MM",'2:sMM to MM',"3:de novo MM"))

names = c("score","mild","intermediate","severe")
for (i in 1:4){
  dev.new()
  # png("0527_balplot.png",res=300, width=3000, height=1800)
  bal.plot(w.out, var.name=names[i], which='both', grid=TRUE)
  ggsave(paste0("0530_",names[i],"_balplot.pdf"),height=7,width=10,dpi=300)
  dev.off()
}


# plot using 6mths landmark ------
set.seed(1)
scoring_MM = scoring_MM %>% 
  mutate(key=1:nrow(scoring_MM))
scoring_MM$key

scoring_MM_6mths <- subset(scoring_MM, !(death_year <= 0.5 & event==1))
scoring_MM_6mths$death_year = scoring_MM_6mths$death_year - 0.5
# # remove negative values
scoring_MM_6mths_filtered <- scoring_MM_6mths[scoring_MM_6mths$death_year >=0,]
scoring_MM_6mths_filtered$key

w_6mths <- w.out$weights[scoring_MM_6mths_filtered$key]

fit_scoring_MM_6mths <- survfit(Surv(death_year, event) ~ group, data=scoring_MM_6mths_filtered, weights= w_6mths)

last_times = c()
last_surv_probs = c()

for (i in 1:3){
  
  time = fit_scoring_MM_6mths$time[scoring_MM_6mths_filtered$group==i]
  n.event = fit_scoring_MM_6mths$n.event[scoring_MM_6mths_filtered$group==i]
  surv = fit_scoring_MM_6mths$surv[scoring_MM_6mths_filtered$group==i]
  
  last_time <- max(time[n.event > 0], na.rm=TRUE)
  last_surv_prob <- surv[which.max(time == last_time)]
  
  last_times <- c(last_times, last_time)
  last_surv_probs <- c(last_surv_probs, last_surv_prob)
}
last_times; last_surv_probs
last_points <- data.frame(time = last_times, surv=last_surv_probs, group=c(1,2,3))
last_points


dev.new()
pdf('0530_Adj_ggsurvplot_6mths.pdf',height=7,width=10)
# png('0527_Adj_ggsurvplot_6mths.png',res=300, width=3000, height=1800)
new_breaks <- c(-0.5,0, seq(0.5,7.5,1))
new_labels <- c(0,0.5,1:8)

p <- ggsurvplot(fit_scoring_MM_6mths,
                data=scoring_MM_6mths_filtered,
                surv.median.line = "hv",
                conf.int = TRUE,
                # risk.table=TRUE,
                # risk.table.col="strata",
                # risk.table.y.text.col=FALSE,
                # risk.table.y.text=TRUE,
                xlim=c(0,7.5),
                xlab="Time (years)",
                ylab = "Adjusted Survival Probability",
                legend.labs=c("MGUS to MM","sMM to MM","de novo MM"),
                legend.title = "Cohort",
                surv.scale="percent")

p$plot <- p$plot +
  geom_segment(aes(x=-0.5, xend=0, y=1, yend=1), color="#619CFF",size=1) +
  scale_x_continuous(breaks=new_breaks, labels=new_labels)

p$plot <- p$plot +ggplot2::annotate("text",x=4.8,y=0.85,
                                    label=paste0("HR = 0.575 (MGUS to MM, p <.0001)\n",
                                                 "HR = 0.9956 (sMM to MM, p = 0.903)\n",
                                                 "(reference: de novo MM)"),
                                    hjust=0, size=5)
p
dev.off()

### Survival probability------
Surv_Prov(fit=fit_scoring_MM_6mths, data=scoring_MM_6mths_filtered, time_point=3)
Surv_Prov(fit=fit_scoring_MM_6mths, data=scoring_MM_6mths_filtered, time_point=5)
Surv_Prov(fit=fit_scoring_MM_6mths, data=scoring_MM_6mths_filtered, time_point=10)




############################################################
### cuminc task ------
# first import MGUS_to_MM dataset, we should import (N=5,673) dataset
############################################################

# read data --------------------------------------
mgus_surv <- read_sas("mgus_surv_mm.sas7bdat") # finalized dataset for survival anaylsis

mgus_surv_v193<-read_sas("mgus_v_ex.sas7bdat") # 462
mgus_surv_v193$c90_v193_yn<-1

mgus_surv <- merge(mgus_surv, mgus_surv_v193[,c("JID","c90_v193_yn")],by="JID",all.x=T)
mgus_surv$c90_v193_yn <- ifelse(is.na(mgus_surv$c90_v193_yn), 0, 1)


# competing risk
# status: 1 = mm (n = ), 2 = death (n = ), 0 = censor (n = )
mgus_surv <- mgus_surv %>% 
  mutate(first_c90_date = as.Date(first_c90_date, origin = "1960-01-01"),  # SAS origin date = 1960-01-01, R = 1970-01-01
         c90_medi_date = as.Date(c90_medi_date, origin = "1960-01-01"))

mgus_surv <- as.data.table(mgus_surv)

#c90_yn==1, n=483
mgus_surv[!is.na(first_c90_date),c90_yn:=1,]
mgus_surv[is.na(c90_yn),c90_yn:=0,]
mgus_surv$c90_yn %>% as.factor %>% summary

#c90_v193_yn==1, n=462
mgus_surv$c90_v193_yn %>% as.factor %>% summary


#prescription within 60days after c90
#drug_60days_c90==1, n=226
mgus_surv[c90_v193_yn==1, drug_60days_c90:=ifelse(as.numeric(first_mm_date-first_c90_date)<=60 & c90_v193_yn==1,1,0)]
mgus_surv[is.na(drug_60days_c90),drug_60days_c90:=0]
mgus_surv$drug_60days_c90 %>% as.factor %>% summary


#death within 6months after c90
#death_6months_c90==1, n=43(when <=180)
mgus_surv[c90_v193_yn==1,death_6months_c90:=ifelse(death_yn==1 & (death_day-as.numeric(first_c90_date-mgus_index_date))<=180,1,0)]
mgus_surv[is.na(death_6months_c90),death_6months_c90:=0]
mgus_surv$death_6months_c90 %>% as.factor %>% summary


### (240414) drug_60days_c90!=1 & death_6months_c90==1을 세기------
# mgus_surv 복제
mgus_surv_dup = copy(mgus_surv)

# drug_60days_c90!=1 & death_6months_c90==1을 세기, n=14
mgus_surv_dup[c90_v193_yn==1 & (drug_60days_c90!=1 & death_6months_c90==1),c90_directly_MM:=1]
mgus_surv_dup[is.na(c90_directly_MM),c90_directly_MM:=0]
mgus_surv_dup$c90_directly_MM %>% as.factor %>% summary



### (240414) drug_60days_c90만 취급한다.------
#c90 but not sMM, directly to MM
#c90_directly_MM==1, n=226
mgus_surv[c90_v193_yn==1 & (drug_60days_c90==1),c90_directly_MM:=1]
mgus_surv[is.na(c90_directly_MM),c90_directly_MM:=0]
mgus_surv$c90_directly_MM %>% as.factor %>% summary
### ------

#final sMM (among MGUS)
mgus_surv[c90_v193_yn==1 & c90_directly_MM==0,sMM_yn:=1]
mgus_surv[is.na(sMM_yn),sMM_yn:=0]
mgus_surv$sMM_yn %>% as.factor %>% summary

#no prescription among sMM: final sMM (no MM, sMM only): n=181
mgus_surv[sMM_yn==1 & is.na(first_mm_date),sMM_final_sMM:=1]
mgus_surv[is.na(sMM_final_sMM),sMM_final_sMM:=0]
mgus_surv$sMM_final_sMM %>% as.factor %>% summary

#prescription among sMM: final MM (sMM to MM): n=55
mgus_surv[sMM_yn==1 & !is.na(first_mm_date),sMM_to_MM:=1]
mgus_surv[is.na(sMM_to_MM),sMM_to_MM:=0]
mgus_surv$sMM_to_MM %>% as.factor %>% summary


mgus_surv[,status:=ifelse(c90_directly_MM==1 | sMM_to_MM==1,1,
                          ifelse(death_yn==1, 2, 0))]
mgus_surv$status %>% as.factor %>% summary #0:4579 / 1:281 / 2:813




# mgus_surv[,mm_time:=ifelse(status==1 & c90_directly_MM==1, as.numeric(first_c90_date-mgus_index_date),
#                            ifelse(status==1 & sMM_to_MM==1, as.numeric(first_mm_date-mgus_index_date),NA))]
# mgus_surv[,time:=ifelse(status==1,mm_time,death_day)]
# mgus_surv[,time_yr:=time/365.25]

mgus_surv_tidy<-mgus_surv[,.SD,.SDcols=c("JID","status","SEX_TP_CD","index_age",
                                         "mi_yes", "chf_yes", "pvd_yes", "cvd_yes", "dem_yes", "cpd_yes", 
                                         "rhe_yes", "pud_yes", "mld_yes", "dwoc_yes", "dwcc_yes", "hp_yes", 
                                         "rd_yes", "cancer_yes", "sld_yes", "mst_yes", "aids_yes", "first_mm_age", "first_c90_date","mgus_index_date",
                                         "c90_yn","c90_v193_yn","c90_directly_MM","sMM_to_MM","c90_medi_date","death_date","first_mm_date","sMM_yn")]
mgus_surv_tidy[,status_01:=ifelse(status==1,1,0)]


# table 1 ---------------------------------

mgus_surv_tidy <- merge(mgus_surv_tidy, mgus_surv[,c("JID","death_yn","death_year","death_day")],by="JID")
# mgus_surv_tidy <- merge(mgus_surv_tidy, mgus_surv[,c("JID","death_yn","death_year_MM","death_day_MM")],by="JID")


# mgus setting
mgus_to_mm = mgus_surv_tidy

# install.packages('casebase')
# install.packages('etm')
# install.packages('prodlim')
library(prodlim)
library(casebase)
library(survival)
library(ggplot2)

mgus_to_mm$status %>% as.factor %>% summary

# idx date is mgus_index_date
mgus_to_mm <- mgus_to_mm %>% mutate(
  time_yr = case_when(
    status == 1 ~ (first_mm_date - mgus_index_date)/365.25,
    status == 0 ~ (as.Date('2022-11-30') - mgus_index_date)/365.25,
    status == 2 ~ (death_date - mgus_index_date)/365.25
  )
)

sum(is.na(mgus_to_mm$time_yr))
sum(mgus_to_mm$time_yr<0)
mgus_to_mm$time_yr = ifelse(mgus_to_mm$time_yr<0, 0, mgus_to_mm$time_yr)


mgus_to_mm$time_yr = as.numeric(mgus_to_mm$time_yr)



# 3mths landmark
mgus_to_mm <- mgus_to_mm %>% filter(time_yr >= 0.25)

ci.mgus <- cuminc(ftime=mgus_to_mm$time_yr, fstatus=mgus_to_mm$status, cencode=0)
citable <- ci.mgus$`1 1` %>% as.data.table
ci_5y <- sprintf("%.1f", citable[time>5][1,2]*100)
ci_10y <- sprintf("%.1f", citable[time>10][1,2]*100)
ci_half <- as.numeric((citable[time>10][1,2])/2)
ci_half_time <- citable[est > ci_half][1,1]

result_cmprsk <- with(mgus_to_mm, cuminc(time_yr, status, cencode=0))
ci.mgus_prod <- prodlim(Hist(time_yr, status, cens.code=0) ~ 1, data=mgus_to_mm)

dev.new()
pdf("0530_cuminc_FinalMgusCohort_to_MgusToSympto_3mths.pdf",height=7,width=10)
# png('0527_cuminc_mgus_to_mm_3mths.png',res=300, width=3000, height=1800)
plot(ci.mgus_prod,
     xlim=c(0.25,10), ylim=c(0,0.2),
     legend.x="topleft",
     legend.cex=1,
     legend.title="",
     atrisk=FALSE,
     background.horizontal = seq(0,0.2,0.05),
     col="red",
     xlab="Time (years)",
     cex.axis=1.5,
     # axis1.cex=3,
     # axis2.cex=3,
     axis2.at=seq(0,0.2,0.05),
     axis1.at=seq(0.25,10,1))
abline(v=10, col="grey", lty=2, lwd=2)
abline(v=ci_half_time, col="grey", lty=2, lwd=2)
text(10,0.15,pos=2,paste0("10 years\ncumulative incidence\n", ci_10y, " %"),cex=1)
text(ci_half_time+0.5,0.15,pos=2,paste0("Time to median\ncumulative incidence (", round(ci_half*100,1), " %)\n",round(ci_half_time,1)," years"),cex=1)
dev.off()


###### C90 CI in MGUS part
mgus_to_mm = mgus_surv_tidy

sum(!is.na(mgus_to_mm$first_c90_date))

c90_mgus = mgus_to_mm[!is.na(mgus_to_mm$first_c90_date),]

c90_mgus$status %>% as.factor %>% summary
# 0   1   2 
# 151 281  51 

# idx date is first_c90_date
c90_mgus <- c90_mgus %>% mutate(
  time_yr = case_when(
    status == 1 ~ abs(first_mm_date - first_c90_date)/365.25,
    status == 0 ~ (as.Date('2022-11-30') - first_c90_date)/365.25,
    status == 2 ~ (death_date - first_c90_date)/365.25
  )
)

sum(is.na(c90_mgus$time_yr))
sum(c90_mgus$time_yr<0)

c90_mgus$time_yr = as.numeric(c90_mgus$time_yr)



# 3mths landmark
c90_mgus <- c90_mgus %>% filter(time_yr >= 0.25)

ci.c90_mgus <- cuminc(ftime=c90_mgus$time_yr, fstatus=c90_mgus$status, cencode=0)
citable <- ci.c90_mgus$`1 1` %>% as.data.table
ci_5y <- sprintf("%.1f", citable[time>5][1,2]*100)
ci_10y <- sprintf("%.1f", citable[time>10][1,2]*100)
ci_half <- as.numeric((citable[time>10][1,2])/2)
ci_half_time <- citable[est > ci_half][1,1]

result_cmprsk <- with(c90_mgus, cuminc(time_yr, status, cencode=0))
ci.c90_prod <- prodlim(Hist(time_yr, status, cens.code=0) ~ 1, data=c90_mgus)

dev.new()
pdf("0530_cuminc_ProgressionToMM_MgusToSympto_3mths.pdf",height=7,width=10)
# png('0527_cuminc_c90_to_mm_mgus_3mths.png',res=300, width=3000, height=1800)
plot(ci.c90_prod,
     xlim=c(0.25,10), ylim=c(0,0.5),
     legend.x="topleft",
     legend.cex=1,
     legend.title="",
     atrisk=FALSE,
     background.horizontal = seq(0,0.5,0.1),
     col="red",
     xlab="Time (years)",
     cex.axis=1.5,
     # axis1.cex=3,
     # axis2.cex=3,
     axis2.at=seq(0,0.5,0.1),
     axis1.at=seq(0.25,10,1))
abline(v=10, col="grey", lty=2, lwd=2)
abline(v=ci_half_time, col="grey", lty=2, lwd=2)
text(10,0.45,pos=2,paste0("10 years\ncumulative incidence\n", ci_10y, " %"),cex=1)
text(ci_half_time+1.5,0.45,pos=2,paste0("Time to median\ncumulative incidence (", round(ci_half*100,1), " %)\n",round(ci_half_time,1)," years"),cex=1)
dev.off()


###### sMM to MM CI in MGUS part
mgus_to_mm = mgus_surv_tidy

sMM_to_MM =mgus_to_mm[mgus_to_mm$sMM_yn==1,] #236

sMM_to_MM$status %>% as.factor %>% summary
# 0   1   2 
# 138  55  43  

# idx date is first_c90_date
sMM_to_MM <- sMM_to_MM %>% mutate(
  time_yr = case_when(
    status == 1 ~ abs(first_mm_date - first_c90_date)/365.25,
    status == 0 ~ (as.Date('2022-11-30') - first_c90_date)/365.25,
    status == 2 ~ (death_date - first_c90_date)/365.25
  )
)

sum(is.na(sMM_to_MM$time_yr))
sum(sMM_to_MM$time_yr<0)

sMM_to_MM$time_yr = as.numeric(sMM_to_MM$time_yr)



# 3mths landmark
sMM_to_MM <- sMM_to_MM %>% filter(time_yr >= 0.25)

ci.sMM_to_MM <- cuminc(ftime=sMM_to_MM$time_yr, fstatus=sMM_to_MM$status, cencode=0)
citable <- ci.sMM_to_MM$`1 1` %>% as.data.table
ci_5y <- sprintf("%.1f", citable[time>5][1,2]*100)
ci_10y <- sprintf("%.1f", citable[time>10][1,2]*100)
ci_half <- as.numeric((citable[time>10][1,2])/2)
ci_half_time <- citable[est > ci_half][1,1]

result_cmprsk <- with(sMM_to_MM, cuminc(time_yr, status, cencode=0))
ci.sMM_to_MM_prod <- prodlim(Hist(time_yr, status, cens.code=0) ~ 1, data=sMM_to_MM)

dev.new()
pdf("0530_cuminc_sMM_to_MM_MGUS_3mths.pdf",height=7,width=10)
# png('0527_cuminc_sMM_to_MM_mgus_3mths.png',res=300, width=3000, height=1800)
plot(ci.sMM_to_MM_prod,
     xlim=c(0.25,10), ylim=c(0,0.5),
     legend.x="topleft",
     legend.cex=1,
     legend.title="",
     atrisk=FALSE,
     background.horizontal = seq(0,0.5,0.1),
     col="red",
     xlab="Time (years)",
     cex.axis=1.5,
     # axis1.cex=3,
     # axis2.cex=3,
     axis2.at=seq(0,0.5,0.1),
     axis1.at=seq(0.25,10,1))
abline(v=10, col="grey", lty=2, lwd=2)
abline(v=ci_half_time, col="grey", lty=2, lwd=2)
text(10,0.45,pos=2,paste0("10 years\ncumulative incidence\n", ci_10y, " %"),cex=1)
text(ci_half_time+1.5,0.45,pos=2,paste0("Time to median\ncumulative incidence (", round(ci_half*100,1), " %)\n",round(ci_half_time,1)," years"),cex=1)
dev.off()



###### sMM cohort CI------
smm_to_mm <- read_sas("final_smm_cohort.sas7bdat")
nrow(smm_to_mm) #4107

smm_to_mm$mm_outcome %>% as.factor %>% summary

smm_to_mm$status = ifelse(smm_to_mm$mm_outcome==1, 1,
                          ifelse(smm_to_mm$death_yn==1, 2, 0))
smm_to_mm$status %>% as.factor %>% summary


# idx date is first_c90_date 
smm_to_mm <- smm_to_mm %>% mutate(
  time_yr = case_when(
    status == 1 ~ (first_mm_date - first_c90_date)/365.25,
    status == 0 ~ (as.Date('2022-11-30') - first_c90_date)/365.25,
    status == 2 ~ (death_date - first_c90_date)/365.25
  )
)
sum(is.na(smm_to_mm$time_yr))
sum(is.na(smm_to_mm$time_yr<0))
smm_to_mm$time_yr = as.numeric(smm_to_mm$time_yr)


smm_to_mm <- smm_to_mm %>% filter(time_yr >= 0.25)

ci.smm <- cuminc(ftime=smm_to_mm$time_yr, fstatus=smm_to_mm$status, cencode=0)
citable <- ci.smm$`1 1` %>% as.data.table
ci_10y <- sprintf("%.1f", citable[time>10][1,2]*100)
ci_half <- as.numeric((citable[time>10][1,2])/2)
ci_half_time <- citable[est > ci_half][1,1]

result_cmprsk <- with(smm_to_mm, cuminc(time_yr, status, cencode=0))
ci.smm_prod <- prodlim(Hist(time_yr, status, cens.code = 0)~1, data=smm_to_mm)

pdf("0530_cuminc_FinalsMMCohort_sMMToMM_3mths.pdf",height=7,width=10)
# png('0527_cuminc_smm_to_mm_smm_3mths.png',res=300, width=3000, height=1800)
plot(ci.smm_prod,
     xlim=c(0.25,10),ylim=c(0,0.5),
     legend.x="topleft",
     legend.cex=1,
     legend.title="",
     atrisk=FALSE,
     background.horizontal = seq(0,0.5,0.1),
     col="red",
     xlab="Time (years)",
     cex.axis=1.5,
     axis2.at=seq(0,0.5,0.1),
     axis1.at=seq(0.25,10,1))
abline(v=10,col="grey",lty=2,lwd=2)
abline(v=ci_half_time,col="grey",lty=2,lwd=2)
text(10,0.15,pos=2,paste0("10 years\ncumulative incidence\n",ci_10y," %"),cex=1)
text(ci_half_time+2,0.15,pos=2,paste0("Time to median\ncumulative incidence (",round(ci_half*100,1)," %)\n",round(ci_half_time,1)," year"),cex=1)
dev.off()

# final sMM(n=4,107) to MM(1,884) median
median(smm_to_mm[smm_to_mm$status==1,]$time_yr)*12



###### c90_smm_mm CI in sMM part------ 
final_c90_cohort <- read_sas("smm_surv.sas7bdat") 
smm_to_mm <- read_sas("final_smm_cohort.sas7bdat")
smm_to_mm$mm_outcome %>% as.factor %>% summary #1884

final_c90_cohort$status = ifelse(final_c90_cohort$JID %in% smm_to_mm[smm_to_mm$mm_outcome==1,]$JID, 1,
                         ifelse(final_c90_cohort$death_yn==1,2,0))

final_c90_cohort$status %>% as.factor %>% summary
# 0    1    2 
# 7394 1884 8484 

final_c90_cohort <- final_c90_cohort %>% mutate(
  time_yr = case_when(
    status == 1 ~ (first_mm_date - first_c90_date)/365.25,
    status == 0 ~ (as.Date('2022-11-30') - first_c90_date)/365.25,
    status == 2 ~ (death_date - first_c90_date)/365.25
  )
)


sum(is.na(final_c90_cohort$time_yr))
sum(final_c90_cohort$time_yr<0)
# denovoMM$time_yr = ifelse(denovoMM$time_yr<0, 0, denovoMM$time_yr)
final_c90_cohort$time_yr = as.numeric(final_c90_cohort$time_yr)

# final C90(n=18,905) to MM(1,884) median
median(final_c90_cohort[final_c90_cohort$status==1,]$time_yr)*12


final_c90_cohort <- final_c90_cohort %>% filter(time_yr >= 0.25)

ci.final_c90_cohort <- cuminc(ftime=final_c90_cohort$time_yr, fstatus=final_c90_cohort$status, cencode=0)
citable <- ci.final_c90_cohort$`1 1` %>% as.data.table
ci_10y <- sprintf("%.1f", citable[time>10][1,2]*100)
ci_half <- as.numeric((citable[time>10][1,2])/2)
ci_half_time <- citable[est > ci_half][1,1]

result_cmprsk <- with(final_c90_cohort, cuminc(time_yr, status, cencode=0))
ci.smm_prod <- prodlim(Hist(time_yr, status, cens.code = 0)~1, data=final_c90_cohort)

pdf("0530_cuminc_FinalC90Cohort_sMMToMM_3mths.pdf",height=7,width=10)
# png('0527_cuminc_c90_smm_mm_sMM_3mths.png',res=300, width=3000, height=1800)
plot(ci.smm_prod,
     xlim=c(0.25,10),ylim=c(0,0.2),
     legend.x="topleft",
     legend.cex=1,
     legend.title="",
     atrisk=FALSE,
     background.horizontal = seq(0,0.2,0.05),
     col="red",
     xlab="Time (years)",
     cex.axis=1.5,
     axis2.at=seq(0,0.2,0.05),
     axis1.at=seq(0.25,10,1))
abline(v=10,col="grey",lty=2,lwd=2)
abline(v=ci_half_time,col="grey",lty=2,lwd=2)
text(10,0.15,pos=2,paste0("10 years\ncumulative incidence\n",ci_10y," %"),cex=1)
text(ci_half_time+2,0.15,pos=2,paste0("Time to median\ncumulative incidence (",round(ci_half*100,1)," %)\n",round(ci_half_time,1)," year"),cex=1)
dev.off()




###### c90_mm CI in sMM part------ 
denovoMM <- read_sas("final_smm_cohort_cci.sas7bdat") 
smm_to_mm <- read_sas("smm_to_mm_cci.sas7bdat")
denovoMM$mm_outcome %>% as.factor %>% summary #13655 
smm_to_mm$mm_outcome %>% as.factor %>% summary #1884

denovoMM$status = ifelse(denovoMM$JID %in% (smm_to_mm[smm_to_mm$mm_outcome==1,]$JID) | denovoMM$JID %in% (denovoMM[denovoMM$mm_outcome==1,]$JID), 1,
                         ifelse(denovoMM$death_yn==1,2,0))

denovoMM$status %>% as.factor %>% summary
# 0     1     2 
# 1274 15539  2092 

denovoMM <- denovoMM %>% mutate(
  time_yr = case_when(
    status == 1 ~ (first_mm_date - first_c90_date)/365.25,
    status == 0 ~ (as.Date('2022-11-30') - first_c90_date)/365.25,
    status == 2 ~ (death_date - first_c90_date)/365.25
  )
)


sum(is.na(denovoMM$time_yr))
sum(denovoMM$time_yr<0)
denovoMM$time_yr = ifelse(denovoMM$time_yr<0, 0, denovoMM$time_yr)
denovoMM$time_yr = as.numeric(denovoMM$time_yr)

# final C90(n=18,905) to MM(13,655+1,884) median
median(denovoMM[denovoMM$status==1,]$time_yr)*12

denovoMM <- denovoMM %>% filter(time_yr >= 0.25)

ci.denovoMM <- cuminc(ftime=denovoMM$time_yr, fstatus=denovoMM$status, cencode=0)
citable <- ci.denovoMM$`1 1` %>% as.data.table
ci_10y <- sprintf("%.1f", citable[time>10][1,2]*100)
ci_half <- as.numeric((citable[time>10][1,2])/2)
ci_half_time <- citable[est > ci_half][1,1]

result_cmprsk <- with(denovoMM, cuminc(time_yr, status, cencode=0))
ci.smm_prod <- prodlim(Hist(time_yr, status, cens.code = 0)~1, data=denovoMM)

pdf("0530_cuminc_FinalC90Cohort_MM_3mths.pdf",height=7,width=10)
# png('0527_cuminc_c90_mm_sMM_3mths.png',res=300, width=3000, height=1800)
plot(ci.smm_prod,
     xlim=c(0.25,10),ylim=c(0,0.5),
     legend.x="topleft",
     legend.cex=1,
     legend.title="",
     atrisk=FALSE,
     background.horizontal = seq(0,0.5,0.1),
     col="red",
     xlab="Time (years)",
     cex.axis=1.5,
     axis2.at=seq(0,0.5,0.1),
     axis1.at=seq(0.25,10,1))
abline(v=10,col="grey",lty=2,lwd=2)
abline(v=ci_half_time,col="grey",lty=2,lwd=2)
text(10,0.15,pos=2,paste0("10 years\ncumulative incidence\n",ci_10y," %"),cex=1)
text(ci_half_time+2,0.15,pos=2,paste0("Time to median\ncumulative incidence (",round(ci_half*100,1)," %)\n",round(ci_half_time,1)," year"),cex=1)
dev.off()






# median------------------------------
### mgus setting------
mgus_to_mm = mgus_surv_tidy

mgus_to_mm$status %>% as.factor %>% summary

# idx date is mgus_index_date
mgus_to_mm <- mgus_to_mm %>% mutate(
  time_yr = case_when(
    status == 1 ~ (first_mm_date - mgus_index_date)/365.25,
    status == 0 ~ (as.Date('2022-11-30') - mgus_index_date)/365.25,
    status == 2 ~ (death_date - mgus_index_date)/365.25
  )
)

sum(is.na(mgus_to_mm$time_yr))
sum(mgus_to_mm$time_yr<0)
mgus_to_mm$time_yr = ifelse(mgus_to_mm$time_yr<0, 0, mgus_to_mm$time_yr)

mgus_to_mm$time_yr = as.numeric(mgus_to_mm$time_yr)


library(dplyr)
# install.packages('lubridate')
# library(lubridate)

nrow(mgus_to_mm)

# final MGUS(n=5,673) to MM(281) median
median(mgus_to_mm[mgus_to_mm$status==1,]$time_yr)*12


c90_mgus = mgus_to_mm[!is.na(mgus_to_mm$first_c90_date),]
# Final C90(n=483) to MM(281) median
median(c90_mgus[c90_mgus$status==1,]$time_yr)*12

# Final C90(n=483) to MM(55) median
median(c90_mgus[c90_mgus$sMM_to_MM==1,]$time_yr)*12


sMM_to_MM = c90_mgus[c90_mgus$sMM_yn==1,]
# final sMM(n=236) to MM(55) median
median(sMM_to_MM[sMM_to_MM$status==1,]$time_yr)*12



### sMM setting------







