#install libraries
library(data.table)
library(dplyr)
library(alr4)
library(ggplot2)

#read in and attach the data
cf_data = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/no_qbs.csv')
attach(cf_data)

#run a regression on each success measure included
talent_m1 = lm(FPI~Talent)
summary(talent_m1)
talent_m2 = lm(SP~Talent)
summary(talent_m2)
talent_m3 = lm(F_Plus~Talent)
summary(talent_m3)

#test normality of FPI as response variable
ggplot(cf_data, aes(FPI)) +
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  stat_function(fun = dnorm,
                args = list(mean = mean(FPI),
                            sd = sd(FPI)),
                col = "red") +
  geom_density(col = 'blue')

qqnorm(FPI, pch = 16)
qqline(FPI)

#read in files for underperformers
FSU = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/FSU.csv')
Ark = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Arkansas.csv')
Neb = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Nebraska.csv')
UCLA = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/UCLA.csv')
Tenn = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Tenn.csv')

#use predict to see what values we expect for FPI in each underperformer
FSU_predicted = predict(talent_m1, newdata = FSU)
Ark_predicted = predict(talent_m1, newdata = Ark)
Neb_predicted = predict(talent_m1, newdata = Neb)
UCLA_predicted = predict(talent_m1, newdata = UCLA)
Tenn_predicted = predict(talent_m1, newdata = Tenn)

FSU$predictedFPI = FSU_predicted
Ark$predictedFPI = Ark_predicted
Neb$predictedFPI = Neb_predicted
UCLA$predictedFPI = UCLA_predicted
Tenn$predictedFPI = Tenn_predicted

#calculate the difference between the predicted FPI and the actual FPI and add it as a variable to the
# original dataframes
FSU$Diff = FSU$FPI - FSU_predicted
Ark$Diff = Ark$FPI - Ark_predicted
Neb$Diff = Neb$FPI - Neb_predicted
UCLA$Diff = UCLA$FPI - UCLA_predicted
Tenn$Diff = Tenn$FPI - Tenn_predicted

#read in files for overperformers
AppSt = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/AppSt.csv')
Memphis = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Memphis.csv')
IowaSt = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/IowaSt.csv')
UCF = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/UCF.csv')
Wisc = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Wisconsin.csv')

#use predict for overperformers
AppSt_predicted = predict(talent_m1, newdata = AppSt)
Memphis_predicted = predict(talent_m1, newdata = Memphis)
IowaSt_predicted = predict(talent_m1, newdata = IowaSt)
UCF_predicted = predict(talent_m1, newdata = UCF)
Wisc_predicted = predict(talent_m1, newdata = Wisc)

AppSt$predictedFPI = AppSt_predicted
Memphis$predictedFPI = Memphis_predicted
IowaSt$predictedFPI = IowaSt_predicted
UCF$predictedFPI = UCF_predicted
Wisc$predictedFPI = Wisc_predicted

#calculate difference for overperformers
AppSt$Diff = AppSt$FPI - AppSt_predicted
Memphis$Diff = Memphis$FPI - Memphis_predicted
IowaSt$Diff = IowaSt$FPI - IowaSt_predicted
UCF$Diff = UCF$FPI - UCF_predicted
Wisc$Diff = Wisc$FPI - Wisc_predicted

#calculate all predicted values and store the difference in cf_data
allPredicted = predict(talent_m1, newdata = cf_data)
cf_data$predictedFPI = allPredicted
cf_data$Diff = cf_data$FPI - allPredicted

#look at the distribution of difference and obtain the mean and standard dev.
hist(cf_data$Diff)
mean(cf_data$Diff)
FPI_Diff_sd = sd(cf_data$Diff)

#calculate the amount of standard deviations from the mean each team was in each season
FSU$Devs = abs(FSU$Diff/FPI_Diff_sd)
Ark$Devs = abs(Ark$Diff/FPI_Diff_sd)
Neb$Devs = abs(Neb$Diff/FPI_Diff_sd)
UCLA$Devs = abs(UCLA$Diff/FPI_Diff_sd)
Tenn$Devs = abs(Tenn$Diff/FPI_Diff_sd)

AppSt$Devs = abs(AppSt$Diff/FPI_Diff_sd)
Memphis$Devs = abs(Memphis$Diff/FPI_Diff_sd)
IowaSt$Devs = abs(IowaSt$Diff/FPI_Diff_sd)
UCF$Devs = abs(UCF$Diff/FPI_Diff_sd)
Wisc$Devs = abs(Wisc$Diff/FPI_Diff_sd)

#Create regression  models for talent predicting several key metrics to obtain expected values based on 
# a team's overall talent
m_str = lm(STR~Talent)
summary(m_str)

m_nfp = lm(NFP~Talent)
summary(m_nfp)

m_oppa = lm(offense.totalPPA~Talent)
summary(m_oppa)

m_osr = lm(offense.successRate~Talent)
summary(m_osr)

m_oapp = lm(offense.fieldPosition.averagePredictedPoints~Talent)
summary(m_oapp)

m_dppa = lm(defense.totalPPA~Talent)
summary(m_dppa)

m_dsr = lm(defense.successRate~Talent)
summary(m_dsr)

m_dapp = lm(defense.fieldPosition.averagePredictedPoints~Talent)
summary(m_dapp)

cf_data$predictedSTR = predict(m_str, newdata = cf_data)
cf_data$STR_diff = cf_data$STR - cf_data$predictedSTR

cf_data$predictedNFP = predict(m_nfp, newdata = cf_data)
cf_data$NFP_diff = cf_data$NFP - cf_data$predictedNFP

cf_data$predicted_offPPA = predict(m_oppa, newdata = cf_data)
cf_data$offPPA_diff = cf_data$offense.totalPPA - cf_data$predicted_offPPA

cf_data$predictedOSR = predict(m_osr, newdata = cf_data)
cf_data$OSR_diff = cf_data$offense.successRate - cf_data$predictedOSR

cf_data$predictedPP = predict(m_oapp, newdata = cf_data)
cf_data$PP_diff = cf_data$offense.fieldPosition.averagePredictedPoints - cf_data$predictedPP

cf_data$predicted_defPPA = predict(m_dppa, newdata = cf_data)
cf_data$defPPA_diff = cf_data$defense.totalPPA - cf_data$predicted_defPPA

cf_data$predictedDSR = predict(m_dsr, newdata = cf_data)
cf_data$DSR_diff = cf_data$defense.successRate - cf_data$predictedDSR

cf_data$predictedDPP = predict(m_dapp, newdata = cf_data)
cf_data$DPP_diff = cf_data$defense.fieldPosition.averagePredictedPoints - cf_data$predictedDPP

#clean the dataset up a little bit
cf_data1 = cf_data %>% select(V1, Team, STR, NFP, Year, offense.totalPPA, offense.successRate, 
                              offense.fieldPosition.averagePredictedPoints, defense.totalPPA, 
                              defense.successRate, defense.fieldPosition.averagePredictedPoints, 
                              F_Rank:Talent_rank, predictedFPI:DPP_diff)

#filter out the years that will be part of the analysis for each team
FSU_under = cf_data1 %>% filter(Team == 'Florida State' & Year == 2018 | Team == 'Florida State' & Year == 2019 |
                                  Team == 'Florida State' & Year == 2020)

Ark_under = cf_data1 %>% filter(Team == 'Arkansas' & Year == 2017 | Team == 'Arkansas' & Year == 2018 |
                                  Team == 'Arkansas' & Year == 2019)

Neb_under = cf_data1 %>% filter(Team == 'Nebraska' & Year == 2017)

Tenn_under = cf_data1 %>% filter(Team == 'Tennessee' & Year == 2017 | Team == 'Tennessee' & Year == 2018 |
                                   Team == 'Tennessee' & Year == 2020)

UCLA_under = cf_data1 %>% filter(Team == 'UCLA' & Year == 2017 | Team == 'UCLA' & Year == 2018 |
                                   Team == 'UCLA' & Year == 2019)

AppSt_over = cf_data1 %>% filter(Team == 'Appalachian State' & Year == 2015 | 
                                   Team == 'Appalachian State' & Year == 2016 |
                                   Team == 'Appalachian State' & Year == 2017 |
                                   Team == 'Appalachian State' & Year == 2018 |
                                   Team == 'Appalachian State' & Year == 2019 |
                                   Team == 'Appalachian State' & Year == 2020)

IowaSt_over = cf_data1 %>% filter(Team == 'Iowa State' & Year == 2017 | Team == 'Iowa State' & Year == 2020)

Memphis_over = cf_data1 %>% filter(Team == 'Memphis' & Year == 2015 | Team == 'Memphis' & Year == 2016 |
                                    Team == 'Memphis' & Year == 2017 | Team == 'Memphis' & Year == 2019)

UCF_over = cf_data1 %>% filter(Team == 'UCF' & Year == 2017 | Team == 'UCF' & Year == 2018 |
                                 Team == 'UCF' & Year == 2019 | Team == 'UCF' & Year == 2020)

Wisc_over = cf_data1 %>% filter(Team == 'Wisconsin' & Year == 2015 | Team == 'Wisconsin' & Year == 2016 |
                                  Team == 'Wisconsin' & Year == 2017 | Team == 'Wisconsin' & Year == 2019 |
                                  Team == 'Wisconsin' & Year == 2020)

#create histograms of each difference variable to check if they are normally distributed to make sure
# i can use standard deviations without converting to z-scores
hist(cf_data1$STR_diff)
hist(cf_data1$NFP_diff)
hist(cf_data1$offPPA_diff)
hist(cf_data1$OSR_diff)
hist(cf_data1$PP_diff)
hist(cf_data1$defPPA_diff)
hist(cf_data1$DSR_diff)
hist(cf_data$DPP_diff)

#obtain standard deviations of each difference variable to calculate how many deviations from the mean each
# team is
sd_STR = sd(cf_data1$STR_diff)
sd_NFP = sd(cf_data1$NFP_diff)
sd_oPPA = sd(cf_data1$offPPA_diff)
sd_OSR = sd(cf_data1$OSR_diff)
sd_PP = sd(cf_data1$PP_diff)
sd_dPPA = sd(cf_data1$defPPA_diff)
sd_DSR = sd(cf_data1$DSR_diff)
sd_DPP = sd(cf_data1$DPP_diff)

#use the sds from above to calculate deviations in each under and over performer
FSU_under$STR_devs = FSU_under$STR_diff/sd_STR
Ark_under$STR_devs = Ark_under$STR_diff/sd_STR
Neb_under$STR_devs = Neb_under$STR_diff/sd_STR
Tenn_under$STR_devs = Tenn_under$STR_diff/sd_STR
UCLA_under$STR_devs = UCLA_under$STR_diff/sd_STR

AppSt_over$STR_devs = AppSt_over$STR_diff/sd_STR
IowaSt_over$STR_devs = IowaSt_over$STR_diff/sd_STR
Memphis_over$STR_devs = Memphis_over$STR_diff/sd_STR
UCF_over$STR_devs = UCF_over$STR_diff/sd_STR
Wisc_over$STR_devs = Wisc_over$STR_diff/sd_STR

FSU_under$NFP_devs = FSU_under$NFP_diff/sd_NFP
Ark_under$NFP_devs = Ark_under$NFP_diff/sd_NFP
Neb_under$NFP_devs = Neb_under$NFP_diff/sd_NFP
Tenn_under$NFP_devs = Tenn_under$NFP_diff/sd_NFP
UCLA_under$NFP_devs = UCLA_under$NFP_diff/sd_NFP

AppSt_over$NFP_devs = AppSt_over$NFP_diff/sd_NFP
IowaSt_over$NFP_devs = IowaSt_over$NFP_diff/sd_NFP
Memphis_over$NFP_devs = Memphis_over$NFP_diff/sd_NFP
UCF_over$NFP_devs = UCF_over$NFP_diff/sd_NFP
Wisc_over$NFP_devs = Wisc_over$NFP_diff/sd_NFP

FSU_under$oPPA_devs = FSU_under$offPPA_diff/sd_oPPA
Ark_under$oPPA_devs = Ark_under$offPPA_diff/sd_oPPA
Neb_under$oPPA_devs = Neb_under$offPPA_diff/sd_oPPA
Tenn_under$oPPA_devs = Tenn_under$offPPA_diff/sd_oPPA
UCLA_under$oPPA_devs = UCLA_under$offPPA_diff/sd_oPPA

AppSt_over$oPPA_devs = AppSt_over$offPPA_diff/sd_oPPA
IowaSt_over$oPPA_devs = IowaSt_over$offPPA_diff/sd_oPPA
Memphis_over$oPPA_devs = Memphis_over$offPPA_diff/sd_oPPA
UCF_over$oPPA_devs = UCF_over$offPPA_diff/sd_oPPA
Wisc_over$oPPA_devs = Wisc_over$offPPA_diff/sd_oPPA

FSU_under$OSR_devs = FSU_under$OSR_diff/sd_OSR
Ark_under$OSR_devs = Ark_under$OSR_diff/sd_OSR
Neb_under$OSR_devs = Neb_under$OSR_diff/sd_OSR
Tenn_under$OSR_devs = Tenn_under$OSR_diff/sd_OSR
UCLA_under$OSR_devs = UCLA_under$OSR_diff/sd_OSR

AppSt_over$OSR_devs = AppSt_over$OSR_diff/sd_OSR
IowaSt_over$OSR_devs = IowaSt_over$OSR_diff/sd_OSR
Memphis_over$OSR_devs = Memphis_over$OSR_diff/sd_OSR
UCF_over$OSR_devs = UCF_over$OSR_diff/sd_OSR
Wisc_over$OSR_devs = Wisc_over$OSR_diff/sd_OSR

FSU_under$PP_devs = FSU_under$PP_diff/sd_PP
Ark_under$PP_devs = Ark_under$PP_diff/sd_PP
Neb_under$PP_devs = Neb_under$PP_diff/sd_PP
Tenn_under$PP_devs = Tenn_under$PP_diff/sd_PP
UCLA_under$PP_devs = UCLA_under$PP_diff/sd_PP

AppSt_over$PP_devs = AppSt_over$PP_diff/sd_PP
IowaSt_over$PP_devs = IowaSt_over$PP_diff/sd_PP
Memphis_over$PP_devs = Memphis_over$PP_diff/sd_PP
UCF_over$PP_devs = UCF_over$PP_diff/sd_PP
Wisc_over$PP_devs = Wisc_over$PP_diff/sd_PP

FSU_under$dPPA_devs = FSU_under$defPPA_diff/sd_dPPA
Ark_under$dPPA_devs = Ark_under$defPPA_diff/sd_dPPA
Neb_under$dPPA_devs = Neb_under$defPPA_diff/sd_dPPA
Tenn_under$dPPA_devs = Tenn_under$defPPA_diff/sd_dPPA
UCLA_under$dPPA_devs = UCLA_under$defPPA_diff/sd_dPPA

AppSt_over$dPPA_devs = AppSt_over$defPPA_diff/sd_dPPA
IowaSt_over$dPPA_devs = IowaSt_over$defPPA_diff/sd_dPPA
Memphis_over$dPPA_devs = Memphis_over$defPPA_diff/sd_dPPA
UCF_over$dPPA_devs = UCF_over$defPPA_diff/sd_dPPA
Wisc_over$dPPA_devs = Wisc_over$defPPA_diff/sd_dPPA

FSU_under$DSR_devs = FSU_under$DSR_diff/sd_DSR
Ark_under$DSR_devs = Ark_under$DSR_diff/sd_DSR
Neb_under$DSR_devs = Neb_under$DSR_diff/sd_DSR
Tenn_under$DSR_devs = Tenn_under$DSR_diff/sd_DSR
UCLA_under$DSR_devs = UCLA_under$DSR_diff/sd_DSR

AppSt_over$DSR_devs = AppSt_over$DSR_diff/sd_DSR
IowaSt_over$DSR_devs = IowaSt_over$DSR_diff/sd_DSR
Memphis_over$DSR_devs = Memphis_over$DSR_diff/sd_DSR
UCF_over$DSR_devs = UCF_over$DSR_diff/sd_DSR
Wisc_over$DSR_devs = Wisc_over$DSR_diff/sd_DSR

FSU_under$DPP_devs = FSU_under$DPP_diff/sd_DPP
Ark_under$DPP_devs = Ark_under$DPP_diff/sd_DPP
Neb_under$DPP_devs = Neb_under$DPP_diff/sd_DPP
Tenn_under$DPP_devs = Tenn_under$DPP_diff/sd_DPP
UCLA_under$DPP_devs = UCLA_under$DPP_diff/sd_DPP

AppSt_over$DPP_devs = AppSt_over$DPP_diff/sd_DPP
IowaSt_over$DPP_devs = IowaSt_over$DPP_diff/sd_DPP
Memphis_over$DPP_devs = Memphis_over$DPP_diff/sd_DPP
UCF_over$DPP_devs = UCF_over$DPP_diff/sd_DPP
Wisc_over$DPP_devs = Wisc_over$DPP_diff/sd_DPP

write.csv(Ark_under, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Ark_under.csv')
write.csv(FSU_under, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/FSU_under.csv')
write.csv(Neb_under, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Neb_under.csv')
write.csv(Tenn_under, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Tenn_under.csv')
write.csv(UCLA_under, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/UCLA_under.csv')
write.csv(AppSt_over, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/AppSt_over.csv')
write.csv(IowaSt_over, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/IowaSt_over.csv')
write.csv(Memphis_over, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Memphis_over.csv')
write.csv(UCF_over, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/UCF_over.csv')
write.csv(Wisc_over, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Wisc_over.csv')

#Data calculations for App State
mean(AppSt_over$Diff)
mean(AppSt_over$Diff)/sd(cf_data1$Diff)
mean(AppSt_over$dPPA_devs)
mean(AppSt_over$oPPA_devs)

#data calculations for Memphis
mean(Memphis_over$Diff)
mean(Memphis$Diff)

#data calculations for Wisc
mean(Wisc_over$Diff)
mean(UCF_over$Diff)
mean(IowaSt_over$Diff)

#data calculations for arkansas
mean(Ark_under$Diff)
mean(FSU_under$Diff)
mean(Neb$Diff)
mean(Tenn_under$Diff)
mean(UCLA_under$Diff)

#data calculations for FSU


