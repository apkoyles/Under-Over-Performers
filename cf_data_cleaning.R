library(data.table)
library(dplyr)
success_metrics = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/CF_Success_Metrics.csv')
success_metrics[Talent == 872.51, Talent := 572.51]

Adv2015 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2015_AdvCFB.csv')
Adv2016 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2016_AdvCFB.csv')
Adv2017 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2017_AdvCFB.csv')
Adv2018 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2018_AdvCFB.csv')
Adv2019 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2019_AdvCFB.csv')
Adv2020 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2020_AdvCFB.csv')

advCFB = rbind(Adv2015, Adv2016, Adv2017, Adv2018, Adv2019, Adv2020)

QB2015 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2015_CFBQB.csv')
QB2016 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2016_CFBQB.csv')
QB2017 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2017_CFBQB.csv')
QB2018 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2018_CFBQB.csv')
QB2019 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2019_CFBQB.csv')
QB2020 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2020_CFBQB.csv')

qbs = rbind(QB2015, QB2016, QB2017, QB2018, QB2019, QB2020)

st2015 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2015_CFBSTR.csv')
st2015$Year = 2015
st2016 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2016_CFBSTR.csv')
st2016$Year = 2016
st2017 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2017_CFBSTR.csv')
st2017$Year = 2017
st2018 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2018_CFBSTR.csv')
st2018$Year = 2018
st2019 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2019_CFBSTR.csv')
st2019$Year = 2019
st2020 = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/2020_CFBSTR.csv')
st2020$Year = 2020

st = rbind(st2015, st2016, st2017, st2018, st2019, st2020)
st1 = subset(st1, select = -Rk)

write.csv(st1, 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/special_teams_total.csv')
write.csv(qbs, 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/QB_total.csv')
write.csv(advCFB, 'Desktop/Research/CFP/UnderOverPerformers/Raw Data/advCFB_total.csv')

qb_cleaned = qbs %>% select(season:averagePPA.rush)

advCFB_cleaned = advCFB %>% select(season:conference, offense.ppa:offense.explosiveness, 
                                   offense.pointsPerOpportunity:offense.fieldPosition.averagePredictedPoints,
                                   defense.ppa:defense.explosiveness, defense.stuffRate, 
                                   defense.pointsPerOpportunity:defense.fieldPosition.averagePredictedPoints)

st_cleaned = st1 %>% select(Team, STR, PRE, PE, NFP, Year)

write.csv(st_cleaned, 'Desktop/Research/CFP/UnderOverPerformers/Cleaned Data Sets/spec_teams_clean.csv')
write.csv(advCFB_cleaned, 'Desktop/Research/CFP/UnderOverPerformers/Cleaned Data Sets/adv_cleaned.csv')
write.csv(qb_cleaned, 'Desktop/Research/CFP/UnderOverPerformers/Cleaned Data Sets/qb_cleaned.csv')

qb_cleaned = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Cleaned Data Sets/qb_cleaned.csv')
advCFB_cleaned = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Cleaned Data Sets/adv_cleaned.csv')
st_cleaned = fread(file = 'Desktop/Research/CFP/UnderOverPerformers/Cleaned Data Sets/spec_teams_clean.csv')

stqb = full_join(st_cleaned, qb_cleaned, by = c('Year' = 'season', 'Team' = 'team'))
adv_stqb = inner_join(stqb, advCFB_cleaned, by = c('Team' = 'team', 'Year' = 'season'))
full_data = inner_join(adv_stqb, success_metrics, by = c('Team', 'Year'))
adv_st = inner_join(st_cleaned, advCFB_cleaned, by = c('Team' = 'team', 'Year' = 'season'))
full_data_no_qbs = inner_join(adv_st, success_metrics, by = c('Team', 'Year'))

write.csv(full_data, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/full_data.csv')
write.csv(full_data_no_qbs, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/no_qbs.csv')


unders = full_data_no_qbs %>% filter(Team == 'Florida State' | Team == 'Arkansas' | Team == 'UCLA' | 
                                       Team == 'Nebraska' | Team == 'Tennessee')

FSU = full_data_no_qbs %>% filter(Team == 'Florida State')
Ark = full_data_no_qbs %>% filter(Team == 'Arkansas')
UCLA = full_data_no_qbs %>% filter(Team == 'UCLA')
Neb = full_data_no_qbs %>% filter(Team == 'Nebraska')
Tenn = full_data_no_qbs %>% filter(Team == 'Tennessee')

overs = full_data_no_qbs %>% filter(Team == 'Appalachian State' | Team == 'UCF' | Team == 'Wisconsin'
                                    | Team == 'Iowa State' | Team == 'Memphis')

AppSt = full_data_no_qbs %>% filter(Team == 'Appalachian State')
UCF = full_data_no_qbs %>% filter(Team == 'UCF')
Wisc = full_data_no_qbs %>% filter(Team == 'Wisconsin')
ISU = full_data_no_qbs %>% filter(Team == 'Iowa State')
Memp = full_data_no_qbs %>% filter(Team == 'Memphis')

write.csv(unders, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/underperformers.csv')
write.csv(overs, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/overperformers.csv')
write.csv(FSU, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/FSU.csv')
write.csv(Ark, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Arkansas.csv')
write.csv(UCLA, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/UCLA.csv')
write.csv(Neb, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Nebraska.csv')
write.csv(Tenn, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Tenn.csv')
write.csv(AppSt, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/AppSt.csv')
write.csv(UCF, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/UCF.csv')
write.csv(ISU, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/IowaSt.csv')
write.csv(Wisc, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Wisconsin.csv')
write.csv(Memp, 'Desktop/Research/CFP/UnderOverPerformers/Datasets/Memphis.csv')

qb_sorted = qb_cleaned %>% arrange(team)

write.csv(qb_sorted, 'Desktop/Research/CFP/UnderOverPerformers/Cleaned Data Sets/qb_sorted.csv')
