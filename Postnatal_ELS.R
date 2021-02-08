#This script replicates Cecil et al. (2014) cumulative environmental risk scores, adapting it from SPSS to R

# POSTNATAL 8-9Y 

# 1. LIFE EVENTS
LE_interview <- readquick("insert ALSPAC data") # insert nr of obs and nr of columns

# Exclude unreliable interviews
LE_interview <- subset(LE_interview, LE_interview$unreliable == 0) # need to see ALSPAC data for the corresponding code here

# Select the necessary item 
# LE_interview to be replaced with ALSPAC equivalent

#CS_LE_9Y = cumulative score for life events at 9 years (mother-based)

CS_LE_9Y <- data.frame(LE_interview$?, # child id to be added 
                       LE_interview$p2000_rec, # Husband/partner died since the study child's 6th birthday 
                       LE_interview$p2001_rec, # One of mother's children died since the study child's 6th birthday
                       LE_interview$p2002_rec, # Mother's friend or relative died since the study child's 6th birthday
                       LE_interview$p2003_rec, # One of mother's children was ill since the study child's 6th birthday
                       LE_interview$p2004_rec, # Mother's husband/partner was ill since the study child's 6th birthday
                       LE_interview$p2005_rec, # Mother's friend or relative was ill since the study child's 6th birthday
                       LE_interview$p2006_rec, # Mother was admitted to hospital since the study child's 6th birthday
                       LE_interview$p2010_rec, # Mother was very ill since the study child's 6th birthday
                       LE_interview$p2011_rec, # Mother's husband/partner lost his job since the study child's 6th birthday
                       LE_interview$p2012_rec, # Mother's husband/partner had problems at work since the study child's 6th birthday
                       LE_interview$p2013_rec, # Mother had problems at work since the study child's 6th birthday
                       LE_interview$p2014_rec, # Mother lost her job since the study child's 6th birthday
                       LE_interview$p2021_rec, # Mother moved house since the study child's 6th birthday
                       LE_interview$p2030_rec, # Mother became pregnant since the study child's 6th birthday
                       LE_interview$p2031_rec, # Mother started a new job since the study child's 6th birthday
                       LE_interview$p2032_rec, # Mother returned to work since the study child's 6th birthday
                       LE_interview$p2033_rec, # Mother had a miscarriage since the study child's 6th birthday
                       LE_interview$p2035_rec, # Mother took an examination since the study child's 6th birthday
                       LE_interview$p2039_rec, # Mother's house or car was burgled since the study child's 6th birthday
                       LE_interview$p2041_rec, # One of mother's children started school since the study child's 6th birthday
                       LE_interview$p2042_rec, # Mother's husband/partner started a new job since the study child's 6th birthday
                       LE_interview$p2043_rec, # A pet died since the study child's 6th birthday
                       LE_interview$p2044_rec) # Mother had an accident since the study child's 6th birthday


colnames(CS_LE_9Y) <- c("IDC","husband/partner_died","m_children_died","m_friend/relative_died","m_children_ill","m_friend/relative_ill","m_admitted_hospital","m_very_ill",
                           "m_husband/partner_lost_job","m_husband/partner_problems_work","m_problems_work ", "m_lost_job","m_moved_house","m_became_pregnant","m_new_job",
                           "m_returned_work","m_miscarriage","m_examination","m_house/car_burgled","m_child_start_school","m_husband/partner_new_job","pet_died",
                           "m_accident")


# CS_LEChild_9Y = cumulative score for life events at 9 years (child-based)

CS_LEChild_9Y <- data.frame(kt5000a_rec, # Since 7th birthday child has been taken into care
                                    kt5001a_rec, # Since 7th birthday child's pet died
                                    kt5002a_rec, # Since 7th birthday child moved home
                                    kt5003a_rec, # Since 7th birthday child had shock or fright
                                    kt5006a_rec, # Since 7th birthday child has had someone in family die
                                    kt5007a_rec, # Since 7th birthday child has been separated from mother
                                    kt5008a_rec, # Since 7th birthday child has been separated from mother
                                    kt5009a_rec, # Since 7th birthday child has been separated from father
                                    kt5010a_rec, # Since 7th birthday child has had a new mother or father
                                    kt5011a_rec, # Since 7th birthday child has had a new brother or sister
                                    kt5012a_rec, # Since 7th birthday child has been admitted to hospital
                                    kt5013a_rec, # Since 7th birthday child has been separated from someone else
                                    kt5014a_rec, # Since 7th birthday child has started a new school
                                    kt5015a_rec) # Since 7th birthday child has lost their best friend)


####################################################################################################################################################

# 2. CONTEXTUAL RISKS

# CS_8_10_ContextualRisk = cumulative score from 8-10 years for contextual risk 

CS_8_10_ContextualRisks <- data.frame(p2018_rec, # Mother's income was reduced since the study child's 6th birthday
                                     p2023_rec, # Mother became homeless since the study child's 6th birthday
                                     p2024_rec) # Mother had a major financial problem since the study child's 6th birthday

####################################################################################################################################################

# 3. MH/LIFESTYLE RISKS 

# CS_8_10_MH_LifestyleRisks = cumulative score from 8-10 years for MH/Lifestyle Risks 

CS_8_10_MH_LifestyleRisks <- data.frame(p2007_rec, # Mother was in trouble with the law since the study child's 6th birthday
                                        p2016_rec, # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
                                        p2028_rec, # Mother attempted suicide since the study child's 6th birthday
                                        p2029_rec, # Mother was convicted of an offence since the study child's 6th birthday
                                        p2034_rec) # Mother had an abortion since the study child's 6th birthday

####################################################################################################################################################

# 4. FAMILY RISKS/DIS

# CS_8_10_DIS_FamilyRisks = cumulative score from 8-10 years for Family risks/DIS

CS_8_10_DIS_FamilyRisks <- data.frame(p2008_rec, # Mother was divorced since the study child's 6th birthday
                                      p2009_rec, # Mother found out that her husband/partner didn't want her child since the study child's 6th birthday
                                      p2015_rec, # Mother's husband/partner went away since the study child's 6th birthday
                                      p2017_rec, # Mother and husband/partner separated since the study child's 6th birthda
                                      p2019_rec, # Mother argued with husband/partner since the study child's 6th birthday
                                      p2020_rec, # Mother argued with family and friends since the study child's 6th birthday
                                      p2022_rec, # Mother's husband/partner was physically cruel to her since the study child's 6th birthday
                                      p2025_rec, # Mother got married since the study child's 6th birthday
                                      p2026_rec, # Mother's husband/partner was physically cruel to her children since the study child's 6th birthday
                                      p2027_rec, # Mother was physically cruel to her children since the study child's 6th birthday
                                      p2036_rec, # Mother's husband/partner was emotionally cruel to her since the study child's 6th birthday
                                      p2037_rec, # Mother's husband/partner was emotionally cruel to her children since the study child's 6th birthday
                                      p2038_rec, # Mother was emotionally cruel to her children since the study child's 6th birthday
                                      p2040_rec, # Mother found a new partner since the study child's 6th birthday
                                      DV_Shouted_9Y, # Mother/husband/partner shouted or called one another names in the past 3 months (original, non-dichotomised variable: p3153)
                                      DV_Hit_9Y # Mother/husband/partner hit or slapped one another in the past 3 months (original, non-dichotomised variable: p3154)
                                      DV_Break_9Y # Mother/husband/partner threw or broke things in the past 3 months (original, non-dichotomised variable: p3155)
                                      Par_Smack_9Y_Any) # Child is slapped or hit (original, non-dichotomised variable: ku298)

####################################################################################################################################################

# 5. DIRECT VICTIMIZATION

# CS_8_10_DirectVictim = cumulative score from 8-10 years for direct victimization

CS_8_10_DirectVictim <- data.frame(kt5004a_rec, # Since 7th birthday child has been physically hurt by someone
                                   kt5005a_rec, # Since 7th birthday child has been sexually abused
                                   SDQBullied_8_YN, # Child is picked on or bullied by other children in past 6 months (original variable: ku698) 
                                   RelationalVictim_8yrs_Recoded, # Bullying, child is a relational victim (original variable: F8f8fp475)
                                   OvertVictim_8yrs_Recoded) # Bullying, child is an overt victim (original variable: f8fp470)


####################################################################################################################################################





