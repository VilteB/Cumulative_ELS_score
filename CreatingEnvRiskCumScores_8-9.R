


####################################################################################################################################################

# POSTNATAL 8-9 years 

####################################################################################################################################################

# The following script builds a dataset with all variables and domain scores used in
# the creation of prenatal cumulative risk score by Cecil et al. (2014). 
# It adapts the scripts developed by S. Defina where she created a similar score for Generation R. 

# The script contains the following functions: readquick, percent_missing and domainscore
# You can find them at SereDef GitHub account (https://github.com/SereDef/cumulative-ELS-score) in 'Setup_and_functions.R'.

####################################################################################################################################################


# first, exclude duplicated values (based on first 4 column names; keep first and remove second occurence)
alspac.table = alspac.table %>% distinct(aln,qlet,kz021,ff1ms100, .keep_all = T) # I copied this from prenatal but not sure whether it applies for postnatal as well

# remote siblings (all qlet=B)
alspac.table=alspac.table[alspac.table$qlet=="A",] # copied from prenatal 

# check if any duplicated ALNs left
table(duplicated(alspac.table$aln)) # copied from prenatal 

####################################################################################################################################################


# 1. LIFE EVENTS

# Steps 1-5 assign variables to the pertinent ELS domains (LE/CR/PR/IR/DV) 

# Postnatal_LifeEvents_9Y_mum = cumulative score for life events at 9 years (mother-based)

Postnatal_LifeEvents_9Y_mum <-  data.frame(alspac.table[,c("aln", # add child ID here
                           "p2000_rec", # Husband/partner died since the study child's 6th birthday 
                           "p2001_rec", # One of mother's children died since the study child's 6th birthday
                           "p2002_rec", # Mother's friend or relative died since the study child's 6th birthday
                           "p2003_rec", # One of mother's children was ill since the study child's 6th birthday
                           "p2004_rec", # Mother's husband/partner was ill since the study child's 6th birthday
                           "p2005_rec", # Mother's friend or relative was ill since the study child's 6th birthday
                           "p2006_rec", # Mother was admitted to hospital since the study child's 6th birthday
                           "p2010_rec", # Mother was very ill since the study child's 6th birthday
                           "p2011_rec", # Mother's husband/partner lost his job since the study child's 6th birthday
                           "p2012_rec", # Mother's husband/partner had problems at work since the study child's 6th birthday
                           "p2013_rec", # Mother had problems at work since the study child's 6th birthday
                           "p2014_rec", # Mother lost her job since the study child's 6th birthday
                           "p2021_rec", # Mother moved house since the study child's 6th birthday
                           "p2030_rec", # Mother became pregnant since the study child's 6th birthday
                           "p2031_rec", # Mother started a new job since the study child's 6th birthday
                           "p2032_rec", # Mother returned to work since the study child's 6th birthday
                           "p2033_rec", # Mother had a miscarriage since the study child's 6th birthday
                           "p2035_rec", # Mother took an examination since the study child's 6th birthday
                           "p2039_rec", # Mother's house or car was burgled since the study child's 6th birthday
                           "p2041_rec", # One of mother's children started school since the study child's 6th birthday
                           "p2042_rec", # Mother's husband/partner started a new job since the study child's 6th birthday
                           "p2043_rec", # A pet died since the study child's 6th birthday
                           "p2044_rec")]) # Mother had an accident since the study child's 6th birthday


# colnames(Postnatal_LifeEvents_9Y_mum) <- c("IDC","husband/partner_died","m_children_died","m_friend/relative_died","m_children_ill","m_friend/relative_ill","m_admitted_hospital","m_very_ill",
                           "m_husband/partner_lost_job","m_husband/partner_problems_work","m_problems_work ", "m_lost_job","m_moved_house","m_became_pregnant","m_new_job",
                           "m_returned_work","m_miscarriage","m_examination","m_house/car_burgled","m_child_start_school","m_husband/partner_new_job","pet_died",
                           "m_accident") # As an example, I have added more intuitive variable names here to align with Serena's scripts. 
                           # Do you prefer to rename all of the variables included in the ELS score or would we rather keep the original names?


# Postnatal_LifeEvents_9Y_child = cumulative score for life events at 9 years (child-based)

Postnatal_LifeEvents_9Y_child <- data.frame(alspac.table[,c("aln", # add child ID here
                                            "kt5000a_rec", # Since 7th birthday child has been taken into care
                                            "kt5001a_rec", # Since 7th birthday child's pet died
                                            "kt5002a_rec", # Since 7th birthday child moved home
                                            "kt5003a_rec", # Since 7th birthday child had shock or fright
                                            "kt5006a_rec", # Since 7th birthday child has had someone in family die
                                            "kt5007a_rec", # Since 7th birthday child has been separated from mother
                                            "kt5008a_rec", # Since 7th birthday child has been separated from mother
                                            "kt5009a_rec", # Since 7th birthday child has been separated from father
                                            "kt5010a_rec", # Since 7th birthday child has had a new mother or father
                                            "kt5011a_rec", # Since 7th birthday child has had a new brother or sister
                                            "kt5012a_rec", # Since 7th birthday child has been admitted to hospital
                                            "kt5013a_rec", # Since 7th birthday child has been separated from someone else
                                            "kt5014a_rec", # Since 7th birthday child has started a new school
                                            "kt5015a_rec")]) # Since 7th birthday child has lost their best friend)
                          
####################################################################################################################################################

# 2. CONTEXTUAL RISKS

# Postnatal_8_10_ContextualRisks = cumulative score from 8-10 years for contextual risk 

Postnatal_8_10_ContextualRisks <- data.frame(alspac.table[,c("aln", # add child ID here,
                                      "p2018_rec", # Mother's income was reduced since the study child's 6th birthday
                                      "p2023_rec", # Mother became homeless since the study child's 6th birthday
                                      "p2024_rec")]) # Mother had a major financial problem since the study child's 6th birthday

####################################################################################################################################################

# 3. PARENTAL RISKS

# Postnatal_8_10_ParentalRisks = cumulative score from 8-10 years for MH/Lifestyle Risks 

Postnatal_8_10_ParentalRisks <- data.frame(alspac.table[,c("aln", # add child ID here,
                                        "p2007_rec", # Mother was in trouble with the law since the study child's 6th birthday
                                        "p2016_rec", # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
                                        "p2028_rec", # Mother attempted suicide since the study child's 6th birthday
                                        "p2029_rec", # Mother was convicted of an offence since the study child's 6th birthday
                                        "p2034_rec")]) # Mother had an abortion since the study child's 6th birthday

####################################################################################################################################################

# 4. INTERPERSONAL RISKS

# Postnatal_8_10_InterersonalRisks = cumulative score from 8-10 years for Family risks/DIS

Postnatal_8_10_InterpersonalRisks <- data.frame(alspac.table[,c("aln", # add child ID here
                                      "p2008_rec", # Mother was divorced since the study child's 6th birthday
                                      "p2009_rec", # Mother found out that her husband/partner didn't want her child since the study child's 6th birthday
                                      "p2015_rec", # Mother's husband/partner went away since the study child's 6th birthday
                                      "p2017_rec", # Mother and husband/partner separated since the study child's 6th birthda
                                      "p2019_rec", # Mother argued with husband/partner since the study child's 6th birthday
                                      "p2020_rec", # Mother argued with family and friends since the study child's 6th birthday
                                      "p2022_rec", # Mother's husband/partner was physically cruel to her since the study child's 6th birthday
                                      "p2025_rec", # Mother got married since the study child's 6th birthday
                                      "p2026_rec", # Mother's husband/partner was physically cruel to her children since the study child's 6th birthday
                                      "p2027_rec", # Mother was physically cruel to her children since the study child's 6th birthday
                                      "p2036_rec", # Mother's husband/partner was emotionally cruel to her since the study child's 6th birthday
                                      "p2037_rec", # Mother's husband/partner was emotionally cruel to her children since the study child's 6th birthday
                                      "p2038_rec", # Mother was emotionally cruel to her children since the study child's 6th birthday
                                      "p2040_rec")]) # Mother found a new partner since the study child's 6th birthday
                                      # DV_Shouted_9Y, # Mother/husband/partner shouted or called one another names in the past 3 months (original, non-dichotomised variable: p3153)
                                      # DV_Hit_9Y # Mother/husband/partner hit or slapped one another in the past 3 months (original, non-dichotomised variable: p3154)
                                      # DV_Break_9Y # Mother/husband/partner threw or broke things in the past 3 months (original, non-dichotomised variable: p3155)
                                      # Par_Smack_9Y_Any)]) # Child is slapped or hit (original, non-dichotomised variable: ku298)

####################################################################################################################################################

# 5. DIRECT VICTIMIZATION

# Postnatal_8_10_DirectVictim = cumulative score from 8-10 years for direct victimization

Postnatal_8_10_DirectVictim <- data.frame(data.frame(alspac.table[,c("aln", # add child ID here
                                   "kt5004a_rec", # Since 7th birthday child has been physically hurt by someone
                                   "kt5005a_rec")]) # Since 7th birthday child has been sexually abused
                                   # SDQBullied_8_YN, # Child is picked on or bullied by other children in past 6 months (original variable: ku698) 
                                   # RelationalVictim_8yrs_Recoded, # Bullying, child is a relational victim (original variable: f8fp475)
                                   # OvertVictim_8yrs_Recoded)]) # Bullying, child is an overt victim (original variable: f8fp470)


####################################################################################################################################################


# 6. CREATE POSTNATAL CUMULATIVE RISK SCORE 

# This function merges together all prenatal ELS domains according to the 'aln' column 
# results in a dataframe with all relevant items for prenatal stress.
# tech-tip from Serena: use Reduce because merge can only take two dataframes at a time 

postnatal_stress_8_9Y <- Reduce(function(x,y) merge(x = x, y = y, by = 'aln',  all.x = TRUE), 
                          list(Postnatal_LifeEvents_9Y_mum,
                               Postnatal_LifeEvents_9Y_child,
                               Postnatal_8_10_ContextualRisks,
                               Postnatal_8_10_ParentalRisks,
                               Postnatal_8_10_InterpersonalRisks,
                               Postnatal_8_10_DirectVictim)

# ####################################################################################################################################################

# 7. SUMMARY STATISTICS 

# Let's have a look at risk distribution and missing data per indicator (as per Serena's script) 

postnatal_summary_8_9Y = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 2:ncol(postnatal_stress_8_9Y)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(postnatal_stress_8_9Y[,i]))
  c = colnames(postnatal_stress_8_9Y)[i]
  postnatal_summary_8_9Y[1:3,c] <- s
  postnatal_summary_8_9Y[4,c] <- round((postnatal_summary_8_9Y[2,c] / 11906)*100, 2)
  postnatal_summary_8_9Y[5,c] <- round((postnatal_summary_8_9Y[3,c] / 11906)*100, 2)
}

####################################################################################################################################################

# 8. MISSINGNESS

# Calculate the percentage missing data (obtained from SereDef 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score  
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }


# Apply the percent_missing function to the rows (1) of the entire dataset 

postnatal_stress_8_9Y$post_percent_missing = apply(postnatal_stress_8_9Y[,2:ncol(postnatal_stress_8_9Y)], # Same as above, if not merged with child_id, count from 2.
                                            1, percent_missing)


####################################################################################################################################################

# 9. CREATE UN-WEIGHTED DOMAIN SCORES 

# ATTENTION! Here we use the default argument of domainscore function: calculating  a 
# *mean domain score* (range = 0 to 1) that is NOT adjusted for 25% missingness as in 
# e.g. Rijlaarsdam et al. (2016). If you prefer working with the actual number of risks
# (i.e. sum score) or the weighted version of it, you can set the argument score_type
# to 'sum_simple' or 'sum_weighted' respectively (see 'Setup_and_functions.R' script
# for calculation details at https://github.com/SereDef/cumulative-ELS-score). 


# BEFORE RUNNING THE SCRIPT BELOW MAKE SURE YOU OBTAINED THE 'DOMAINSCORE' FUNCTION FROM SEREDEF
# 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score 
# Running the domainscore function will add two extra columns to prenatal_stress dataframe: 
# one containing the % of missingnes per participant and the other containing the domain score 

# LIFE EVENTS

postnatal_stress_8_9Y[,c('post_LE_percent_missing','post_life_events')] <- domainscore(postnatal_stress_8_9Y[,c(
      "p2000_rec", # Husband/partner died since the study child's 6th birthday 
      "p2001_rec", # One of mother's children died since the study child's 6th birthday
      "p2002_rec", # Mother's friend or relative died since the study child's 6th birthday
      "p2003_rec", # One of mother's children was ill since the study child's 6th birthday
      "p2004_rec", # Mother's husband/partner was ill since the study child's 6th birthday
      "p2005_rec", # Mother's friend or relative was ill since the study child's 6th birthday
      "p2006_rec", # Mother was admitted to hospital since the study child's 6th birthday
      "p2010_rec", # Mother was very ill since the study child's 6th birthday
      "p2011_rec", # Mother's husband/partner lost his job since the study child's 6th birthday
      "p2012_rec", # Mother's husband/partner had problems at work since the study child's 6th birthday
      "p2013_rec", # Mother had problems at work since the study child's 6th birthday
      "p2014_rec", # Mother lost her job since the study child's 6th birthday
      "p2021_rec", # Mother moved house since the study child's 6th birthday
      "p2030_rec", # Mother became pregnant since the study child's 6th birthday
      "p2031_rec", # Mother started a new job since the study child's 6th birthday
      "p2032_rec", # Mother returned to work since the study child's 6th birthday
      "p2033_rec", # Mother had a miscarriage since the study child's 6th birthday
      "p2035_rec", # Mother took an examination since the study child's 6th birthday
      "p2039_rec", # Mother's house or car was burgled since the study child's 6th birthday
      "p2041_rec", # One of mother's children started school since the study child's 6th birthday
      "p2042_rec", # Mother's husband/partner started a new job since the study child's 6th birthday
      "p2043_rec", # A pet died since the study child's 6th birthday
      "p2044_rec", # Mother had an accident since the study child's 6th birthday
      "kt5000a_rec", # Since 7th birthday child has been taken into care
      "kt5001a_rec", # Since 7th birthday child's pet died
      "kt5002a_rec", # Since 7th birthday child moved home
      "kt5003a_rec", # Since 7th birthday child had shock or fright
      "kt5006a_rec", # Since 7th birthday child has had someone in family die
      "kt5007a_rec", # Since 7th birthday child has been separated from mother
      "kt5008a_rec", # Since 7th birthday child has been separated from mother
      "kt5009a_rec", # Since 7th birthday child has been separated from father
      "kt5010a_rec", # Since 7th birthday child has had a new mother or father
      "kt5011a_rec", # Since 7th birthday child has had a new brother or sister
      "kt5012a_rec", # Since 7th birthday child has been admitted to hospital
      "kt5013a_rec", # Since 7th birthday child has been separated from someone else
      "kt5014a_rec", # Since 7th birthday child has started a new school
      "kt5015a_rec")]) # Since 7th birthday child has lost their best friend


# CONTEXTUAL RISKS

postnatal_stress_8_9Y[,c('post_CR_percent_missing','post_contextual_risk')] <- domainscore(postnatal_stress_8_9Y[,c(
      "p2018_rec", # Mother's income was reduced since the study child's 6th birthday
      "p2023_rec", # Mother became homeless since the study child's 6th birthday
      "p2024_rec")]) # Mother had a major financial problem since the study child's 6th birthday



# PARENTAL RISKS

postnatal_stress_8_9Y[,c('post_PR_percent_missing','post_parental_risk')] <- domainscore(postnatal_stress_8_9Y[,c(
      "p2007_rec", # Mother was in trouble with the law since the study child's 6th birthday
      "p2016_rec", # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
      "p2028_rec", # Mother attempted suicide since the study child's 6th birthday
      "p2029_rec", # Mother was convicted of an offence since the study child's 6th birthday
      "p2034_rec")]) # Mother had an abortion since the study child's 6th birthday
  


# INTERPERSONAL RISKS

postnatal_stress_8_9Y[,c('post_IR_percent_missing','post_interpersonal_risk')] <- domainscore(postnatal_stress_8_9Y[,c(
      "p2008_rec", # Mother was divorced since the study child's 6th birthday
      "p2009_rec", # Mother found out that her husband/partner didn't want her child since the study child's 6th birthday
      "p2015_rec", # Mother's husband/partner went away since the study child's 6th birthday
      "p2017_rec", # Mother and husband/partner separated since the study child's 6th birthda
      "p2019_rec", # Mother argued with husband/partner since the study child's 6th birthday
      "p2020_rec", # Mother argued with family and friends since the study child's 6th birthday
      "p2022_rec", # Mother's husband/partner was physically cruel to her since the study child's 6th birthday
      "p2025_rec", # Mother got married since the study child's 6th birthday
      "p2026_rec", # Mother's husband/partner was physically cruel to her children since the study child's 6th birthday
      "p2027_rec", # Mother was physically cruel to her children since the study child's 6th birthday
      "p2036_rec", # Mother's husband/partner was emotionally cruel to her since the study child's 6th birthday
      "p2037_rec", # Mother's husband/partner was emotionally cruel to her children since the study child's 6th birthday
      "p2038_rec", # Mother was emotionally cruel to her children since the study child's 6th birthday
      "p2040_rec", # Mother found a new partner since the study child's 6th birthday
      "p3153_rec", # Mother/husband/partner shouted or called one another names in the past 3 months (in Cha script called: DV_Shouted_9Y)
      "p3154_rec", # Mother/husband/partner hit or slapped one another in the past 3 months (in Cha script called: DV_Hit_9Y)
      "p3155_rec", # Mother/husband/partner threw or broke things in the past 3 months (in Cha script called:: DV_Break_9Y)
      "ku298_rec")]) # Child is slapped or hit (in Cha script called: Par_Smack_9Y_Any)

  
# DIRECT VICTIMISATION
  
postnatal_stress_8_9Y[,c('post_DV_percent_missing','post_direct_victimization')] <- domainscore(postnatal_stress_8_9Y[,c(
      "kt5004a_rec", # Since 7th birthday child has been physically hurt by someone
      "kt5005a_rec" # Since 7th birthday child has been sexually abused
      "f8fp475a_rec", # Bullying, child is a relational victim (in Cha script called: RelationalVictim_8yrs_Recoded)
      "f8fp470")]) # Bullying, child is an overt victim (in Cha script called: OvertVictim_8yrs_Recoded)
      # SDQBullied_8_YN, # Child is picked on or bullied by other children in past 6 months (original variable: ku698) 
   


####################################################################################################################################################

# 10. SAVE DATA

# Save the dataset in an .rds file, in the directory where you have the raw data
saveRDS(postnatal_stress_8_9Y, paste(pathtodata,'postnatal_stress_8_9Y.rds', sep = ""))
saveRDS(postnatal_summary_8_9Y, paste(pathtodata,'postnatal_stress_summary_8_9Y.rds', sep = ""))

# Or save the dataset in a .csv format, in the directory where you have the raw data
write.csv(postnatal_stress_8_9Y, file = "postnatal_stress_8_9Y.csv", row.names = FALSE, quote = FALSE)
write.csv(postnatal_summary_8_9Y, file = "postnatal_stress_summary_8_9Y.csv", row.names = T, quote = FALSE)

####################################################################################################################################################



  
                                                             
  
