
####################################################################################################################################################

# PRENATAL

####################################################################################################################################################

# The following script builds a dataset with all variables and domain scores used in
# the creation of prenatal cumulative risk score by Cecil et al. (2014). 
# It adapts the scripts developed by S. Defina where she created a similar score for Generation R. 

# The script contains the following functions: readquick, percent_missing and domainscore
# You can find them at SereDef GitHub account (https://github.com/SereDef/cumulative-ELS-score) in 'Setup_and_functions.R'.

####################################################################################################################################################


# first, exclude duplicated values (based on first 4 column names; keep first and remove second occurence)
alspac.table = alspac.table %>% distinct(aln,qlet,kz021,ff1ms100, .keep_all = T)

# remote siblings (all qlet=B)
alspac.table=alspac.table[alspac.table$qlet=="A",]

# check if any duplicated ALNs left
table(duplicated(alspac.table$aln))

# 1. LIFE EVENTS

# Steps 1-4 assign variables to the pertinent ELS domains (LE/CR/PR/IR) 
# depending on the format of the data, you may want to write "attach(nameofdataframe)" and then use the code below. 
# This will permit the variables to be accessed directly without having to use the dollar sign, as here: 'nameofdata$variablename'


Prenatal_LifeEvents <- data.frame(alspac.table[,c("aln", # add mothers ID here (find the equivalent term within ALSPAC)
                                     "b570a_rec", # PTNR died since PREG
                                     "b571a_rec", # CH died since PREG
                                     "b572a_rec", # Friend or relative died since PREG
                                     "b573a_rec", # CH was ill since PREG	
                                     "b574a_rec", # PTNR was ill since PREG	
                                     "b575a_rec", # Friend or relative was ill since PREG
                                     "b576a_rec", # Admitted to hospital since PREG
                                     "b580a_rec", # V ill since PREG
                                     "b581a_rec", # PTNR lost job since PREG
                                     "b582a_rec", # PTNR had PROBS at work since PREG	
                                     "b583a_rec", # PROBS at work since PREG
                                     "b584a_rec", # Lost job since PREG		
                                     "b591a_rec", # Moved house since PREG	
                                     "b599a_rec", # Bled & thought might miscarry	
                                     "b600a_rec", # Started new job since PREG
                                     "b601a_rec", # Test to see if baby abnormal
                                     "b602a_rec", # Test result suggesting POSS abnormality	
                                     "b603a_rec", # Told having twins
                                     "b604a_rec", # POSS harm to baby
                                     "b606a_rec", # Took an exam since PREG
                                     "b609a_rec", # House or car burgled since PREG
                                     "b610a_rec")]) # Had an accident since PREG
                                    

####################################################################################################################################################

# 2. CONTEXTUAL RISKS


Prenatal_ContextualRisks  <- data.frame(alspac.table[,c("aln", # add mothers ID here,
                                           "b588a_rec", # Income reduced since PREG
                                           "b593a_rec", # Became homeless since PREG
                                           "b594a_rec")]) #, # Major financial PROB since PREG
                                           # "p2", # Housing adequacy 
                                           # "p3", # Housing Basic Living 
                                           # "p4", # Housing Defects
                                           # "p6")]) # Financial difficulties 
                                                  

####################################################################################################################################################

# 3. PARENTAL RISKS



Prenatal_ParentalRisks  <- data.frame(alspac.table[,c("aln", # add mothers ID here,
                                         "b577a_rec", # In trouble with the law since PREG
                                         "b586a_rec", # PTNR in trouble with law since PREG
                                         "b597a_rec", # Attempted suicide since PREG
                                         "b598a_rec", # Convicted of an offence since PREG
                                         "b605a_rec")])#, # Tried to have abortion
                                         # p1, # Early parenthood
                                         # p5, # Maternal education 
                                         # p12, # Psychopathology of mother
                                         # p13, # Substance abuse 
                                         # p14, # Crime trouble with police 
                                         # p15) # Crime convictions   

# This first version appears in CumRisk_Instructions&Information.xlsx, 
# likely suggesting from the three version, this one was selected to form the cumulative score)

                                            
Prenatal_Parental_NoSubUse <- data.frame(alspac.table[,c("aln", # add mothers ID here,
                                            "b577a_rec", # In trouble with the law since PREG
                                            "b586a_rec", # PTNR in trouble with law since PREG
                                            "b597a_rec", # Attempted suicide since PREG
                                            "b598a_rec", # Convicted of an offence since PREG
                                            "b605a_rec")])#, # Tried to have abortion
                                            # p1, # Early parenthood
                                            # p5, # Maternal education 
                                            # p12, # Psychopathology of mother
                                            # p14, # Crime trouble with police 
                                            # p15) # Crime convictions   

# The difference between Prenatal_MH_LifestyleRisks and Prenatal_Parental_NoSubUse is that the latter excludes substance abuse (p13) 


Prenatal_MH_LifestyleRisks_NoMI <- data.frame(alspac.table[,c("aln", # add mothers ID here,
                                                 "b577a_rec", # In trouble with the law since PREG
                                                 "b586a_rec", # PTNR in trouble with law since PREG
                                                 "b597a_rec", # Attempted suicide since PREG
                                                 "b598a_rec", # Convicted of an offence since PREG
                                                 "b605a_rec")])#, # Tried to have abortion
                                                 # p1, # Early parenthood
                                                 # p5, # Maternal education 
                                                 # p13, # Substance abuse 
                                                 # p14, # Crime trouble with police 
                                                 # p15) # Crime convictions   

# The difference between Prenatal_MH_LifestyleRisks and Prenatal_MH_LifestyleRisks_NoMI is that the latter excludes psychopathology of mother (p12) 

####################################################################################################################################################


# 4. INTERPERSONAL RISKS


Prenatal_InterpersonalRisks <- data.frame(alspac.table[,c("aln", # add mothers ID here,
                                             "b578a_rec", # Divorced since PREG
                                             "b579a_rec", # PTNR rejected PREG
                                             "b585a_rec", # PTNR went away since PREG		
                                             "b587a_rec", # Separated since PREG		
                                             "b589a_rec", # Argued with PTNR since PREG	
                                             "b590a_rec", # Argued with family or friends since PREG
                                             "b592a_rec", # PTNR hurt mum since PREG	
                                             "b595a_rec", # Got married since PREG		
                                             "b596a_rec", # PTNR hurt CH since PREG	
                                             "b607a_rec", # PTNR was EMOT cruel to mum since PREG
                                             "b608a_rec")])#, # PTNR was EMOT cruel to CH since PREG
                                             # p7, #Partner Status 
                                             # p8, # Partner Affection 
                                             # p9, # Partner cruelty 
                                             # p10, # Family Size
                                             # p11, # Family Major problems 
                                             # p16, # Partner Support
                                             # p17, # Social Network - Emotional 
                                             # p18) # Social Network - Practical 

                                          

####################################################################################################################################################


# 5. CREATE PRENATAL CUMULATIVE RISK SCORE 

# This function merges together all prenatal ELS domains according to the 'aln' column 
# results in a dataframe with all relevant items for prenatal stress.
# tech-tip from Serena: use Reduce because merge can only take two dataframes at a time 

prenatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = 'aln',  all.x = TRUE), 
                                     list(Prenatal_LifeEvents,
                                          Prenatal_ContextualRisks,
                                          Prenatal_ParentalRisks,
                                          Prenatal_InterpersonalRisks))

# ####################################################################################################################################################
# 
# # 6. LINK IDM to IDC 
# 
# # In order to later merge the prenatal dataset with postnatal variables, the code below links
# # pregnancy ID (IDM) to the child ID (IDC) (according to Esther this will most likely not be required for ALSPAC data)
# 
# child_general <- readquick("nameofchilddataset.sav") # read in the data containing mother and child IDs 
# # if you use the readquick function you need to obtain it from SereDef 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score 
# 
# child_id <- data.frame(child_general$IDM, child_general$IDC) # extract mother and child IDs
# colnames(child_id) <- c("IDM","IDC") # rename the columns to 'IDM' and 'IDC'
# 
# 
# # ATTENTION! Only need to run the line below if you are using the prenatal score together with postnatal outcomes/scores. 
# # Note: this will probably change the number of observations
# 
# prenatal_stress <- merge(child_id, prenatal_stress, by = 'IDM', all.x = T) 
# 
# ####################################################################################################################################################

# 7. SUMMARY STATISTICS 

# Let's have a look at risk distribution and missing data per indicator (as per Serena's script) 

prenatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 2:ncol(prenatal_stress)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(prenatal_stress[,i]))
  c = colnames(prenatal_stress)[i]
  prenatal_summary[1:3,c] <- s
  prenatal_summary[4,c] <- round((prenatal_summary[2,c] / 11906)*100, 2)
  prenatal_summary[5,c] <- round((prenatal_summary[3,c] / 11906)*100, 2)
}

####################################################################################################################################################

# 8. MISSINGNESS

# Calculate the percentage missing data (obtained from SereDef 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score  
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }


# Apply the percent_missing function to the rows (1) of the entire dataset 

prenatal_stress$pre_percent_missing = apply(prenatal_stress[,2:ncol(prenatal_stress)], # Same as above, if not merged with child_id, count from 2.
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


prenatal_stress[,c('pre_LE_percent_missing','pre_life_events')] <- domainscore(prenatal_stress[,c(
  "b570a_rec", # PTNR died since PREG
  "b571a_rec", # CH died since PREG
  "b572a_rec", # Friend or relative died since PREG
  "b573a_rec", # CH was ill since PREG	
  "b574a_rec", # PTNR was ill since PREG	
  "b575a_rec", # Friend or relative was ill since PREG
  "b576a_rec", # Admitted to hospital since PREG
  "b580a_rec", # V ill since PREG
  "b581a_rec", # PTNR lost job since PREG
  "b582a_rec", # PTNR had PROBS at work since PREG	
  "b583a_rec", # PROBS at work since PREG
  "b584a_rec", # Lost job since PREG		
  "b591a_rec", # Moved house since PREG	
  "b599a_rec", # Bled & thought might miscarry	
  "b600a_rec", # Started new job since PREG
  "b601a_rec", # Test to see if baby abnormal
  "b602a_rec", # Test result suggesting POSS abnormality	
  "b603a_rec", # Told having twins
  "b604a_rec", # POSS harm to baby
  "b606a_rec", # Took an exam since PREG
  "b609a_rec", # House or car burgled since PREG
  "b610a_rec")]) # Had an accident since PREG


                                                                                                                        
# CONTEXTUAL RISKS


prenatal_stress[,c('pre_CR_percent_missing','pre_contextual_risk')] <- domainscore(prenatal_stress[,c(
  "b588a_rec", # Income reduced since PREG
  "b593a_rec", # Became homeless since PREG
  "b594a_rec")])#, # Major financial PROB since PREG
  # p2, # Housing adequacy 
  # p3, # Housing Basic Living 
  # p4, # Housing Defects
  # p6)]) # Financial difficulties 


                                                                                                                            
# PARENTAL RISKS
  

prenatal_stress[,c('pre_PR_percent_missing','pre_parental_risks')] <- domainscore(prenatal_stress[,c(
  "b577a_rec", # In trouble with the law since PREG
  "b586a_rec", # PTNR in trouble with law since PREG
  "b597a_rec", # Attempted suicide since PREG
  "b598a_rec", # Convicted of an offence since PREG
  "b605a_rec")])#, # Tried to have abortion
  # p1, # Early parenthood
  # p5, # Maternal education 
  # p12, # Psychopathology of mother
  # p13, # Substance abuse 
  # p14, # Crime trouble with police 
  # p15)]) # Crime convictions   
                                                                                                                            


# INTERPERSONAL RISKS


prenatal_stress[,c('pre_IS_percent_missing','pre_interpersonal_risks')] <- domainscore(prenatal_stress[,c(
  "b578a_rec", # Divorced since PREG
  "b579a_rec", # PTNR rejected PREG
  "b585a_rec", # PTNR went away since PREG		
  "b587a_rec", # Separated since PREG		
  "b589a_rec", # Argued with PTNR since PREG	
  "b590a_rec", # Argued with family or friends since PREG
  "b592a_rec", # PTNR hurt mum since PREG	
  "b595a_rec", # Got married since PREG		
  "b596a_rec", # PTNR hurt CH since PREG	
  "b607a_rec", # PTNR was EMOT cruel to mum since PREG
  "b608a_rec")])#, # PTNR was EMOT cruel to CH since PREG
  # p7, #Partner Status 
  # p8, # Partner Affection 
  # p9, # Partner cruelty 
  # p10, # Family Size
  # p11, # Family Major problems 
  # p16, # Partner Support
  # p17, # Social Network - Emotional 
  # p18)]) # Social Network - Practical 

                                                                                                                                 

####################################################################################################################################################

# 10. SAVE DATA

# Save the dataset in an .rds file, in the directory where you have the raw data
saveRDS(prenatal_stress, paste(pathtodata,'prenatal_stress.rds', sep = ""))
saveRDS(prenatal_summary, paste(pathtodata,'prenatal_stress_summary.rds', sep = ""))

# Or save the dataset in a .csv format, in the directory where you have the raw data
write.csv(prenatal_stress, file = "prenatal_stress.csv", row.names = FALSE, quote = FALSE)
write.csv(prenatal_summary, file = "prenatal_stress_summary.csv", row.names = T, quote = FALSE)

# if you used attach(nameofdatafrandata) makes sure to detach the data by typing: detach(nameofdataframe).

####################################################################################################################################################


