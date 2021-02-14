
####################################################################################################################################################

# PRENATAL

####################################################################################################################################################


# 1. LIFE EVENTS

# depending on the format of the data, you may want to write "attach(nameofdataframe)" and then use the code below. 
# This will permit the variables to be accessed directly without having to use the dollar sign, as here: 'nameofdata$variablename'

CS_Prenatal_LifeEvents <- data.frame(IDM, # add mothers ID here (find the equivalent term within ALSPAC)
                                     b570a_rec, # PTNR died since PREG
                                     b571a_rec, # CH died since PREG
                                     b572a_rec, # Friend or relative died since PREG
                                     b573a_rec, # CH was ill since PREG	
                                     b574a_rec, # PTNR was ill since PREG	
                                     b575a_rec, # Friend or relative was ill since PREG
                                     b576a_rec, # Admitted to hospital since PREG
                                     b580a_rec, # V ill since PREG
                                     b581a_rec, # PTNR lost job since PREG
                                     b582a_rec, # PTNR had PROBS at work since PREG	
                                     b583a_rec, # PROBS at work since PREG
                                     b584a_rec, # Lost job since PREG		
                                     b591a_rec, # Moved house since PREG	
                                     b599a_rec, # Bled & thought might miscarry	
                                     b600a_rec, # Started new job since PREG
                                     b601a_rec, # Test to see if baby abnormal
                                     b602a_rec, # Test result suggesting POSS abnormality	
                                     b603a_rec, # Told having twins
                                     b604a_rec, # POSS harm to baby
                                     b606a_rec, # Took an exam since PREG
                                     b609a_rec, # House or car burgled since PREG
                                     b610a_rec) # Had an accident since PREG
                                    

####################################################################################################################################################

# 2. CONTEXTUAL RISKS


CS_Prenatal_ContextualRisks  <- data.frame(IDM, # add mothers ID here,
                                           b588a_rec, # Income reduced since PREG
                                           b593a_rec, # Became homeless since PREG
                                           b594a_rec, # Major financial PROB since PREG
                                           p2, # Housing adequacy 
                                           p3, # Housing Basic Living 
                                           p4, # Housing Defects
                                           p6) # Financial difficulties 
                                                  

####################################################################################################################################################

# 3. PARENTAL RISKS



CS_Prenatal_ParentalRisks  <- data.frame(IDM, # add mothers ID here,
                                         b577a_rec, # In trouble with the law since PREG
                                         b586a_rec, # PTNR in trouble with law since PREG
                                         b597a_rec, # Attempted suicide since PREG
                                         b598a_rec, # Convicted of an offence since PREG
                                         b605a_rec, # Tried to have abortion
                                         p1, # Early parenthood
                                         p5, # Maternal education 
                                         p12, # Psychopathology of mother
                                         p13, # Substance abuse 
                                         p14, # Crime trouble with police 
                                         p15) # Crime convictions   
# This first version appears in CumRisk_Instructions&Information.xlsx, 
# likely suggesting from the three version, this one was selected to form the cumulative score)

                                            
CS_Prenatal_Parental_NoSubUse <- data.frame(IDM, # add mothers ID here,
                                            b577a_rec, # In trouble with the law since PREG
                                            b586a_rec, # PTNR in trouble with law since PREG
                                            b597a_rec, # Attempted suicide since PREG
                                            b598a_rec, # Convicted of an offence since PREG
                                            b605a_rec, # Tried to have abortion
                                            p1, # Early parenthood
                                            p5, # Maternal education 
                                            p12, # Psychopathology of mother
                                            p14, # Crime trouble with police 
                                            p15) # Crime convictions   

# The difference between CS_Prenatal_MH_LifestyleRisks and CS_Prenatal_Parental_NoSubUse is that the latter excludes substance abuse (p13) 


CS_Prenatal_MH_LifestyleRisks_NoMI <- data.frame(IDM, # add mothers ID here,
                                                 b577a_rec, # In trouble with the law since PREG
                                                 b586a_rec, # PTNR in trouble with law since PREG
                                                 b597a_rec, # Attempted suicide since PREG
                                                 b598a_rec, # Convicted of an offence since PREG
                                                 b605a_rec, # Tried to have abortion
                                                 p1, # Early parenthood
                                                 p5, # Maternal education 
                                                 p13, # Substance abuse 
                                                 p14, # Crime trouble with police 
                                                 p15) # Crime convictions   

# The difference between CS_Prenatal_MH_LifestyleRisks and CS_Prenatal_MH_LifestyleRisks_NoMI is that the latter excludes psychopathology of mother (p12) 

####################################################################################################################################################


# 4. INTERPERSONAL RISKS


CS_Prenatal_InterpersonalRisks <- data.frame(IDM, # add mothers ID here,
                                             b578a_rec, # Divorced since PREG
                                             b579a_rec, # PTNR rejected PREG
                                             b585a_rec, # PTNR went away since PREG		
                                             b587a_rec, # Separated since PREG		
                                             b589a_rec, # Argued with PTNR since PREG	
                                             b590a_rec, # Argued with family or friends since PREG
                                             b592a_rec, # PTNR hurt mum since PREG	
                                             b595a_rec, # Got married since PREG		
                                             b596a_rec, # PTNR hurt CH since PREG	
                                             b607a_rec, # PTNR was EMOT cruel to mum since PREG
                                             b608a_rec, # PTNR was EMOT cruel to CH since PREG
                                             p7, #Partner Status 
                                             p8, # Partner Affection 
                                             p9, # Partner cruelty 
                                             p10, # Family Size
                                             p11, # Family Major problems 
                                             p16, # Partner Support
                                             p17, # Social Network - Emotional 
                                             p18) # Social Network - Practical 

                                          

####################################################################################################################################################


# 5. CREATE PRENATAL CUMULATIVE RISK SCORE 

# This function merges together all prenatal ELS domains according to the IDM column 
# results in a dataframe with all relevant items for prenatal stress.
# tech-tip from Serena: use Reduce because merge can only take two dataframes at a time 

CS_CumulativeRisk_Prenatal <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDM',  all.x = TRUE), 
                                     list(CS_Prenatal_LifeEvents,
                                          CS_Prenatal_ContextualRisks,
                                          CS_Prenatal_ParentalRisks,
                                          CS_Prenatal_InterpersonalRisks))

####################################################################################################################################################

# 6. LINK IDM to IDC 

# In order to later merge the prenatal dataset with postnatal variables, the code below links
# pregnancy ID (IDM) to the child ID (IDC) 

child_general <- readquick("nameofchilddataset.sav") # read in the data containing mother and child IDs
child_id <- data.frame(child_general$IDM, child_general$IDC) # extract mother and child IDs
colnames(child_id) <- c("IDM","IDC") # rename the columns to 'IDM' and 'IDC'


# ATTENTION! Only need to run the line below if you are using the prenatal score together with postnatal outcomes/scores. 
# Note: this will probably change the number of observations

CS_CumulativeRisk_Prenatal <- merge(child_id, CS_CumulativeRisk_Prenatal, by = 'IDM', all.x = T) 

####################################################################################################################################################

# 7. SUMMARY STATISTICS 

# Let's have a look at risk distribution and missing data per indicator (as per Serena's script) 
# I don't think this section will work with ALSPAC data. We probably will need to alter the code. 

prenatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 3:ncol(CS_CumulativeRisk_Prenatal)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(CS_CumulativeRisk_Prenatal[,i]))
  c = colnames(CS_CumulativeRisk_Prenatal)[i]
  prenatal_summary[1:3,c] <- s
  prenatal_summary[4,c] <- round((prenatal_summary[2,c] / 9901)*100, 2)
  prenatal_summary[5,c] <- round((prenatal_summary[3,c] / 9901)*100, 2)
}

####################################################################################################################################################

