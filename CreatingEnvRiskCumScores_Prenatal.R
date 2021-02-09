
####################################################################################################################################################

# PRENATAL

####################################################################################################################################################


# 1. LIFE EVENTS

CS_Prenatal_LifeEvents <- data.frame(b570a_rec, # PTNR died since PREG
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


CS_Prenatal_ContextualRisks  <- data.frame(b588a_rec, # Income reduced since PREG
                                           b593a_rec, # Became homeless since PREG
                                           b594a_rec, # Major financial PROB since PREG
                                           p2,
                                           p3,
                                           p4,
                                           p6)
                                                  

####################################################################################################################################################

3. MH/LIFESTYLE RISKS



CS_Prenatal_MH_LifestyleRisks  <- data.frame(b577a_rec, # In trouble with the law since PREG
                                             b586a_rec, # PTNR in trouble with law since PREG
                                             b597a_rec, # Attempted suicide since PREG
                                             b598a_rec, # Convicted of an offence since PREG
                                             b605a_rec, # Tried to have abortion
                                             p1,
                                             p5,
                                             p12,
                                             p13,
                                             p14,
                                             p15)
                                                      

CS_Prenatal_Parental_NoSubUse <- data.frame(b577a_rec, # In trouble with the law since PREG
                                            b586a_rec, # PTNR in trouble with law since PREG
                                            b597a_rec, # Attempted suicide since PREG
                                            b598a_rec, # Convicted of an offence since PREG
                                            b605a_rec, # Tried to have abortion
                                            p1,
                                            p5,
                                            p12,
                                            p14,
                                            p15) 

# The difference between CS_Prenatal_MH_LifestyleRisks and CS_Prenatal_Parental_NoSubUse is that the latter excludes p13 


CS_Prenatal_MH_LifestyleRisks_NoMI <- data.frame(b577a_rec, # In trouble with the law since PREG
                                                 b586a_rec, # PTNR in trouble with law since PREG
                                                 b597a_rec, # Attempted suicide since PREG
                                                 b598a_rec, # Convicted of an offence since PREG
                                                 b605a_rec, # Tried to have abortion
                                                 p1,
                                                 p5,
                                                 p13,
                                                 p14,
                                                 p15)

####################################################################################################################################################


# 4. FAMILY RISKS/DIS


CS_Prenatal_DIS_FamilyRisks <- data.frame(b578a_rec, # Divorced since PREG
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
                                          p7,
                                          p8,
                                          p9,
                                          p10,
                                          p11,
                                          p16,
                                          p17,
                                          p18)

####################################################################################################################################################

