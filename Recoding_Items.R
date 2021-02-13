b570
b571
b572
b573
b574
b575
b576
b580
b581
b582
b583
b584
b591
b599
b600
b601
b602
b603
b604
b606
b609
b610

# PRENATAL

# 1. LIFE EVENTS

#read in the relevant dataset

# recode variables for LE domain.


data <- data.frame(idm = c(1,2,3,4,5,6,7,0), b=c(1,2,3,4,5,6,7,0),c= c(1,2,3,4,0,6,7,8),d=c(1,0, 0,4,5,6,7,8))

GR1001H <- data.frame(data$idm,
                      new = ifelse(data$b < 3, yes = 1, no = 0), # I worry whether the pregnancy will go well or not.
                      # DICH: "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.
                      new2= ifelse(data$c< 3, yes = 1, no = 0)) # I am worried about the health of my baby.
# DICH: "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.

LE_prenatal <- data.frame(data$idm,
                          b570a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # PTNR died since PREG
                          b571a_rec = ifelse(data$b571 <= 4, yes = 1, no = 0), # CH died since PREG
                          b572a_rec = ifelse(data$b572 <= 4, yes = 1, no = 0), # Friend or relative died since PREG
                          b573a_rec = ifelse(data$b573 <= 4, yes = 1, no = 0), # CH was ill since PREG	
                          b574a_rec = ifelse(data$b574 <= 4, yes = 1, no = 0), # PTNR was ill since PREG	
                          b575a_rec = ifelse(data$b575 <= 4, yes = 1, no = 0), # Friend or relative was ill since PREG
                          b576a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Admitted to hospital since PREG
                          b580a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # V ill since PREG
                          b581a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # PTNR lost job since PREG
                          b582a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # PTNR had PROBS at work since PREG	
                          b583a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # PROBS at work since PREG
                          b584a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Lost job since PREG
                          b591a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Moved house since PREG
                          b599a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Bled & thought might miscarry	
                          b600a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Started new job since PREG
                          b601a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Test to see if baby abnormal
                          b602a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Test result suggesting POSS abnormality
                          b603a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Told having twins
                          b604a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # POSS harm to baby
                          b606a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # Took an exam since PREG
                          b609a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0), # House or car burgled since PREG
                          b610a_rec = ifelse(data$b570 <= 4, yes = 1, no = 0)) # Had an accident since PREG
                          
                          
                          
                          
                          
colnames(data) <- c("IDM","pregnancy_worried","baby_worried")




