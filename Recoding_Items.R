

####################################################################################################################################################

# PRENATAL

####################################################################################################################################################

# This script is used for dichotomising prenatal ELS variables into 1 = risk and 0 = no risk. 

# 1. LIFE EVENTS

# Originally, variables have been coded as:

      # 1 = affected a lot
      # 2 = fairly affected
      # 3 = mildly affected
      # 4 = not effected at all
      # 5 = didn't happen
      # for total number of options inlcuded need to check the dataset 

# Below script recodes them into: 

      # 1 = risk (for values between 1-4)
      # 0 = no risk (for value of 5) 
      # any other number = NA (missing)


#RECODE TO BINARY

b570a_rec <- c()
for (i in 1:length(b570)) {    # for each value in b570 
  if(b570[i] <= 4 & b570[i] >= 1){   # if it is smaller or equal to 4 and greater or equal to 1
    b570a_rec <- append(b570a_rec, 1) # append the new b570a_rec to include 1 
  } 
  else if(b570[i] == 5){   # else, if b570 is equal to 5
    b570a_rec <- append(b570a_rec, 0)  # append the new b570a_rec to include 0
  } 
  else{
    b570a_rec <- append(b570a_rec, NA) # for any other value of b570 include NA 
  } 
}

b571a_rec <- c()
for (i in 1:length(b571)) {
  if(b571[i] <= 4 & b571[i] >= 1){
    b571a_rec <- append(b571a_rec, 1)
  } 
  else if(b571[i] == 5){
    b571a_rec <- append(b571a_rec, 0)
  } 
  else{
    b571a_rec <- append(b571a_rec, NA)
  } 
}

b572a_rec <- c()
for (i in 1:length(b572)) {
  if(b572[i] <= 4 & b572[i] >= 1){
    b572a_rec <- append(b572a_rec, 1)
  } 
  else if(b572[i] == 5){
    b572a_rec <- append(b572a_rec, 0)
  } 
  else{
    b572a_rec <- append(b572a_rec, NA)
  } 
}

b573a_rec <- c()
for (i in 1:length(b573)) {
  if(b573[i] <= 4 & b573[i] >= 1){
    b573a_rec <- append(b573a_rec, 1)
  } 
  else if(b573[i] == 5){
    b573a_rec <- append(b573a_rec, 0)
  } 
  else{
    b573a_rec <- append(b573a_rec, NA)
  } 
}

b574a_rec <- c()
for (i in 1:length(b574)) {
  if(b574[i] <= 4 & b574[i] >= 1){
    b574a_rec <- append(b574a_rec, 1)
  } 
  else if(b574[i] == 5){
    b574a_rec <- append(b574a_rec, 0)
  } 
  else{
    b574a_rec <- append(b574a_rec, NA)
  } 
}

b575a_rec <- c()
for (i in 1:length(b575)) {
  if(b575[i] <= 4 & b575[i] >= 1){
    b575a_rec <- append(b575a_rec, 1)
  } 
  else if(b575[i] == 5){
    b575a_rec <- append(b575a_rec, 0)
  } 
  else{
    b575a_rec <- append(b575a_rec, NA)
  } 
}

b576a_rec <- c()
for (i in 1:length(b576)) {
  if(b576[i] <= 4 & b576[i] >= 1){
    b576a_rec <- append(b576a_rec, 1)
  } 
  else if(b576[i] == 5){
    b576a_rec <- append(b576a_rec, 0)
  } 
  else{
    b576a_rec <- append(b576a_rec, NA)
  } 
}

b580a_rec <- c()
for (i in 1:length(b580)) {
  if(b580[i] <= 4 & b580[i] >= 1){
    b580a_rec <- append(b580a_rec, 1)
  } 
  else if(b580[i] == 5){
    b580a_rec <- append(b580a_rec, 0)
  } 
  else{
    b580a_rec <- append(b580a_rec, NA)
  } 
}

b581a_rec <- c()
for (i in 1:length(b581)) {
  if(b581[i] <= 4 & b581[i] >= 1){
    b581a_rec <- append(b581a_rec, 1)
  } 
  else if(b581[i] == 5){
    b581a_rec <- append(b581a_rec, 0)
  } 
  else{
    b581a_rec <- append(b581a_rec, NA)
  } 
}

b582a_rec <- c()
for (i in 1:length(b582)) {
  if(b582[i] <= 4 & b582[i] >= 1){
    b582a_rec <- append(b582a_rec, 1)
  } 
  else if(b582[i] == 5){
    b582a_rec <- append(b582a_rec, 0)
  } 
  else{
    b582a_rec <- append(b582a_rec, NA)
  } 
}

b583a_rec <- c()
for (i in 1:length(b583)) {
  if(b583[i] <= 4 & b583[i] >= 1){
    b583a_rec <- append(b583a_rec, 1)
  } 
  else if(b583[i] == 5){
    b583a_rec <- append(b583a_rec, 0)
  } 
  else{
    b583a_rec <- append(b583a_rec, NA)
  } 
}

b584a_rec <- c()
for (i in 1:length(b584)) {
  if(b584[i] <= 4 & b584[i] >= 1){
    b584a_rec <- append(b584a_rec, 1)
  } 
  else if(b584[i] == 5){
    b584a_rec <- append(b584a_rec, 0)
  } 
  else{
    b584a_rec <- append(b584a_rec, NA)
  } 
}

b591a_rec <- c()
for (i in 1:length(b591)) {
  if(b591[i] <= 4 & b591[i] >= 1){
    b591a_rec <- append(b591a_rec, 1)
  } 
  else if(b591[i] == 5){
    b591a_rec <- append(b591a_rec, 0)
  } 
  else{
    b591a_rec <- append(b591a_rec, NA)
  } 
}

b599a_rec <- c()
for (i in 1:length(b599)) {
  if(b599[i] <= 4 & b599[i] >= 1){
    b599a_rec <- append(b599a_rec, 1)
  } 
  else if(b599[i] == 5){
    b599a_rec <- append(b599a_rec, 0)
  } 
  else{
    b599a_rec <- append(b599a_rec, NA)
  } 
}

b600a_rec <- c()
for (i in 1:length(b600)) {
  if(b600[i] <= 4 & b600[i] >= 1){
    b600a_rec <- append(b600a_rec, 1)
  } 
  else if(b600[i] == 5){
    b600a_rec <- append(b600a_rec, 0)
  } 
  else{
    b600a_rec <- append(b600a_rec, NA)
  } 
}

b601a_rec <- c()
for (i in 1:length(b601)) {
  if(b601[i] <= 4 & b601[i] >= 1){
    b601a_rec <- append(b601a_rec, 1)
  } 
  else if(b601[i] == 5){
    b601a_rec <- append(b601a_rec, 0)
  } 
  else{
    b601a_rec <- append(b601a_rec, NA)
  } 
}

b602a_rec <- c()
for (i in 1:length(b602)) {
  if(b602[i] <= 4 & b602[i] >= 1){
    b602a_rec <- append(b602a_rec, 1)
  } 
  else if(b602[i] == 5){
    b602a_rec <- append(b602a_rec, 0)
  } 
  else{
    b602a_rec <- append(b602a_rec, NA)
  } 
}

b603a_rec <- c()
for (i in 1:length(b603)) {
  if(b603[i] <= 4 & b603[i] >= 1){
    b603a_rec <- append(b603a_rec, 1)
  } 
  else if(b603[i] == 5){
    b603a_rec <- append(b603a_rec, 0)
  } 
  else{
    b603a_rec <- append(b603a_rec, NA)
  } 
}

b604a_rec <- c()
for (i in 1:length(b604)) {
  if(b604[i] <= 4 & b604[i] >= 1){
    b604a_rec <- append(b604a_rec, 1)
  } 
  else if(b604[i] == 5){
    b604a_rec <- append(b604a_rec, 0)
  } 
  else{
    b604a_rec <- append(b604a_rec, NA)
  } 
}

b606a_rec <- c()
for (i in 1:length(b606)) {
  if(b606[i] <= 4 & b606[i] >= 1){
    b606a_rec <- append(b606a_rec, 1)
  } 
  else if(b606[i] == 5){
    b606a_rec <- append(b606a_rec, 0)
  } 
  else{
    b606a_rec <- append(b606a_rec, NA)
  } 
}

b609a_rec <- c()
for (i in 1:length(b609)) {
  if(b609[i] <= 4 & b609[i] >= 1){
    b609a_rec <- append(b609a_rec, 1)
  } 
  else if(b609[i] == 5){
    b609a_rec <- append(b609a_rec, 0)
  } 
  else{
    b609a_rec <- append(b609a_rec, NA)
  } 
}

b610a_rec <- c()
for (i in 1:length(b610)) {
  if(b610[i] <= 4 & b610[i] >= 1){
    b610a_rec <- append(b610a_rec, 1)
  } 
  else if(b610[i] == 5){
    b610a_rec <- append(b610a_rec, 0)
  } 
  else{
    b610a_rec <- append(b610a_rec, NA)
  } 
}


# Creating a data frame with the original LE variables 

LE_prenatal_continuous <- data.frame(b570, # PTNR died since PREG
                                     b571, # CH died since PREG
                                     b572, # Friend or relative died since PREG
                                     b573, # CH was ill since PREG	
                                     b574, # PTNR was ill since PREG	
                                     b575, # Friend or relative was ill since PREG
                                     b576, # Admitted to hospital since PREG
                                     b580, # V ill since PREG
                                     b581, # PTNR lost job since PREG
                                     b582, # PTNR had PROBS at work since PREG	
                                     b583, # PROBS at work since PREG
                                     b584, # Lost job since PREG		
                                     b591, # Moved house since PREG	
                                     b599, # Bled & thought might miscarry	
                                     b600, # Started new job since PREG
                                     b601, # Test to see if baby abnormal
                                     b602, # Test result suggesting POSS abnormality	
                                     b603, # Told having twins
                                     b604, # POSS harm to baby
                                     b606, # Took an exam since PREG
                                     b609, # House or car burgled since PREG
                                     b610) # Had an accident since PREG
                 
# Creating a data frame containing the newly created binary LE variables

LE_prenatal_binary <- data.frame(b570a_rec, # PTNR died since PREG
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
                  

#install package 'lineup' which contains the 'corbetw2mat' function
library(lineup)

#Checking correlations btw columns of LE_prenatal_continuous and columns of LE_prenatal_binary
corbetw2mat(LE_prenatal_continuous, LE_prenatal_binary, what = "paired")    

#Above correlattions are equivalent to: cor(b570, b570a_rec, use="pairwise", method = "pearson")


####################################################################################################################################################
                          
# 2. CONTEXTUAL RISKS


b588a_rec <- c()
for (i in 1:length(b588)) {
  if(b588[i] <= 4 & b588[i] >= 1){
    b588a_rec <- append(b588a_rec, 1)
  } 
  else if(b588[i] == 5){
    b588a_rec <- append(b588a_rec, 0)
  } 
  else{
    b588a_rec <- append(b588a_rec, NA)
  } 
}

b593a_rec <- c()
for (i in 1:length(b593)) {
  if(b593[i] <= 4 & b593[i] >= 1){
    b593a_rec <- append(b593a_rec, 1)
  } 
  else if(b593[i] == 5){
    b593a_rec <- append(b593a_rec, 0)
  } 
  else{
    b593a_rec <- append(b593a_rec, NA)
  } 
}

b594a_rec <- c()
for (i in 1:length(b594)) {
  if(b594[i] <= 4 & b594[i] >= 1){
    b594a_rec <- append(b594a_rec, 1)
  } 
  else if(b594[i] == 5){
    b594a_rec <- append(b594a_rec, 0)
  } 
  else{
    b594a_rec <- append(b594a_rec, NA)
  } 
}


# Creating a data frame with the original CR variables 

CR_prenatal_continuous <- data.frame(b588, # Income reduced since PREG
                                     b593, # Became homeless since PREG
                                     b594) # Major financial PROB since PREG

# Creating a data frame containing the newly created binary CR variables

CR_prenatal_binary <- data.frame(b588a_rec, # Income reduced since PREG
                                 b593a_rec, # Became homeless since PREG
                                 b594a_rec) # Major financial PROB since PREG


# Checking correlations btw columns of CR_prenatal_continuous and columns of CR_prenatal_binary

corbetw2mat(CR_prenatal_continuous, CR_prenatal_binary, what = "paired")  
                                    

####################################################################################################################################################


# 3. PARENTAL RISKS 


b577a_rec <- c()
for (i in 1:length(b577)) {
  if(b577[i] <= 4 & b577[i] >= 1){
    b577a_rec <- append(b577a_rec, 1)
  } 
  else if(b577[i] == 5){
    b577a_rec <- append(b577a_rec, 0)
  } 
  else{
    b577a_rec <- append(b577a_rec, NA)
  } 
}

b586a_rec <- c()
for (i in 1:length(b586)) {
  if(b586[i] <= 4 & b586[i] >= 1){
    b586a_rec <- append(b586a_rec, 1)
  } 
  else if(b586[i] == 5){
    b586a_rec <- append(b586a_rec, 0)
  } 
  else{
    b586a_rec <- append(b586a_rec, NA)
  } 
}

b597a_rec <- c()
for (i in 1:length(b597)) {
  if(b597[i] <= 4 & b597[i] >= 1){
    b597a_rec <- append(b597a_rec, 1)
  } 
  else if(b597[i] == 5){
    b597a_rec <- append(b597a_rec, 0)
  } 
  else{
    b597a_rec <- append(b597a_rec, NA)
  } 
}

b598a_rec <- c()
for (i in 1:length(b598)) {
  if(b598[i] <= 4 & b598[i] >= 1){
    b598a_rec <- append(b598a_rec, 1)
  } 
  else if(b598[i] == 5){
    b598a_rec <- append(b598a_rec, 0)
  } 
  else{
    b598a_rec <- append(b598a_rec, NA)
  } 
}

b605a_rec <- c()
for (i in 1:length(b605)) {
  if(b605[i] <= 4 & b605[i] >= 1){
    b605a_rec <- append(b605a_rec, 1)
  } 
  else if(b605[i] == 5){
    b605a_rec <- append(b605a_rec, 0)
  } 
  else{
    b605a_rec <- append(b605a_rec, NA)
  } 
}


# Creating a data frame with the original PR variables 

PR_prenatal_continuous <- data.frame(b577, # In trouble with the law since PREG
                                     b586, # PTNR in trouble with law since PREG
                                     b597, # Attempted suicide since PREG
                                     b598, # Convicted of an offence since PREG
                                     b605) # Tried to have abortion

# Creating a data frame containing the newly created binary PR variables 

PR_prenatal_binary <- data.frame(b577a_rec, # In trouble with the law since PREG
                                 b586a_rec, # PTNR in trouble with law since PREG
                                 b597a_rec, # Attempted suicide since PREG
                                 b598a_rec, # Convicted of an offence since PREG
                                 b605a_rec) # Tried to have abortion
                                     

# Checking correlations btw columns of PR_prenatal_continuous and columns of PR_prenatal_binary

corbetw2mat(PR_prenatal_continuous, PR_prenatal_binary, what = "paired")  

                                     
####################################################################################################################################################


# INTERPERSONAL RISKS

b578a_rec <- c()
for (i in 1:length(b578)) {
  if(b578[i] <= 4 & b578[i] >= 1){
    b578a_rec <- append(b578a_rec, 1)
  } 
  else if(b578[i] == 5){
    b578a_rec <- append(b578a_rec, 0)
  } 
  else{
    b578a_rec <- append(b578a_rec, NA)
  } 
}

b579a_rec <- c()
for (i in 1:length(b579)) {
  if(b579[i] <= 4 & b579[i] >= 1){
    b579a_rec <- append(b579a_rec, 1)
  } 
  else if(b579[i] == 5){
    b579a_rec <- append(b579a_rec, 0)
  } 
  else{
    b579a_rec <- append(b579a_rec, NA)
  } 
}

b585a_rec <- c()
for (i in 1:length(b585)) {
  if(b585[i] <= 4 & b585[i] >= 1){
    b585a_rec <- append(b585a_rec, 1)
  } 
  else if(b585[i] == 5){
    b585a_rec <- append(b585a_rec, 0)
  } 
  else{
    b585a_rec <- append(b585a_rec, NA)
  } 
}

b587a_rec <- c()
for (i in 1:length(b587)) {
  if(b587[i] <= 4 & b587[i] >= 1){
    b587a_rec <- append(b587a_rec, 1)
  } 
  else if(b587[i] == 5){
    b587a_rec <- append(b587a_rec, 0)
  } 
  else{
    b587a_rec <- append(b587a_rec, NA)
  } 
}

b589a_rec <- c()
for (i in 1:length(b589)) {
  if(b589[i] <= 4 & b589[i] >= 1){
    b589a_rec <- append(b589a_rec, 1)
  } 
  else if(b589[i] == 5){
    b589a_rec <- append(b589a_rec, 0)
  } 
  else{
    b589a_rec <- append(b589a_rec, NA)
  } 
}

b590a_rec <- c()
for (i in 1:length(b590)) {
  if(b590[i] <= 4 & b590[i] >= 1){
    b590a_rec <- append(b590a_rec, 1)
  } 
  else if(b590[i] == 5){
    b590a_rec <- append(b590a_rec, 0)
  } 
  else{
    b590a_rec <- append(b590a_rec, NA)
  } 
}


b592a_rec <- c()
for (i in 1:length(b592)) {
  if(b592[i] <= 4 & b592[i] >= 1){
    b592a_rec <- append(b592a_rec, 1)
  } 
  else if(b592[i] == 5){
    b592a_rec <- append(b592a_rec, 0)
  } 
  else{
    b592a_rec <- append(b592a_rec, NA)
  } 
}

b595a_rec <- c()
for (i in 1:length(b595)) {
  if(b595[i] <= 4 & b595[i] >= 1){
    b595a_rec <- append(b595a_rec, 1)
  } 
  else if(b595[i] == 5){
    b595a_rec <- append(b595a_rec, 0)
  } 
  else{
    b595a_rec <- append(b595a_rec, NA)
  } 
}

b596a_rec <- c()
for (i in 1:length(b596)) {
  if(b596[i] <= 4 & b596[i] >= 1){
    b596a_rec <- append(b596a_rec, 1)
  } 
  else if(b596[i] == 5){
    b596a_rec <- append(b596a_rec, 0)
  } 
  else{
    b596a_rec <- append(b596a_rec, NA)
  } 
}

b607a_rec <- c()
for (i in 1:length(b607)) {
  if(b607[i] <= 4 & b607[i] >= 1){
    b607a_rec <- append(b607a_rec, 1)
  } 
  else if(b607[i] == 5){
    b607a_rec <- append(b607a_rec, 0)
  } 
  else{
    b607a_rec <- append(b607a_rec, NA)
  } 
}

b608a_rec <- c()
for (i in 1:length(b608)) {
  if(b608[i] <= 4 & b608[i] >= 1){
    b608a_rec <- append(b608a_rec, 1)
  } 
  else if(b608[i] == 5){
    b608a_rec <- append(b608a_rec, 0)
  } 
  else{
    b608a_rec <- append(b608a_rec, NA)
  } 
}

# Creating a data frame with the original IR variables 

IR_prenatal_continuous <- data.frame(b578, # Divorced since PREG
                                     b579, # PTNR rejected PREG
                                     b585, # PTNR went away since PREG	
                                     b587, # Separated since PREG	
                                     b589, # Argued with PTNR since PREG
                                     b590, # Argued with family or friends since PREG
                                     b592, # PTNR hurt mum since PREG	
                                     b595, # Got married since PREG		
                                     b596, # PTNR hurt CH since PREG	
                                     b607, # PTNR was EMOT cruel to mum since PREG
                                     b608) # PTNR was EMOT cruel to mum since PREG

# Creating a data frame containing the newly created binary IR variables 

IR_prenatal_binary <- data.frame(b578a_rec, # Divorced since PREG
                                 b579a_rec, # PTNR rejected PREG
                                 b585a_rec, # PTNR went away since PREG		
                                 b587a_rec, # Separated since PREG		
                                 b589a_rec, # Argued with PTNR since PREG	
                                 b590a_rec, # Argued with family or friends since PREG
                                 b592a_rec, # PTNR hurt mum since PREG	
                                 b595a_rec, # Got married since PREG		
                                 b596a_rec, # PTNR hurt CH since PREG	
                                 b607a_rec, # PTNR was EMOT cruel to mum since PREG
                                 b608a_rec) # PTNR was EMOT cruel to CH since PREG


# Checking correlations btw columns of IR_prenatal_continuous and columns of IR_prenatal_binary

corbetw2mat(IR_prenatal_continuous, IR_prenatal_binary, what = "paired")  

####################################################################################################################################################


# THIS SECTION WILL BE DELETED 

# Below would have been a much shorter version, very similar to what Serena used, but did not work for 3 categories. 
# Any ideas how to make it work for more than two categories?

# remember to change the 'data' and 'idm' (ID number of mother)


# this one won't work because it lacks categories: 
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
                             
