
####################################################################################################################

# SCRIPT 2: DICHOTOMISING EARLY LIFE STRESS VARIABLES 

####################################################################################################################

# 1. FAMILY RISKS: RECODE NOMINAL VARIABLES INTO BINARY - SHOUT, HIT & BREAK THINGS - 21M, 33M, 6Y, 9Y
#originally nominal var (no; yes: mum/partner/both did) - collapsed so that either no or yes (regardless of who). 

####################################################################################################################


#Shouting variables:
# g711 -> DV_Shouted_21M  # Shouted in Anger in past 3 Months
# h580 -> DV_Shouted_33M  # In past 3 months, mum shouted at partner and/or called them names
# l6153 -> DV_Shouted_6Y  # Shouting/calling partner names has happened in past 3 months
# p3153 -> DV_Shouted_9Y  # Mother/husband/partner shouted or called one another names in the past 3 months

#Hitting variables:
# g712 -> DV_Hit_21M # Hit or slapped PTNR in past 3 Months
# h581 -> DV_Hit_33M # In past 3 months, mum hit or slapped partner
# l6154 -> DV_Hit_6Y # Hitting/slapping partner has happened in past 3 months
# p3154 -> DV_Hit_9Y # Mother/husband/partner hit or slapped one another in the past 3 months

#Breaking variables:
# g713 -> DV_Break_21M # Threw something in anger in past 3 mths
# h582 -> DV_Break_33M # In past 3 months, things were thrown or deliberately broken
# l6155 -> DV_Break_6Y # Throwing/breaking things has happened in past 3 months
# p3155 -> DV_Break_9Y # Mother/husband/partner threw or broke things in the past 3 months


# SHOUTING VARIABLES

#Creating empty matrix
DV_Shouted_21M <- c()

#Loop to recode into binary variable
for (i in 1:length(g711)) {
  if(g711[i] <= 3 & g711[i] >= 1){
    DV_Shouted_21M <- append(DV_Shouted_21M, 1)
  } 
  else if(g711[i] == 4){
    DV_Shouted_21M <- append(DV_Shouted_21M, 0)
  } 
  else{
    DV_Shouted_21M <- append(DV_Shouted_21M, NA)
  } 
}

#Creating empty matrix
DV_Shouted_33M<- c()

#Loop to recode into binary variable
for (i in 1:length(h580)) {
  if(h580[i] <= 3 & h580[i] >= 1){
    DV_Shouted_33M <- append(DV_Shouted_33M, 1)
  } 
  else if(h580[i] == 4){
    DV_Shouted_33M <- append(DV_Shouted_33M, 0)
  } 
  else{
    DV_Shouted_33M <- append(DV_Shouted_33M, NA)
  } 
}

#Creating empty matrix
DV_Shouted_6Y <- c()

#Loop to recode into binary variable
for (i in 1:length(l6153)) {
  if(l6153[i] <= 3 & l6153[i] >= 1){
    DV_Shouted_6Y <- append(DV_Shouted_6Y, 1)
  } 
  else if(l6153[i] == 4){
    DV_Shouted_6Y <- append(DV_Shouted_6Y, 0)
  } 
  else{
    DV_Shouted_6Y <- append(DV_Shouted_6Y, NA)
  } 
}

#Creating empty matrix
DV_Shouted_9Y<- c()

#Loop to recode into binary variable
for (i in 1:length(p3153)) {
  if(p3153[i] <= 3 & p3153[i] >= 1){
    DV_Shouted_9Y <- append(DV_Shouted_9Y, 1)
  } 
  else if(p3153[i] == 4){
    DV_Shouted_9Y <- append(DV_Shouted_9Y, 0)
  } 
  else{
    DV_Shouted_9Y <- append(DV_Shouted_9Y, NA)
  } 
}

# HITTING VARIABLES

#Creating empty matrix
DV_Hit_21M <- c()

#Loop to recode into binary variable
for (i in 1:length(g712)) {
  if(g712[i] <= 3 & g712[i] >= 1){
    DV_Hit_21M <- append(DV_Hit_21M, 1)
  } 
  else if(g712[i] == 4){
    DV_Hit_21M <- append(DV_Hit_21M, 0)
  } 
  else{
    DV_Hit_21M <- append(DV_Hit_21M, NA)
  } 
}

#Creating empty matrix
DV_Hit_33M <- c()

#Loop to recode into binary variable
for (i in 1:length(h581)) {
  if(h581[i] <= 3 & h581[i] >= 1){
    DV_Hit_33M <- append(DV_Hit_33M, 1)
  } 
  else if(h581[i] == 4){
    DV_Hit_33M <- append(DV_Hit_33M, 0)
  } 
  else{
    DV_Hit_33M <- append(DV_Hit_33M, NA)
  } 
}

#Creating empty matrix
DV_Hit_6Y <- c()

#Loop to recode into binary variable
for (i in 1:length(l6154)) {
  if(l6154[i] <= 3 & l6154[i] >= 1){
    DV_Hit_6Y <- append(DV_Hit_6Y, 1)
  } 
  else if(l6154[i] == 4){
    DV_Hit_6Y <- append(DV_Hit_6Y, 0)
  } 
  else{
    DV_Hit_6Y <- append(DV_Hit_6Y, NA)
  } 
}

#Creating empty matrix
DV_Hit_9Y <- c()

#Loop to recode into binary variable
for (i in 1:length(p3154)) {
  if(p3154[i] <= 3 & p3154[i] >= 1){
    DV_Hit_9Y <- append(DV_Hit_9Y, 1)
  } 
  else if(p3154[i] == 4){
    DV_Hit_9Y <- append(DV_Hit_9Y, 0)
  } 
  else{
    DV_Hit_9Y <- append(DV_Hit_9Y, NA)
  } 
}


# BREAKING VARIABLES

#Creating empty matrix
DV_Break_21M <- c()

#Loop to recode into binary variable
for (i in 1:length(g713)) {
  if(g713[i] <= 3 & g713[i] >= 1){
    DV_Break_21M <- append(DV_Break_21M, 1)
  } 
  else if(g713[i] == 4){
    DV_Break_21M <- append(DV_Break_21M, 0)
  } 
  else{
    DV_Break_21M <- append(DV_Break_21M, NA)
  } 
}

#Creating empty matrix
DV_Break_33M <- c()

#Loop to recode into binary variable
for (i in 1:length(h582)) {
  if(h582[i] <= 3 & h582[i] >= 1){
    DV_Break_33M <- append(DV_Break_33M, 1)
  } 
  else if(h582[i] == 4){
    DV_Break_33M <- append(DV_Break_33M, 0)
  } 
  else{
    DV_Break_33M <- append(DV_Break_33M, NA)
  } 
}

#Creating empty matrix
DV_Break_6Y <- c()

#Loop to recode into binary variable
for (i in 1:length(l6155)) {
  if(l6155[i] <= 3 & l6155[i] >= 1){
    DV_Break_6Y <- append(DV_Break_6Y, 1)
  } 
  else if(l6155[i] == 4){
    DV_Break_6Y <- append(DV_Break_6Y, 0)
  } 
  else{
    DV_Break_6Y <- append(DV_Break_6Y, NA)
  } 
}

#Creating empty matrix
DV_Break_9Y  <- c()

#Loop to recode into binary variable
for (i in 1:length(p3155)) {
  if(p3155[i] <= 3 & p3155[i] >= 1){
    DV_Break_9Y  <- append(DV_Break_9Y , 1)
  } 
  else if(p3155[i] == 4){
    DV_Break_9Y  <- append(DV_Break_9Y , 0)
  } 
  else{
    DV_Break_9Y  <- append(DV_Break_9Y , NA)
  } 
}


####################################################################################################################

# 2. FAMILY RISKS = MOTHER FEELINGS at 8M & 33M
# Bonding, confidence and enjoyment scale scores (ordinal continuous)
# take BOTTOM 10% (because higher scores = better, while we want negative)

####################################################################################################################

# f117 -> MatBonding_8M_Bottom10
# f114 -> MatConfidence_8M_Bottom10
# f111 -> MatEnjoyment_8M_Bottom10
# h766 -> MatBonding_33M_Bottom10
# h763 -> MatConfidence_33M_Bottom10
# h760 -> MatEnjoyment_33M_Bottom10

#Bonding at 8M -> 10% cutoff = 23.
MatBonding_8M_Bottom10 <- c()

for (i in 1:length(f117)) {
  if(f117[i] <= 23 & f117[i] >= 0){
    MatBonding_8M_Bottom10 <- append(MatBonding_8M_Bottom10, 1)
  } 
  else if(f117[i] > 23){
    MatBonding_8M_Bottom10 <- append(MatBonding_8M_Bottom10, 0)
  } 
  else{
    MatBonding_8M_Bottom10 <- append(MatBonding_8M_Bottom10, NA)
  } 
}

#Confidence at 8M = 12.

MatConfidence_8M_Bottom10 <- c()

for (i in 1:length(f114)) {
  if(f114[i] <= 12 & f114[i] >= 0){
    MatConfidence_8M_Bottom10 <- append(MatConfidence_8M_Bottom10, 1)
  } 
  else if(f114[i] > 12){
    MatConfidence_8M_Bottom10 <- append(MatConfidence_8M_Bottom10, 0)
  } 
  else{
    MatConfidence_8M_Bottom10 <- append(MatConfidence_8M_Bottom10, NA)
  } 
}

#Enjoyment at 8M = 10.

MatEnjoyment_8M_Bottom10 <- c()

for (i in 1:length(f111)) {
  if(f111[i] <= 10 & f111[i] >= 0){
    MatEnjoyment_8M_Bottom10 <- append(MatEnjoyment_8M_Bottom10, 1)
  } 
  else if(f111[i] > 10){
    MatEnjoyment_8M_Bottom10 <- append(MatEnjoyment_8M_Bottom10, 0)
  } 
  else{
    MatEnjoyment_8M_Bottom10 <- append(MatEnjoyment_8M_Bottom10, NA)
  } 
}

#Bonding at 33M = 22.

MatBonding_33M_Bottom10 <- c()

for (i in 1:length(h766)) {
  if(h766[i] <= 22 & h766[i] >= 0){
    MatBonding_33M_Bottom10 <- append(MatBonding_33M_Bottom10, 1)
  } 
  else if(h766[i] > 22){
    MatBonding_33M_Bottom10 <- append(MatBonding_33M_Bottom10, 0)
  } 
  else{
    MatBonding_33M_Bottom10 <- append(MatBonding_33M_Bottom10, NA)
  } 
}


#Confidence at 33M = 11.

MatConfidence_33M_Bottom10 <- c()

for (i in 1:length(h763)) {
  if(h763[i] <= 11 & h763[i] >= 0){
    MatConfidence_33M_Bottom10 <- append(MatConfidence_33M_Bottom10, 1)
  } 
  else if(h763[i] > 11){
    MatConfidence_33M_Bottom10 <- append(MatConfidence_33M_Bottom10, 0)
  } 
  else{
    MatConfidence_33M_Bottom10 <- append(MatConfidence_33M_Bottom10, NA)
  } 
}


#Enjoyment at 33M = 9.
MatEnjoyment_33M_Bottom10 <- c()

for (i in 1:length(h760)) {
  if(h760[i] <= 9 & h760[i] >= 0){
    MatEnjoyment_33M_Bottom10 <- append(MatEnjoyment_33M_Bottom10, 1)
  } 
  else if(h760[i] > 9){
    MatEnjoyment_33M_Bottom10 <- append(MatEnjoyment_33M_Bottom10, 0)
  } 
  else{
    MatEnjoyment_33M_Bottom10 <- append(MatEnjoyment_33M_Bottom10, NA)
  } 
}


####################################################################################################################

# 3. FAMILY RISKS = LOW PARENTAL INVOLVMENT at 6M, 3Y, 6Y

####################################################################################################################

# LOW PARENTAL INVOLVEMENT 6M.
# Higher scores = lower parental involvement

#combining into a data frame
kb_data_frame <- data.frame(kb556, # FREQ MUM sings to CH
                            kb558, # FREQ MUM shows CH picture books
                            kb560, # FREQ MUM & CH play with toys
                            kb562, # FREQ MUM cuddles CH
                            kb564, # FREQ MUM physically plays with CH
                            kb566, # FREQ MUM takes CH for walks
                            kb572, # PTNR baths CH
                            kb573, # PTNR feeds CH
                            kb574, # PTNR sings to CH
                            kb575, # PTNR shows CH picture books
                            kb576, # PTNR & CH play with toys
                            kb577, # PTNR cuddles CH
                            kb578, # PTNR plays physically with CH
                            kb579) # PTNR takes CH for walks
                            
#summing all the rows to get total lower par involvement score for each participant
LowParInvolv_6M <- rowSums(kb_data_frame, na.rm = FALSE)

#10% CUT-OFF = 26 (check if this is true)

quantile(LowParInvolv_6M, 0.90, na.rm = TRUE) # This permits to see the value of the top 10%

LowParInvolv_6M_Top10 <- c() # We might need to recode missing values into NAs for each of the items that compose LowParInvolv_6m before creating this variable

for (i in 1:length(LowParInvolv_6M)) {   
  if(LowParInvolv_6M[i] <= 25 &  LowParInvolv_6M[i] >= 0){  # check if 25 corresponds to the 10% treshold, if not replace it 
    LowParInvolv_6M_Top10 <- append(LowParInvolv_6M_Top10, 0)
  } 
  else if(LowParInvolv_6M[i] > 25){   # replace the 25 here as well 
    LowParInvolv_6M_Top10 <- append(LowParInvolv_6M_Top10, 1)
  } 
  else{
    LowParInvolv_6M_Top10 <- append(LowParInvolv_6M_Top10, NA)
  } 
}


###############################################
# LOW PARENTAL INVOLVEMENT 3Y 
# HIGHER SCORES = LOWER PAR INVOLVEMENT

###############################################

#combining into a data frame
kj_data_frame <- data.frame(kj420, # MUM Bathes CH
                            kj421, # MUM Feeds CH
                            kj422, # MUM Sings to CH
                            kj423, # MUM Reads CH Stories
                            kj424, # MUM Plays W Toys W CH
                            kj425, # MUM Cuddles CH
                            kj426, # MUM & CH Play Imitation Games
                            kj427, # MUM Plays W CH Physically
                            kj428, # MUM Takes CH for Walk
                            kj400, # Partner Bathes CH
                            kj401, # Partner Feeds CH
                            kj402, # Partner Sings to CH
                            kj403, # Partner Reads CH Stories
                            kj404, # Partner Plays W Toys W CH
                            kj405, # Partner Cuddles CH
                            kj406, # PTR & CH Play Imitation Games
                            kj407, # PTR Plays W CH Physically
                            kj408) # Partner Takes CH for Walk 


#summing all the rows to get total lower par involvement score for each participant
LowParInvolv_3Y <- rowSums(kj_data_frame, na.rm = FALSE)  

#10% CUT-OFF = 53.

quantile(LowParInvolv_3Y, 0.90, na.rm = TRUE) # For this dataset top 10% = 62


LowParInvolv_3Y_Top10 <- c() # We might need to recode missing values into NAs for each of the items that compose LowParInvolv_3y before creating this variable

for (i in 1:length(LowParInvolv_3Y)) {
  if(LowParInvolv_3Y[i] <= 52 &  LowParInvolv_3Y[i] >= 0){
    LowParInvolv_3Y_Top10 <- append(LowParInvolv_3Y_Top10, 0)
  } 
  else if(LowParInvolv_3Y[i] > 52){
    LowParInvolv_3Y_Top10 <- append(LowParInvolv_3Y_Top10, 1)
  } 
  else{
    LowParInvolv_3Y_Top10 <- append(LowParInvolv_3Y_Top10, NA)
  } 
}




###############################################
# LOW PARENTAL INVOLVEMENT 6Y 

###############################################


#combining into a data frame
kq_data_frame <- data.frame(kq575, # Mum/Female Baths CH
                            kq576, # Mum/Female Makes Things With CH
                            kq577, # Mum/Female Sings To CH
                            kq578, # Mum/Female Reads To CH
                            kq579, # Mum/Female Plays With Toys With CH
                            kq580, # Mum/Female Cuddles CH
                            kq581, # Mum/Female Does Active Play With CH
                            kq582, # Mum/Female Takes CH To Park/Playground
                            kq583, # Mum/Female Puts CH To Bed
                            kq584, # Mum/Female Takes CH Swimming
                            kq585, # Mum/Female Draws/Paints With CH
                            kq586, # Mum/Female Prepares Food For CH
                            kq587, # Mum/Female Takes CH To Classes
                            kq588, # Mum/Female Takes CH Shopping
                            kq589, # Mum/Female Takes CH To Watch Sports
                            kq590, # Mum/Female Does Homework With CH
                            kq591, # Mum/Female Has Conversations With CH
                            kq592, # Mum/Female Helps CH Prepare For School
                            kq601, # Dad/Male Makes Things With CH
                            kq602, # Dad/Male Sings To CH
                            kq603, # Dad/Male Reads To CH
                            kq604, # Dad/Male Plays With Toys With CH
                            kq605, # Dad/Male Cuddles CH
                            kq606, # Dad/Male Does Active Play With CH
                            kq607, # Dad/Male Takes CH To Park/Playgrnd
                            kq608, # Dad/Male Puts CH To Bed
                            kq609, # Dad/Male Takes CH Swimming
                            kq610, # Dad/Male Draws/Paints With CH
                            kq611, # Dad/Male Prepares Food For CH
                            kq612, # Dad/Male Takes CH To Classes
                            kq613, # Dad/Male Takes CH Shopping
                            kq614, # Dad/Male Takes CH To Watch Sports
                            kq615, # Dad/Male Does Homework With CH
                            kq616, # Dad/Male Has Conversations With CH
                            kq617) # Dad/Male Helps CH Prepare For School

#summing all the rows to get total lower par involvement score for each participant
LowParInvolv_6Y <- rowSums(kq_data_frame, na.rm = FALSE)

#10% CUT-OFF = 114.

quantile(LowParInvolv_6Y, 0.90, na.rm = TRUE) #For this dataset top 10% = 114

LowParInvolv_6Y_Top10 <- c() 

for (i in 1:length(LowParInvolv_6Y)) {
  if(LowParInvolv_6Y[i] <= 113 &  LowParInvolv_6Y[i] >= 0){
    LowParInvolv_6Y_Top10 <- append(LowParInvolv_6Y_Top10, 0)
  } 
  else if(LowParInvolv_6Y[i] > 113){
    LowParInvolv_6Y_Top10 <- append(LowParInvolv_6Y_Top10, 1)
  } 
  else{
    LowParInvolv_6Y_Top10 <- append(LowParInvolv_6Y_Top10, NA)
  } 
}


####################################################################################################################
# 4. SHOUT
# RECODE SO THAT HIGHER VALUES = HIGHER HARSH DISCIPLINE
####################################################################################################################


# kd304 -> Par_Shout_1Y  # MUM Shouts at CH During Tantrums
# ke016 -> Par_Shout_2Y  # CH shouted at
# kj374 -> Par_Shout_3Y  # MUM Shouts at CH when Naughty
# kl299 -> Par_Shout_4Y  # Parent shouts at child when child has tantrum
# kp6049 -> Par_Shout_6Y # Mother shouts at child when child has tantrum


#Shout at 1 year

Par_Shout_1Y <- c()

for (i in 1:length(kd304)) {
  if(kd304[i] == 3){
    Par_Shout_1Y <- append(Par_Shout_1Y, 0)
  } 
  else if(kd304[i] == 2){
    Par_Shout_1Y <- append(Par_Shout_1Y, 1)
  } 
  else if(kd304[i] == 1){
    Par_Shout_1Y <- append(Par_Shout_1Y, 2)
  } 
  else{
    Par_Shout_1Y <- append(Par_Shout_1Y, NA)
  } 
}


#Shout at 2 years

Par_Shout_2Y <- c()

for (i in 1:length(ke016)) {
  if(ke016[i] == 5){
    Par_Shout_2Y <- append(Par_Shout_2Y, 0)
  } 
  else if(ke016[i] == 4){
    Par_Shout_2Y <- append(Par_Shout_2Y, 1)
  } 
  else if(ke016[i] == 3){
    Par_Shout_2Y <- append(Par_Shout_2Y, 2)
  } 
  else if(ke016[i] == 2){
    Par_Shout_2Y <- append(Par_Shout_2Y, 3)
  } 
  else if(ke016[i] == 1){
    Par_Shout_2Y <- append(Par_Shout_2Y, 4)
  } 
  else{
    Par_Shout_2Y <- append(Par_Shout_2Y, NA)
  } 
}


#Shout at 3 years

Par_Shout_3Y <- c()

for (i in 1:length(kj374)) {
  if(kj374[i] == 5){
    Par_Shout_3Y <- append(Par_Shout_3Y, 4)
  } 
  else if(kj374 [i] == 4){
    Par_Shout_3Y <- append(Par_Shout_3Y, 3)
  } 
  else if(kj374 [i] == 3){
    Par_Shout_3Y <- append(Par_Shout_3Y, 2)
  } 
  else if(kj374 [i] == 2){
    Par_Shout_3Y <- append(Par_Shout_3Y, 1)
  } 
  else if(kj374 [i] == 1){
    Par_Shout_3Y <- append(Par_Shout_3Y, 0)
  } 
  else{
    Par_Shout_3Y <- append(Par_Shout_3Y, NA)
  } 
}

#Shout at 4 years 

Par_Shout_4Y <- c()

for (i in 1:length(kl299)) {
  if(kl299[i] == 3){
    Par_Shout_4Y <- append(Par_Shout_4Y, 0)
  } 
  else if(kl299  [i] == 2){
    Par_Shout_4Y <- append(Par_Shout_4Y, 1)
  } 
  else if(kl299  [i] == 1){
    Par_Shout_4Y <- append(Par_Shout_4Y, 2)
  } 
  else{
    Par_Shout_4Y <- append(Par_Shout_4Y, NA)
  } 
}

#Shout at 6 years

Par_Shout_6Y <- c()

for (i in 1:length(kp6049)) {
  if(kp6049[i] == 3){
    Par_Shout_6Y <- append(Par_Shout_6Y, 0)
  } 
  else if(kp6049  [i] == 2){
    Par_Shout_6Y <- append(Par_Shout_6Y, 1)
  } 
  else if(kp6049  [i] == 1){
    Par_Shout_6Y <- append(Par_Shout_6Y, 2)
  } 
  else{
    Par_Shout_6Y <- append(Par_Shout_6Y, NA)
  } 
}

# Create a data frame with all shout variables 

Par_Shout_df <- data.frame(Par_Shout_1Y,Par_Shout_2Y,Par_Shout_3Y,Par_Shout_4Y,Par_Shout_6Y) 

# Compute Parental Shouting 0-7y Total
Par_Shout_0_7_Tot <- rowSums(Par_Shout_df, na.rm=TRUE)

# Explore the new variable
summary(Par_Shout_0_7_Tot)
hist(Par_Shout_0_7_Tot)


# TOP 20% CUT-OFF  
# RECODE Par_Shout_0_7_Tot into Par_Shout_0_7_Top20 
quantile(Par_Shout_0_7_Tot, 0.80, na.rm = TRUE) # Check what the top 20% is for this data set - 11? 

Par_Shout_0_7_Top20 <- c()  

for (i in 1:length(Par_Shout_0_7_Tot)){
  if(Par_Shout_0_7_Tot[i] <= 10 & Par_Shout_0_7_Tot[i] >=  0){
    Par_Shout_0_7_Top20 <- append(Par_Shout_0_7_Top20, 0)
  } 
  else if(Par_Shout_0_7_Tot[i] > 10){
    Par_Shout_0_7_Top20 <- append(Par_Shout_0_7_Top20, 1)
  } 
  else{
    Par_Shout_0_7_Top20 <- append(Par_Shout_0_7_Top20, NA)
  } 
}


####################################################################################################################
# 5. SMACK
# RECODE SO THAT HIGHER VALUES = HIGHER HARSH DISCIPLINE
####################################################################################################################

# kd303 -> Par_Smack_1Y    #MUM Smacks CH During Tantrums
# ke017 -> Par_Smack_2Y    #CH slapped
# kj373 -> Par_Smack_3Y    #MUM Smacks CH when Naughty
# kl297 -> Par_Smack_4Y    #Parent slaps or hits child when child has tantrum
# kp6047 -> Par_Smack_6Y   #Mother slaps child when child has tantrum

#Smack at 1 year

Par_Smack_1Y <- c()

for (i in 1:length(kd303)) {
  if(kd303 [i] == 3){
    Par_Smack_1Y <- append(Par_Smack_1Y, 0)
  } 
  else if(kd303[i] == 2){
    Par_Smack_1Y <- append(Par_Smack_1Y, 1)
  } 
  else if(kd303[i] == 1){
    Par_Smack_1Y <- append(Par_Smack_1Y, 2)
  } 
  else{
    Par_Smack_1Y <- append(Par_Smack_1Y, NA)
  } 
}


#Smack at 2 years

Par_Smack_2Y <- c()

for (i in 1:length(ke017)) {
  if(ke017[i] == 5){
    Par_Smack_2Y <- append(Par_Smack_2Y, 0)
  } 
  else if(ke017[i] == 4){
    Par_Smack_2Y <- append(Par_Smack_2Y, 1)
  } 
  else if(ke017[i] == 3){
    Par_Smack_2Y <- append(Par_Smack_2Y, 2)
  } 
  else if(ke017[i] == 2){
    Par_Smack_2Y <- append(Par_Smack_2Y, 3)
  } 
  else if(ke017[i] == 1){
    Par_Smack_2Y <- append(Par_Smack_2Y, 4)
  } 
  else{
    Par_Smack_2Y <- append(Par_Smack_2Y, NA)
  } 
}

#Smack at 3 years

Par_Smack_3Y <- c()

for (i in 1:length(kj373)) {
  if(kj373 [i] == 5){
    Par_Smack_3Y <- append(Par_Smack_3Y, 4)
  } 
  else if(kj373[i] == 4){
    Par_Smack_3Y <- append(Par_Smack_3Y, 3)
  } 
  else if(kj373[i] == 3){
    Par_Smack_3Y <- append(Par_Smack_3Y, 2)
  } 
  else if(kj373[i] == 2){
    Par_Smack_3Y <- append(Par_Smack_3Y, 1)
  } 
  else if(kj373  [i] == 1){
    Par_Smack_3Y <- append(Par_Smack_3Y, 0)
  } 
  else{
    Par_Smack_3Y <- append(Par_Smack_3Y, NA)
  } 
}


#Smack at 4 years

Par_Smack_4Y <- c()

for (i in 1:length(kl297)) {
  if(kl297[i] == 3){
    Par_Smack_4Y <- append(Par_Smack_4Y, 0)
  } 
  else if(kl297  [i] == 2){
    Par_Smack_4Y <- append(Par_Smack_4Y, 1)
  } 
  else if(kl297  [i] == 1){
    Par_Smack_4Y <- append(Par_Smack_4Y, 2)
  } 
  else{
    Par_Smack_4Y <- append(Par_Smack_4Y, NA)
  } 
}

#Smack at 6 years

Par_Smack_6Y <- c()

for (i in 1:length(kp6047)) {
  if(kp6047[i] == 3){
    Par_Smack_6Y <- append(Par_Smack_6Y, 0)
  } 
  else if(kp6047  [i] == 2){
    Par_Smack_6Y <- append(Par_Smack_6Y, 1)
  } 
  else if(kp6047  [i] == 1){
    Par_Smack_6Y <- append(Par_Smack_6Y, 2)
  } 
  else{
    Par_Smack_6Y <- append(Par_Smack_6Y, NA)
  } 
}


# Create a data frame with all Smack variables 
Par_Smack_df <- data.frame(Par_Smack_1Y,Par_Smack_2Y,Par_Smack_3Y,Par_Smack_4Y,Par_Smack_6Y) 


# Compute Parental Smacking 0-7y Total
Par_Smack_0_7_Tot <- rowSums(Par_Smack_df, na.rm=TRUE)

# Explore the new variable
summary(Par_Smack_0_7_Tot)
hist(Par_Smack_0_7_Tot)


#Descriptive statistics
#install package psych
library(psych)
describe(Par_Smack_0_7_Tot)


# Charlotte's script does not icnlude top 10% or 20% calculation for Par_Smack_0_7_Tot, which is why I did not 
# include it here. If needed, calculate it using the 'Par_Shout_0_7_Top20' example above. 


####################################################################################################################

# 6. DIRECT VICTIMIZATION = BULLY ITEM SDQ AGE 4, 7, 8, 10

####################################################################################################################


# j548 -> SDQBullied_4_YN  # CH is Bullied > 6 MTHs
# kq338 -> SDQBullied_7_YN # CH Been Bullied In Past 6 Mths
# n8358 -> SDQBullied_8_YN # Study child has been picked on or bullied by other children in the past six months
# ku698 -> SDQBullied_10_YN # Child is picked on or bullied by other children in past 6 months


# Recode into binary YES/NO variables

SDQBullied_4_YN <- c()

for (i in 1:length(j548)) {
  if(j548[i] == 1){
    SDQBullied_4_YN <- append(SDQBullied_4_YN, 0)
  } 
  else if(j548[i] == 2 || j548[i] == 3){
    SDQBullied_4_YN <- append(SDQBullied_4_YN, 1)
  } 
  else{
    SDQBullied_4_YN <- append(SDQBullied_4_YN, NA)
  } 
}

SDQBullied_7_YN <- c()

for (i in 1:length(kq338)) {
  if(kq338[i] == 1){
    SDQBullied_7_YN<- append(SDQBullied_7_YN, 0)
  } 
  else if(kq338[i] == 2 || kq338[i] == 3){
    SDQBullied_7_YN<- append(SDQBullied_7_YN, 1)
  } 
  else{
    SDQBullied_7_YN<- append(SDQBullied_7_YN, NA)
  } 
}

SDQBullied_8_YN <- c()

for (i in 1:length(n8358)) {
  if(n8358[i] == 1){
    SDQBullied_8_YN<- append(SDQBullied_8_YN, 0)
  } 
  else if(n8358[i] == 2 || n8358[i] == 3){
    SDQBullied_8_YN<- append(SDQBullied_8_YN, 1)
  } 
  else{
    SDQBullied_8_YN<- append(SDQBullied_8_YN, NA)
  } 
}

SDQBullied_10_YN <- c()

for (i in 1:length(ku698)) {
  if(ku698[i] == 1){
    SDQBullied_10_YN<- append(SDQBullied_10_YN, 0)
  } 
  else if(ku698[i] == 2 || ku698[i] == 3){
    SDQBullied_10_YN<- append(SDQBullied_10_YN, 1)
  } 
  else{
    SDQBullied_10_YN<- append(SDQBullied_10_YN, NA)
  } 
}

#Illustrating recoding
head(data.frame(ku698,SDQBullied_10_YN))


####################################################################################################################


