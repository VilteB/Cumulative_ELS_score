################################################################################
###########################  POSTNATAL ELS SCORE  ##############################
################################################################################

# The following script builds a dataset with all indicators and domain scores in
# the postnatal cumulative risk score used in Schuumans (in preparation). 

# First, let's point to the necessary libraries and define all the functions that 
# are going to be used: readquick, percent_missing, repmeas, domainscore, 
# bsi_scores & fad_scores.
source("Setup_and_functions.R") 

# ATTENTION!!! You will be prompted with an "Enter path to data:" message 
# -> Enter the location of your datafiles. The code assumes that all (raw) data is 
# stored in ONE folder. Do not forget the final slash in your path, and, speaking of slashes, 
# beware of OS sensitive changes when you want to modify the structure of your dirs!

# For this version of the score
# You will need the following files (or updated versions from datamanagemet)
# Parenting 3 years of age_GR1065 F1F6 -GR1066 B1-B5_22112016.sav
###### !!! ATTENTION !!! Before you run this, you need to run the one SPSS Syntax. 
# (V:\Promovendi\Datasets\Gedragsgroep\Questionnaire data\After birth\0-3 years\Harsh parenting_Parent Child Conflict Tactics Scale\Syntax harsh parenting 3jr_PJ10012011)
# or most recent version, on this file. 
# BSI 3 years of age_GR1065 G1-GR1066 C1_22112016.sav; 
# GR1065-G2_01072012.sav; GR1065-G3-6_01072012.sav; GR1065-X_01072012.sav; 
# SESFOLLOWUP_03022020.sav;  PARENTEMPLOYMENT5_13082012.sav; GR1075-B3_17072015.sav; GR1075-B4_17072015.sav;
# 20141027_TRFteacherCleaned.sav; GR1080-C10-11_04042017.sav; GR1080-E_Bullying_17072015.sav;
# GR1081_E1-3_5-6_08082016.sav; GR1081_E4_30082016.sav; GR1081_I1-9_08082016.sav;
# GR1081-GR1083_D1_BSI_19042017.sav; GR1082_C1-5_22092016.sav; MOTHERTRAUMAINTERVIEW9_24112016.sav
# GR1083_E7_19102016.sav and FETALPERIOD-ALLGENERALDATA_29012018.sav

#-------------------------------------------------------------------------------
# I first read in the data and select the necessary columns (i.e. indicators), 
# and dichotomize when necessary. Then I merge them, calculate the five domain 
# scores and save two files: (1) the full dataset and (2) a summary overview of 
# how many risk, no-risk and missing values are present for each indicator. 

# LE = Life Events, CR = Contextual Risk, PR = Parental risk, IR = Interpersonal risk
# DV = Direct Victimization

################################################################################
#### ----------------------- reading in the data  ------------------------- ####
################################################################################

# NOTE, before we get going: when you call readquick on a file, the function will 
# replace values of 777, 888 or 999 with NAs unless they are IDCs or IDMs (see 
# repleacenas function). If you do NOT want this to happen for any other column 
# (for some reason) use the exclude_col argument with the name of the column that 
# has "real" 777, 888, or 999 values. 

#-------------------------------------------------------------------------------
## Parenting behavior (3 yr)

# ATTENTION! The code assumes you have run the SPSS Syntax that calculates 80th percentile harsh parenting scores.
# (V:\Promovendi\Datasets\Gedragsgroep\Questionnaire data\After birth\0-3 years\Harsh parenting_Parent Child Conflict Tactics Scale\Syntax harsh parenting 3jr_PJ10012011)
# or most recent version.
parentingv1 <- readquick("New Parenting 3 years of age_GR1065 F1F6 -GR1066 B1-B5_22112016.sav") # 9897 obs of 130 columns

# NOTE: There are no official guidelines for dichotomization of the Parent Child Conflict Tactics scale. 
# We based dichotomization on previous work in Gen R (Jansen et al., 2017): i.e. 80th percentile split. 

# Construct GR1065 # used for DV domain.
parenting <- data.frame(parentingv1$idc, parentingv1$harsh80_p, parentingv1$harsh80_m)
colnames(parenting) <- c("IDC", "p_harsh_parent", "m_harsh_parent")

#-------------------------------------------------------------------------------
##Adding 3 week Life Events (not present in GenR) from mother
## GR1065-G2 (3 yrs)
GR1065v1 <- readquick("insert ALSPAC data here") # 9901 obs. of  28 

  
# Construct LE3weeks # items used LE at 3 weeks in ALSPAC mothers

#ASLPAC variables are not YES/NO, here, 1-4 = YES, 5 = NO, -1 = missing, all others = NA
LE8weeks_mother <- data.frame(GR1065v1$idm, #add ALSPAC mother ID here!
                     GR1065v1$e400a_rec, # PTNR died since MID PREG
                     GR1065v1$e401a_rec, # CH died since MID PREG
                     GR1065v1$e402a_rec, # FRD or REL died since MID PREG
                     GR1065v1$e403a_rec, # CH ill since MID PREG
                     GR1065v1$e404a_rec, # PTNR ill since MID PREG
                     GR1065v1$e405a_rec, # Friend or REL ill since MID PREG
                     GR1065v1$e406a_rec, # Admitted to HOSP since MID PREG
                     GR1065v1$e410a_rec, # Ill since MID PREG
                     GR1065v1$e411a_rec, # PTNR lost job since MID PREG
                     GR1065v1$e412a_rec, # PTNR had PROBS at work since MID PREG
                     GR1065v1$e413a_rec, # PROBS at work since MID PREG
                     GR1065v1$e414a_rec, # Lost job since MID PREG
                     GR1065v1$e421a_rec, # Moved house since PREG
                     GR1065v1$e429a_rec, # MC scare since MID PREG
                     GR1065v1$e430a_rec, # Started new job since MID PREG
                     GR1065v1$e431a_rec, # Test for CH abnormality since MID PREG
                     GR1065v1$e432a_rec, # Test suggested CH PROB since MID PREG
                     GR1065v1$e433a_rec, # Discovered having twins since MID PREG
                     GR1065v1$e434a_rec, # Heard event might harm CH since MID PREG
                     GR1065v1$e436a_rec, # Took an exam since MID PREG
                     GR1065v1$e439a_rec, # House burglary/car theft since MID PREG
                     GR1065v1$e440a_rec) # Accident since MID PREG

#recode LE 8 weeks in mothers to dichotomised variable


#LE 8 month answers are dichotomised (retain "a" in variable code here). 1=Y, 2=N, -1 = missing
LE8months_mother <- data.frame(idm, #ALSPAC ID mother
                        f220a_rec, # Death of partner
                        f221a_rec, # Death of one of children
                        f222a_rec, # Death of friend or relative
                        f223a_rec, # Child ill
                        f224a_rec, # PTNR ill 
                        f225a_rec, # Friend or relative ill
                        f226a_rec, # Admitted to hospital
                        f230a_rec, # Mum ill
                        f231a_rec, # Parnter lost job
                        f232a_rec, # Work problems for partner
                        f233a_rec, # Work problems for Mum
                        f234a_rec, # Mum lost job
                        f241a_rec, # Moved house
                        f253a_rec, # Mum had miscarriage
                        f251a_rec, # New job for Mum
                        f250a_rec, # Mum became pregnant
                        f252a_rec, # Return to work
                        f255a_rec, # Exam taken
                        f260a_rec, # New job for partner
                        f261a_rec) # Pet died

#LE21month variables are dichotomised, therefore retain "a". 1=Y, 2=N
LE21months_mother <- data.frame(idm, # ID ALSPAC mother
                          g300a_rec, # Partner died >CH8MTHs
                         g301a_rec, # One of Mums children died >CH8MTHs
                         g302a_rec, # Friend or relative died >CH8MTHs
                         g303a_rec, # One of Mums children ill >CH8MTHs
                         g304a_rec, # Partner ill >CH8MTHs
                         g305a_rec, # Friend or relative ill >CH8MTHs
                         g306a_rec, # Mum in hospital >CH8MTHs
                         g310a_rec, # Mum very ill >CH8MTHs
                         g311a_rec, # Partner lost job >CH8MTHs
                         g312a_rec, # Partner had problems with work >CH8MTHs
                         g313a_rec, # Mum had problems with work >CH8MTHs
                         g314a_rec, # Mum lost job >CH8MTHs
                         g321a_rec, # Mum moved house >CH8MTHs
                         g330a_rec, # Mum pregnant >CH8MTHs
                         g331a_rec, # Mum started new job >CH8MTHs
                         g332a_rec, # Mum returned to work >CH8MTHs
                         g333a_rec, # Mum had miscarraige >CH8MTHs
                         g335a_rec, # Mum took exam >CH8MTHs
                         g339a_rec, # Mums house or car burgled >CH 8MTHs
                         g340a_rec, # Partner started new job >CH8MTHs
                         g341a_rec, # A pet died >CH8MTHs
                         g342a_rec) # Mum had accident >CH8MTHs).)

#No GenR variables up until 3 years
#LE 3 years is a dichotomised variable, so retain "a" and code as 1=Y, 2=N etc
LE3years_mother <- data.frame(h210a_rec, # Whether partner died since study child was 18 months old, Y/N
                       h211a_rec, # Whether one of mums children died since study child was 18 months old, Y/N
                       h212a_rec, # Whether a friend or relative died since study child was 18 months old, Y/N
                       h213a_rec, # Whether one of mums children was ill since study child was 18 months old, Y/N
                       h214a_rec, # Whether partner was ill since study child was 18 months old, Y/N
                       h215a_rec, # Whether a friend or relative was ill since study child was 18 months old, Y/N
                       h216a_rec, # Whether mum was admitted to hospital since study child was 18 months old, Y/N
                       h220a_rec, # Whether mum was very ill since study child was 18 months old, Y/N
                       h221a_rec, # Whether partner lost job since study child was 18 months old, Y/N
                       h222a_rec, # Whether partner had problems at work since study child was 18 months old, Y/N
                       h223a_rec, # Whether mum had problems at work since study child was 18 months old, Y/N
                       h224a_rec, # Whether mum lost job since study child was 18 months old, Y/N
                       h231a_rec, # Whether mum moved house since study child was 18 months old, Y/N
                       h240a_rec, # Whether mum became pregnant since study child was 18 months old, Y/N
                       h241a_rec, # Whether mum started a new job since study child was 18 months old, Y/N
                       h242a_rec, # Whether mum returned to work since study child was 18 months old, Y/N
                       h243a_rec, # Whether mum had a miscarriage since study child was 18 months old, Y/N
                       h245a_rec, # Whether mum took an exam since study child was 18 months old, Y/N
                       h249a_rec, # Whether house or car was burgled since study child was 18 months old, Y/N
                       h250a_rec, # Whether partner started a new job since study child was 18 months old, Y/N
                       h251a_rec, # Whether a pet died since study child was 18 months old, Y/N
                       h252a_rec) # Whether mum had an accident since study child was 18 months old, Y/N)

#LE 4 years is a binary variable, retain "a" and code as Y=1, N=2
LE4years_mother <- data.frame(idm, # add ALPSAC mother ID here
                       j300a, # Partner Died > CH 30 MTHs y/n
                       j301a_rec, # 1 of MUMs Children Died> CH 30 MTHs y/n
                       j302a_rec, # MUMs FRD or Relative Died> CH 30 MTHs y/n
                       j303a_rec, # 1 of MUMs CDRN Ill> CH 30 MTHs y/n
                       j304a_rec, # Partner Ill> CH 30 MTHs y/n
                       j305a_rec, # MUM FRD or Relative Ill> CH 30 MTHs y/n
                       j306a_rec, # MUM in HOSP> CH 30 MTHs y/n
                       j310a_rec, # MUM was Very Ill> CH 30 MTHs y/n
                       j311a_rec, # Partner Lost Job> CH 30 MTHs y/n
                       j312a_rec, # Partner Had PROBs at Work> CH 30 MTHs y/n
                       j313a_rec, # MUM Had PROBs at Work> CH 30 MTHs y/n
                       j314a_rec, # MUM lost Job> CH 30 MTHs y/n
                       j321a_rec, # MUM Moved House> CH 30 MTHs y/n
                       j330a_rec, #MUM Became PREG> CH 30 MTHs y/n
                       j331a_rec, # MUM Began New Job> CH 30 MTHs y/n
                       j332a_rec, # MUM Returned to Work> CH 30 MTHs y/n
                       j333a_rec, # MUM Miscarried> CH 30 MTHs y/n
                       j335a_rec, # MUM Took An Exam> CH 30 MTHs y/n
                       j339a_rec, # MUMs House or Car Burgled> CH 30 MTHs y/n
                       j340a_rec, # Partner Began New Job> CH 30 MTHs y/n
                       j341a_rec, # MUMs Pet Died> CH 30 MTHs y/n
                       j342a_rec) # MUM Had Accident> CH 30 MTHs y/n) 

#LE 5 years (k4000 etc) are catagorical variables and therefore require re-coding to binarise. currently: 1-4 = yes, 5= no anything else = NA
LE5years_mother <- data.frame(idm, # add ALPSAC mother id here 
                       k4000a_rec, # Mothers partner died in past year
                       k4001a_rec, # Mothers child died in past year
                       k4002a_rec, # D3: Mothers friend or relative died in past year
                       k4003a_rec, # D4: Mothers child was ill in past year
                       k4004a_rec, # D5: Mothers partner was ill in past year
                       k4005a_rec, # D6: Mothers friend or relative was ill in past year
                       k4006a_rec, # D7: Mother was admitted to hospital in past year
                       k4010a_rec, # D11: Mother was very ill in past year
                       k4011a_rec, # Mothers partner lost job in past year
                       k4012a_rec, # Mothers partner had problems at work in past year
                       k4013a_rec, # Mother had problems at work in past year
                       k4014a_rec, # Mother lost her job in past year
                       k4021a_rec, # Mother moved house in past year
                       k4030a_rec, # Mother became pregnant in past year
                       k4031a_rec, # Mother started a new job in past year
                       k4032a_rec, #  Mother returned to work in past year
                       k4033a_rec, # Mother had a miscarriage in past year
                       k4035a_rec, # Mother took an examination in past year
                       k4039a_rec, # Mothers house or car was burgled in past year
                       k4040a_rec, # Mothers partner started a new job in past year
                       k4041a_rec, # Mothers pet died in past year
                       k4042a_rec) # Mother had an accident in past year)


#asabove, LE of mothers at 6 years have catageorical varibales, dischotmoise based on previous incidents
LE6years_mother <- data.frame(idm, # ID of ALPSAC mother
                       l4000a_rec, # Respondent's partner died since study child's 5th birthday
                       l4001a_rec, # One of respondent's children died since study child's 5th birthday
                       l4002a_rec, # Respondent's friend/relative died since study child's 5th birthday
                       l4003a_rec, # One of respondent's children was ill since study child's 5th birthday
                       l4004a_rec, # Respondent's partner was ill since study child's 5th birthday
                       l4005a_rec, # Respondent's friend/relative was ill since study child's 5th birthday
                       l4006a_rec, # Respondent was admitted to hospital since study child's 5th birthday
                       l4010a_rec, # Respondent was very ill since study child's 5th birthday
                       l4011a_rec, # Respondent's partner lost their job since study child's 5th birthday
                       l4012a_rec, # Respondent's partner had problems at work since study child's 5th birthday
                       l4013a_rec, # Respondent had problems at work since study child's 5th birthday
                       l4014a_rec, # Respondent lost their job since study child's 5th birthday
                       l4021a_rec, # Respondent moved house since study child's 5th birthday
                       l4030a_rec, # Respondent became pregnant since study child's 5th birthday
                       l4031a_rec, # Respondent started new job since study child's 5th birthday
                       l4032a_rec, # Respondent returned to work since study child's 5th birthday
                       l4033a_rec, # Respondent had miscarriage since study child's 5th birthday
                       l4035a_rec, # Respondent took an examination since study child's 5th birthday
                       l4039a_rec, # Respondent's house/car was burgled since study child's 5th birthday
                       l4042a_rec, # Respondent's partner started new job since study child's 5th birthday
                       l4043a_rec, # A pet of respondent died since study child's 5th birthday
                       l4044a_rec, # Respondent had an accident since study child's 5th birthday
                       l4041a_rec) # One of respondent's children started new school since study child's 5th birthday)


###### Now we will construct variables based on the child life events

#retain "b" for DICH variables, 1=YES, 2=NO, child LE at 18 months
LE18months_child <- data.frame(idc, # add ALSPAC child ID here
                               kd500b_rec, # Ch taken into car
                               kd501b_rec, # A pet died (adj)
                               kd502b_rec, # Ch moved home (adj)
                               kd503b_rec, # Ch had fright (adj)
                               kd506b_rec, # Ch separated from mum for > a wk (adj)
                               kd507b_rec, # Ch separated from dad for > a wk (adj)
                               kd508b_rec, # CH Acquired New Parent > 6 MTHS
                               kd509b_rec, # Ch had a new sibling (adj)
                               kd510b_rec, # Ch admitted to hospital (adj)
                               kd511b_rec, # Ch changed carer (adj)
                               kd512b_rec, # Ch separated from someone else (adj)
                               kd513b). # Ch started nursery (adj))

#binarised variables: Y=1, N=2, child LE at 30 months
LE30months_child <- data.frame(idc, #add ALPSAC child ID here
                               kf450a_rec # Child taken into care > 18 months, Y/N
                               kf451a_rec # A pet died > 18 months, Y/N
                               kf452a_rec # Child moved home > 18 months, Y/N
                               kf453a_rec # Child had fright > 18 months, Y/N
                               kf456a_rec # Child sep.from mother >1wk >18 mths, Y/N
                               kf457a_rec # Child sep.from father >1wk >18 mths, Y/N
                               kf458a_rec # Child got new parent > 18 months, Y/N
                               kf459a_rec # Child got new sibling > 18 months, Y/N
                               kf460a_rec # Child admitted to hospital >18 mths, Y/N
                               kf461a_rec # Child changed carer > 18 months, Y/N
                               kf462a_rec # Child sep.from somebody > 18 months, Y/N
                               kf463a_rec) # Child started new creche >18 months, Y/N)

LE3years_child <- data.frame(idc, #ID of ALSPAC child here
                             kj460a_rec, # Child Taken Into Care Y/N
                             kj461a_rec, # Pet died Y/N
                             kj462a_rec, # Child Moved Home Y/N
                             kj463a_rec, # Child Had Shock Y/N
                             kj466a_rec, # Child & Mum Separated Y/N
                             kj467a_rec, # Child & Dad Separated Y/N
                             kj468a_rec, # Child Got a New Parent Y/N
                             kj469a_rec, # Child Got a New Sibling Y/N
                             kj470a_rec, # Child Admitted To Hospital Y/N
                             kj471a_rec, # Child Changed Carer Y/N
                             kj472a_rec, # Child Separated From Someone Else Y/N
                             kj473a_rec). # Child Started New Creche Y/N)

#non binarised variables, emit "a" in variable name,  1-4 = YES, 5= NO, all other = NA
LE4years_child <- data.frame(idc, #add ID of ALSPAC child here
                             kl470a_rec, # Child taken into care since age 3
                             kl471a_rec, # A pet died since child age 3
                             kl472a_rec, # Child moved home since age 3
                             kl473a_rec, # Child had shock or fright since age 3
                             kl476a_rec, # Child separated from mother since age 3
                             kl477a_rec, # Child separated from father since age 3
                             kl478a_rec, # Child acquired new mother or father since age 3
                             kl479a_rec, # Child had new brother or sister since age 3
                             kl480a_rec, # Child admitted to hospital since age 3
                             kl481a_rec, # Child changed care taker since age 3
                             kl482a_rec, # Child separated from someone else since age 3
                             kl483a_rec, # Child started new nursery/kindergarten since age 3
                             kl484a_rec). # Child started school since age 3)

#non DICH variable, omit "a" in varibale name, 1-4 = YES, 5= NO, all other = NA
LE5years_child <- data.frame(idc, #add alspac child ID
                             kn4000a_rec, #  Child taken into care in past 15 months
                             kn4001a_rec, # Child's pet die in past 15 months
                             kn4002a_rec, # Child move home in past 15 months
                             kn4003a_rec, # Child have a fright or shock in past 15 months
                             kn4006a_rec, # Child separated from mother in past 15 months
                             kn4007a_rec, # Child separated from Father in past 15 months
                             kn4008a_rec, # Child acquire new parent in past 15 months
                             kn4009a_rec, # Child have a new brother or sister in past 15 months
                             kn4010a_rec, #  Child admitted to hospital in past 15 months
                             kn4011a_rec, # Child's main carer change in past 15 months
                             kn4012a_rec, # Child separated from another person in past 15 months
                             kn4013a_rec, # Child start a new nursery in past 15 months
                             kn4014a_rec) # Child start school in past 15 months)

#DICH variable for LE in children at 6 years, retain "a", 2=NO 1= YES
LE6years_child <- data.frame(idc, # 
                             kq360a_rec, # Child was taken into care since his/her 5th birthday (Y/N)
                             kq361a_rec, # A pet died since child's 5th birthday (Y/N)
                             kq362a_rec, # Child moved home since his/her 5th birthday (Y/N)
                             kq363a_rec, # Child had a shock/fright since his/her 5th birthday (Y/N)
                             kq366a_rec, #  Somebody in the family died since child's 5th birthday (Y/N)
                             kq367a_rec, # Child was separated from his/her mother since his/her 5th birthday (Y/N)
                             kq368a_rec, # Child was separated from his/her father since his/her 5th birthday (Y/N)
                             kq369a_rec, # Child acquired a new mother/father since his/her 5th birthday (Y/N)
                             kq370a_rec, # Child had a new brother or sister since his/her 5th birthday (Y/N)
                             kq371a_rec, # Child was admitted to hospital since his/her 5th birthday (Y/N)
                             kq372a_rec, # Child changed care taker since his/her 5th birthday (Y/N)
                             kq373a_rec) # Child was separated from another close person since his/her 5th birthday (Y/N)


#combining all life event variables with mother ID
LEsum_mother <- data.frame(LE8weeks_mother, LE8months_mother, LE21months_mother, LE3years_mother, LE4years_mother, 
                                     LE5years_mother, LE6years_mother)

#combining all life event vairables with child ID
LEsum_child <- data.frame(LE18months_child, LE30months_child, LE3years_child, 
                          LE4years_child, LE5years_child, LE6years_child)

#_______________________________________________________________________________
#_______________________________________________________________________________



CRsum_mother <- data.frame(e418a_rec, # Income reduced since MID PREG	
                           f238a_rec,	# Reduced income
                           g318a_rec,	
                           h228a_rec,	
                           j318a_rec,	
                           k4018a_rec,	
                           l4018a_rec,
                           e423a_rec, # Became homeless since PREG	
                           f243a_rec, # Became homeless	
                           g323a_rec,	
                           h233a_rec,	
                           j323a_rec,	
                           k4023a_rec,	
                           l4023a_rec,
                           e424a_rec, # Major financial PROB since MID PREG	
                           f244a_rec, # Major financial problems
                           g324a_rec,	
                           h234a_rec,	
                           j324a_rec,	
                           k4024a_rec,	
                           l4024a_rec,
                           b2,	t2, b3,	t3, b4,	t4, b6,	t6). #need to find out what this is from Serena/charlotte.
 
#___________________________________________________________________________________

MHsum_mother <- data.frame(e407a_rec, #Trouble with law since MID PREG
                           f227a_rec,	#Mum in trouble with law Y/N
                           g307a_rec,	
                           h217a_rec,	
                           j307a_rec,	
                           k4007a_rec,
                           l4007a_rec,
                           e416a_rec, # PTNR in trouble with law since MID PREG
                           f236a_rec, # Partner in trouble with law	
                           g316a_rec,
                           h226a_rec,	
                           j316a_rec,	
                           k4016a_rec,
                           l4016a_rec,
                           e427a_rec, # Attempted suicide since MID PREG
                           f248a_rec, # 	Attempted suicide
                           g328a_rec,	
                           h238a_rec,	
                           j328a_rec,	
                           k4028a_rec,	
                           l4028a_rec,
                           e428a_rec, # Convicted since MID PREG
                           f249a_rec, # Court conviction
                           g329a_rec,
                           h239a_rec,
                           j329a_rec,
                           k4029a_rec,
                           l4029a_rec,
                           e435a_rec, # Attempted abortion since MID PREG
                           f254a_rec,	#Mum had abortion
                           g334a_rec,
                           h244a_rec,
                           j334a_rec,	
                           k4034a_rec,	
                           l4034a_rec,
                           b1,	t1, # find values for these variables (include/delete?)
                           b5,	t5,
                           b12,	t12,
                           b13	,t13,
                           b14,	t14,
                           b15,	t15)

#______________________________________________________

FRsum_mother <- data.frame(e408a_rec, # Divorced since MID PREG
                           e409a_rec, #PTNR rejected CH since MID PREG
                           e415a_rec, #PTNR went away since MID PREG
                           e417a_rec, # Separated since MID PREG
                           e419a_rec, # Argued with PTNR since MID PREG
                           e420a_rec, # Argued with FAM/FRDS since MID PREG
                           e422a_rec, # PTNR hurt MUM since MID PREG
                           e425a_rec, # Married since MID PREG
                           e426a_rec, # PTNR hurt CHDR since MID PREG
                           e437a_rec, # PTNR EMOT cruel to MUM since MID PREG
                           e438a_rec, # PTNR EMOT cruel to CH since MID PREG
                           f228a_rec, # Divorce 
                           f229a_rec, # Child not wanted by partner
                           f235a_rec, # Partner went away
                           f237a_rec, # Separation with partner
                           f239a_rec, # Argued with partner
                           f240a_rec, # Argued with family or friend
                           f242a_rec, # Physically hurt by partner
                           f245a_rec, # Got married
                           f246a_rec, # Parnter physically cruel to children
                           f256a_rec, # Partner emotionally cruel to Mum
                           f257a_rec, # Partner emotionally cruel to children
                           f247a_rec, # Mum physically cruel to children
                           g308a_rec, # Mum divorced >CH8MTHs
                           g309a_rec, # Partner rejected child >CH8MTHs
                           g315a_rec, # Partner went away >CH8MTHs
                           g317a_rec, # Mum and partner seperated >CH8MTHs
                           g319a_rec, # Mum argued with partner >CH8MTHs
                           g320a_rec, # Mum argued with family and friends >CH8MTHs
                           g322a_rec, # Partner physically cruel to Mum >CH8MTHs
                           g325a_rec, # Mum got married >CH8MTHs
                           g326a_rec, # Partner physically cruel to children >CH8MTHs
                           g327a_rec, # Mum physically cruel to children >CH8MTHs
                           g336a_rec, # Partner emotionally cruel to Mum >CH8MTHs
                           g337a_rec, # Partner emotionally cruel to children >CH8MTHs
                           g338a_rec, # Mum emotionally cruel to children >CH8MTHs
                           h218a_rec, # Whether got divorced since study child was 18 months old, Y/N
                           h219a_rec, # Whether partner rejected children since study child was 18 months old, Y/N
                           h225a_rec, # Whether partner went away since study child was 18 months old, Y/N
                           h227a_rec, # Whether mum and partner separated since study child was 18 months old, Y/N
                           h229a_rec, # Whether mum argued with partner since study child was 18 months old, Y/N
                           h230a_rec, # Whether mum argued with family and friends since study child was 18 months old, Y/N
                           h232a_rec, # Whether partner was physically cruel to mum since study child was 18 months old, Y/N
                           h235a_rec, # Whether mum got married since study child was 18 months old, Y/N
                           h236a_rec, # Whether partner was physically cruel to children since study child was 18 months old, Y/N
                           h237a_rec, # Whether mum was physically cruel to children since study child was 18 months old, Y/N
                           h246a_rec, # Whether partner was emotionally cruel to mum since study child was 18 months old, Y/N
                           h247a_rec, # Whether partner was emotionally cruel to children since study child was 18 months old, Y/N
                           h248a_rec, # Whether mum was emotionally cruel to children since study child was 18 months old, Y/N
                           j308a_rec, # MUM Divorced> CH 30 MTHs y/n
                           j309a_rec, # MUM Found PTR Not Want CH> CH 30 MTHs y/n
                           j315a_rec, # Partner Went Away> CH 30 MTHs y/n
                           j317a_rec, # MUM & Partner Separated> CH 30 MTHs y/n
                           j319a_rec, # MUM Argued W Partner> CH 30 MTHs y/n
                           j320a_rec, # MUM Argued W FMLY & FRDs> CH 30 MTHs y/n
                           j322a_rec, # Partner PHYS Cruel to MUM> CH 30 MTHs y/n
                           j325a_rec, # MUM Got Married> CH 30 MTHs y/n
                           j326a_rec, # PTR PHYS Cruel to CDRN> CH 30 MTHs y/n
                           j327a_rec, # MUM PHYS Cruel to CDRN> CH 30 MTHs y/n
                           j336a_rec, # PTR Emotionally Cruel to MUM> CH 30 MTHs y/n
                           j337a_rec, # PTR Emotional Cruel to CDRN> CH 30 MTHs y/n
                           j338a_rec, # MUM Emotional Cruel to CDRN> CH 30 MTHs y/n
                           k4008a_rec, # Mother was divorced in past year
                           k4009a_rec, # Mother found that her partner did not want her child in past year
                           k4015a_rec, # Mothers partner went away in past year
                           k4017a_rec, # Mother and partner separated in past year
                           k4019a_rec, # Mother argued with her partner in past year
                           k4020a_rec, # Mother argued with family and friends in past year
                           k4022a_rec, # Mothers partner was physically cruel to her in past year
                           k4025a_rec, # Mother got married in past year
                           k4026a_rec, # Mothers partner was physically cruel to children in past year
                           k4027a_rec, # Mother was physically cruel to children in past year
                           k4036a_rec, # Mothers partner was emotionally cruel to her in past year
                           k4037a_rec, # Mothers partner was emotionally cruel to children in past year
                           k4038a_rec, # Mother was emotionally cruel to children in past year
                           l4008a_rec, # Respondent was divorced since study child's 5th birthday
                           l4009a_rec, # Respondent found their partner did not want their child since study child's 5th birthday
                           l4015a_rec, # Respondent's partner went away since study child's 5th birthday
                           l4017a_rec, # Respondent separated from partner since study child's 5th birthday
                           l4019a_rec, # Respondent argued with partner since study child's 5th birthday
                           l4020a_rec, # Respondent argued with family/friends since study child's 5th birthday
                           l4022a_rec, # Respondent's partner was physically cruel to them since study child's 5th birthday
                           l4025a_rec, # Respondent got married since study child's 5th birthday
                           l4026a_rec, # Respondent's partner physically cruel to respondent's children since study child's 5th birthday
                           l4027a_rec, # Respondent physically cruel to own children since study child's 5th birthday
                           l4036a_rec, # Respondent's partner was emotionally cruel to them since study child's 5th birthday
                           l4037a_rec, # Respondent's partner was emotionally cruel to respondent's children since study child's 5th birthday
                           l4038a_rec, # Respondent was emotionally cruel to their children since study child's 5th birthday
                           l4040a_rec, # Respondent found new partner since study child's 5th birthday
                           b7,
                           b8,
                           b9,
                           b10,
                           b11,
                           b16,
                           b17,
                           b18,
                           t7,
                           t8,
                           t9,
                           t10,
                           t11,
                           DV_Shouted_21M,    #find these variables (delete/include?)
                           DV_Hit_21M,
                           DV_Break_21M,
                           DV_Shouted_33M,
                           DV_Hit_33M,
                           DV_Break_33M,
                           DV_Shouted_6Y,
                           DV_Hit_6Y,
                           DV_Break_6Y,
                           Par_Shout_0_7_MedSplit,
                           Par_Smack_0_7_MedSplit).)

#__________________________________________________________________

DVsum_child <- data.frame( kd504b_rec,	 # Ch physically hurt by someone (adj)
                            kf454a_rec,	# Child physically hurt > 18 months, Y/N
                            kj464a_rec,	# Child Was Physically Hurt By Person Y/N
                            kl474a_rec,	# Child was physically hurt by someone since age 3 (not Y/N)
                            kn4004a_rec,	# Child physically hurt by someone in past 15 months
                            kq364a_rec, # Child was physically hurt by someone since his/her 5th birthday (Y/N)
                            kd505b_rec,	# Ch sexually abused (adj)
                            kf455a_rec,	# Child sexually abused > 18 months, Y/N
                            kj465a_rec,	# Child Sexually Abused Y/N
                            kl475a_rec,	# Child was sexually abused since age 3
                            kn4005a_rec,	# Child sexually abused in past 15 months
                            kq365a_rec, # Child was sexually abused since his/her 5th birthday (Y/N)
                            SDQBullied_4_YN,	SDQBullied_7_YN)

#________________________________________________________________________


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# This function merges together all separate data.frames according to the IDC
# results in a dataframe with all relevant items for postnatal stress.
# tech-tip: because merge can only take two data.frames at the time and I am lazy, I used Reduce.
postnatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDC',  all = TRUE),
                           list(LEsum_child, DVsum_child)) 
#-------------------------------------------------------------------------------
fetal_general <- readquick("FETALPERIOD-ALLGENERALDATA_29012018.sav") # 9778 obs of 95 var

early_parent <- data.frame(fetal_general$idm, 
                           ifelse(fetal_general$age_m_v2 < 19, yes = 1, no = 0), # mother younger than 19 years at intake (for PR)
                           # DICH: based on Cecil et al. (2014); Rijlaarsdam et al. (2016).
                           ifelse(fetal_general$age_p_v2 < 19, yes = 1, no = 0), # partner younger than 19 years (for PR)
                           ifelse(fetal_general$age_bf_v2 < 19, yes = 1, no = 0)) # biological father younger than 19 years (for PR). 
# In many cases only one of the last two was filled, but cases 
# which had both variables available will be combined later.
colnames(early_parent) <- c("IDM","m_age","partner_age","biofather_age")

postnatal_stress <- merge(postnatal_stress, early_parent, by = 'IDM', all = T)

################################################################################
#### -------------------- Combine repeated measures  ---------------------- ####
################################################################################

# Repeating a grade was measured by maternal report at age 8 (GR1080), 9 (le8) 
# and 10 (GR1082). # Once a risk, always a risk strategy:
postnatal_stress$repeated_grade <- repmeas(postnatal_stress[,c('rep_grade_8yrs','rep_grade_9yrs','rep_grade_10yrs')])

# Financial difficulties were measured at age 3 (GR1065) and 9 (le10). 
# Once a risk, always a risk strategy:
postnatal_stress$financial_difficulties <- repmeas(postnatal_stress[,c('fidi_3yrs', 'fidi_9yrs')]) 

# Trouble paying for food was measured at age 3 (GR1065) and 9 (GR1081). 
# Once a risk, always a risk strategy:
postnatal_stress$trouble_pay_childhood <- repmeas(postnatal_stress[,c('trouble_pay_3yrs','trouble_pay_9yrs')]) 

# Income was assessed at age 3 (GR1065), 5 (GR1075) and 9 (GR1081).
# Two items: once-always + chronic risk strategy.
postnatal_stress$income_once <- repmeas(postnatal_stress[,c('income_3yrs', 'income_5yrs', 'income_9yrs')])
postnatal_stress$income_chronic <- repmeas(postnatal_stress[,c('income_3yrs', 'income_5yrs', 'income_9yrs')], 
                                           strategy = 'chronic')

# Unemployment was measured at age 3 (GR1065), 5 (GR1075, by mother and partner)
# and 9 (GR1081). # Two items: once-always + chronic risk strategy.
postnatal_stress$unemployed_once <- repmeas(postnatal_stress[,c('empl_3yrs','m_empl_5yrs', 'p_empl_5yrs', 'empl_9yrs')]) 
postnatal_stress$unemployed_chronic <- repmeas(postnatal_stress[,c('empl_3yrs','m_empl_5yrs', 'p_empl_5yrs', 'empl_9yrs')],
                                               strategy = 'chronic')

# Material deprivation measure is based on Statistics Netherlands and EU-SILC,
# (e.g. family possessed a car or a fridge, yes/no). To increase resemblance with EU-SILC, 
# we added an item about how many televisions the family possessed. 
# Combine material deprivation + television items. 
made <- postnatal_stress[, c("mat_depr_heating", "mat_depr_rent", "mat_depr_meal", "mat_depr_car", 
                             "mat_depr_washingmachine", "mat_depr_refrigerator", "mat_depr_telephone", 
                             "mat_depr_holidays","mat_depr_tv")]
# material_deprivation reflects the family's ability to afford basic needs and services.
# If 'possession rate' is lower than 75%, risk is present. Rate is based on EU-SILC (2009).
postnatal_stress$material_deprivation <- ifelse((rowMeans(is.na(made)) < .25),
                                                yes = ifelse(rowMeans(made, na.rm = T) >= .25, yes = 1, no = 0), no = NA) 
# NOTE: we set material deprivation values to NA if the missing frequency within the questionnaire is >25%

# Education level of main caregiver and partner was assessed at age 3 and 5 (GR1075).
# Education level of each parent is included separately in the domain score. 
# Once a risk, always a risk strategy:
postnatal_stress$m_education <- repmeas(postnatal_stress[,c("m_educ_3yrs", "m_educ_5yrs")])
postnatal_stress$p_education <- repmeas(postnatal_stress[,c("p_educ_3yrs", "p_educ_5yrs")])

# Early parenthood. Age of father (partner or biological father) was assessed during intake. 
# In many cases only one of these was filled, but for cases which had both variables 
# available (and different values), we apply the once-always strategy.
postnatal_stress$p_age <- repmeas(postnatal_stress[,c("partner_age","biofather_age")])

# Psychopathology included three syndrome domains: interpersonal sensitivity, 
# depression and anxiety, measured in main caregivers and partners at the ages 3 and 9.
postnatal_stress$m_interpersonal_sensitivity <- repmeas(postnatal_stress[,c("m_is_3yrs", "m_is_9yrs")])
postnatal_stress$p_interpersonal_sensitivity <- repmeas(postnatal_stress[,c("f_is_3yrs", "f_is_9yrs")])
postnatal_stress$m_depression <- repmeas(postnatal_stress[,c("m_dep_3yrs", "m_dep_9yrs")])
postnatal_stress$p_depression <- repmeas(postnatal_stress[,c("f_dep_3yrs", "f_dep_9yrs")])
postnatal_stress$m_anxiety <- repmeas(postnatal_stress[,c("m_anx_3yrs", "m_anx_9yrs")])
postnatal_stress$p_anxiety <- repmeas(postnatal_stress[,c("f_anx_3yrs", "f_anx_9yrs")])

# Marital status was measured at age 3 (GR1065X), 5 (GR1075) and 9 (le14).
# Once a risk, always a risk strategy:
postnatal_stress$marital_status <- repmeas(postnatal_stress[,c('marital_status_3yrs','marital_status_5yrs','divorce_childhood')])

# Family size was measured at age 3 (GR1065G), 5 (GR1075B) and 9 (GR1081)
# Once a risk, always a risk strategy:
postnatal_stress$family_size <- repmeas(postnatal_stress[,c('fam_size_3yrs', 'fam_size_5yrs', 'fam_size_9yrs')])

# Bullying was assessed by main caregiver and teacher. 
# Once a risk, always a risk strategy: if child was verbally/physically/relationally 
# bullied once a week or more, as reported by the main caregiver or the teacher = risk. 
postnatal_stress$bullying <- repmeas(postnatal_stress[,c('bully_physical_m','bully_verbal_m','bully_excluded_m', 
                                                         'bully_physical_t','bully_verbal_t','bully_excluded_t')])

################################################################################
#------------------------------------------------------------------------------#
# Before we get to the final scores, some summary stats that may come in handy #
#------------------------------------------------------------------------------#

# Let's have a look at risk distribution and missing data per indicator.
postnatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))
for (i in 3:ncol(postnatal_stress)) { # because the third column is not dichotomous (BSI_age)
  s = summary(as.factor(postnatal_stress[,i]))
  if (length(s) < 3) { # I am doing this ugly thing as a quick & dirty fix for lack of risk in fam_size_9yrs
    s = c(s[1], 0, s[2])
    names(s)[2] = "1"
  }
  c = colnames(postnatal_stress)[i]
  postnatal_summary[1:3,c] <- s 
  postnatal_summary[4,c] <- round((postnatal_summary[2,c] / 9901)*100, 2)
  postnatal_summary[5,c] <- round((postnatal_summary[3,c] / 9901)*100, 2)
}

#-------------------------------------------------------------------------------
# Apply the percent_missing function to the rows (1) of the entire dataset (total
# 51 indicators). NOTE: this is a little less straightforward compared to the prenatal 
# score, so to be safe, I will list all the indicator names. 
postnatal_stress$post_percent_missing = apply(postnatal_stress[,c(
  'sick_or_accident','family_member_ill','smbd_important_ill','parent_died','smbd_important_died','pet_died','school_workload','repeated_grade','lost_smth_important','moved','changed_school','friend_moved','fire_or_burglary', # LE
  'tension_at_work','material_deprivation','financial_difficulties','neiborhood_problems','trouble_pay_childhood','income_once','income_chronic','unemployed_once','unemployed_chronic', # CR
  'm_education','p_education','m_interpersonal_sensitivity','p_interpersonal_sensitivity','m_depression','p_depression','m_anxiety','p_anxiety','m_age','p_age', # PR
  'marital_problems','marital_status','family_size','m_fad_5yrs','m_fad_9yrs','p_fad_9yrs','conflict_family_member','conflict_smbd_else','conflict_in_family','divorce_childhood','argument_friend', # IR
  'm_harsh_parent','p_harsh_parent','bullying','physical_violence','physical_threats','sexual_harrasment','sexual_behavior','rumors_or_gossip')], # DV
  1, percent_missing)

#-------------------------------------------------------------------------------

################################################################################
#### -------------- create the (un-weighted) domain scores ---------------- ####
################################################################################

# ATTENTION! Here we use de default argument of domainscore function: calculating  a 
# *mean domain score* (range = 0 to 1) that is NOT adjusted for 25% missingness as in 
# e.g. Rijlaarsdam et al. (2016). If you prefer working with the actual number of risks
# (i.e. sum score) or the weighted version of it, you can set the argument score_type
# to 'sum_simple' or 'sum_weighted' respectively (see Setup and functions script
# for calculation details). 

# LE
postnatal_stress[,c('post_LE_percent_missing','post_life_events')] <- domainscore(postnatal_stress[,c(
  'sick_or_accident', # Did your child get seriously sick or did he/she have an accident? Yes
  'family_member_ill', # Did a family member get seriously sick or a serious accident? Yes
  'smbd_important_ill', # Did somebody else, who is important to your child, get seriously sick or have a serious accident? Yes
  'parent_died', # Is the father/mother or other caretaker still alive? No 
  'smbd_important_died', # Did somebody else, who your child cared about a lot, die? Yes
  'pet_died', # Did ever a pet, who you child cared about a lot, die? Yes
  'school_workload', # Does or did your child have to deal with a high workload at school? Yes
  'repeated_grade', # Did your child ever repeat a grade? Yes
  'lost_smth_important', # Did your child ever lose something which was important to him/her? Yes
  'moved', # Did your child move? Yes 
  'changed_school', # Did your child change schools? Yes
  'friend_moved', # Did one of your child's friends move to a new house? Yes
  'fire_or_burglary' # Was there a fire or burglary? Yes 
)])

# CR
postnatal_stress[,c('post_CR_percent_missing','post_contextual_risk')] <- domainscore(postnatal_stress[,c(
  'tension_at_work', # Tension at the parents' work that has been felt at home; yes
  'material_deprivation', # Material deprivation; yes
  'financial_difficulties', # Does your family have or ever had financial difficulties? Yes 
  'neiborhood_problems', # Are/were there problems in the neighborhood? (e.g. vandalism or insecurity); yes
  'trouble_pay_childhood', # Trouble paying for your food, rent, electricity bill and such in the past year? Yes
  'income_once', # Income household; < 1600 euros p/m; once 
  'income_chronic', # Income household; < 1600 euros p/m; chronically
  'unemployed_once', # Unemployment within the family, once
  'unemployed_chronic' # Unemployment within the family, chronically
)])

# PR
postnatal_stress[,c('post_PR_percent_missing','post_parental_risk')] <- domainscore(postnatal_stress[,c(
  'm_education', # Education main caregiver; < phase 1 (higher) education, once 
  'p_education', # Education partner; < phase 1 (higher) education , once
  'm_interpersonal_sensitivity', # Interpersonal sensitivity main caregiver; >= 0.95
  'p_interpersonal_sensitivity', # Interpersonal sensitivity partner; >= 0.78
  'm_depression', # Depression main caregiver; >= 0.80
  'p_depression', # Depression partner; >= 0.71
  'm_anxiety', # Anxiety main caregiver; >= 0.71
  'p_anxiety', # Anxiety partner; >= 0.65
  'm_age', # Early parenthood; age mother < 19 yrs
  'p_age' # Early parenthood; age partner < 19 yrs
)])

# IR
postnatal_stress[,c('post_IR_percent_missing','post_interpersonal_risk')] <- domainscore(postnatal_stress[,c(
  'marital_problems', # Problems with marriage relations; yes
  'marital_status', # Marital status; single
  'family_size', # Family size; > 3 children
  'm_fad_5yrs', # Family distress according to main caregiver; > 2.17
  'm_fad_9yrs', # Family distress according to main caregiver; > 2.17
  'p_fad_9yrs', # Family distress according to partner; > 2.17
  'conflict_family_member', # Has your child ongoing conflicts with a family member (or ever had them)? Yes
  'conflict_smbd_else', # Has your child ongoing conflict with somebody else (or ever had them)? Yes 
  'conflict_in_family', # Do other family members have ongoing conflicts with each other (or ever had them)? Yes
  'divorce_childhood', # Are you and your partner divorced or separated? Yes
  'argument_friend' # Did your child lose a good friend due to an argument? Yes
)])

# DV
postnatal_stress[,c('post_DV_percent_missing','post_direct_victimization')] <- domainscore(postnatal_stress[,c(
  'm_harsh_parent', # Harsh parenting main caregiver > 80th percentile
  'p_harsh_parent', # Harsh parenting partner > 80th percentile
  'bullying', # Bullying more than once a week according to teacher or main caregiver; yes
  'physical_violence', # Did someone ever use physical violence against your child? For example, beating him/her up. Yes
  'physical_threats', # Did somebody use almost physical violence against your child? So that it did not actually happen, but the child was frightened. Yes
  'sexual_harrasment', # Did anybody make sexual comments or movements towards your child? Yes 
  'sexual_behavior', # Did your child participate in inappropriate sexual behavior? Yes
  'rumors_or_gossip' # Did somebody spread mean rumors or gossip about your child? Yes
)])


################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .rds file, in the directory where the raw data are stored
saveRDS(postnatal_stress, paste(pathtodata,'postnatal_stress.rds', sep = ""))
saveRDS(postnatal_summary, paste(pathtodata,'postnatal_stress_summary.rds', sep = ""))