*************************************************************************************************************
  CORRELATIONS BETWEEN RISK ITEMS & CUMULATIVE RISK INDEXES
*************************************************************************************************************.

******************************************************************
  POSTNATAL 0-7
*****************************************************************
  1. LIFE EVENTS
****************************************
  INDIVIDUAL SUBSCALES
*************************************
  LE 8 W

## Life Events Interview ( 8 weeks)
# some of the tot 26 items are used mainly for lE domain but also for IR, DV, CR. 
LE_interview <- readquick("MOTHERTRAUMAINTERVIEW9_24112016.sav") # Find questionairre code from ALSPAC

# Exclude unreliable interviews
LE_interview <- subset(LE_interview, LE_interview$unreliable == 0) # Find questionairre code from ALSPAC

# Select the necessary item 

*************************************.

#ALSPAC non-binary variables 
life_events <- 
data.frame(
  e400a_rec, # PTNR died since MID PREG
  e401a_rec, # CH died since MID PREG
  e402a_rec, # FRD or REL died since MID PREG
  e403a_rec, # CH ill since MID PREG
  e404a_rec, # PTNR ill since MID PREG
  e405a_rec, # Friend or REL ill since MID PREG
  e406a_rec, # Admitted to HOSP since MID PREG
  e410a_rec, # Ill since MID PREG
  e411a_rec, # PTNR lost job since MID PREG
  e412a_rec, # PTNR had PROBS at work since MID PREG
  e413a_rec, # PROBS at work since MID PREG
  e414a_rec, # Lost job since MID PREG
  e421a_rec, # Moved house since PREG
  e429a_rec, # MC scare since MID PREG
  e430a_rec, # Started new job since MID PREG
  e431a_rec, # Test for CH abnormality since MID PREG
  e432a_rec, # Test suggested CH PROB since MID PREG
  e433a_rec, # Discovered having twins since MID PREG
  e434a_rec, # Heard event might harm CH since MID PREG
  e436a_rec, # Took an exam since MID PREG
  e439a_rec, # House burglary/car theft since MID PREG
  e440a_rec) # Accident since MID PREG
Execute.

missing values 
e400a_rec # PTNR died since MID PREG
e401a_rec # CH died since MID PREG
e402a_rec # FRD or REL died since MID PREG
e403a_rec # CH ill since MID PREG
e404a_rec # PTNR ill since MID PREG
e405a_rec # Friend or REL ill since MID PREG
e406a_rec # Admitted to HOSP since MID PREG
e410a_rec # Ill since MID PREG
e411a_rec # PTNR lost job since MID PREG
e412a_rec # PTNR had PROBS at work since MID PREG
e413a_rec # PROBS at work since MID PREG
e414a_rec # Lost job since MID PREG
e421a_rec # Moved house since PREG
e429a_rec # MC scare since MID PREG
e430a_rec # Started new job since MID PREG
e431a_rec # Test for CH abnormality since MID PREG
e432a_rec # Test suggested CH PROB since MID PREG
e433a_rec # Discovered having twins since MID PREG
e434a_rec # Heard event might harm CH since MID PREG
e436a_rec # Took an exam since MID PREG
e439a_rec # House burglary/car theft since MID PREG
e440a_rec # Accident since MID PREG
(-999).

*************************************
  LE 8 M #ALSPAC using 1= YES, 2= NO, keep a on varabel name, for non-binary, delete "a"
*************************************.
missing values 
f220a_rec # Death of partner
f221a_rec # Death of one of children
f222a_rec # Death of friend or relative
f223a_rec # Child ill
f224a_rec # PTNR ill 
f225a_rec # Friend or relative ill
f226a_rec # Admitted to hospital
f230a_rec # Mum ill
f231a_rec # Parnter lost job
f232a_rec # Work problems for partner
f233a_rec # Work problems for Mum
f234a_rec # Mum lost job
f241a_rec # Moved house
f253a_rec # Mum had miscarriage
f251a_rec # New job for Mum
f250a_rec # Mum became pregnant
f252a_rec # Return to work
f255a_rec # Exam taken
f260a_rec # New job for partner
f261a_rec # Pet died
(-999).

COMPUTE CS_LE_8M = SUM(
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
  f261a_rec, # Pet died
Execute.

*************************************
  LE 21 M
*************************************.
missing values 
g300a_rec # Partner died >CH8MTHs
g301a_rec # One of Mums children died >CH8MTHs
g302a_rec # Friend or relative died >CH8MTHs
g303a_rec # One of Mums children ill >CH8MTHs
g304a_rec # Partner ill >CH8MTHs
g305a_rec # Friend or relative ill >CH8MTHs
g306a_rec # Mum in hospital >CH8MTHs
g310a_rec # Mum very ill >CH8MTHs
g311a_rec # Partner lost job >CH8MTHs
g312a_rec # Partner had problems with work >CH8MTHs
g313a_rec # Mum had problems with work >CH8MTHs
g314a_rec # Mum lost job >CH8MTHs
g321a_rec # Mum moved house >CH8MTHs
g330a_rec # Mum pregnant >CH8MTHs
g331a_rec # Mum started new job >CH8MTHs
g332a_rec # Mum returned to work >CH8MTHs
g333a_rec # Mum had miscarraige >CH8MTHs
g335a_rec # Mum took exam >CH8MTHs
g339a_rec # Mums house or car burgled >CH 8MTHs
g340a_rec # Partner started new job >CH8MTHs
g341a_rec # A pet died >CH8MTHs
g342a_rec # Mum had accident >CH8MTHs
(-999).

COMPUTE CS_LE_1Y = SUM(
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
  g342a_rec, # Mum had accident >CH8MTHs).
Execute.