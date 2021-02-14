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

*************************************
  LE 3Y
*************************************.

 h210a_rec # Whether partner died since study child was 18 months old, Y/N
h211a_rec # Whether one of mums children died since study child was 18 months old, Y/N
h212a_rec # Whether a friend or relative died since study child was 18 months old, Y/N
h213a_rec # Whether one of mums children was ill since study child was 18 months old, Y/N
h214a_rec # Whether partner was ill since study child was 18 months old, Y/N
h215a_rec # Whether a friend or relative was ill since study child was 18 months old, Y/N
h216a_rec # Whether mum was admitted to hospital since study child was 18 months old, Y/N
h220a_rec # Whether mum was very ill since study child was 18 months old, Y/N
h221a_rec # Whether partner lost job since study child was 18 months old, Y/N
h222a_rec # Whether partner had problems at work since study child was 18 months old, Y/N
h223a_rec # Whether mum had problems at work since study child was 18 months old, Y/N
h224a_rec # Whether mum lost job since study child was 18 months old, Y/N
h231a_rec # Whether mum moved house since study child was 18 months old, Y/N
h240a_rec # Whether mum became pregnant since study child was 18 months old, Y/N
h241a_rec # Whether mum started a new job since study child was 18 months old, Y/N
h242a_rec # Whether mum returned to work since study child was 18 months old, Y/N
h243a_rec # Whether mum had a miscarriage since study child was 18 months old, Y/N
h245a_rec # Whether mum took an exam since study child was 18 months old, Y/N
h249a_rec # Whether house or car was burgled since study child was 18 months old, Y/N
h250a_rec # Whether partner started a new job since study child was 18 months old, Y/N
h251a_rec # Whether a pet died since study child was 18 months old, Y/N
h252a_rec # Whether mum had an accident since study child was 18 months old, Y/N


*************************************
  LE 4Y
*************************************.
missing values 
j300a # Partner Died > CH 30 MTHs y/n
j301a_rec # 1 of MUMs Children Died> CH 30 MTHs y/n
j302a_rec # MUMs FRD or Relative Died> CH 30 MTHs y/n
j303a_rec # 1 of MUMs CDRN Ill> CH 30 MTHs y/n
j304a_rec # Partner Ill> CH 30 MTHs y/n
j305a_rec # MUM FRD or Relative Ill> CH 30 MTHs y/n
j306a_rec # MUM in HOSP> CH 30 MTHs y/n
j310a_rec # MUM was Very Ill> CH 30 MTHs y/n
j311a_rec # Partner Lost Job> CH 30 MTHs y/n
j312a_rec # Partner Had PROBs at Work> CH 30 MTHs y/n
j313a_rec # MUM Had PROBs at Work> CH 30 MTHs y/n
j314a_rec # MUM lost Job> CH 30 MTHs y/n
j321a_rec # MUM Moved House> CH 30 MTHs y/n
j330a_rec #MUM Became PREG> CH 30 MTHs y/n
j331a_rec # MUM Began New Job> CH 30 MTHs y/n
j332a_rec # MUM Returned to Work> CH 30 MTHs y/n
j333a_rec # MUM Miscarried> CH 30 MTHs y/n
j335a_rec # MUM Took An Exam> CH 30 MTHs y/n
j339a_rec # MUMs House or Car Burgled> CH 30 MTHs y/n
j340a_rec # Partner Began New Job> CH 30 MTHs y/n
j341a_rec # MUMs Pet Died> CH 30 MTHs y/n
j342a_rec # MUM Had Accident> CH 30 MTHs y/n
(-999).

************************************* *************************************
LE 5Y # in closer and search catalogue the "a" is removed, and so isnt binary, 
*************************************.
missing values 
k4000a_rec # Mothers partner died in past year
k4001a_rec # Mothers child died in past year
k4002a_rec # D3: Mothers friend or relative died in past year
k4003a_rec # D4: Mothers child was ill in past year
k4004a_rec # D5: Mothers partner was ill in past year
k4005a_rec # D6: Mothers friend or relative was ill in past year
k4006a_rec # D7: Mother was admitted to hospital in past year
k4010a_rec # D11: Mother was very ill in past year
k4011a_rec # Mothers partner lost job in past year
k4012a_rec # Mothers partner had problems at work in past year
k4013a_rec # Mother had problems at work in past year
k4014a_rec # Mother lost her job in past year
k4021a_rec # Mother moved house in past year
k4030a_rec # Mother became pregnant in past year
k4031a_rec # Mother started a new job in past year
k4032a_rec #  Mother returned to work in past year
k4033a_rec # Mother had a miscarriage in past year
k4035a_rec # Mother took an examination in past year
k4039a_rec # Mothers house or car was burgled in past year
k4040a_rec # Mothers partner started a new job in past year
k4041a_rec # Mothers pet died in past year
k4042a_rec # Mother had an accident in past year
(-999).

*************************************
  LE 6Y
*************************************.
missing values 
l4000a_rec # Respondent's partner died since study child's 5th birthday
l4001a_rec # One of respondent's children died since study child's 5th birthday
l4002a_rec # Respondent's friend/relative died since study child's 5th birthday
l4003a_rec # One of respondent's children was ill since study child's 5th birthday
l4004a_rec # Respondent's partner was ill since study child's 5th birthday
l4005a_rec # Respondent's friend/relative was ill since study child's 5th birthday
l4006a_rec # Respondent was admitted to hospital since study child's 5th birthday
l4010a_rec # Respondent was very ill since study child's 5th birthday
l4011a_rec # Respondent's partner lost their job since study child's 5th birthday
l4012a_rec # Respondent's partner had problems at work since study child's 5th birthday
l4013a_rec # Respondent had problems at work since study child's 5th birthday
l4014a_rec # Respondent lost their job since study child's 5th birthday
l4021a_rec # Respondent moved house since study child's 5th birthday
l4030a_rec # Respondent became pregnant since study child's 5th birthday
l4031a_rec # Respondent started new job since study child's 5th birthday
l4032a_rec # Respondent returned to work since study child's 5th birthday
l4033a_rec # Respondent had miscarriage since study child's 5th birthday
l4035a_rec # Respondent took an examination since study child's 5th birthday
l4039a_rec # Respondent's house/car was burgled since study child's 5th birthday
l4042a_rec # Respondent's partner started new job since study child's 5th birthday
l4043a_rec # A pet of respondent died since study child's 5th birthday
l4044a_rec # Respondent had an accident since study child's 5th birthday
l4041a_rec # One of respondent's children started new school since study child's 5th birthday
(-999).

*************************************
  LE CHILD 18M
*************************************.

COMPUTE CS_LEChild_1Y = SUM(
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
  kd513b). # Ch started nursery (adj)
Execute.

*************************************
  LE CHILD 30M
*************************************.
missing values 
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
kf463a_rec # Child started new creche >18 months, Y/N
(-999).

*************************************
  LE CHILD 3Y
*************************************.
COMPUTE CS_LEChild_3Y = SUM(
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
  kj473a_rec). # Child Started New Creche Y/N
Execute.

*************************************
  LE CHILD 4Y #ALSPAC non-binary
*************************************.


COMPUTE CS_LEChild_4Y = SUM(
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
  kl484a_rec). # Child started school since age 3
Execute.

*************************************
  LE CHILD 5Y # ASLPAC non binary
*************************************.

COMPUTE CS_LEChild_5Y = SUM(
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
  kn4014a_rec) # Child start school in past 15 months
Execute.

*************************************
  LE CHILD 6Y
*************************************.

COMPUTE CS_LEChild_6Y = SUM(
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
  kq373a_rec). # Child was separated from another close person since his/her 5th birthday (Y/N
execute. 