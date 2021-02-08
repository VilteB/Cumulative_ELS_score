## Life Events Interview (9 yr)

LE_interview <- readquick("insert ALSPAC data") # 5862 obs of 96 columns

# Exclude unreliable interviews
LE_interview <- subset(LE_interview, LE_interview$unreliable == 0) # need to see ALSPAC data for the corresponding code here

# Select the necessary item 
life_events <- 
  data.frame(LE_interview$?, # child id to be added 
             LE_interview$p2000_rec, # Husband/partner died since the study child's 6th birthday 
             LE_interview$p2001_rec, # One of mother's children died since the study child's 6th birthday
             LE_interview$p2002_rec, # Mother's friend or relative died since the study child's 6th birthday
             LE_interview$p2003_rec, # One of mother's children was ill since the study child's 6th birthday
             LE_interview$p2004_rec, # Mother's husband/partner was ill since the study child's 6th birthday
             LE_interview$p2005_rec, # Mother's friend or relative was ill since the study child's 6th birthday
             LE_interview$p2006_rec, # Mother was admitted to hospital since the study child's 6th birthday
             LE_interview$p2010_rec, # Mother was very ill since the study child's 6th birthday
             LE_interview$p2011_rec, # Mother's husband/partner lost his job since the study child's 6th birthday
             LE_interview$p2012_rec, # Mother's husband/partner had problems at work since the study child's 6th birthday
             LE_interview$p2013_rec, # Mother had problems at work since the study child's 6th birthday
             LE_interview$p2014_rec, # Mother lost her job since the study child's 6th birthday
             LE_interview$p2021_rec, # Mother moved house since the study child's 6th birthday
             LE_interview$p2030_rec, # Mother became pregnant since the study child's 6th birthday
             LE_interview$p2031_rec, # Mother started a new job since the study child's 6th birthday
             LE_interview$p2032_rec, # Mother returned to work since the study child's 6th birthday
             LE_interview$p2033_rec, # Mother had a miscarriage since the study child's 6th birthday
             LE_interview$p2035_rec, # Mother took an examination since the study child's 6th birthday
             LE_interview$p2039_rec, # Mother's house or car was burgled since the study child's 6th birthday
             LE_interview$p2041_rec, # One of mother's children started school since the study child's 6th birthday
             LE_interview$p2042_rec, # Mother's husband/partner started a new job since the study child's 6th birthday
             LE_interview$p2043_rec, # A pet died since the study child's 6th birthday
             LE_interview$p2044_rec) # Mother had an accident since the study child's 6th birthday
colnames(life_events) <- c("IDC","sick_or_accident","family_member_ill","smbd_important_ill","parent_died","smbd_important_died","pet_died","school_workload",
                           "rep_grade_9yrs","neiborhood_problems","fidi_9yrs","conflict_family_member","conflict_smbd_else","conflict_in_family","divorce_childhood",
                           "argument_friend","lost_smth_important","physical_violence","physical_threats","sexual_harrasment","sexual_behavior","rumors_or_gossip",
                           "moved","changed_school")