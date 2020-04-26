
## Shiny App: Outcome Prediction Viz ##
## Data Cleaning ## 

#################################################################################
#  DATA CLEANING STEPS                                                          #
#  1. Select only the first course of treatments in 17-19 dataset               #
#  2. Pare down unnecessary variables                                           #
#  3. Separate first lines of CLICC and SDS for each client                     #
#  4. Remove duplicate appointment lines                                        #
#  5. Average across lines of duplicate CCAPS                                   #
#  6. Filter on clients with 2+ individual therapy sessions                     #
#  7. Filter on clients with 2+ CCAPS administrations                           #
#  8. Recombine appointment and CCAPS data                                      #
#  9. Pull dates of first and last appointments and CCAPS; only keep clients    #
#     with CCAPS administrations within 14 days of first and last appointments  #
# 10. Grouping by client, create a variable that sums individual appointments   #
# 11. Create percentage attended variable: attended appointments/total scheduled#
# 12. Operationalize CCAPS subscale scores: First and last                      # 
# 13. Operationalize CCAPS subscale scores: Change score (post-pre)             #
# 14. Use CCMHr package to create high- and low-cut variables                   #
# 15. Create CCAPS subscale score ranking variables based on high and low cuts  #
# 16. Operationalize single CCAPS items: full-scale factor + ranking factor     #
#     a. 51 - "I have thoughts of ending my life."                              #
#     b. 68 - "I have thoughts of hurting others."                              #
# 17. Keep/operationalize SDS items:                                            #
#     a. 25 - Client age                                                        #
#     b. 88 - Gender identity (factor)                                          #
#     c. 91 - Sexuality (factor)                                                #
#     d. 95 - Race/Ethnicity (factor + minority factor)                         #
#     e. 1 - Prior counseling (factor + dichotomous factor)                     #
#     f. 2 - Prior medication use (factor + history factor)                     #
#     g. 64 - Prior hospitalization (factor + dichotomous factor)               #
#     h. 72 - Prior self-injury (factor + dichotomous factor)                   #
#     i. 74 - Prior suicidal ideation (factor + dichotomous factor)             #
#     j. 76 - Prior suicide attempt (factor + dichotomous factor)               #
# 18. Keep/operationalize CLICC items:                                          #
#     a. CLICC_01_*                                                             #
#     b. Number of concerns - Within a client, sum across CLICC_01_*            #
#     c. CLICC_03 - Create factor                                               #
# 19. Full-join appointment/CCAPS + SDS + CLICC variables                       #
#                                                                               #
#################################################################################


####  0. Set-up ####
library(tidyverse)
load("Data/2017-2019_TI_2019-11-04.RDa")

#### 1. Select only the first course of treatments in 17-19 dataset ####
TI1719_courses <- CCMHr::create_courses(TI1719, firstOnly = T)

rm(TI1719)

#### 2. Pare down unnecessary variables ####
TI1719_select <- select(TI1719_courses, UniqueClientID,
                       AppointID:RecordType,
                       Has_SDS, Has_CLICC, Is_ValidCCAPS, Is_appointment:Attended,
                       ClientAge, CCAPS_51, CCAPS_68,
                       Depression34:DI,
                       contains("CLICC_"), contains("SDS_"))

rm(TI1719_courses)

#### 3. Separate first lines of CLICC and SDS for each client ####
## CLICC
CLICC_slice <- filter(TI1719_select, Has_CLICC == 1) %>%
  arrange(UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  slice(1) %>%
  ungroup() %>%
  select(UniqueClientID, contains("CLICC_"))

## SDS
SDS_slice <- filter(TI1719_select, Has_SDS == 1) %>%
  arrange(UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  slice(1) %>%
  ungroup() %>%
  select(UniqueClientID, contains("SDS_"))

# Ditch CLICC and SDS variables from working dataset -- we'll add them back in when needed
TI1719_apptCCAPS <- select(TI1719_select, -contains("CLICC_"), -contains("SDS_"))

#### 4. Remove duplicate appointment lines ####
## Only keep individual appointments or CCAPS lines
TI1719_apptCCAPS <- filter(TI1719_apptCCAPS, RecordType == "Individual" | Is_ValidCCAPS == 1)

## Separate appointment and CCAPS data for now
appt <- as.data.frame(filter(TI1719_apptCCAPS, Is_appointment == 1))
survey <- as.data.frame(filter(TI1719_apptCCAPS, Is_appointment == 0))

## For appointment data, count as duplicate if same client and appointment ID - filter
appt$duplicate <- duplicated(appt[c("UniqueClientID","AppointID")])

appt <- filter(appt, duplicate == FALSE)

appt <- select(appt, -duplicate)

#### 5. Average across lines of duplicate CCAPS ####
## Count as duplicate if CCAPS on same day
survey$duplicate <- duplicated(survey[c("UniqueClientID","Date")])

## Create a variable to assign the same number to each same-date admin of the CCAPS
survey <- group_by(survey, UniqueClientID) %>%
  mutate(seq_CCAPS = rank(Date, ties.method = "max")) %>%
  ungroup()

## Max for clinically significant CCAPS items and average for subscale scores for same-day observations
survey <- group_by(survey, UniqueClientID, seq_CCAPS) %>%
  mutate_at(vars(Depression34:DI), list(mean), na.rm = T) %>%
  mutate_at(vars(contains("CCAPS_")), list(max)) %>%
  ungroup()

## Delete duplicates
survey <- filter(survey, duplicate == FALSE)

## Delete variables not needed.
survey <- select(survey, -duplicate, -seq_CCAPS)

#### 6. Filter for clients with 2+ individual therapy sessions ####
appt <- arrange(appt, UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  mutate(num_appt_total = n(), num_appt_attended = sum(Attended == 1)) %>%
  ungroup()

appt <- filter(appt, num_appt_attended > 1)

#### 7. Filter for clients with 2+ CCAPS administrations ####
survey <- arrange(survey, UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  mutate(num_ccaps = n()) %>%
  ungroup()

survey <- filter(survey, num_ccaps > 1)

survey <- select(survey, -num_ccaps)

## Create placeholders for combining with appt data
survey$num_appt_total <- NA
survey$num_appt_attended <- NA

#### 8. Recombine appointment and CCAPS data ####
combined <- rbind(appt, survey)

rm(appt, survey)

## Sort data for easy viewing, only keep essential variables
combined <- arrange(combined, UniqueClientID, Date) %>%
  select(UniqueClientID, Date, Is_ValidCCAPS, Is_appointment,
         Attended, num_appt_total, num_appt_attended,
         ClientAge, CCAPS_51:DI)

#### 9. Pull dates of first and last appointments and CCAPS; only keep clients ####
#    with CCAPS administrations within 14 days of first and last appointments.
#    This helps ensure that CCAPS scores are representative of beginning and end of tx

## Create variables for the first and last CCAPS administration dates
dates_ccaps <- filter(combined, Is_ValidCCAPS == 1) %>%
  arrange(UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  mutate(first_ccaps = Date[1],
         last_ccaps = Date[n()]) %>%
  ungroup()

dates_ccaps$first_appt <- as.Date(NA)
dates_ccaps$last_appt <- as.Date(NA)

## Create variables for the first and last appointment dates
dates_appt <- filter(combined, Is_appointment == 1) %>%
  arrange(UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  mutate(first_appt = Date[1],
         last_appt = Date[n()]) %>%
  ungroup()

dates_appt$first_ccaps <- as.Date(NA)
dates_appt$last_ccaps <- as.Date(NA)

## Re-combine the two dates datasets
dates <- rbind(dates_ccaps, dates_appt)
dates <- arrange(dates, UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  mutate(num_ccaps = sum(Is_ValidCCAPS)) %>%
  ungroup()

## Conserve variables not present in all lines of data
dates <- arrange(dates, UniqueClientID, Date, Is_appointment) %>%
  group_by(UniqueClientID, Date) %>%
  fill(num_appt_total:num_appt_attended, .direction = "up") %>%
  ungroup()

## Make sure all clients have at least 2 appointments and CCAPS
dates <- filter(dates, (num_ccaps > 1 & num_appt_attended > 1))

## Fill in NA values in the date variables created
dates <- group_by(dates, UniqueClientID) %>%
  fill(first_ccaps:last_appt) %>%
  fill(first_ccaps:last_appt, .direction = "up") %>%
  ungroup()

## Calculate the difference in days between "firsts" and "lasts"
dates$firstDiff = difftime(dates$first_appt, dates$first_ccaps, units = c("days"))
dates$lastDiff = difftime(dates$last_appt, dates$last_ccaps, units = c("days"))

## Select those clients whose first and last CCAPS/appointment dates fall within 2 weeks of each other
match_dates <- arrange(dates, UniqueClientID, Date) %>%
  filter(abs(firstDiff) <= 14 & abs(lastDiff) <= 14)

    # Could be a decision point whether matching CCAPS/appt dates is necessary

rm(dates_appt, dates_ccaps)

#### 11. Create percentage attended variable: attended appointments/total scheduled ####
match_dates <- mutate(match_dates, attend_rate = (num_appt_attended/num_appt_total)*100)

#### 12. Operationalize CCAPS subscale scores: First and last ####
CCAPSappt <- filter(match_dates, Is_ValidCCAPS == 1) %>%
  arrange(UniqueClientID, Date) %>%
  group_by(UniqueClientID) %>%
  slice(1, n()) %>%
  mutate(CCAPS_admin = c("first","last"),
         pre_SI = CCAPS_51[1],
         pre_HI = CCAPS_68[1]) %>%
  ungroup() %>%
  select(-c(CCAPS_51, CCAPS_68))

#### 13. Operationalize CCAPS subscale scores: Change score (post-pre) ####
    # Will calculate later instead
#CCAPSappt <- group_by(CCAPSappt, UniqueClientID) %>%
#  mutate(d_Depression34 = Depression34[2] - Depression34[1],
#         d_Anxiety34 = Anxiety34[2] - Anxiety34[1],
#         d_Social_Anxiety34 = Social_Anxiety34[2] - Social_Anxiety34[1],
#         d_Academics34 = Academics34[2] - Academics34[1],
#         d_Eating34 = Eating34[2] - Eating34[1],
#         d_Hostility34 = Hostility34[2] - Hostility34[1],
#         d_Alcohol34 = Alcohol34[2] - Alcohol34[1],
#         d_DI = DI[2] - DI[1]) %>%
#  ungroup()

#### 14. Use CCMHr package to create high- and low-cut variables ####
# 15. Create CCAPS subscale score ranking variables based on high and low cuts

## Creates dichotomous variables for each subscale for whether the score is
## above the low cut and above the hi cut
CCAPSappt_cuts <- CCMHr::ccaps34_cuts(CCAPSappt)

## To combine the above cut information in a useful way, below creates a variable as follows:
    ## 0 - mild: below low cut (low = 0)
    ## 1 - moderate: above low cut, below high cut (low = 1, high = 0)
    ## 2 - severe: above high cut (high = 1)

CCAPSappt_cuts <- mutate(CCAPSappt_cuts,
                         Depression34.Severity = (depression_low_cut34 + depression_hi_cut34),
                         Anxiety34.Severity = (anxiety_low_cut34 + anxiety_hi_cut34),
                         SocialAnxiety34.Severity = (social_anxiety_low_cut34 + social_anxiety_hi_cut34),
                         Academics34.Severity = (academics_low_cut34 + academics_hi_cut34),
                         Eating34.Severity = (eating_low_cut34 + eating_hi_cut34),
                         Hostility34.Severity = (hostility_low_cut34 + hostility_hi_cut34),
                         Alcohol34.Severity = (alcohol_low_cut34 + alcohol_hi_cut34),
                         DI.Severity = (DI_low_cut + DI_hi_cut)) %>%
  select(UniqueClientID, ClientAge, num_appt_attended, attend_rate, CCAPS_admin, pre_SI:pre_HI,
         Depression34.Score = Depression34,
         Anxiety34.Score = Anxiety34,
         SocialAnxiety34.Score = Social_Anxiety34,
         Academics34.Score = Academics34,
         Eating34.Score = Eating34,
         Hostility34.Score = Hostility34,
         Alcohol34.Score = Alcohol34,
         DI.Score = DI,
         Depression34.Severity:DI.Severity)


## Elongate the data
CCAPSappt_long <- CCAPSappt_cuts %>%
  gather(key = "Subscale.Type", value = "Value", c(Depression34.Score:DI.Severity)) %>%
  separate(Subscale.Type, into = c("Subscale", "ScoreType")) %>%
  unite(temp, ScoreType, CCAPS_admin) %>%
  spread(key = "temp", value = "Value") %>%
  mutate(change_score = Score_last - Score_first) %>%
  arrange(UniqueClientID)

CCAPSappt_long$Severity_first <- factor(CCAPSappt_long$Severity_first, levels = 0:2,
                                  labels = c("Low", "Medium", "High"))
CCAPSappt_long$Severity_last <- factor(CCAPSappt_long$Severity_last, levels = 0:2,
                                        labels = c("Low", "Medium", "High"))

## Create static variables for the CCAPS subscale low/high cuts and the reliable change index
CCAPSappt_long <- mutate(CCAPSappt_long, 
                         lowCut = dplyr::recode(Subscale,
                                                Depression34 = "1.00",
                                                Anxiety34 = "1.33",
                                                SocialAnxiety34 = "1.40",
                                                Academics34 = "1.25",
                                                Eating34 = "0.96",
                                                Hostility34 = "0.84",
                                                Alcohol34 = "0.60",
                                                DI = "1.30"),
                         highCut = dplyr::recode(Subscale,
                                                Depression34 = "1.83",
                                                Anxiety34 = "2.10",
                                                SocialAnxiety34 = "2.50",
                                                Academics34 = "2.50",
                                                Eating34 = "1.50",
                                                Hostility34 = "1.17",
                                                Alcohol34 = "1.10",
                                                DI = "2.25"),
                         RCI = dplyr::recode(Subscale,
                                                Depression34 = "1.05",
                                                Anxiety34 = "1.07",
                                                SocialAnxiety34 = "1.09",
                                                Academics34 = "1.38",
                                                Eating34 = "1.34",
                                                Hostility34 = "0.98",
                                                Alcohol34 = "1.12",
                                                DI = "0.79"))

#### 16. Operationalize single CCAPS items: full-scale factor + ranking factor  ####
#     a. 51 - "I have thoughts of ending my life."
#     b. 68 - "I have thoughts of hurting others."

## Factor item 51
CCAPSappt_long$pre_SI <- as.factor(CCAPSappt_long$pre_SI)

## Factor for level of suicidal ideation, making either 3 or 4 "extreme"
CCAPSappt_long$SI_level <- factor(CCAPSappt_long$pre_SI,
                                  levels = 0:4,
                                  labels = c("none", "mild", "moderate", "extreme", "extreme"))

## Factor item 68
CCAPSappt_long$pre_HI <- as.factor(CCAPSappt_long$pre_HI)

## Factor for level of homicidal ideation, making either 3 or 4 "extreme"
CCAPSappt_long$HI_level <- factor(CCAPSappt_long$pre_HI,
                                  levels = 0:4,
                                  labels = c("none", "mild", "moderate", "extreme", "extreme"))


### Additionally, clean up ClientAge variable and make a factor version for initial in-app visualizations
CCAPSappt_long$ClientAge[CCAPSappt_long$ClientAge == 99] <- NA

CCAPSappt_long <- mutate(CCAPSappt_long, ClientAgeGroups = factor(ClientAge,
                                                                  levels = c(18:60),
                                                                  labels = c("18-22", "18-22", "18-22", "18-22", "18-22",
                                                                             "23-29", "23-29", "23-29", "23-29", "23-29", "23-29", "23-29", 
                                                                             "30-39", "30-39", "30-39", "30-39", "30-39",
                                                                             "30-39", "30-39", "30-39", "30-39", "30-39",
                                                                             "40-49", "40-49", "40-49", "40-49", "40-49",
                                                                             "40-49", "40-49", "40-49", "40-49", "40-49",
                                                                             "50+", "50+", "50+", "50+", "50+",
                                                                             "50+", "50+", "50+", "50+", "50+", "50+")))


#### 17. Keep/operationalize SDS items: ####
SDS <- select(SDS_slice, UniqueClientID,
              SDS_01, SDS_02, SDS_64, SDS_72, SDS_74, SDS_76, SDS_88, SDS_91, SDS_95)

#     b. 88 - Gender identity (factor)
SDS <- mutate(SDS, Gender = factor(SDS_88, levels = 1:4,
                                   labels = c("Woman", "Man", "Transgender", "Other")))

#     c. 91 - Sexuality (factor)
SDS <- mutate(SDS, Sexuality = factor(SDS_91, levels = c(1:6, 1001),
                                   labels = c("Heterosexual", "Homosexual", "Homosexual",
                                              "Bisexual", "Questioning", "Other", "Heterosexual")))

#     d. 95 - Race/Ethnicity (factor + minority factor)
SDS <- mutate(SDS, Race_Ethnicity = factor(SDS_95, levels = 1:8,
                                   labels = c("African American/Black", 
                                              "American Indian or Alaskan Native", 
                                              "Asian American/Asian", 
                                              "Hispanic/Latinx",
                                              "Native Hawaiian or Pacific Islander",
                                              "Multi-Racial",
                                              "White",
                                              "Other")),
              RacialMinority = factor(SDS_95, levels = 1:8,
                                    labels = c("Yes", 
                                               "Yes", 
                                               "Yes", 
                                               "Yes",
                                               "Yes",
                                               "Yes",
                                               "No",
                                               "Yes")))

#     e. 1 - Prior counseling (factor + dichotomous factor)
SDS <- mutate(SDS, PriorCounseling = factor(SDS_01, levels = 1:4,
                                      labels = c("Never", "Prior to college", 
                                                 "After starting college", "Both")),
              PriorCounseling_yn = factor(SDS_01, levels = 1:4,
                                          labels = c("No", "Yes", "Yes", "Yes")))

#     f. 2 - Prior medication use (factor + dichotomous factor)
SDS <- mutate(SDS, PriorMedication = factor(SDS_02, levels = 1:4,
                                            labels = c("Never", "Prior to college", 
                                                       "After starting college", "Both")),
              PriorMedication_yn = factor(SDS_02, levels = 1:4,
                                          labels = c("No", "Yes", "Yes", "Yes")))

#     g. 64 - Prior hospitalization (factor + dichotomous factor)
SDS <- mutate(SDS, PriorHospital = factor(SDS_64, levels = 1:5,
                                            labels = c("Never", "1 time", 
                                                       "2-3 times", "4-5 times",
                                                       "More than 5 times")),
              PriorHospital_yn = factor(SDS_64, levels = 1:5,
                                          labels = c("No", "Yes", "Yes", "Yes", "Yes")))

#     h. 72 - Prior self-injury (factor + dichotomous factor)
SDS <- mutate(SDS, PriorNSSI = factor(SDS_72, levels = 1:5,
                                          labels = c("Never", "1 time", 
                                                     "2-3 times", "4-5 times",
                                                     "More than 5 times")),
              PriorNSSI_yn = factor(SDS_72, levels = 1:5,
                                        labels = c("No", "Yes", "Yes", "Yes", "Yes")))

#     i. 74 - Prior suicidal ideation (factor + dichotomous factor)
SDS <- mutate(SDS, PriorSI = factor(SDS_64, levels = 1:5,
                                          labels = c("Never", "1 time", 
                                                     "2-3 times", "4-5 times",
                                                     "More than 5 times")),
              PriorSI_yn = factor(SDS_64, levels = 1:5,
                                        labels = c("No", "Yes", "Yes", "Yes", "Yes")))

#     j. 76 - Prior suicide attempt (factor + dichotomous factor)
SDS <- mutate(SDS, PriorAttempt = factor(SDS_76, levels = 1:5,
                                          labels = c("Never", "1 time", 
                                                     "2-3 times", "4-5 times",
                                                     "More than 5 times")),
              PriorAttempt_yn = factor(SDS_76, levels = 1:5,
                                        labels = c("No", "Yes", "Yes", "Yes", "Yes")))

## Remove original (unfactored) SDS variables
SDS <- select(SDS, -c(SDS_01:SDS_95))

#### 18. Keep/operationalize CLICC items: ####
#     a. CLICC_01_*
CLICC <- mutate(CLICC_slice,
                Anxiety = (ifelse(is.na(CLICC_01_01), 0, 1)),
                Generalized_anx = (ifelse(is.na(CLICC_01_1101), 0, 1)),
                Social_anx = (ifelse(is.na(CLICC_01_1102), 0, 1)),
                Panic = (ifelse(is.na(CLICC_01_1103), 0, 1)),
                Test_anx = (ifelse(is.na(CLICC_01_1104), 0, 1)),
                Phobia = (ifelse(is.na(CLICC_01_1105), 0, 1)),
                Other_anx = (ifelse(is.na(CLICC_01_1106), 0, 1)),
                OC = (ifelse(is.na(CLICC_01_02), 0, 1)),
                Perfectionism = (ifelse(is.na(CLICC_01_03), 0, 1)),
                Stress = (ifelse(is.na(CLICC_01_04), 0, 1)),
                Depression = (ifelse(is.na(CLICC_01_05), 0, 1)),
                Mood_instablity = (ifelse(is.na(CLICC_01_1006), 0, 1)),
                Emotion_dysregulation = (ifelse(is.na(CLICC_01_46), 0, 1)),
                Anger_management = (ifelse(is.na(CLICC_01_07), 0, 1)),
                Relationship = (ifelse(is.na(CLICC_01_08), 0, 1)),
                Interpersonal_fx = (ifelse(is.na(CLICC_01_09), 0, 1)),
                Social_isolation = (ifelse(is.na(CLICC_01_10), 0, 1)),
                Family = (ifelse(is.na(CLICC_01_11), 0, 1)),
                Grief = (ifelse(is.na(CLICC_01_12), 0, 1)),
                Medical = (ifelse(is.na(CLICC_01_13), 0, 1)),
                Eating = (ifelse(is.na(CLICC_01_14), 0, 1)),
                Sleep = (ifelse(is.na(CLICC_01_15), 0, 1)),
                Sexual = (ifelse(is.na(CLICC_01_16), 0, 1)),
                Pregnancy = (ifelse(is.na(CLICC_01_17), 0, 1)),
                Identity = (ifelse(is.na(CLICC_01_18), 0, 1)),
                Self_esteem = (ifelse(is.na(CLICC_01_19), 0, 1)),
                Adjustment = (ifelse(is.na(CLICC_01_20), 0, 1)),
                Cultural = (ifelse(is.na(CLICC_01_21), 0, 1)),
                Sexual_orientation = (ifelse(is.na(CLICC_01_22), 0, 1)),
                Gender_identity = (ifelse(is.na(CLICC_01_23), 0, 1)),
                Religion = (ifelse(is.na(CLICC_01_24), 0, 1)),
                Discrimination = (ifelse(is.na(CLICC_01_25), 0, 1)),
                Academic = (ifelse(is.na(CLICC_01_26), 0, 1)),
                Career = (ifelse(is.na(CLICC_01_27), 0, 1)),
                Attention = (ifelse(is.na(CLICC_01_1028), 0, 1)),
                Autism = (ifelse(is.na(CLICC_01_47), 0, 1)),
                Learning_disability = (ifelse(is.na(CLICC_01_48), 0, 1)),
                Alcohol = (ifelse(is.na(CLICC_01_29), 0, 1)),
                Drugs = (ifelse(is.na(CLICC_01_30), 0, 1)),
                Addiction = (ifelse(is.na(CLICC_01_31), 0, 1)),
                Self_injury = (ifelse(is.na(CLICC_01_32), 0, 1)),
                Suicidality = (ifelse(is.na(CLICC_01_33), 0, 1)),
                Violence = (ifelse(is.na(CLICC_01_34), 0, 1)),
                Psychoticism = (ifelse(is.na(CLICC_01_35), 0, 1)),
                Dissociation = (ifelse(is.na(CLICC_01_49), 0, 1)),
                Trauma = (ifelse(is.na(CLICC_01_36), 0, 1)),
                Physical_abuse = (ifelse(is.na(CLICC_01_37), 0, 1)),
                Sexual_abuse = (ifelse(is.na(CLICC_01_38), 0, 1)),
                Harassment = (ifelse(is.na(CLICC_01_39), 0, 1)),
                Stalking = (ifelse(is.na(CLICC_01_40), 0, 1)),
                Financial = (ifelse(is.na(CLICC_01_41), 0, 1)),
                Legal = (ifelse(is.na(CLICC_01_42), 0, 1)),
                None = (ifelse(is.na(CLICC_01_43), 0, 1)),
                Other = (ifelse(is.na(CLICC_01_44), 0, 1))) %>%
  select(UniqueClientID, Anxiety:Other, CLICC_03)

#     b. Number of concerns - Within a client, sum across CLICC_01_*
CLICC <- mutate(CLICC, NumConcerns = rowSums(CLICC[2:55]))

CLICC_long <- gather(CLICC, key = Concern, value = Checked,
                     Anxiety:Other) %>%
  filter(Checked == 1) %>%    # Removes unnecessary rows, all possible concerns that aren't checked for a given client
  select(-Checked)

CLICC_long$Concern <- as.factor(CLICC_long$Concern)

#     c. CLICC_03 - Create factor
CLICC_long <- mutate(CLICC_long, 
                     TopConcern = factor(CLICC_03,
                                         levels = c(1:44, 46:49, 1006, 1028, 1101:1106),
                                         labels = c("Anxiety", "O/C", "Perfectionism",
                                                    "Stress", "Depression",
                                                    "Mood_instability",
                                                    "Anger_management", "Relationship",
                                                    "Interpersonal_fx", "Social_isolation",
                                                    "Family", "Grief", "Medical",
                                                    "Eating", "Sleep", "Sexual", "Pregnancy",
                                                    "Identity", "Self_esteem", "Adjustment",
                                                    "Cultural", "Sexual_orientation",
                                                    "Gender_identity", "Religion",
                                                    "Discrimination", "Academic",
                                                    "Career", "Attention", "Alcohol", 
                                                    "Drugs", "Addiction", "Self_injury",
                                                    "Suicidality", "Violence", "Psychoticism",
                                                    "Trauma", "Physical_abuse", "Sexual_abuse",
                                                    "Harassment", "Stalking", "Financial",
                                                    "Legal", "None", "Other",
                                                    "Emotion_dysregulation", "Autism",
                                                    "Learning_disability", "Dissociation",
                                                    "Mood_instability", "Attention",
                                                    "Generalized_anx", "Social_anx",
                                                    "Panic", "Test_anx", "Phobia", "Other_anx"))) %>% 
  select(-CLICC_03) %>%
  arrange(UniqueClientID)

## Factor for number of concern groups - for initial app visualizations
CLICC_long <- mutate(CLICC_long, NumConcernsGroups = factor(NumConcerns,
                                                            levels = c(1:26, 28:30),
                                                            labels = c("1", "2-4", "2-4", "2-4",
                                                                       "5-9", "5-9", "5-9", "5-9", "5-9",
                                                                       "10-19", "10-19", "10-19", "10-19", "10-19",
                                                                       "10-19", "10-19", "10-19", "10-19", "10-19",
                                                                       "20+", "20+", "20+", "20+", "20+",
                                                                       "20+", "20+", "20+", "20+", "20+")))

#### 19. Inner-join appointment/CCAPS + SDS + CLICC variables ####

## All outcome variables are in the CCAPSappt_cuts dataset, so these must exist in all 
## combined datasets. Some predictors are also in the outcomes dataset, so it's included
## as a final dataset as well -- depending on the choices of the app user, it may
## be possible to choose datasets in order to maximize inclusion...

## The first is simply the finished CCAPS/appt dataset from above.
Outcomes_CCAPSpred <- CCAPSappt_long

n_distinct(Outcomes_CCAPSpred$UniqueClientID) # n = 69846

## The first includes SDS predictors:
Outcomes_SDSpred <- inner_join(CCAPSappt_long, SDS)

n_distinct(Outcomes_SDSpred$UniqueClientID) # n = 62069

## The second includes CLICC predictors (long form):
Outcomes_CLICCpred <- inner_join(CCAPSappt_long, CLICC_long)

n_distinct(Outcomes_CLICCpred$UniqueClientID) # n = 39903

## Finally, all combined variables of interest (long form):
OutPredApp <- inner_join(Outcomes_SDSpred, CLICC_long)

n_distinct(OutPredApp$UniqueClientID) # n = 38621

rm(CCAPSappt_cuts, CLICC, CLICC_long, SDS, Outcomes_SDSpred, CCAPSappt, CCAPSappt_long)


### Last but not least, save final dataset as an Rda
save(OutPredApp, file = "CCMH_Outcome_Prediction/prediction_app_data.rda")









