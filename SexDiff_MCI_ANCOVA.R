library(tidyverse)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(car)

#Merge dataframes MCI-MCI and MCI-AD together in one single dataframe
MCI_MCI <- read_excel("MCI-MCI data.xlsx")
MCI_AD <- read_excel("MCI-AD data.xlsx")

merged_df <- rbind(MCI_MCI, MCI_AD)
merged_df_clean <- merged_df %>% subset(select = -c(SUBJID, PHC_MEM_PreciseFilter, PHC_EXF_PreciseFilter, PHC_LAN_PreciseFilter, PHC_VSP_PreciseFilter)) %>%
  rename(c("Date_Cog"="EXAMDATE"))

#Keep only baseline visit
merged_df_bl <- merged_df_clean %>%
  filter(VISCODE2 == "bl")

#Add ADAS scores to dataframe
#ADAS scores from ADNI1 and other ADNI phases are not in the same dataframe
ADAS_ADNI1 <- read.csv("ADAS_ADNI1_17Jul2024.csv")
ADAS_ADNI1_clean <- ADAS_ADNI1 %>% subset(select = c(RID, VISCODE, EXAMDATE, TOTAL11)) %>%
  rename(c("Date_ADAS"="EXAMDATE"))

df_ADAS_ADNI1 <- left_join(merged_df_bl, ADAS_ADNI1_clean, by= c("RID", "VISCODE") , multiple = "all")

ADAS_ADNIGO23 <- read.csv("ADAS_ADNIGO23_17Jul2024.csv")
ADAS_ADNIGO23_clean <- ADAS_ADNIGO23 %>% subset(select = c(RID, VISCODE2, VISDATE, TOTSCORE)) %>%
  rename(c("Date_ADAS"="VISDATE"))

df_ADAS_all <- left_join(df_ADAS_ADNI1, ADAS_ADNIGO23_clean, by= c("RID", "VISCODE2"), multiple = "all")

#Make a new column for ADAS Score by taking TOTAL11 for ADNI1 and TOTSCORE for other phases
#and a new column for ADAS Exam date

df_ADAS <- df_ADAS_all %>%
  mutate(
    Date_ADAS = coalesce(Date_ADAS.x, Date_ADAS.y),
    ADAS_Score = coalesce(TOTAL11, TOTSCORE)
  ) %>%
  subset(select = -c(Date_ADAS.x, Date_ADAS.y, TOTAL11, TOTSCORE))

df_ADAS = df_ADAS %>% mutate(Date_Cog = as.Date(Date_Cog)) %>%
  mutate(Date_ADAS = as.Date(Date_ADAS))

#Add CDR scores to dataframe
CDR <- read.csv("CDR_17Jul2024.csv")
CDR_clean <- CDR %>% subset(select = c(RID, VISDATE, CDGLOBAL)) %>%
  rename(c("Date_CDR"="VISDATE", "CDR_Score"="CDGLOBAL"))

df_CDR <- left_join(df_ADAS, CDR_clean, by= "RID", multiple = "all")
df_CDR = df_CDR %>% mutate(Date_CDR = as.Date(Date_CDR))

#Since CDR was not done at baseline, but at screening, calculate the delay between cognition exam date and CDR date and choose minimum delay so keep only one row per participant#
df_CDR_diff = df_CDR %>%
  mutate(abs_time_diff_cog_CDR = abs(as.numeric(difftime(Date_Cog, Date_CDR, units='days')))) %>% #Find time diff, transform to numeric and keep the absolute value (no signs)
  group_by(RID) %>% #Group the data by participant
  slice_min(abs_time_diff_cog_CDR, with_ties = FALSE) %>% #Filter (remove) rows that are not the minimum absolute time difference within each group.
  ungroup()

#Add Ecog scores to dataframe
Ecog <- read.csv("ECOGPT_17Jul2024.csv")
#Replace the 9 in dataframe as NA (treat them as missing values)#
Ecog_NA <- Ecog %>%
  mutate(across(12:51, ~ifelse(. == 9, NA, .)))
# Add total E-cog score in dataframe
Ecog_NA$ECOG_Score <- rowSums(Ecog[12:51])

Ecog_clean <- Ecog_NA %>% subset(select = c(RID, VISDATE, ECOG_Score)) %>%
  rename(c("Date_ECOG"="VISDATE"))

df_ECOG <- left_join(df_CDR_diff, Ecog_clean, by="RID", multiple = "all")
df_ECOG = df_ECOG %>% mutate(Date_ECOG = as.Date(Date_ECOG))

#Since we lose half the data if keep only baseline visit, calculate the delay between cognition exam date and Ecog date and choose minimum delay so keep only one row per participant#
df_ECOG_diff = df_ECOG %>%
  mutate(abs_time_diff_cog_ecog = abs(as.numeric(difftime(Date_Cog, Date_ECOG, units='days')))) %>% #Find time diff, transform to numeric and keep the absolute value (no signs)
  group_by(RID) %>% #Group the data by participant
  slice_min(abs_time_diff_cog_ecog, with_ties = FALSE) %>% #Filter (remove) rows that are not the minimum absolute time difference within each group.
  ungroup()


#Add Functional Activities Questionnaire
FAQ <- read.csv("FAQ_17Jul2024.csv")
FAQ_clean <- FAQ %>% subset(select = c(RID, VISCODE2, EXAMDATE, FAQTOTAL)) %>%
  rename(c("Date_FAQ"="EXAMDATE", "FAQ_Score"="FAQTOTAL"))

df_FAQ <- left_join(df_ECOG_diff, FAQ_clean, by= c("RID", "VISCODE2"), multiple = "all")
df_FAQ = df_FAQ %>% mutate(Date_FAQ = as.Date(Date_FAQ))

#Add Depression Score
GDS <- read.csv("GDSCALE_17Jul2024.csv")
GDS_clean <- GDS %>% subset(select = c(RID, VISDATE, GDTOTAL)) %>%
  rename(c("Date_GDS"="VISDATE", "GDS_Score"="GDTOTAL"))

df_GDS <- left_join(df_FAQ, GDS_clean, by="RID", multiple = "all")
df_GDS = df_GDS %>% mutate(Date_GDS = as.Date(Date_GDS))

#Since GDS was not done at baseline, but at screening, calculate the delay between cognition exam date and GDS date and choose minimum delay so keep only one row per participant#
df_GDS_diff = df_GDS %>%
  mutate(abs_time_diff_cog_GDS = abs(as.numeric(difftime(Date_Cog, Date_GDS, units='days')))) %>% #Find time diff, transform to numeric and keep the absolute value (no signs)
  group_by(RID) %>% #Group the data by participant
  slice_min(abs_time_diff_cog_GDS, with_ties = FALSE) %>% #Filter (remove) rows that are not the minimum absolute time difference within each group.
  ungroup()

#Add MMSE
MMSE_df <- read.csv("MMSE_17Jul2024.csv")
MMSE_df_clean <- MMSE_df %>% subset(select = c(RID, VISDATE, MMSCORE)) %>%
  rename(c("Date_MMSE"="VISDATE", "MMSE_Score"="MMSCORE"))

df_MMSE <- left_join(df_GDS_diff, MMSE_df_clean, by="RID", multiple = "all")
df_MMSE = df_MMSE %>% mutate(Date_MMSE = as.Date(Date_MMSE))

#Since MMSE was not done at baseline, but at screening, calculate the delay between cognition exam date and MMSE date and choose minimum delay so keep only one row per participant#
df_MMSE_diff = df_MMSE %>%
  mutate(abs_time_diff_cog_MMSE = abs(as.numeric(difftime(Date_Cog, Date_MMSE, units='days')))) %>% #Find time diff, transform to numeric and keep the absolute value (no signs)
  group_by(RID) %>% #Group the data by participant
  slice_min(abs_time_diff_cog_MMSE, with_ties = FALSE) %>% #Filter (remove) rows that are not the minimum absolute time difference within each group.
  ungroup()

#Add MOCA
MoCA <- read.csv("MOCA_17Jul2024.csv")
#Make a new column to have subscores for the digits series, verbal fluency and word recall

MoCA_subscores <- MoCA %>%
  mutate(serial_score = case_when(
    SERIAL1 + SERIAL2 + SERIAL3 + SERIAL4 + SERIAL5 == 0 ~ 0,
    SERIAL1 + SERIAL2 + SERIAL3 + SERIAL4 + SERIAL5 == 1 ~ 1,
    SERIAL1 + SERIAL2 + SERIAL3 + SERIAL4 + SERIAL5 == 2 ~ 2,
    SERIAL1 + SERIAL2 + SERIAL3 + SERIAL4 + SERIAL5 == 3 ~ 2,
    SERIAL1 + SERIAL2 + SERIAL3 + SERIAL4 + SERIAL5 == 4 ~ 3,
    SERIAL1 + SERIAL2 + SERIAL3 + SERIAL4 + SERIAL5 == 5 ~ 3,)) %>%
  mutate(fluency_score = ifelse(FFLUENCY >= 11, 1, 0)) %>%
  mutate(letters_score = ifelse(LETTERS <2,1,0))

MoCA_subscores$Words_Score <- rowSums(MoCA_subscores[, c("DELW1", "DELW2", "DELW3", "DELW4", "DELW5")] == 1)

#calculate total MoCA score
MoCA_subscores$MOCA_Total <- rowSums(MoCA_subscores[, c(11, 12, 13, 14, 15, 16, 17, 18, 29, 30, 37, 38, 40, 41, 47, 48, 49, 50, 51, 52, 57, 58,59,60)])


MoCA_clean <- MoCA_subscores %>% subset(select = c(RID, VISDATE, MOCA_Total)) %>%
  rename(c("Date_MoCA"="VISDATE", "MoCA_Score"="MOCA_Total"))

df_MoCA <- left_join(df_MMSE_diff, MoCA_clean, by= "RID", multiple = "all")
df_MoCA = df_MoCA %>% mutate(Date_MoCA = as.Date(Date_MoCA))

#Since we lose half the data if keep only baseline visit, calculate the delay between cognition exam date and MoCA date and choose minimum delay so keep only one row per participant#
df_MoCA_diff = df_MoCA %>%
  mutate(abs_time_diff_cog_MoCA = abs(as.numeric(difftime(Date_Cog, Date_MoCA, units='days')))) %>% #Find time diff, transform to numeric and keep the absolute value (no signs)
  group_by(RID) %>% #Group the data by participant
  slice_min(abs_time_diff_cog_MoCA, with_ties = FALSE) %>% #Filter (remove) rows that are not the minimum absolute time difference within each group.
  ungroup()

#Add anxiety score
anxiety_df <- read.csv("NPI_17Jul2024.csv")
anxiety_df_clean <- anxiety_df %>% subset(select = c(RID, EXAMDATE, NPIETOT)) %>%
  rename(c("Date_Anxiety"="EXAMDATE", "Anxiety_Score"="NPIETOT"))

df_anx <- left_join(df_MoCA_diff, anxiety_df_clean, by= "RID", multiple = "all")
df_anx = df_anx %>% mutate(Date_Anxiety = as.Date(Date_Anxiety))

#Since we lose a lot of data if keep only baseline visit, calculate the delay between cognition exam date and NPI date and choose minimum delay so keep only one row per participant#
df_anx_diff = df_anx %>%
  mutate(abs_time_diff_cog_anx = abs(as.numeric(difftime(Date_Cog, Date_Anxiety, units='days')))) %>% #Find time diff, transform to numeric and keep the absolute value (no signs)
  group_by(RID) %>% #Group the data by participant
  slice_min(abs_time_diff_cog_anx, with_ties = FALSE) %>% #Filter (remove) rows that are not the minimum absolute time difference within each group.
  ungroup()

#Add APOE to dataframe
APOE = read.csv("APOERES_02Aug2024.csv")
APOE_clean = subset(APOE, select = c(RID, GENOTYPE, APTESTDT))
data_APOE = left_join(df_anx_diff, APOE_clean,by=c("RID"="RID"), multiple="all")
names(data_APOE)[names(data_APOE) == "APTESTDT"] <- "Date_APOE"

#Add neuropsyc battery
neuro_batt <- read.csv("NEUROBAT_17Jul2024.csv")

##Put logical memory scores in a separate dataframe to merge them with minimal delay instead of merging with visit (bl)
logical_mem <- neuro_batt[, c(3, 6,21, 56)] %>%
  rename(c("Logical_MEM_imm"="LIMMTOTAL","Logical_MEM_delay"="LDELTOTAL", "Date_logical_mem"="VISDATE")) %>%
  drop_na()

df_logical_mem <- left_join(data_APOE, logical_mem, by= "RID", multiple = "all")
df_logical_mem = df_logical_mem %>% mutate(Date_logical_mem = as.Date(Date_logical_mem))

df_logical_mem_diff = df_logical_mem %>%
  mutate(abs_time_diff_cog_log_mem = abs(as.numeric(difftime(Date_Cog, Date_logical_mem, units='days')))) %>% #Find time diff, transform to numeric and keep the absolute value (no signs)
  group_by(RID) %>% #Group the data by participant
  slice_min(abs_time_diff_cog_log_mem, with_ties = FALSE) %>% #Filter (remove) rows that are not the minimum absolute time difference within each group.
  ungroup()

neuro_batt$AVLT_Total <- rowSums(neuro_batt[, c(23, 25, 27, 29, 31)])

neuro_batt_clean <- neuro_batt %>% subset(select = c(RID, VISCODE2, VISDATE, CLOCKSCOR, COPYSCOR, DSPANFOR, DSPANFLTH, DSPANBAC, DSPANBLTH, AVLT_Total, AVDEL30MIN, AVDELTOT, CATANIMSC, CATANPERS, CATANINTR, CATVEGESC, CATVGPERS, CATVGINTR, TRAASCOR, TRAAERRCOM, TRAAERROM, TRABSCOR, TRABERRCOM, TRABERROM, BNTTOTAL)) %>%
  rename(c("Date_neuro_batt"="VISDATE","AVRECOGTOT"="AVDELTOT", "Animals_Total"="CATANIMSC", "Animals_Pers"="CATANPERS", "Animals_Intru"="CATANINTR", "Veg_Total"="CATVEGESC", "Veg_Pers"="CATVGPERS", "Veg_Intru"="CATVGINTR", "Trail_A_Time"="TRAASCOR", "Trail_A_Comm"="TRAAERRCOM", "Trail_A_Omm"="TRAAERROM", "Trail_B_Time"="TRABSCOR", "Trail_B_Comm"="TRABERRCOM", "Trail_B_Omm"="TRABERROM"))

df_neuro_batt <- left_join(df_logical_mem_diff, neuro_batt_clean, by= c("RID", "VISCODE2"), multiple = "all")
df_neuro_batt = df_neuro_batt %>% mutate(Date_neuro_batt = as.Date(Date_neuro_batt))

#Change variables class
df_neuro_batt = df_neuro_batt %>% mutate(Sex = as.factor(Sex)) %>%
  mutate(Ethnicity = as.factor(Ethnicity)) %>%
  mutate(Race = as.factor(Race)) %>%
  mutate(Education = as.numeric(Education)) %>%
  mutate (Age_Cognition = as.numeric(Age_Cognition)) %>%
  mutate(Diagnosis = as.factor(Diagnosis)) %>%
  mutate(Groupe = as.factor(Groupe))

### Transform race in white vs non-white ###
library(dplyr)

df_neuro_batt <- df_neuro_batt %>%
  mutate(Race_binary = if_else(Race == 5, 1, 2))

# Remove participants who have missing data on any of the variables of interest#
library(dplyr)
df_neuro_batt_clean <- df_neuro_batt %>%
  drop_na(ADAS_Score, CDR_Score, FAQ_Score, GDS_Score, MMSE_Score, CLOCKSCOR, 
          COPYSCOR, Logical_MEM_imm, Logical_MEM_delay, AVLT_Total, AVDEL30MIN, 
          AVRECOGTOT, Animals_Total, Animals_Pers, Animals_Intru, Trail_A_Time, 
          Trail_A_Comm, Trail_A_Omm, Trail_B_Time, Trail_B_Comm, Trail_B_Omm, 
          BNTTOTAL, PHC_MEM, PHC_EXF, PHC_LAN, PHC_VSP, Age_Cognition, 
          Education, Race_binary, Sex)

table(df_neuro_batt_clean$Groupe)

df_conversion_participants <- merged_df_clean %>%
  dplyr::semi_join(df_neuro_batt_clean, by = "RID")
length(unique(df_conversion_participants$RID)) == nrow(df_neuro_batt_clean)

write.xlsx(df_conversion_participants,"/Users/sophieboutin/Library/CloudStorage/OneDrive-UniversitedeMontreal/PhD/ADNI_XL_SB/Dataframes/df_conversion_participants.xlsx")

### Calculate mean conversion time ###
library(dplyr)
library(lubridate)

conversion_times <- df_conversion_participants %>%
  # Keep only participants who convert
  filter(Groupe == 2) %>%
  # Sort visits chronologically for each participant
  arrange(RID, Date_Cog) %>%
  group_by(RID) %>%
  summarise(
    first_visit = first(Date_Cog),
    conversion_visit = Date_Cog[which(Diagnosis == 3)[1]],  # first time Diagnosis = 3 (AD)
    delay_years = as.numeric(difftime(conversion_visit, first_visit, units = "days")) / 365.25
  ) %>%
  ungroup()

# Compute mean conversion delay (in years)
mean(conversion_times$delay_years, na.rm = TRUE)
sd(conversion_times$delay_years, na.rm = TRUE)
range(conversion_times$delay_years, na.rm = TRUE)

conversion_times <- conversion_times %>%
  left_join(
    df_conversion_participants %>% select(RID, Sex) %>% distinct(),
    by = "RID"
  )
t.test(delay_years ~ Sex, data = conversion_times)

### Calculate the mean follow-up duration ###
library(dplyr)
library(lubridate)

followup_times <- df_conversion_participants %>%
  arrange(RID, Date_Cog) %>%
  group_by(RID) %>%
  summarise(
    first_visit = first(Date_Cog),
    last_visit = last(Date_Cog),
    followup_years = as.numeric(difftime(last_visit, first_visit, units = "days")) / 365.25
  ) %>%
  ungroup()

mean(followup_times$followup_years, na.rm = TRUE)
sd(followup_times$followup_years, na.rm = TRUE)
range(followup_times$followup_years, na.rm = TRUE)

followup_times <- followup_times %>%
  left_join(
    df_conversion_participants %>% select(RID, Sex) %>% distinct(),
    by = "RID"
  )
t.test(followup_years ~ Sex, data = followup_times)

###DEMOGRAPHICS MCI converters vs non-converters###

#Number of participants#
nrow(df_neuro_batt_clean[df_neuro_batt_clean$Groupe == 1,])
nrow(df_neuro_batt_clean[df_neuro_batt_clean$Groupe == 2,])

#Sex
table(df_neuro_batt_clean$Groupe, df_neuro_batt_clean$Sex)

#Age
df_neuro_batt_clean %>%
  group_by(Groupe) %>%
  summarize(
    mean_age = format(round(mean(Age_Cognition, na.rm = TRUE), 2), nsmall = 2),
    sd_age = round(sd(Age_Cognition, na.rm = TRUE), 2)
  )

t.test(Age_Cognition ~ Groupe, data = df_neuro_batt_clean)

#education
df_neuro_batt_clean %>%
  group_by(Groupe) %>%
  summarize(
    mean_age = format(round(mean(Education, na.rm = TRUE), 2), nsmall = 2),
    sd_age = round(sd(Education, na.rm = TRUE), 2)
  )

t.test(Education ~ Groupe, data = df_neuro_batt_clean)

#White race#
table(df_neuro_batt_clean$Groupe, df_neuro_batt_clean$Race_binary)

### Test (chi-square) if ratio white/non-white is different between groups
table_race_group <- table(df_neuro_batt_clean$Groupe, df_neuro_batt_clean$Race_binary)
chisq.test(table_race_group)

##APOE
print(sum(df_neuro_batt$GENOTYPE %in% c("2/4", "3/4", "4/4")))
df_neuro_batt %>%
  filter(Groupe == 1, GENOTYPE %in% c("2/4", "3/4", "4/4")) %>%
  nrow()
df_neuro_batt %>%
  filter(Groupe == 2, GENOTYPE %in% c("2/4", "3/4", "4/4")) %>%
  nrow()

### Test (chi-square) if ratio APOE e4/vs no APOE e4 is different between groups
#Create matrix
# Recode APOE genotype
df_neuro_batt$APOE_matrix <- ifelse(df_neuro_batt$GENOTYPE %in% c("2/4", "3/4", "4/4"), "APOE e4", "No APOE e4")

table_apoe_group <- table(df_neuro_batt$Groupe, df_neuro_batt$APOE_matrix)
print(table_apoe_group)

chisq.test(table_apoe_group)

###DEMOGRAPHICS men and women between MCI converters and non-converters###

#Number of participants#
nrow(df_neuro_batt_clean[df_neuro_batt_clean$Sex == 1,])
nrow(df_neuro_batt_clean[df_neuro_batt_clean$Sex == 2,])

#Age
df_neuro_batt_clean %>%
  group_by(Sex, Groupe) %>%
  summarize(
    mean_age = format(round(mean(Age_Cognition, na.rm = TRUE), 2), nsmall = 2),
    sd_age = round(sd(Age_Cognition, na.rm = TRUE), 2)
  )

df_neuro_batt_clean %>%
  filter(Sex == 1) %>%
  {t.test(Age_Cognition ~ Groupe, data = .)}

df_neuro_batt_clean %>%
  filter(Sex == 2) %>%
  {t.test(Age_Cognition ~ Groupe, data = .)}

#education
df_neuro_batt_clean %>%
  group_by(Sex, Groupe) %>%
  summarize(
    mean_age = format(round(mean(Education, na.rm = TRUE), 2), nsmall = 2),
    sd_age = round(sd(Education, na.rm = TRUE), 2)
  )

df_neuro_batt_clean %>%
  filter(Sex == 1) %>%
  {t.test(Education ~ Groupe, data = .)}

df_neuro_batt_clean %>%
  filter(Sex == 2) %>%
  {t.test(Education ~ Groupe, data = .)}

#White race#
table(df_neuro_batt_clean$Sex, df_neuro_batt_clean$Groupe, df_neuro_batt_clean$Race_binary)

### Test (chi-square) if ratio white/non-white is different between groups within each sex
#Create matrix
df_neuro_batt_clean$Race_matrix <- ifelse(df_neuro_batt_clean$Race == 5, "White", "NonWhite")

by(df_neuro_batt_clean, df_neuro_batt_clean$Sex, function(sub) {
  tab <- table(sub$Groupe, sub$Race_matrix)
  print(tab)
  chisq.test(tab)
})

##APOE
print(sum(df_neuro_batt$GENOTYPE %in% c("2/4", "3/4", "4/4")))
df_neuro_batt %>%
  filter(Sex == 1, GENOTYPE %in% c("2/4", "3/4", "4/4")) %>%
  nrow()
df_neuro_batt %>%
  filter(Sex == 2, GENOTYPE %in% c("2/4", "3/4", "4/4")) %>%
  nrow()

### Test (chi-square) if ratio APOE e4/vs no APOE e4 is different between groups
#Create matrix
table_apoe_sex <- table(df_neuro_batt$Sex, df_neuro_batt$APOE_matrix)
print(table_apoe_sex)

chisq.test(table_apoe_sex)

### Mean delays between baseline and assessments that were not done at baseline ###
mean(df_neuro_batt_clean$abs_time_diff_cog_CDR)
mean(df_neuro_batt_clean$abs_time_diff_cog_GDS)
mean(df_neuro_batt_clean$abs_time_diff_cog_MMSE)
mean(df_neuro_batt_clean$abs_time_diff_cog_log_mem)

### Cognitive profiles MCI converters vs non-converters - Linear regressions ###

#Models separately for each individual test#

model_ADAS <- lm(ADAS_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_ADAS)
library(emmeans)
PosthocGroupeADAS<-emmeans(model_ADAS, ~Groupe)
PosthocSexADAS<-emmeans(model_ADAS, ~Sex)
PosthocGroupeADAS
PosthocSexADAS
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("ADAS_Score"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("ADAS_Score"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_ADAS)

model_CDR <- lm(CDR_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_CDR)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("CDR_Score"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

model_Ecog <- lm(ECOG_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Ecog)

model_FAQ <- lm(FAQ_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_FAQ)
library(emmeans)
PosthocGroupeFAQ<-emmeans(model_FAQ, ~Groupe)
PosthocGroupeFAQ
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("FAQ_Score"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("FAQ_Score"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

effectsize::eta_squared(model_FAQ)

model_GDS <- lm(GDS_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_GDS)
library(emmeans)
PosthocSexGDS<-emmeans(model_GDS, ~Sex)
PosthocSexGDS
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("GDS_Score"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_GDS)

model_MMSE <- lm(MMSE_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_MMSE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("MMSE_Score"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_MMSE)

model_MoCA <- lm(MoCA_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_MoCA)

model_anxiety <- lm(Anxiety_Score ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_anxiety)

model_CLOCKSCOR <- lm(CLOCKSCOR ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_CLOCKSCOR)
library(emmeans)
PosthocGroupeCLOCK<-emmeans(model_CLOCKSCOR, ~Groupe)
PosthocGroupeCLOCK
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("CLOCKSCOR"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("CLOCKSCOR"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_CLOCKSCOR)

model_COPYSCOR <- lm(COPYSCOR ~ Groupe*Sex + Age_Cognition + Education+ Race_binary, data = df_neuro_batt_clean)
summary(model_COPYSCOR)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("COPYSCOR"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

model_Logical_MEM_imm <- lm(Logical_MEM_imm ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Logical_MEM_imm)
library(emmeans)
PosthocGroupeLOGMEMIMM<-emmeans(model_Logical_MEM_imm, ~Groupe)
PosthocGroupeLOGMEMIMM
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Logical_MEM_imm"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Logical_MEM_imm"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Logical_MEM_imm)

model_Logical_MEM_delay <- lm(Logical_MEM_delay ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Logical_MEM_delay)
library(emmeans)
PosthocGroupeLOGMEMDEL<-emmeans(model_Logical_MEM_delay, ~Groupe)
PosthocGroupeLOGMEMDEL
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Logical_MEM_delay"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Logical_MEM_delay"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Logical_MEM_delay)

model_AVLT_Total <- lm(AVLT_Total ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_AVLT_Total)
effectsize::eta_squared(model_AVLT_Total)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("AVLT_Total"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

model_AVDEL30MIN <- lm(AVDEL30MIN ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_AVDEL30MIN)
effectsize::eta_squared(model_AVDEL30MIN)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("AVDEL30MIN"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

model_AVRECOGTOT <- lm(AVRECOGTOT ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_AVRECOGTOT)
effectsize::eta_squared(model_AVRECOGTOT)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("AVRECOGTOT"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

model_DSPANFOR <- lm(DSPANFOR ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_DSPANFOR)

model_DSPANFLTH <- lm(DSPANFLTH ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_DSPANFLTH)

model_DSPANBAC <- lm(DSPANBAC ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_DSPANBAC)

model_DSPANBLTH <- lm(DSPANBLTH ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_DSPANBLTH)

model_Animals_Total <- lm(Animals_Total ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Animals_Total)
library(emmeans)
PosthocGroupeAnimals<-emmeans(model_Animals_Total, ~Groupe)
PosthocGroupeAnimals
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Animals_Total"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Animals_Total"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Animals_Total)

model_Animals_Pers <- lm(Animals_Pers ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Animals_Pers)
library(emmeans)
PosthocGroupeAnimalsPers<-emmeans(model_Animals_Pers, ~Groupe)
PosthocGroupeAnimalsPers
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Animals_Pers"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Animals_Pers"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Animals_Pers)

model_Animals_Intru <- lm(Animals_Intru ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Animals_Intru)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Animals_Intru"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

model_Veg_Total <- lm(Veg_Total ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Veg_Total)

model_Veg_Pers <- lm(Veg_Pers ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Veg_Pers)

model_Veg_Intru <- lm(Veg_Intru ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Veg_Intru)

model_Trail_A_Time <- lm(Trail_A_Time ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Trail_A_Time)
library(emmeans)
PosthocGroupeTrailA<-emmeans(model_Trail_A_Time, ~Groupe)
PosthocGroupeTrailA
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_A_Time"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_A_Time"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Trail_A_Time)

model_Trail_A_Comm <- lm(Trail_A_Comm ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Trail_A_Comm)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_A_Comm"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Trail_A_Comm)

model_Trail_A_Omm <- lm(Trail_A_Omm ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Trail_A_Omm)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_A_Omm"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

model_Trail_B_Time <- lm(Trail_B_Time ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Trail_B_Time)
library(emmeans)
PosthocGroupeTrailB<-emmeans(model_Trail_B_Time, ~Groupe)
PosthocGroupeTrailB
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_B_Time"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_B_Time"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Trail_B_Time)

model_Trail_B_Comm <- lm(Trail_B_Comm ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Trail_B_Comm)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_B_Comm"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Trail_B_Comm)

model_Trail_B_Omm <- lm(Trail_B_Omm ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_Trail_B_Omm)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("Trail_B_Omm"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_Trail_B_Omm)


model_BNTTOTAL <- lm(BNTTOTAL ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_BNTTOTAL)
library(emmeans)
PosthocGroupeBNT<-emmeans(model_BNTTOTAL, ~Groupe)
PosthocGroupeBNT
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("BNTTOTAL"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("BNTTOTAL"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_BNTTOTAL)

#Models with composite cognitive scores
model_MEM <- lm(PHC_MEM ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_MEM)
effectsize::eta_squared(model_MEM)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("PHC_MEM"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

df_neuro_batt_clean$PHC_EXF<-as.numeric(unlist(df_neuro_batt_clean$PHC_EXF))
model_EXF <- lm(PHC_EXF ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_EXF)
library(emmeans)
PosthocGroupeEXF<-emmeans(model_EXF, ~Groupe)
PosthocGroupeEXF
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("PHC_EXF"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("PHC_EXF"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_EXF)

df_neuro_batt_clean$PHC_LAN<-as.numeric(unlist(df_neuro_batt_clean$PHC_LAN))
model_LANG <- lm(PHC_LAN~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_LANG)
library(emmeans)
PosthocGroupeLANG<-emmeans(model_LANG, ~Groupe)
PosthocGroupeLANG
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("PHC_LAN"),
                  splitBy = Groupe,
                  mean=TRUE, sd=TRUE)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("PHC_LAN"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)
effectsize::eta_squared(model_LANG)

model_VSP <- lm(PHC_VSP ~ Groupe*Sex + Age_Cognition + Education + Race_binary, data = df_neuro_batt_clean)
summary(model_VSP)
library(emmeans)
PosthocSexVSP<-emmeans(model_VSP, ~Sex)
PosthocSexVSP
effectsize::eta_squared(model_VSP)
jmv::descriptives(df_neuro_batt_clean,
                  vars = vars("PHC_VSP"),
                  splitBy = Sex,
                  mean=TRUE, sd=TRUE)

##### Ratio M/W for each cognitive test and chi-square test to see if ratio is different between MCI converters and MCI non-converters #####
test_sex_ratio <- function(df, test_var) {
  sub_df <- df[!is.na(df[[test_var]]), ]
  tab <- table(sub_df$Groupe, sub_df$Sex)
  print(paste("Test:", test_var))
  print(tab)
  print(chisq.test(tab))
  cat("\n-----------------------------------\n")
}

# List of test variables you analyzed
cognitive_tests <- c("ADAS_Score", "CDR_Score", "FAQ_Score", "GDS_Score", "MMSE_Score", "CLOCKSCOR", "COPYSCOR", "Logical_MEM_imm", "Logical_MEM_delay", "AVLT_Total", "AVDEL30MIN", "AVRECOGTOT", "Animals_Total", "Animals_Pers", "Animals_Intru", "Trail_A_Time", "Trail_A_Comm", "Trail_A_Omm", "Trail_B_Time", "Trail_B_Comm", "Trail_B_Omm", "BNTTOTAL", 
                      "PHC_MEM", "PHC_EXF", "PHC_LAN", "PHC_VSP")

# Apply to each
lapply(cognitive_tests, function(test) test_sex_ratio(df_neuro_batt_clean, test))


#Extract the p-values from the models to apply benjamini correction (multiple corrections)

# list of models 
model_list <- list(model_ADAS, model_CDR, model_FAQ, model_GDS, model_MMSE, 
                   model_CLOCKSCOR, model_COPYSCOR, model_AVLT_Total, model_AVDEL30MIN, model_AVRECOGTOT, model_Animals_Total, model_Animals_Pers, model_Animals_Intru, model_Trail_A_Time, model_Trail_A_Comm, model_Trail_A_Omm, model_Trail_B_Time, model_Trail_B_Comm, model_Trail_B_Omm, model_BNTTOTAL, 
                   model_MEM, model_EXF, model_LANG, model_VSP, model_Logical_MEM_imm, model_Logical_MEM_delay)

# Function to adjust p-values using BH method and format them
adjust_and_format_p_values <- function(model) {
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
  adjusted_p_values <- p.adjust(p_values, method = "BH")
  formatted_p_values <- format(adjusted_p_values, scientific = FALSE, digits = 3)
  return(formatted_p_values)
}

# Apply the function to each model in the list
adjusted_p_values_list <- lapply(model_list, adjust_and_format_p_values)

print(adjusted_p_values_list)


#LOOP for multiple regressions
# cognitive_scores is a vector of column names for the cognitive scores
cognitive_scores <- c("ADAS_Score", "CDR_Score", "ECOG_Score", "FAQ_Score", "GDS_Score", "MMSE_Score", "MoCA_Score", "Anxiety_Score", 
                      "CLOCKSCOR", "COPYSCOR", "AVLT_Total", "AVDEL30MIN", "AVRECOGTOT", "DSPANFOR", "DSPANFLTH", "DSPANBAC", "DSPANBLTH", "Animals_Total", "Animals_Pers", "Animals_Intru", "Veg_Total", "Veg_Pers", "Veg_Intru", "Trail_A_Time", "Trail_A_Comm", "Trail_A_Omm", "Trail_B_Time", "Trail_B_Comm", "Trail_B_Omm", "BNTTOTAL", 
                      "PHC_MEM", "PHC_EXF", "PHC_LAN", "PHC_VSP", "Logical_MEM_imm", "Logical_MEM_delay")

##### ANOVA with race #####

# Initialize lists to store results
regression_results_race <- list()


for (score in cognitive_scores) {
  formula <- as.formula(paste(score, "~ Groupe*Sex + Age_Cognition + Education + Race_binary"))
  model <- lm(formula, data = df_neuro_batt_clean)
  
  # Multiple Regression
  regression_results_race[[score]] <- summary(model)
}

# Print Multiple Regression results for each cognitive score
for (score in names(regression_results_race)) {
  cat("\nMultiple Regression results for", score, ":\n")
  print(regression_results_race[[score]])
}
t.test(GDS_Score ~ Race_binary, data = df_neuro_batt_clean)
t.test(MMSE_Score ~ Race_binary, data = df_neuro_batt_clean)
t.test(Animals_Total ~ Race_binary, data = df_neuro_batt_clean)
t.test(Trail_A_Time ~ Race_binary, data = df_neuro_batt_clean)
t.test(BNTTOTAL ~ Race_binary, data = df_neuro_batt_clean)
t.test(PHC_EXF ~ Race_binary, data = df_neuro_batt_clean)
t.test(PHC_LAN ~ Race_binary, data = df_neuro_batt_clean)


# Function to adjust p-values using BH method
adjust_p_values <- function(model) {
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
  adjusted_p_values <- p.adjust(p_values, method = "BH")
  return(adjusted_p_values)
}

# Initialize lists to store models
model_list_race <- list()

# Create models for each cognitive score
for (score in cognitive_scores) {
  formula <- as.formula(paste(score, "~ Groupe*Sex + Age_Cognition + Education + Race_binary"))
  model <- lm(formula, data = df_neuro_batt_clean)
  model_list_race[[score]] <- model
}

# Apply the function to each model in the list
adjusted_p_values_list_race <- lapply(model_list_race, adjust_p_values)

# Print the adjusted p-values for each model with 8 decimal places
for (score in names(adjusted_p_values_list_race)) {
  cat("\nAdjusted p-values for", score, ":\n")
  print(format(adjusted_p_values_list_race[[score]], digits = 8, scientific = FALSE))
}


#Final data MCI-MCI and MCI-AD
write.xlsx(df_neuro_batt_clean,"/Users/sophieboutin/Library/CloudStorage/OneDrive-UniversitedeMontreal/PhD/ADNI_XL_SB/Dataframes/df_cogn_MCI_AD.xlsx")

##### Sex interactions #####
###MMSE###
### Create objects that contain simple effects ###
library(emmeans)
effetSimpleMMSE <-emmeans(model_MMSE, ~Groupe|Sex)
effetSimpleMMSE

### Simple effects using mean square of error of ANOVA ###
test(pairs(effetSimpleMMSE), joint=T)

# Compute standardized effect sizes (Cohen's d)
eff_size(effetSimpleMMSE, sigma = sigma(model_MMSE), edf = df.residual(model_MMSE))

# Modify labels of 'Sex' and 'Groupe'
df_neuro_battCopy <- df_neuro_batt_clean
df_neuro_battCopy$Sex <- factor(df_neuro_battCopy$Sex, levels = c(1, 2), labels = c("Men", "Women"))

df_neuro_battCopy$Groupe <- factor(df_neuro_battCopy$Groupe, levels = c(1, 2), labels = c("MCI non-converters", "MCI converters"))

# Model with new labels
model_MMSE_2 <- lm(MMSE_Score ~ Groupe * Sex + Age_Cognition + Education + Race_binary, data = df_neuro_battCopy)

library(ggplot2)
library(afex)

plot_mmse <- afex::afex_plot(model_MMSE_2, 
                             x = "Sex",            
                             trace = "Groupe",     
                             mapping = c("shape", "fill"),
                             data_geom = ggplot2::geom_boxplot, 
                             data_arg = list(width = 0.3))

# Remove connecting lines
plot_mmse$layers <- Filter(function(l) !inherits(l$geom, "GeomLine"), plot_mmse$layers)

# Manually change the legend title (overrides afex defaults)
plot_mmse <- plot_mmse +
  guides(fill = guide_legend(title = "Group"),
         shape = guide_legend(title = "Group")) +
  scale_fill_manual(
    values = c("MCI non-converters" = "steelblue3", "MCI converters" = "tomato3")
  ) +
  labs(x = "Sex", y = "MMSE score") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.position = "top"
  )

# Add asterisks for significance
plot_mmse + 
  annotate("text", x = 1, y = max(model_MMSE_2$model$MMSE_Score, na.rm = TRUE) + 0.5, 
           label = "**", size = 12, color = "black") +  # asterisk for men
  annotate("text", x = 2, y = max(model_MMSE_2$model$MMSE, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black")    # asterisk for women

###RAVLT TOTAL 1-5###
### Create objects that contain simple effects ###
library(emmeans)
effetSimpleRAVLTTotal <-emmeans(model_AVLT_Total, ~Groupe|Sex)
effetSimpleRAVLTTotal

### Simple effects using mean square of error of ANOVA ###
test(pairs(effetSimpleRAVLTTotal), joint=T)

# Compute standardized effect sizes (Cohen's d)
eff_size(effetSimpleRAVLTTotal, sigma = sigma(model_AVLT_Total), edf = df.residual(model_AVLT_Total))


# Model with new labels
model_AVLT_Total_2 <- lm(AVLT_Total ~ Groupe * Sex + Age_Cognition + Education + Race_binary, data = df_neuro_battCopy)

library(ggplot2)
library(afex)

# Base plot
plot_ravlt <- afex::afex_plot(model_AVLT_Total_2, 
                             x = "Sex",            
                             trace = "Groupe",     
                             mapping = c("shape", "fill"),
                             data_geom = ggplot2::geom_boxplot, 
                             data_arg = list(width = 0.3))

# Remove connecting lines
plot_ravlt$layers <- Filter(function(l) !inherits(l$geom, "GeomLine"), plot_ravlt$layers)

# Manually change the legend title (overrides afex defaults)
plot_ravlt <- plot_ravlt +
  guides(fill = guide_legend(title = "Group"),
         shape = guide_legend(title = "Group")) +
  scale_fill_manual(
    values = c("MCI non-converters" = "steelblue3", "MCI converters" = "tomato3")
  ) +
  labs(x = "Sex", y = "RAVLT total trials 1-5") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.position = "top"
  )

# Add asterisks for significance
plot_ravlt + 
  annotate("text", x = 1, y = max(model_AVLT_Total_2$model$AVLT_Total, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black") +  # asterisk for men
  annotate("text", x = 2, y = max(model_AVLT_Total_2$model$AVLT_Total, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black")    # asterisk for women

###RAVLT 30-min delay###
### Create objects that contain simple effects ###
library(emmeans)
effetSimpleRAVLTdelay <-emmeans(model_AVDEL30MIN, ~Groupe|Sex)
effetSimpleRAVLTdelay

### Simple effects using mean square of error of ANOVA ###
test(pairs(effetSimpleRAVLTdelay), joint=T)

# Compute standardized effect sizes (Cohen's d)
eff_size(effetSimpleRAVLTdelay, sigma = sigma(model_AVDEL30MIN), edf = df.residual(model_AVDEL30MIN))

# Model with new labels
model_AVDEL30MIN_2 <- lm(AVDEL30MIN ~ Groupe * Sex + Age_Cognition + Education + Race_binary, data = df_neuro_battCopy)

library(ggplot2)
library(afex)

# Base plot
plot_delay <- afex::afex_plot(model_AVDEL30MIN_2, 
                              x = "Sex",            
                              trace = "Groupe",     
                              mapping = c("shape", "fill"),
                              data_geom = ggplot2::geom_boxplot, 
                              data_arg = list(width = 0.3))

# Remove connecting lines
plot_delay$layers <- Filter(function(l) !inherits(l$geom, "GeomLine"), plot_delay$layers)

# Manually change the legend title (overrides afex defaults)
plot_delay <- plot_delay +
  guides(fill = guide_legend(title = "Group"),
         shape = guide_legend(title = "Group")) +
  scale_fill_manual(
    values = c("MCI non-converters" = "steelblue3", "MCI converters" = "tomato3")
  ) +
  labs(x = "Sex", y = "RAVLT 30-min delay") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.position = "top"
  )

# Add asterisks for significance
plot_delay + 
  annotate("text", x = 1, y = max(model_AVDEL30MIN_2$model$AVDEL30MIN, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black") +  # asterisk for men
  annotate("text", x = 2, y = max(model_AVDEL30MIN_2$model$AVDEL30MIN, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black")    # asterisk for women

###RAVLT Recognition###
### Create objects that contain simple effects ###
library(emmeans)
effetSimpleRAVLTrecog <-emmeans(model_AVRECOGTOT, ~Groupe|Sex)
effetSimpleRAVLTrecog

### Simple effects using mean square of error of ANOVA ###
test(pairs(effetSimpleRAVLTrecog), joint=T)

# Compute standardized effect sizes (Cohen's d)
eff_size(effetSimpleRAVLTrecog, sigma = sigma(model_AVRECOGTOT), edf = df.residual(model_AVRECOGTOT))

# Model with new labels
model_AVRECOGTOT_2 <- lm(AVRECOGTOT ~ Groupe * Sex + Age_Cognition + Education + Race_binary, data = df_neuro_battCopy)

library(ggplot2)
library(afex)

# Base plot
plot_RECOG <- afex::afex_plot(model_AVRECOGTOT_2, 
                              x = "Sex",            
                              trace = "Groupe",     
                              mapping = c("shape", "fill"),
                              data_geom = ggplot2::geom_boxplot, 
                              data_arg = list(width = 0.3))

# Remove connecting lines
plot_RECOG$layers <- Filter(function(l) !inherits(l$geom, "GeomLine"), plot_RECOG$layers)

# Manually change the legend title (overrides afex defaults)
plot_RECOG <- plot_RECOG +
  guides(fill = guide_legend(title = "Group"),
         shape = guide_legend(title = "Group")) +
  scale_fill_manual(
    values = c("MCI non-converters" = "steelblue3", "MCI converters" = "tomato3")
  ) +
  labs(x = "Sex", y = "RAVLT Recognition") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.position = "top"
  )

# Add asterisks for significance
plot_RECOG + 
  annotate("text", x = 1, y = max(model_AVRECOGTOT_2$model$AVRECOGTOT, na.rm = TRUE) + 0.5, 
           label = "**", size = 12, color = "black") +  # asterisk for men
  annotate("text", x = 2, y = max(model_AVRECOGTOT_2$model$AVRECOGTOT, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black")    # asterisk for women

###Memory Composite###
### Create objects that contain simple effects ###
library(emmeans)
effetSimpleMEM <-emmeans(model_MEM, ~Groupe|Sex)
effetSimpleMEM

### Simple effects using mean square of error of ANOVA ###
test(pairs(effetSimpleMEM), joint=T)

# Compute standardized effect sizes (Cohen's d)
eff_size(effetSimpleMEM, sigma = sigma(model_MEM), edf = df.residual(model_MEM))

# Model with new labels
model_MEM_2 <- lm(PHC_MEM ~ Groupe * Sex + Age_Cognition + Education + Race_binary, data = df_neuro_battCopy)

# Plot
plot_MEM <- afex::afex_plot(model_MEM_2, 
                              x = "Sex",            
                              trace = "Groupe",     
                              mapping = c("shape", "fill"),
                              data_geom = ggplot2::geom_boxplot, 
                              data_arg = list(width = 0.3))

# Remove connecting lines
plot_MEM$layers <- Filter(function(l) !inherits(l$geom, "GeomLine"), plot_MEM$layers)

# Manually change the legend title (overrides afex defaults)
plot_MEM <- plot_MEM +
  guides(fill = guide_legend(title = "Group"),
         shape = guide_legend(title = "Group")) +
  scale_fill_manual(
    values = c("MCI non-converters" = "steelblue3", "MCI converters" = "tomato3")
  ) +
  labs(x = "Sex", y = "Memory composite score") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.position = "top"
  )

# Add asterisks for significance
plot_MEM + 
  annotate("text", x = 1, y = max(model_MEM_2$model$PHC_MEM, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black") +  # asterisk for men
  annotate("text", x = 2, y = max(model_MEM_2$model$PHC_MEM, na.rm = TRUE) + 0.5, 
           label = "***", size = 12, color = "black")    # asterisk for women

##### Violin plots #####
library(ggdist)
library(ggplot2)
library(dplyr)
library(tidyr)

CogScores <- c(
  "MMSE_Score",
  "AVLT_Total",
  "AVDEL30MIN",
  "AVRECOGTOT",
  "PHC_MEM"
)

df_long <- df_neuro_battCopy %>%
  pivot_longer(
    cols = all_of(CogScores),
    names_to = "Test",
    values_to = "Score"
  )

df_long <- df_long %>%
  mutate(
    Test = dplyr::recode(Test,
                         MMSE_Score = "MMSE",
                         AVLT_Total = "RAVLT trials 1–5 total",
                         AVDEL30MIN = "RAVLT 30-minute delay",
                         AVRECOGTOT = "RAVLT recognition",
                         PHC_MEM = "Memory composite score"
    )
  )

df_long$Test <- factor(
  df_long$Test,
  levels = c(
    "MMSE",
    "RAVLT trials 1–5 total",
    "RAVLT 30-minute delay",
    "RAVLT recognition",
    "Memory composite score"
  )
)

ggplot(df_long, aes(x = Sex, y = Score, fill = Groupe)) +
  
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8), alpha = 0.7) +
  
  geom_boxplot(
    width = 0.15,
    position = position_dodge(width = 0.8),
    outlier.shape = NA,
    alpha = 0.5,
    color = "black",          # box border color
    linetype = "solid",       # normal lines
    size = 0.5
  ) +
  
  facet_wrap(~Test, scales = "free_y") +
  
  scale_fill_manual(
    values = c("MCI non-converters" = "green3", "MCI converters" = "mediumorchid3")
  ) +
  
  theme_classic() +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    strip.text = element_text(face = "bold", size = 15)
  ) +
  
  labs(
    x = "",
    y = "Neuropsychological score",
    fill = "Group"
  )