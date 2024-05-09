# Case ascertainment

library(dplyr)

audit_asthma1 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/Audit_England.csv",
                   stringsAsFactors = FALSE)

audit_asthma2 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/Audit_Wales.csv",
                          stringsAsFactors = FALSE)

audit_asthma <- bind_rows(audit_asthma1, audit_asthma2)

# trim white space

audit_asthma$Apr2021.to.Sep2021[1:8]
nchar(audit_asthma$Apr2021.to.Sep2021[1:8])

audit_asthma$Apr2021.to.Sep2021 <- trimws(audit_asthma$Apr2021.to.Sep2021)
audit_asthma$Apr2021.to.Sep2021[audit_asthma$Apr2021.to.Sep2021 == "-"] <- 0

audit_asthma$Oct2021.to.Mar2022 <- trimws(audit_asthma$Oct2021.to.Mar2022)
audit_asthma$Oct2021.to.Mar2022[audit_asthma$Oct2021.to.Mar2022 == "-"] <- 0

audit_asthma <- audit_asthma %>% filter(hosp_code != "")
audit_asthma <- audit_asthma %>% filter(Apr2021.to.Sep2021 != "" | Oct2021.to.Mar2022 != "")

audit_asthma$Apr2021.to.Sep2021 <- as.numeric(audit_asthma$Apr2021.to.Sep2021)
audit_asthma$Oct2021.to.Mar2022 <- as.numeric(audit_asthma$Oct2021.to.Mar2022)

colnames(audit_asthma)

# Use the copd case ascertainment one to add the other trust info

audit_copd <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/COPD/tidyData/COPD_case_ascertainment.csv")

audit_copd <- audit_copd %>% select(hosp_code, audit_trust_code, trust_name, Region, Country)

audit_asthma <- left_join(audit_asthma, audit_copd, by = "hosp_code")
audit_asthma %>% filter(is.na(trust_name))

# some are missing the info, but they didn't record any cases anyway.

#  drop all the ones that didn't link.

audit_asthma <- audit_asthma %>% filter(!is.na(trust_name))


audit_asthma$total_audit <- audit_asthma$Apr2021.to.Sep2021 + audit_asthma$Oct2021.to.Mar2022

head(audit_asthma)

audit_asthma$Total <- NULL

# Need to left_join it with the linking file.

# remove scotland

audit_asthma <- audit_asthma %>% filter(Country != "Scotland")


linkage_file <- read.csv("Z:/Group_work/PS_AA/General UK data/audit_names_codes_HES_linkage_from_Tim_Alex_format.csv",
                         stringsAsFactors = TRUE)



linkage_file <- linkage_file %>% rename(hosp_name = Crown.hosp_name, hosp_code = Crown.hosp_code, 
                                        NHS_hospital_code = ODS.Site.Code) %>% 
  select(hosp_code, HES.Hospital.Name, NHS_hospital_code, Trust, TrustMatch, Comments)


audit_asthma <- left_join(audit_asthma, linkage_file, by = "hosp_code")


audit_asthma_eng <- audit_asthma %>% filter(Country == "England")

# HES1 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/HES_England_April_Sep.csv")
# HES2 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/HES_England_Oct_March.csv")

HES1 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/HES_England_April_Sep_Alex_update_asthma.csv")
HES2 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/HES_England_Oct_March_Alex_update_asthma.csv")

colnames(audit_asthma)
colnames(HES1)
colnames(HES2)

colnames(HES1)
colnames(HES2)

HES1 <- HES1 %>% rename(NHS_hospital_code = Site.Code6)
HES2 <- HES2 %>% rename(NHS_hospital_code = Site.Code6)

audit_asthma_eng <- left_join(audit_asthma_eng, HES1, by = "NHS_hospital_code")


copd_audit





audit_asthma %>% filter(Comments != "")

# Seems good.


audit_asthma_eng <- left_join(audit_asthma_eng, HES2, by = "NHS_hospital_code")


audit_asthma_wales <- audit_asthma %>% filter(Country == "Wales")

HES3 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/PEDW_April_March.csv",
                 stringsAsFactors = FALSE)

# HES4 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/asthma/Analysis/PEDW_Wales_Oct_March_Alex_update.csv",
#                  stringsAsFactors = FALSE)


colnames(HES3)
head(HES3)
HES3 <- HES3 %>% rename(NHS_hospital_code = Hospital_code)
# HES4 <- HES4 %>% rename(NHS_hospital_code = Site.Code)

audit_asthma_wales <- left_join(audit_asthma_wales, HES3, by = "NHS_hospital_code")
# audit_asthma_wales <- left_join(audit_asthma_wales, HES4, by = "NHS_hospital_code")


colnames(audit_asthma_eng)
colnames(audit_asthma_wales)


audit_asthma_eng <- audit_asthma_eng %>% select(hosp_code, hosp_name, audit_trust_code, trust_name,
                                            Region, Country, total_audit, Apr.21:Sep.21, Oct.21:Mar.22)

audit_asthma_eng <- audit_asthma_eng %>% mutate(across(Apr.21:Mar.22, list(av = ~as.numeric(ifelse(. == "-", 0, 
                                                                                    ifelse(. == "*", 3.5, .))),
                                                                       min = ~as.numeric(ifelse(. == "-", 0, 
                                                                                    ifelse(. == "*", 1, .))),
                                                                       max = ~as.numeric(ifelse(. == "-", 0, 
                                                                                    ifelse(. == "*", 7, .))))))



audit_asthma_eng <- audit_asthma_eng %>% mutate(across(ends_with("min"), ~ifelse(. < 10, ., . - 2)))
audit_asthma_eng <- audit_asthma_eng %>% mutate(across(ends_with("max"), ~ifelse(. < 10, ., . + 2)))

audit_asthma_eng <- audit_asthma_eng %>% rowwise() %>% mutate(total_HES_PEDW_min = sum(c_across(ends_with("min"))))
audit_asthma_eng <- audit_asthma_eng %>% rowwise() %>% mutate(total_HES_PEDW_av = sum(c_across(ends_with("av"))))
audit_asthma_eng <- audit_asthma_eng %>% rowwise() %>% mutate(total_HES_PEDW_max = sum(c_across(ends_with("max"))))

audit_asthma_eng <- audit_asthma_eng %>% select(hosp_code:total_audit, starts_with("total"))

# wales doesn't need all this because they don't mess around and just provide proper numbers

# slightly this time around due to the data...

colnames(audit_asthma_wales)

audit_asthma_wales <- audit_asthma_wales %>% select(hosp_code, hosp_name, audit_trust_code, trust_name,
                                            Region, Country, total_audit, total_HES_PEDW_min = Total) %>%
  mutate(total_HES_PEDW_av = total_HES_PEDW_min, total_HES_PEDW_max = total_HES_PEDW_min)


# audit_asthma_wales <- audit_asthma_wales %>% rowwise() %>% mutate(total_HES_PEDW_min = sum(c_across(Apr.21:Mar.22)))
# audit_asthma_wales <- audit_asthma_wales %>% rowwise() %>% mutate(total_HES_PEDW_av = sum(c_across(Apr.21:Mar.22)))
# audit_asthma_wales <- audit_asthma_wales %>% rowwise() %>% mutate(total_HES_PEDW_max = sum(c_across(Apr.21:Mar.22)))

audit_asthma_wales <- audit_asthma_wales %>% select(hosp_code:total_audit, starts_with("total"))

audit_asthma <- bind_rows(audit_asthma_eng, audit_asthma_wales)


# UCL has a few little add-ons which have not been picked up by the code - 9 asterisks...

# therefore...

audit_asthma$total_HES_PEDW_av[audit_asthma$hosp_code == "UCL"] <- audit_asthma$total_HES_PEDW_av[audit_asthma$hosp_code == "UCL"] + 9*3.5
audit_asthma$total_HES_PEDW_min[audit_asthma$hosp_code == "UCL"] <- audit_asthma$total_HES_PEDW_min[audit_asthma$hosp_code == "UCL"] + 9*1
audit_asthma$total_HES_PEDW_max[audit_asthma$hosp_code == "UCL"] <- audit_asthma$total_HES_PEDW_max[audit_asthma$hosp_code == "UCL"] + 9*7




audit_asthma$asc_av <- round((audit_asthma$total_audit/audit_asthma$total_HES_PEDW_av)*100, 1)
audit_asthma$asc_min <- round((audit_asthma$total_audit/audit_asthma$total_HES_PEDW_max)*100, 1)
audit_asthma$asc_max <- round((audit_asthma$total_audit/audit_asthma$total_HES_PEDW_min)*100, 1)


audit_asthma <- audit_asthma %>% arrange(asc_av)

audit_asthma %>% filter(asc_av > 99) %>% as.data.frame() %>% print()


audit_asthma <- audit_asthma %>% arrange(audit_trust_code)

# calculate overall with mean

nume <- audit_asthma %>% filter(total_audit != 0) %>% select(total_audit) %>% sum()
denom1 <- audit_asthma %>% filter(total_audit != 0) %>% select(total_HES_PEDW_min) %>% sum()
denom2 <- audit_asthma %>% filter(total_audit != 0) %>% select(total_HES_PEDW_av) %>% sum()
denom3 <- audit_asthma %>% filter(total_audit != 0) %>% select(total_HES_PEDW_max) %>% sum()

audit_asthma$overall_asc_min <- nume/denom3
audit_asthma$overall_asc_av <- nume/denom2
audit_asthma$overall_asc_max <- nume/denom1

audit_asthma %>% select(starts_with("overall")) %>% head()


# write.csv(audit_asthma, "Z:/Group_work/PS_AA/Case ascertainment/asthma/tidyData/asthma_case_ascertainment.csv")



