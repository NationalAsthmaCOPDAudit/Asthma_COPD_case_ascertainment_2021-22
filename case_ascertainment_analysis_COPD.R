# Case ascertainment

library(dplyr)

audit_copd <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/COPD/Analysis/Audit_England_Wales_Scotland.csv",
                   stringsAsFactors = FALSE)


audit_copd <- audit_copd %>% rename(hosp_name = Orgname, hosp_code = Name, trust_name = Trust, audit_trust_code = TCode)

# drop the total column as it isn't fit for purpose as it is


audit_copd$total_audit <- audit_copd$Apr2021.to.Sep2021 + audit_copd$Oct2021.to.Mar2022

head(audit_copd)

audit_copd$Total <- NULL

# Need to left_join it with the linking file.

# remove scotland

audit_copd <- audit_copd %>% filter(Country != "Scotland")


linkage_file <- read.csv("Z:/Group_work/PS_AA/General UK data/audit_names_codes_HES_linkage_from_Tim_Alex_format.csv",
                         stringsAsFactors = TRUE)



linkage_file <- linkage_file %>% rename(hosp_name = Crown.hosp_name, hosp_code = Crown.hosp_code, 
                                        NHS_hospital_code = ODS.Site.Code) %>% 
  select(hosp_code, HES.Hospital.Name, NHS_hospital_code, Trust, TrustMatch, Comments)


audit_copd <- left_join(audit_copd, linkage_file, by = "hosp_code")

audit_copd_eng <- audit_copd %>% filter(Country == "England")

HES1 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/COPD/Analysis/HES_England_April_Sep_Alex_update.csv")
HES2 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/COPD/Analysis/HES_England_Oct_March_Alex_update.csv")

colnames(audit_copd)
colnames(HES1)
colnames(HES2)


HES1 <- HES1 %>% rename(NHS_hospital_code = Site.code)
HES2 <- HES2 %>% rename(NHS_hospital_code = Site.code)

look <- HES1 %>% filter(!(NHS_hospital_code %in% audit_copd_eng$NHS_hospital_code))


# keep those that have proper numbers and compare to what we have...

look <- look %>% filter(if_any(Apr.21:Sep.21, ~!(. %in% c("-", "*"))))
View(look)

# write.csv(look, "Z:/Group_work/PS_AA/Case ascertainment/COPD/tidyData/manually_check_if_odd.csv")

audit_copd_eng <- left_join(audit_copd_eng, HES1, by = "NHS_hospital_code")



audit_copd %>% filter(Comments != "")

# Seems good.


audit_copd_eng <- left_join(audit_copd_eng, HES2, by = "NHS_hospital_code")


audit_copd_wales <- audit_copd %>% filter(Country == "Wales")

HES3 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/COPD/Analysis/PEDW_Wales_April_Sep_Alex_update.csv",
                 stringsAsFactors = FALSE)

HES4 <- read.csv("Z:/Group_work/PS_AA/Case ascertainment/COPD/Analysis/PEDW_Wales_Oct_March_Alex_update.csv",
                 stringsAsFactors = FALSE)


colnames(HES3)
HES3 <- HES3 %>% rename(NHS_hospital_code = Site.Code)
HES4 <- HES4 %>% rename(NHS_hospital_code = Site.Code)

audit_copd_wales <- left_join(audit_copd_wales, HES3, by = "NHS_hospital_code")
audit_copd_wales <- left_join(audit_copd_wales, HES4, by = "NHS_hospital_code")


colnames(audit_copd_eng)
colnames(audit_copd_wales)


audit_copd_eng <- audit_copd_eng %>% select(hosp_code, hosp_name, audit_trust_code, trust_name,
                                            Region, Country, total_audit, Apr.21:Sep.21, Oct.21:Mar.22)

audit_copd_eng <- audit_copd_eng %>% mutate(across(Apr.21:Mar.22, list(av = ~as.numeric(ifelse(. == "-", 0, 
                                                                                    ifelse(. == "*", 3.5, .))),
                                                                       min = ~as.numeric(ifelse(. == "-", 0, 
                                                                                    ifelse(. == "*", 1, .))),
                                                                       max = ~as.numeric(ifelse(. == "-", 0, 
                                                                                    ifelse(. == "*", 7, .))))))



audit_copd_eng <- audit_copd_eng %>% mutate(across(ends_with("min"), ~ifelse(. < 10, ., . - 2)))
audit_copd_eng <- audit_copd_eng %>% mutate(across(ends_with("max"), ~ifelse(. < 10, ., . + 2)))

audit_copd_eng <- audit_copd_eng %>% rowwise() %>% mutate(total_HES_PEDW_min = sum(c_across(ends_with("min"))))
audit_copd_eng <- audit_copd_eng %>% rowwise() %>% mutate(total_HES_PEDW_av = sum(c_across(ends_with("av"))))
audit_copd_eng <- audit_copd_eng %>% rowwise() %>% mutate(total_HES_PEDW_max = sum(c_across(ends_with("max"))))

audit_copd_eng <- audit_copd_eng %>% select(hosp_code:total_audit, starts_with("total"))

# wales doesn't need all this because they don't mess around and just provide proper numbers

audit_copd_wales <- audit_copd_wales %>% select(hosp_code, hosp_name, audit_trust_code, trust_name,
                                            Region, Country, total_audit, Apr.21:Sep.21, Oct.21:Mar.22)


audit_copd_wales <- audit_copd_wales %>% rowwise() %>% mutate(total_HES_PEDW_min = sum(c_across(Apr.21:Mar.22)))
audit_copd_wales <- audit_copd_wales %>% rowwise() %>% mutate(total_HES_PEDW_av = sum(c_across(Apr.21:Mar.22)))
audit_copd_wales <- audit_copd_wales %>% rowwise() %>% mutate(total_HES_PEDW_max = sum(c_across(Apr.21:Mar.22)))

audit_copd_wales <- audit_copd_wales %>% select(hosp_code:total_audit, starts_with("total"))

audit_copd <- bind_rows(audit_copd_eng, audit_copd_wales)

audit_copd$asc_av <- round((audit_copd$total_audit/audit_copd$total_HES_PEDW_av)*100, 1)
audit_copd$asc_min <- round((audit_copd$total_audit/audit_copd$total_HES_PEDW_max)*100, 1)
audit_copd$asc_max <- round((audit_copd$total_audit/audit_copd$total_HES_PEDW_min)*100, 1)

View(audit_copd)

audit_copd <- audit_copd %>% arrange(asc_av)

audit_copd %>% filter(asc_av > 99) %>% as.data.frame() %>% print()



# calculate overall with mean

nume <- audit_copd %>% filter(total_audit != 0) %>% select(total_audit) %>% sum()
denom1 <- audit_copd %>% filter(total_audit != 0) %>% select(total_HES_PEDW_min) %>% sum()
denom2 <- audit_copd %>% filter(total_audit != 0) %>% select(total_HES_PEDW_av) %>% sum()
denom3 <- audit_copd %>% filter(total_audit != 0) %>% select(total_HES_PEDW_max) %>% sum()

audit_copd$overall_asc_min <- nume/denom3
audit_copd$overall_asc_av <- nume/denom2
audit_copd$overall_asc_max <- nume/denom1

audit_copd %>% select(starts_with("overall")) %>% head()

audit_copd <- audit_copd %>% arrange(audit_trust_code)


# write.csv(audit_copd, "Z:/Group_work/PS_AA/Case ascertainment/COPD/tidyData/COPD_case_ascertainment.csv")


