#### Recreating figures and tables for this paper:
# Water, Sanitation, and Hygiene (WASH) Infrastructure and Capacity, and Environmental Contamination in the neonatal units of two Healthcare Facilities in Ethiopia 

#### This version was edited on 2/17/2025 to try a different dataset and see if it is completed.

#### Libraries ####
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(patchwork)

#### File paths ####
hcf_data_path <- "Copy of 3.Spreadsheet-Scan-CrossCheck (UPDATED DATA).xlsx"  # # Putting the updated database
hcf_data_old_path <- "1.WASH-Env-Graphs-2023.07.xlsx" # Looking at the old version to see how much differs, to make sure that any cleaning I do is still valid.
data_dictionary_path <- "SynergyProjectSamplesResult_DataDictionary_2018-12-05.csv" # Has codes for many variables
# amr_sheet_path <- "H:\\Emory\\WASH in HCF\\Ethiopia\\Synergy\\Copy of AST results for each envirom sample.xlsx" # I don't think this is still needed, but is a sheet that had some AMR data. I believe that all needed data are in hcf_data_path

#### Loading data and initial data processing ####
hcf_data <- read_excel(hcf_data_path) # Loading the data

data_dictionary <- read.csv(data_dictionary_path) # Loading hte codes for the data

data_dictionary_clean <- data_dictionary %>% #cleaning some of the codes for the data
  select(Variable...Field.Name, Choices..Calculations..OR.Slider.Labels) %>% # Only keeping some columns, renaming them for ease
  rename(variable_name = Variable...Field.Name) %>%
  rename(choices = Choices..Calculations..OR.Slider.Labels) %>%
  filter(variable_name %in% c("name_of_hospitals", "site_of_sample_collected", "unit_where_sample_is_colle", "type_of_sample_collected", "sex_of_hand_rinsed_partici", # Only using some of the variables
                              "what_is_the_sample_result", "result_v2", "amr_test_result_v2")) %>%
  separate_rows(choices, sep = " \\| ") %>% # It is currently set as one row having all variable options. So one row may have 20+ answers. This separates each of those into their own rows, which will allow joining.
  separate(choices, into = c("choice_code", "choice_ans"), sep = ", ", convert = TRUE) # Separating the code from what the code indicates


#### Before updating data cleaning, checking what actually differs from the original version ####
hcf_data_old <- read_excel(hcf_data_old_path) # Loading the original data

## Checking to see how much "type_of_sample_collected" differs from before

sample_type_check_data <- left_join( # Joining the new and original data
  hcf_data %>% select(sample_id...2, type_of_sample_collected) %>% rename(type_of_sample_collected_new = type_of_sample_collected),
  hcf_data_old %>% select(sample_id, type_of_sample_collected),
  by = c("sample_id...2" = "sample_id")
) %>%
  mutate(aligned = type_of_sample_collected_new == type_of_sample_collected)

sample_type_check_data_both_reported <- sample_type_check_data %>% # The new version has more samples. This is ONLY looking at samples that both databases have.
  drop_na(type_of_sample_collected)

sample_type_check_data_both_reported_notaligned <- filter(sample_type_check_data_both_reported, aligned == FALSE) # How many did not align

nrow(sample_type_check_data_both_reported_notaligned)/nrow(sample_type_check_data_both_reported) # It's essentially 99% alignment. The ones not looking aligned are really just the ones that are duplicated, which may actually align (i.e. if I grouped by another variable, maybe it would work)

## Checking to see how much "site_of_sample_collected" differs from before

sample_site_check_data <- left_join(
  hcf_data %>% select(sample_id...2, site_of_sample_collected) %>% rename(site_of_sample_collected_new = site_of_sample_collected),
  hcf_data_old %>% select(sample_id, site_of_sample_collected),
  by = c("sample_id...2" = "sample_id")
) %>%
  mutate(aligned = site_of_sample_collected_new == site_of_sample_collected)

sample_site_check_data_both_reported <- sample_site_check_data %>%
  drop_na(site_of_sample_collected)

sample_site_check_data_both_reported_notaligned <- filter(sample_site_check_data_both_reported, aligned == FALSE)

nrow(sample_site_check_data_both_reported_notaligned)/nrow(sample_site_check_data_both_reported) # There were like 10% of cases where it did not align

sample_site_check_data_both_reported_notaligned %>%
  group_by(site_of_sample_collected_new, site_of_sample_collected) %>%
  summarise(count = n()) %>%
  left_join(data_dictionary_clean %>% filter(variable_name == "site_of_sample_collected") %>% mutate(choice_code = as.character(choice_code)) %>% select(-variable_name) %>% rename(choice_ans_new = choice_ans),
            by = c("site_of_sample_collected_new" = "choice_code")) %>%
  left_join(data_dictionary_clean %>% filter(variable_name == "site_of_sample_collected") %>% select(-variable_name) %>% rename(choice_ans_old = choice_ans),
            by = c("site_of_sample_collected" = "choice_code")) %>%
  arrange(desc(count), site_of_sample_collected)
  

# So in general, it seems like in many cases (11) that were originally marked as bed sheets, they are now blankes.
# Some were originally blankets, but were moved to other.
# There are a decent number of cases, so I should double check a few of these.
# Some of this may require manual checking, especially cases that are marked as "Other"

# data_dictionary_clean %>%
#   filter(variable_name == "site_of_sample_collected") %>%
#   arrange(choice_code) %>%
#   print(n = 21)

## Checking to see how much "sex_of_hand_rinsed_partici" differs from before

sample_sex_check_data <- left_join(
  hcf_data %>% select(sample_id...2, Sex_of_HR) %>% rename(Sex_of_HR_new = Sex_of_HR),
  hcf_data_old %>% select(sample_id, Sex_of_HR),
  by = c("sample_id...2" = "sample_id")
) %>%
  mutate(aligned = Sex_of_HR_new == Sex_of_HR)

sample_sex_check_data_both_reported <- sample_sex_check_data %>%
  drop_na(Sex_of_HR)

sample_sex_check_data_both_reported_notaligned <- filter(sample_sex_check_data_both_reported, aligned == FALSE)

nrow(sample_sex_check_data_both_reported_notaligned)/nrow(sample_sex_check_data_both_reported) # All of the cases where both specified sex were in alignment


#### Data cleaning ####
hcf_data_cleaned <- hcf_data %>%
  # left_join( # Needed for the old databse
  #   data_dictionary_clean %>% filter(variable_name == "name_of_hospitals")
  # )
  mutate(facility_clean = case_when( # Putting in a more easily usable version of the facility names
    facility == 1 ~ "Felegehiwot", # Felegehiwot Referral Hospital # These were numbered differently than the original form
    facility == 2 ~ "Deberetabor" #  Deberetabor General Hospital
  ), .after = facility) %>%
  # left_join( # This was already there in the new spreadsheet # Needed for the old database
  #   data_dictionary_clean %>% 
  #     filter(variable_name == "unit_where_sample_is_colle") %>% 
  #     select(-variable_name) %>%
  #     dplyr::rename(unit_clean = choice_ans),
  #   by = c("unit" = "choice_code")
  # ) %>%
  left_join( # Information on what type of sample was collected
    data_dictionary_clean %>%  # This still looks the same, so no need to change
      filter(variable_name == "type_of_sample_collected") %>% 
      select(-variable_name) %>%
      dplyr::rename(sample_type_clean = choice_ans),
    by = c("type_of_sample_collected" = "choice_code")
  ) %>%
  left_join( # Information on what thing was samples
    data_dictionary_clean %>% 
      filter(variable_name == "site_of_sample_collected") %>% 
      select(-variable_name) %>%
      mutate(choice_code = as.character(choice_code)) %>%
      dplyr::rename(site_type_clean = choice_ans),
    by = c("site_of_sample_collected" = "choice_code")
  ) %>% # Use site of sample collected
  left_join( # Info on the sex of participants, This aligned well
    data_dictionary_clean %>% 
      filter(variable_name == "sex_of_hand_rinsed_partici") %>% 
      select(-variable_name) %>%
      dplyr::rename(sex_clean = choice_ans),
    by = c("Sex_of_HR" = "choice_code")
  ) # sex of hand rinse 


# hcf_data_cleaned %>%
#   group_by(facility_clean, unit) %>%
#   dplyr::summarise(
#     samples = n()
#   )
# 
# hcf_data_cleaned %>%
#   group_by(facility_clean, sample_type_clean) %>%
#   dplyr::summarise(
#     samples = n()
#   ) %>%
#   arrange(sample_type_clean)
# 
# 
# hcf_data_cleaned %>%
#   group_by(facility_clean, unit, sample_type_clean) %>%
#   dplyr::summarise(
#     samples = n()
#   ) %>%
#   arrange(sample_type_clean)


# saur_amr <- read_excel(amr_sheet_path, sheet = "s.aureus") %>% # This is the old AMR data. Later need to replace this.
#   left_join(hcf_data_cleaned %>% select(sample_id, facility_clean),
#             by = c("sample_id" = "sample_id")) %>%
#   dplyr::relocate(facility_clean, .after = sample_id) %>%
#   group_by(facility_clean) %>%
#   dplyr::summarise(
#     samples = n()
#   ) 

### Looking at new data on AMR
# I have not spent a lot of time reviewing this, but hopefully this is the source of truth for AMR
hcf_data %>% # I think I should spend more time checking the AMR stuff. It does not look like it matches well.
  drop_na(ecpos)


# mutate(unit_clean = case_when(
#   unit == "1" ~ "Delivery",
#   unit == "2" ~ "PNC",
#   unit == "3" ~ "NICU", 
#   unit == "4" ~ "KMC"
# ), .after = "unit") %>%
# mutate(site_of_sample_collected_clean = case_when(

#   site_of_sample_collected == 1 ~ 'Bed sheet',
#   site_of_sample_collected == 4 ~ 'oxygen cylinder',
#   site_of_sample_collected == 5 ~ 'Cabinet',
#   site_of_sample_collected == 7 ~ 'Door handle and door',
#   site_of_sample_collected == 8 ~ 'medical doctor',
#   site_of_sample_collected == 9 ~ 'Nurse',
#   site_of_sample_collected == 10 ~ 'midwife',
#   site_of_sample_collected == 11 ~ 'mothers',
#   site_of_sample_collected == 12 ~ 'care giver',
#   site_of_sample_collected == 13 ~ 'other',
#   site_of_sample_collected == 14 ~ 'bed rail',
#   site_of_sample_collected == 15 ~ 'IV tube',
#   site_of_sample_collected == 16 ~ 'CPAP machine',
#   site_of_sample_collected == 17 ~ 'Chair',
#   site_of_sample_collected == 18 ~ 'sink faucet',
#   site_of_sample_collected == 19 ~ 'Floor',
#   site_of_sample_collected == 20 ~ 'oxygen nasal tube',
#   site_of_sample_collected == 21 ~ 'Blanket',
#   site_of_sample_collected == 22 ~ 'Ambubag',
#   site_of_sample_collected == 23 ~ 'Radiant warmer',
#   site_of_sample_collected == 24 ~ 'Fetal monitor',
# ))


#### Table 3. ####
# Environmental Samples by Hospital and Unit
# For each facility, with each column being a different unit
# Has a section for surface, hand, tap water, device water, and total
# Each section has a total for that group, then Any positive %, then E coli #, then other coliforms %, the S aureus %

# For these, I looked at the count that are greater than 1. e coli has a cdp and membrane filtration measurement for some samples

tab3_data_cleaned <- hcf_data_cleaned %>%
  group_by(facility_clean, unit, sample_type_clean) %>% # Creating the groups to summarise by
  summarise(
    tot_samps = n(),
    
    ecoli_pos = sum(ecdp_count > 0 | e_coli_mf_count > 0, na.rm = TRUE), # There are two columns relevent for e coli
    ecoli_pos_isna = sum(is.na(ecdp_count) & is.na(e_coli_mf_count)), # Count of rows where BOTH are NA
    
    staph_pos = sum(sacdp_count > 0, na.rm = TRUE),
    staph_pos_isna = sum(is.na(sacdp_count)),
    
    kleb_pos = sum(kcdp_count > 0, na.rm = TRUE),
    kleb_pos_isna = sum(is.na(kcdp_count)),
    
    any_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE) 
  ) 
  #mutate_at(vars(ecoli_pos:kleb_pos), ~ if_else(is.na(.), 0, .)) # Make any cases where it is NA 0

# ifferent types of samples were tested by different methods:  Hand rinse and tap water samples were tested for E. coli using the membrane filtration technique and quantitative results were obtained using m-ColiBlue24® Media and filters (Hach|VWR, USEPA Method #10029)



#### Table 4 ####
### Note: I would double-check the site_type_classes. These have changed a decent bit between datasets, and there may be some sample types that I have not assigned that are now present.
## Also, this table is written as detection of AMR target bacteria. I took it as "any time these bacteria, which do have AMR targets, are positive" and not "the AMR target is positive"
# Frequency of detection of AMR target bacteria from swabs of single-site, high-touch environmental surfaces by infant proximity.

# This answers the "Proximal Swab Site" and "Distal Swab Site" section.
view_table4.1 <- hcf_data_cleaned %>%
  filter(sample_type_clean == "Swab") %>%
  mutate(site_type_class = case_when(
    site_type_clean %in% c("IV tube", "Blanket", "Bed sheet", "CPAP machine", "Radiant warmer", "oxygen cylinder", "bed rail","oxygen nasal tube","Fetal monitor") ~ "Proximal Swab Site",
    site_type_clean %in% c("Door handle and door", "Cabinet", "Chair", "sink faucet") ~ "Distal Swab Site",
    site_type_clean %in% c("Ambubag") ~ NA_character_ # These were some swabs that were not included. I also saw one phone swab on te table but not here, maybe because of my filtering.
    # I did not see stehtoscope/thermometer, I see IV but not nasal tubing (maybe its included in another), we had chair and im not sure if its also supposedto have a table
    ## Note: There are a couple that I did not properly classify. Talk with study leads to discuss what they should be considered.       
    ## THere are also many listed as other, with some  explanation of what they are. You may need to do some manual cleaning.
  )) %>%
  # mutate() %>% # I need one more level of consolidation because some of these are combined in the table
  group_by(facility_clean, site_type_class, site_type_clean) %>%
  dplyr::summarise(
    samples = n(),
    samples_amr_detect = sum(`Any AMR` > 0, na.rm = TRUE) # I don't think this column is actually reflective of AMR. 
  )

## This answers the "Proximal Swab Bacteria" and "Distal Swab Bacteria"
view_table4.2<-hcf_data_cleaned %>%
  filter(sample_type_clean == "Swab") %>%
  mutate(site_type_class = case_when(
    site_type_clean %in% c("IV tube", "Blanket", "Bed sheet", "CPAP machine", "Radiant warmer", "oxygen cylinder", "bed rail","oxygen nasal tube","Fetal monitor") ~ "Proximal Swab Site",
    site_type_clean %in% c("Door handle and door", "Cabinet", "Chair", "sink faucet") ~ "Distal Swab Site",
    site_type_clean %in% c("Ambubag") ~ NA_character_ # These were some swabs that were not included. I also saw one phone swab on te table but not here, maybe because of my filtering.
    # I did not see stehtoscope/thermometer, I see IV but not nasal tubing (maybe its included in another), we had chair and im not sure if its also supposedto have a table
  )) %>%
  group_by(facility_clean, site_type_class) %>%
  dplyr::summarise( # I don't know if this is meant to be positive for AMR detection for that bacteria type, or just that bacteria (what it currently is)
    samples = n(),
    ecoli_pos = sum(ecdp_count > 0 | e_coli_mf_count > 0, na.rm = TRUE), # Will need a more sophisticated approach here.
    staph_pos = sum(sacdp_count > 0),
    kleb_pos = sum(kcdp_count > 0),
    any_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE) 
  ) 

#A chi-square test of independence was performed to examine the relationship between facility (FH vs DT) and bacterial positivity from proximal swabs. 
# Create a 2x2 contingency table
proximal_bacteria <- matrix(c(18, 39, 39, 33), nrow = 2, byrow = TRUE)

# Add row and column names for clarity
rownames(proximal_bacteria) <- c("positive", "Negative")
colnames(proximal_bacteria) <- c("FH", "DT")

# Chi-Square Test of Independence
chisq.test(proximal_bacteria)

Distal_bacteria <- matrix(c(13, 13, 57, 17), nrow = 2, byrow = TRUE)

# Add row and column names for clarity
rownames(Distal_bacteria) <- c("positive", "Negative")
colnames(Distal_bacteria) <- c("FH", "DT")

# Chi-Square Test of Independence
chisq.test(Distal_bacteria)









#### Table 5 ####
#
Hand_rinse1<-hcf_data_cleaned %>%
  filter(sample_type_clean == "Hand rinse") %>%
  filter(site_type_clean %in% c("medical doctor", "care giver", "mothers", "midwife", "Nurse", "other")) %>%
  group_by(facility_clean, site_type_clean) %>%
  dplyr::summarise(
    samples = n(),
    any_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE) # Will need to be improved for e coli
  )

Hand_rinse2<-hcf_data_cleaned %>%
  filter(sample_type_clean == "Hand rinse") %>%
  group_by(facility_clean, sex_clean) %>%
  dplyr::summarise(
    samples = n(),
    any_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE) # Will need to be improved for e coli
  )



#### Table 6 ####
# We did not have the info when I wrote this code. We should now, so this could be written.


Ecoli_AMR<-hcf_data_cleaned %>%
  filter(`Any AMR` == 1, ecpos == 1) %>%
  group_by(facility_clean) %>%
  dplyr::summarise(
    samples = n(),
    MDR = sum(rowSums(across( c("e_coli_amp", "e_coli_cip", "e_coli_cro", "e_coli_gen", "e_coli_sxt", "e_coli_ttc" , "e_coli_aug", "e_coli_pep", "e_coli_caz", "e_coli_caf", "e_coli_cxf")) == 2, na.rm = TRUE) >= 2),
    amp = sum(e_coli_amp == 2, na.rm = TRUE), 
    cip = sum(e_coli_cip == 2, na.rm = TRUE), 
    cro = sum(e_coli_cro == 2, na.rm = TRUE), 
    gen = sum(e_coli_gen == 2, na.rm = TRUE), 
    sxt = sum(e_coli_sxt == 2, na.rm = TRUE), 
    ttc = sum(e_coli_ttc == 2, na.rm = TRUE), 
    aug = sum(e_coli_aug == 2, na.rm = TRUE), 
    pep = sum(e_coli_pep == 2, na.rm = TRUE), 
    caz = sum(e_coli_caz == 2, na.rm = TRUE), 
    caf = sum(e_coli_caf == 2, na.rm = TRUE), 
    cxf = sum(e_coli_cxf == 2, na.rm = TRUE)
  )


kleb_AMR<-hcf_data_cleaned %>%
  filter(`Any AMR` == 1, klebsiella_positive == 1) %>%
  group_by(facility_clean) %>%
  dplyr::summarise(
    samples = n(),
    MDR = sum(rowSums(across( c("klebsiella_amp", "klebsiella_cip", "klebsiella_cro", "klebsiella_gen", "klebsiella_sxt", "klebsiella_ttc" , "klebsiella_aug", "klebsiella_pep", "klebsiella_caz", "klebsiella_caf", "klebsiella_cxf")) == 2, na.rm = TRUE) >= 2),
    amp = sum(klebsiella_amp == 2, na.rm = TRUE), 
    cip = sum(klebsiella_cip == 2, na.rm = TRUE), 
    cro = sum(klebsiella_cro == 2, na.rm = TRUE), 
    gen = sum(klebsiella_gen == 2, na.rm = TRUE), 
    sxt = sum(klebsiella_sxt == 2, na.rm = TRUE), 
    ttc = sum(klebsiella_ttc == 2, na.rm = TRUE), 
    aug = sum(klebsiella_aug == 2, na.rm = TRUE), 
    pep = sum(klebsiella_pep == 2, na.rm = TRUE), 
    caz = sum(klebsiella_caz == 2, na.rm = TRUE), 
    caf = sum(klebsiella_caf == 2, na.rm = TRUE), 
    cxf = sum(klebsiella_cxf == 2, na.rm = TRUE)
  )

saur_AMR<-hcf_data_cleaned %>%
  filter(`Any AMR` == 1, s_aureus_positive == 1) %>%
  group_by(facility_clean) %>%
  dplyr::summarise(
    samples = n(),
    MDR = sum(rowSums(across( c("s_aureus_pen", "s_aureus_cip", "s_aureus_oxa", "s_aureus_da", "s_aureus_cro", "s_aureus_sxt" ,"s_aureus_ttc", "s_aureus_ery", "s_aureus_van", "s_aureus_caf", "s_aureus_cxf")) == 2, na.rm = TRUE) >= 2),
    pen = sum(s_aureus_pen == 2, na.rm = TRUE), 
    cip = sum(s_aureus_cip == 2, na.rm = TRUE), 
    oxa = sum(s_aureus_oxa == 2, na.rm = TRUE), 
    da = sum(s_aureus_da == 2, na.rm = TRUE), 
    cro = sum(s_aureus_cro == 2, na.rm = TRUE), 
    sxt = sum(s_aureus_sxt == 2, na.rm = TRUE), 
    ttc = sum(s_aureus_ttc == 2, na.rm = TRUE),
    ery = sum(s_aureus_ery == 2, na.rm = TRUE), 
    van = sum(s_aureus_van == 2, na.rm = TRUE), 
    caf = sum(s_aureus_caf == 2, na.rm = TRUE), 
    cxf = sum(s_aureus_cxf == 2, na.rm = TRUE)
  )

fac<-hcf_data_cleaned %>%
  group_by(facility_clean) %>%
  dplyr::summarise(
    samples = n()
  )

AMR<-hcf_data_cleaned %>%
  filter(`Any AMR` == 1) %>%
  group_by(facility_clean) %>%
  dplyr::summarise(
    samples = n()
  )






#### Figure 1 ####
### Will first make the data needed for each, then the graph
### Figure 1a: Sample type, percent of samples positive for any target bacteria
hcf_data_1a_data <- hcf_data_cleaned %>%
  group_by(facility_clean, sample_type_clean) %>%
  dplyr::summarise(
    samples = n(),
    samples_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE)
  ) %>%
  mutate(ratio_pos = samples_pos/samples)


plot_1a <- ggplot(hcf_data_1a_data,
                  aes(x = sample_type_clean, y = ratio_pos, fill = facility_clean)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent, expand = c(0, NA), limits = c(0, 0.9)) +
  scale_x_discrete(labels = c(
    "Drinking Water" = "Drinking Water", # Umm I don't see any tap water. Ok it's showing up as no positives
    "Hand Rinse" = "Hand",
    "Swab" = "Surface",
    "Water from medical device" = "Device"
  )) + 
  scale_fill_manual(values = c(
    "Deberetabor" = "#00008B",
    "Felegehiwot" = "#8DA0CB"
  )) +
  # Add axis labels here
  labs(
    title = "Frequency of all samples positive for any target bacteria by HCF and sample type",
    x = "Environmental Sample Type",
    y = "Proportion of Positive Samples",
    fill = "Facility"
  ) +
  # Style the plot
  theme_bw() +
  theme(
    legend.position = "bottom"
  )



### Figure 1b: Unit by facility, percent of samples positive for any target bacteria
hcf_data_1b_data  <- hcf_data_cleaned %>%
  group_by(facility_clean, unit) %>%
  dplyr::summarise(
    samples = n(),
    samples_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE)
  ) %>%
  mutate(ratio_pos = samples_pos/samples)

plot_1b <- ggplot(hcf_data_1b_data,
                  aes(x = unit, y = ratio_pos, fill = facility_clean)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent, expand = c(0, NA), limits = c(0, 0.9)) +
  scale_fill_manual(values = c(
    "Deberetabor" = "#00008B",
    "Felegehiwot" = "#8DA0CB"
  )) +
  # Add axis titles
  labs(
    title = "Frequency of all samples positive for any target bacteria by HCF and unit",
    x = "Hospital Unit",
    y = "Proportion of Positive Samples",
    fill = "Facility"
  ) +
  
  # Improve appearance
  theme_bw() +
  theme(
    legend.position = "bottom"
  )











### Figure 1c: surface swabs positive by unit by facility
hcf_data_1c_data  <- hcf_data_cleaned %>%
  filter(sample_type_clean == "Swab") %>%
  group_by(facility_clean, unit) %>%
  dplyr::summarise(
    samples = n(),
    samples_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE)
  ) %>%
  mutate(ratio_pos = samples_pos/samples)


plot_1c <- ggplot(hcf_data_1c_data,
                  aes(x = unit, y = ratio_pos, fill = facility_clean)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent, expand = c(0, NA), limits = c(0, 0.9)) +
  scale_fill_manual(values = c(
    "Deberetabor" = "#00008B",
    "Felegehiwot" = "#8DA0CB"
  )) +
  # Add axis and legend titles
  labs(
    title = "Frequency of surface swab samples positive for any target bacteria by HCF and unit",
    x = "Hospital Unit",
    y = "Proportion of Surface Swabs Positive",
    fill = "Facility"
  ) +
  
  # Theme settings
  theme_bw() +
  theme(
    legend.position = "bottom"
  )








### Figure 1d: Hand rinse samples positive for anything by hcf and unit
hcf_data_1d_data  <- hcf_data_cleaned %>%
  filter(sample_type_clean == "Hand rinse") %>%
  group_by(facility_clean, unit) %>%
  dplyr::summarise(
    samples = n(),
    samples_pos = sum((ecdp_count > 0) | (e_coli_mf_count > 0) | (sacdp_count > 0) | (kcdp_count > 0), na.rm = TRUE)
  ) %>%
  mutate(ratio_pos = samples_pos/samples)

plot_1d <- ggplot(hcf_data_1d_data,
                  aes(x = unit, y = ratio_pos, fill = facility_clean)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent, expand = c(0, NA), limits = c(0, 0.9)) +
  scale_fill_manual(values = c(
    "Deberetabor" = "#00008B",
    "Felegehiwot" = "#8DA0CB"
  )) +
  # Add axis titles and legend title
  labs(
    title = "Frequency of hand rinse samples positive for any target bacteria by HCF and unit",
    x = "Hospital Unit",
    y = "Proportion of Hand Rinse Samples Positive",
    fill = "Facility"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "bottom"
  )




#plot_1a + 
#  plot_1b + 
#  plot_1c + 
#  plot_1d + 
#  guide_area() +
#  plot_layout(ncol = 1, guides = "collect") + 
#  plot_annotation(tag_levels = "a", tag_prefix = "1", tag_suffix = ")")
















#### Some summary statistics for checking results, not necessarily to include in the paper ####

hcf_data_cleaned %>%
  group_by(facility_clean, unit) %>%
  dplyr::summarise(
    samples = n()
  )

hcf_data_cleaned %>%
  group_by(sample_type_clean) %>%
  dplyr::summarise(
    samples = n()
  ) %>%
  arrange(sample_type_clean)

hcf_data_cleaned %>%
  group_by(facility_clean, sample_type_clean) %>%
  dplyr::summarise(
    samples = n()
  ) %>%
  arrange(sample_type_clean)


hcf_data_cleaned %>%
  group_by(facility_clean, unit, sample_type_clean) %>%
  dplyr::summarise(
    samples = n()
  ) %>%
  arrange(sample_type_clean) %>%
  print(n = 100)

tab3_data_cleaned %>%
  print(n = 100)


