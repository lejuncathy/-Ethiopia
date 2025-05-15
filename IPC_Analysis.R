#### Libraries ####
library(tidyverse)
library(readxl)
library(dplyr) #select(), mutate(), group_by()
library(tidyr) #pivot_longer(), pivot_wider()
library(ggplot2)
#### File paths ####
IPC_data <- read_excel("IPC_Analysis/Daily_IPC_Checklist_-_latest_version_-_False_-_2024-07-29-15-07-02.xlsx", sheet="Daily IPC Checklist") #import the data file
IPC_data_patient_caregiver_hand_hygiene <- read_excel("IPC_Analysis/Daily_IPC_Checklist_-_latest_version_-_False_-_2024-07-29-15-07-02.xlsx", sheet="patient_caregiver_hand_hygiene")
IPC_data_visible_cleanliness <- read_excel("IPC_Analysis/Daily_IPC_Checklist_-_latest_version_-_False_-_2024-07-29-15-07-02.xlsx", sheet="visible_cleanliness")

#### Hand hygiene ####
hand_hygiene_result <- IPC_data  %>%
  select(4:31) %>% #choose column about hand_hygiene
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% # change the form of final table
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%# Keeps only rows where Response is "1", "0", or "NA"
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% #Counts the number of times each response appears per variable
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%#For each Variable, calculate the percentage of each response:
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 

patient_caregiver_hand_hygiene_result <- IPC_data_patient_caregiver_hand_hygiene %>%
  select(5:9) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 



#### clean supplies ####
visible_cleanliness_result <- IPC_data_visible_cleanliness  %>%
  select(5:18) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 

  
#### medical device cleaning (for sterilization or high-level disinfection) ####
medical_device_result <- IPC_data  %>%
  select(44:54) %>% #choose column about hand_hygiene
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 

#### medical device storage ####
medical_device_storage_result <- IPC_data  %>%
  select(55:57) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 

#### Clinician_Washroom_general ####
Clinician_Washroom_general_result <- IPC_data  %>%
  select(86:89) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 

#### Sink_clinician_washroom ####
Sink_clinician_washroom_result <- IPC_data  %>%
  select(90:99) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 


#### Toilet_Clinician_Washroom ####
Toilet_Clinician_Washroom_result <- IPC_data  %>%
  select(101:108) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 

#### Shower_Clinician_washroom ####
Shower_Clinician_washroom_result <- IPC_data  %>%
  select(109:114) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 

#### Patient_caregiver_washrooms ####
Patient_caregiver_washrooms_result <- IPC_data  %>%
  select(115:119) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>% 
  mutate(Response = case_when(
    is.na(Response) ~ "NA",
    Response == 1 ~ "1",
    Response == 0 ~ "0",
  )) %>%
  filter(Response %in% c("1", "0", "NA")) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Variable) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  pivot_wider(names_from = Response, values_from = c(Count, Percent), values_fill = 0) 


#### table for Sink_clinician_washroom ####
ggplot(Sink_clinician_washroom_result, aes(x = Variable, y = Percent_1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # use scale = 1 if already in 0–100
  labs(title = "Availability of IPC Items",
       x = "IPC Variable",
       y = "Percent 'Yes'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### table for Toilet_Clinician_Washroom ####
ggplot(Toilet_Clinician_Washroom_result, aes(x = Variable, y = Percent_1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # use scale = 1 if already in 0–100
  labs(title = "Availability of IPC Items",
       x = "IPC Variable",
       y = "Percent 'Yes'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### table for Shower_Clinician_washroom ####
ggplot(Shower_Clinician_washroom_result, aes(x = Variable, y = Percent_1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # use scale = 1 if already in 0–100
  labs(title = "Availability of IPC Items",
       x = "IPC Variable",
       y = "Percent 'Yes'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### table for Patient_caregiver_washrooms ####
ggplot(Patient_caregiver_washrooms_result, aes(x = Variable, y = Percent_1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # use scale = 1 if already in 0–100
  labs(title = "Availability of IPC Items",
       x = "IPC Variable",
       y = "Percent 'Yes'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))