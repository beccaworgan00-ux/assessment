
library(readxl) # for reading Excel files
library(janitor) # for cleaning column names
library(tidyverse) # for data wrangling and plotting
library(reshape2) # for melting correlation matrix
library(corrplot)

# import data IDs ------------------------------------------------------------------


# 1) Read everything as text so we control parsing ourselves
raw_ID <- read_excel(
  "IDs.xlsx",
  col_types = c("text", "text", "text", "text") # e.g., ID, Name, DOB, Group
) %>%
  select(-Group) %>%
  clean_names()

# 2) Parse DOB robustly:
#    - Excel serial numbers (pure digits) -> as.Date with Excel origin
#    - Text dates in m/d/Y (with or without leading zeros) -> lubridate::mdy
ID <- raw_ID %>%
  mutate(
    dob = case_when(
      # Excel serial numbers (all digits). Some files may have decimals, so handle that too.
      str_detect(dob, "^\\d+(\\.\\d+)?$") ~ as.Date(as.numeric(dob), origin = "1899-12-30"),
      
      # Text dates like 2/28/2012 or 05/03/2012 -> force mdy interpretation
      str_detect(dob, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ mdy(dob),
      
      TRUE ~ NA_Date_
    )
  )

ID <- clean_names(ID)

Stature <- read_excel("Stature.xlsx", 
                      col_types = c("text", "text",
                                    "date", "numeric", "numeric", "numeric" 
                      )) %>%
  mutate(
    Sit_height = SH_raw - 40
  ) %>% 
  select(-c(SH_raw)) %>% 
  clean_names() 



# import force data ------------------------------------------------------------------


cmj<- read_csv("CMJ.csv", 
               col_types = cols(Date = col_date(format = "%d/%m/%Y")))
# manually tidy long col names ----
# use colnames() to quickly fetch current col names

colnames(cmj)

cmj<- clean_names(cmj) %>%
  select(id = name,
         date,
         cmj_height = jump_height_imp_mom_cm,
         cmj_rsi = rsi_modified_imp_mom_m_s,
         ct = contraction_time_ms,
         epv = eccentric_peak_velocity_m_s,
         rel_pp = peak_power_bm_w_kg,
         con_imp = concentric_impulse_n_s)  

# merge data ------------------------------------------------------------------

# merge on ID

merged_data <- ID %>%
  inner_join(cmj, by = "id") %>%
  mutate(age_at_test = round(interval(dob, date) / years(1), 2)) %>%
  inner_join(Stature, by = c("id", "date")) %>% 
  select(-c(mass)) # mass is in imtp, so we drop it here to avoid confusion when we merge imtp data next


# we need to create timepoints as some players have had imtp and cmj on different dates. We can create a timepoint variable based on the year of the test date, and then merge imtp data based on ID and timepoint.

merged_data <- merged_data %>%
  mutate(
    id = str_replace_all(id, " ", ""),
    time_point = case_when(
      between(date, as.Date("2024-02-01"), as.Date("2024-02-28")) ~ "Feb 2024",
      between(date, as.Date("2024-04-03"), as.Date("2024-04-15")) ~ "April 2024",
      between(date, as.Date("2024-08-27"), as.Date("2024-08-30")) ~ "August 2024",
      between(date, as.Date("2025-04-15"), as.Date("2025-04-25")) ~ "April 2025",
      between(date, as.Date("2025-09-08"), as.Date("2025-09-10")) ~ "September 2025",
      between(date, as.Date("2026-01-01"), as.Date("2026-01-14")) ~ "Jan 2026",
      TRUE ~ NA_character_  # Add this line if you want to handle cases not falling into any range
    ))



imtp<- read_csv("imtp.csv", 
               col_types = cols(Date = col_date(format = "%d/%m/%Y")))


imtp<- clean_names(imtp) %>%
  select(id = name,
         date,
         mass = bw_kg,
         rel_peak = peak_vertical_force_bm_n_kg, 
         peak_v_f = peak_vertical_force_n) %>%
  mutate(
    id = str_replace_all(id, " ", ""),
    time_point = case_when(
      between(date, as.Date("2024-02-01"), as.Date("2024-02-28")) ~ "Feb 2024",
      between(date, as.Date("2024-04-03"), as.Date("2024-04-15")) ~ "April 2024",
      between(date, as.Date("2024-08-27"), as.Date("2024-08-30")) ~ "August 2024",
      between(date, as.Date("2025-04-15"), as.Date("2025-04-25")) ~ "April 2025",
      between(date, as.Date("2025-09-08"), as.Date("2025-09-10")) ~ "September 2025",
      between(date, as.Date("2026-01-01"), as.Date("2026-01-14")) ~ "Jan 2026",
      TRUE ~ NA_character_  # Add this line if you want to handle cases not falling into any range
    )) %>%
  select(-c(date))

merged_data <- merged_data %>%
  inner_join(imtp, by = c("id", "time_point"))



rjt<- read_csv("RJT.csv", 
                col_types = cols(Date = col_date(format = "%d/%m/%Y")))


rjt <- clean_names(rjt) %>%
  select(id = name,
         date,
         mean_rsi_fc_time = mean_rsi_flight_contact_time,
         mean_rsi_ms = mean_rsi_jump_height_contact_time_m_s
         ) %>%
  
  mutate(
    id = str_replace_all(id, " ", ""),
    time_point = case_when(
      between(date, as.Date("2024-02-01"), as.Date("2024-02-28")) ~ "Feb 2024",
      between(date, as.Date("2024-04-03"), as.Date("2024-04-15")) ~ "April 2024",
      between(date, as.Date("2024-08-27"), as.Date("2024-08-30")) ~ "August 2024",
      between(date, as.Date("2025-04-15"), as.Date("2025-04-25")) ~ "April 2025",
      between(date, as.Date("2025-09-08"), as.Date("2025-09-10")) ~ "September 2025",
      between(date, as.Date("2026-01-01"), as.Date("2026-01-14")) ~ "Jan 2026",
      TRUE ~ NA_character_  # Add this line if you want to handle cases not falling into any range
)) %>%
  select(-c(date))

merged_data <- merged_data %>%
  inner_join(rjt, by = c("id", "time_point"))

write.csv(merged_data, "data.csv")
