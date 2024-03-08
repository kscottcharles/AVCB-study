# Demographic Table -------------------------------------------------------

# Data Wrangling for Tables -----------------------------------------------


#Dems I want in my table:
#Gender (kids)
#Race.Eth_group (kids)
#Income
#P1_pol
#P1_ed

#Factor + Recode + Level for dem table
AVCB_wp_dyads$Gender <- factor(AVCB_wp_dyads$Gender)
levels(AVCB_wp_dyads$Gender)

AVCB_wp_dyads$Race.Eth_group <- factor(AVCB_wp_dyads$Race.Eth_group)
levels(AVCB_wp_dyads$Race.Eth_group)
AVCB_wp_dyads$Race.Eth_group <- recode_factor(AVCB_wp_dyads$Race.Eth_group, "Hispanic or Latino"="Hispanic or Latino", "White or Caucasian"="White or Caucasian", "Black or African American"="Black or African American",.default = "Bi- or Multi-Racial" )

AVCB_wp_dyads$Income <- factor(AVCB_wp_dyads$Income)
levels(AVCB_wp_dyads$Income)
AVCB_wp_dyads$Income <- factor(AVCB_wp_dyads$Income, levels = c("Less than $15,000", "$20,000 to $24,999", "$35,000 to $49,999","$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 or more", "Prefer not to answer", "N/A"))

AVCB_wp_dyads$P1_pol <- factor(AVCB_wp_dyads$P1_pol)
levels(AVCB_wp_dyads$P1_pol)
AVCB_wp_dyads$P1_pol <- factor(AVCB_wp_dyads$P1_pol, levels = c("Extremely Conservative", "Conservative", "Slightly Conservative","Moderate, Middle of the Road", "Slightly Liberal", "Liberal", "Extremely Liberal", "Haven't thought much about it/I don't know", "Prefer not to answer"))

AVCB_wp_dyads$P1_ed <- factor(AVCB_wp_dyads$P1_ed)
levels(AVCB_wp_dyads$P1_ed)
AVCB_wp_dyads$P1_ed <- recode_factor(AVCB_wp_dyads$P1_ed, "Associate's Degree or equivalent 2-year degree"="Associate's Degree or equivalent 2-year degree", "At least one year college"="At least one year of college", "At least one year in college"="At least one year of college","Bachelor's Degree or equivalent 4-year undergraduate degree" = "Bachelor's Degree or equivalent 4-year undergraduate degree", "Graduate Degree"="Graduate Degree", "High School or GED"="High School or GED", "Some graduate training (not completed)"="Some graduate training (not completed)" )

AVCB_wp_dyads$P1_ed <- factor(AVCB_wp_dyads$P1_ed, levels = c("High School or GED", "At least one year of college", "Associate's Degree or equivalent 2-year degree","Bachelor's Degree or equivalent 4-year undergraduate degree", "Some graduate training (not completed)", "Graduate Degree"))

# Create a new variable to represent parent-child race-match
AVCB_wp_dyads <- AVCB_wp_dyads %>%
  mutate(race_match = ifelse(P1_Race.Eth == Race.Eth_group, "Same Race/Ethnicity (White or Caucasian)", "Child Biracial,  Multiracial, or Other"))

AVCB_wp_dyads$race_match <- factor(AVCB_wp_dyads$race_match)

# One observation for household income is N/A

AVCB_wp_dyads$Income[126] <- NA

# Creating demographic tables ---------------------------------------------

#Gender table

gender_table <- AVCB_wp_dyads %>% filter(!is.na(Gender)) %>%
  group_by(Gender) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = round(count / sum(count), 2)) %>%
  rename(`Children's Gender` = Gender, N = count, P = proportion)

gender_table %>% apa_table()

#race_match table

# Create a table summarizing the counts
race_match_table <- AVCB_wp_dyads %>% filter(!is.na(race_match)) %>%
  group_by(race_match) %>%
  summarise(count = n()) %>% 
  mutate(proportion = round(count / sum(count), 2)) %>%
  rename(`Parent 1 and Children's Race/Ethnicity` = race_match, N = count, P = proportion)

race_match_table %>% apa_table()

#Race.Eth_group table

race_table <- AVCB_wp_dyads %>% filter(!is.na(Race.Eth_group)) %>%
  group_by(Race.Eth_group) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = round(count / sum(count), 2)) %>%
  rename(`Children's Race/Ethnicity` = Race.Eth_group, N = count, P = proportion)

race_table %>% apa_table()


#Income table

income_table <- AVCB_wp_dyads %>% filter(!is.na(Income)) %>%
  group_by(Income) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = round(count / sum(count), 2)) %>%
  rename(`Parents' Yearly Household Income` = Income, N = count, P = proportion)

income_table %>% apa_table()


#P1_pol

pol_table <- AVCB_wp_dyads %>% filter(!is.na(P1_pol)) %>%
  group_by(P1_pol) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = round(count / sum(count), 2)) %>%
  rename(`Parents' Political Orientation` = P1_pol, N = count, P = proportion)

pol_table %>% apa_table()

#P1_ed

ed_table <- AVCB_wp_dyads %>% filter(!is.na(P1_ed)) %>%
  group_by(P1_ed) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = round(count / sum(count), 2)) %>%
  rename(`Parents' Education Level` = P1_ed, N = count, P = proportion) 

ed_table %>% apa_table()
