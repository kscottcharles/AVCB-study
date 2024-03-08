
# Hypotheses and Data Visualizations --------------------------------------

#Articulate hypotheses/research Qs/mock figures:
 
  #H1: Parents’ authoritarian values will positively predict children’s own authoritarian values and this relation will strengthen with age.    
    #Scatterplot with parent AV score on x axis, child AV score on y axis with child age in different colors (6-10 years) 
    #Natallie's Suggestion: Scatterplot + smooth with age plotted with different colors 

  #H2: Parents’ diversity ideology will positively predict children’s own diversity ideology, and this relation will strengthen with age.   
    #For child binary/parent continuous: Grouped Barplot with child classroom choice (binary) on x axis and parent COBRA score on y axis, plot age with different colored bars
    #Natalie's Suggestion: 2 Boxplots of parent COBRA score comparing kids who choose CC vs. kids who choose CB 
      #But how do I add kids' age into this plot????

    #For child binary/parent binary: Stacked barchart with child classroom choice (binary) on x axis with each bar broken down by parent classroom choice (binary)
    #Natalie Suggestion: ???
  
  #H3: Higher authoritarian values will positively predict more colorblind attitudes.
    #For parents: Scatterplot with parent AV score on x axis, parent COBRA score on y axis
    #For kids: Barplot with child classroom choice (binary) on x axis and child AV score on y axis
    #Natalie's Suggestion: 2 Boxplots of parent AV score comparing kids who choose CC vs. kids who choose CB
      #But how do I add kids' age into this plot????


#Secondary questions/data visualizations:
  #How do kids own classroom choice compare to their perceptions of parents classroom choice
  #How do kids own AV score compare to their perceptions of parents' AV score
  #How do parents classroom choice (binary and scale versions) relate to their COBRA score
  #How do parents beliefs about parent vs. school responsibility to teach about racism vs. DEI relate to diversity ideology
  #How do parents beliefs about parent vs. school responsibility to teach about racism vs. DEI relate to AV score


# Loading Packages --------------------------------------------------------

#Load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(readr)
library(knitr)
library(kableExtra)
library(psych)
library(stats)
library(lme4)
library(scales)
library(apaTables)

# Loading Data ------------------------------------------------------------

AVCB_wp_kids <- read.csv("Data/AVCB_wp_kids.csv", na.strings = c("", "NA", "#N/A"))
AVCB_wp_parents <- read.csv("Data/AVCB_wp_parents.csv", na.strings = "")

#combine files
AVCB_wp_dyads <- merge(AVCB_wp_kids, AVCB_wp_parents, by = "Family_ID")

#renaming variables
AVCB_wp_dyads <- rename(AVCB_wp_dyads, "Single Child ID" = "ID.x")
AVCB_wp_dyads <- rename(AVCB_wp_dyads, "All Child IDs" = "ID.y")
AVCB_wp_dyads <- rename(AVCB_wp_dyads, "Child Version" = "Version.x")
AVCB_wp_dyads <- rename(AVCB_wp_dyads, "Parent Version" = "Version.y")
AVCB_wp_dyads <- rename(AVCB_wp_dyads, "Child_CB_Self_choice" = "CB_Self_choice.x")
AVCB_wp_dyads <- rename(AVCB_wp_dyads, "Parent_CB_Self_choice" = "CB_Self_choice.y")

# Creating Intermediate Dataset -------------------------------------------

#Checking column names before creating ID
print(names(AVCB_wp_dyads))

AVCB_wp_dyads_ID <- AVCB_wp_dyads |> 
  select(Family_ID:Gender, P1_Race.Eth, P2_Race.Eth, P1_pol:P2_ed, Child_CB_Self_choice, CB_Parents_choice, RE_Ind_Self_choice:Parent_sum, RE_Ind:WB_Cons,COBRAS_1_num:AV_sum)

#moving AV choice variables tgo right before AV num variables (for parents) - will want to rename these later so that it's clear which ones are kid vs. parent variables

AVCB_wp_dyads_ID <- AVCB_wp_dyads_ID |> 
  relocate(RE_Ind:WB_Cons, .before = RE_Ind_num)

AVCB_wp_dyads_ID

#Checking column names
print(names(AVCB_wp_dyads_ID))

#Write csv
write_csv(AVCB_wp_dyads_ID, "AVCB_wp_dyads_ID.csv")

# Loading Data ------------------------------------------------------------

#Loading ID data
#setwd("/Users/kailamarinascott-charles/Desktop/AVCB-study/Data")
#AVCB_wp_dyads_ID <-read.csv("AVCB_wp_dyads_ID.csv")

# Data Wrangling for Full Dataset ------------------------------------------

#Data wrangling

  #transform variables

to_num <-  c("Years", "RE_Ind_Self_num", "RE_Ind_Parents_num", "Obed_SR_Self_num", "Obed_SR_Parents_num", "GM_Cur_Self_num", "GM_Cur_Parents_num","WB_Cons_Self_num", "WB_Cons_Parents_num","COBRAS_1_num", "COBRAS_2_num", "COBRAS_3_num", "COBRAS_4_num", "COBRAS_avg", "CB_Self_Rating_CB_num", "CB_Self_Rating_CC_num", "CB_Teacher_Rating_CB_num", "CB_Teacher_Rating_CC_num", "Self_diff", "Teacher_diff", "Responsibility_Parents_Racism_num", "Responsibility_Schools_Racism_num", "Responsibility_Parents_DEI_num", "Responsibility_Schools_DEI_num", "Responsibility_avg", "Responsibility_Parents_avg", "Responsibility_Schools_avg", "Responsibility_Racism_avg", "Responsibility_DEI_avg", "RE_Ind_num", "Obed_SR_num", "GM_Cur_num", "WB_Cons_num", "AV_sum")
to_factor <- c('Family_ID', 'Single Child ID', "Gender", "Child_CB_Self_choice", "CB_Parents_choice", "RE_Ind_Self_choice", "RE_Ind_Parents_choice", "Obed_SR_Self_choice", "Obed_SR_Parents_choice", "GM_Cur_Self_choice", "GM_Cur_Parents_choice","WB_Cons_Self_choice", "WB_Cons_Parents_choice", "Parent_CB_Self_choice", "CB_Self_Rating_CB", "CB_Self_Rating_CC", "CB_Teacher_choice", "CB_Teacher_Rating_CB", "CB_Teacher_Rating_CC", "RE_Ind", "Obed_SR", "GM_Cur", "WB_Cons")
               
AVCB_wp_dyads_ID[,to_num] <- lapply(AVCB_wp_dyads_ID[,to_num] , as.numeric)
AVCB_wp_dyads_ID[,to_factor] <- lapply(AVCB_wp_dyads_ID[,to_factor] , factor)

  #For my main analyses, I don't need to keep the following columns so I would take them out: P1_Race.Eth,P2_Race.Eth, P1_pol,	P2_pol, P1_ed, P2_ed

columns_to_delete <- which(names(AVCB_wp_dyads_ID) %in% c("P1_Race.Eth", "P2_Race.Eth", "P1_pol", "P2_pol", "P1_ed", "P2_ed"))

AVCB_wp_dyads_ID <- AVCB_wp_dyads_ID[, -columns_to_delete]


  #Rename some columns to be more sensible

names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "Child_CB_Self_choice"] <- "Child_Class_Self_choice"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Parents_choice"] <- "Child_Class_Parents_choice"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "Parent_CB_Self_choice"] <- "Parent_Class_Self_choice"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Self_Rating_CB"] <- "Class_Self_Rating_CB"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Self_Rating_CC"] <- "Class_Self_Rating_CC"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Self_Rating_CB_num"] <- "Class_Self_Rating_CB_num"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Self_Rating_CC_num"] <- "Class_Self_Rating_CC_num"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Teacher_choice"] <- "Class_Teacher_choice"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Teacher_Rating_CB"] <- "Class_Teacher_Rating_CB"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Teacher_Rating_CC"] <- "Class_Teacher_Rating_CC"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Teacher_Rating_CB_num"] <- "Class_Teacher_Rating_CB_num"
names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "CB_Teacher_Rating_CC_num"] <- "Class_Teacher_Rating_CC_num"

  #Create new Parent_AVscore and Child_AVscore variables 
  #Transform AV scores + rename

#Parent AV
AVCB_wp_dyads_ID <- AVCB_wp_dyads_ID %>%
  mutate(AV_sum = AV_sum / 4)

names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "AV_sum"] <- "Parent_AVscore"

#Child - Self
AVCB_wp_dyads_ID <- AVCB_wp_dyads_ID %>%
  mutate(Self_sum = Self_sum / 4)

names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "Self_sum"] <- "Child_AVscore_Self"

#Child - Parent perception
AVCB_wp_dyads_ID <- AVCB_wp_dyads_ID %>%
  mutate(Parent_sum = Parent_sum / 4)

names(AVCB_wp_dyads_ID)[names(AVCB_wp_dyads_ID) == "Parent_sum"] <- "Child_AVscore_Parent"


# Creating Different Datasets for Each Hypothesis -------------------------

#Creating dif datasets for dif questions (only keep relevant columns)

#H1: Parents’ authoritarian values will positively predict children’s own authoritarian values and this relation will strengthen with age.    
#Scatterplot with parent AV score on x axis, child AV score on y axis with child age in different colors (6-10 years) 
#Natallie's Suggestion: Scatterplot + smooth with age plotted with different colors 

AVCB_AVscores <- AVCB_wp_dyads_ID %>% select(Family_ID, 'Single Child ID', Years, Parent_AVscore, Child_AVscore_Self, Child_AVscore_Parent)
write_csv(AVCB_AVscores, "Data/AVCB_AVscores.csv")

#H2: Parents’ diversity ideology will positively predict children’s own diversity ideology, and this relation will strengthen with age.   
#For child binary/parent continuous: Barplot with child classroom choice (binary) on x axis and parent COBRA score on y axis, plot age with different colored dots on top of barplot
#Natalie's Suggestion: 2 Boxplots of parent COBRA score comparing kids who choose CC vs. kids who choose CB 

#For child binary/parent binary: Stacked barchart with child classroom choice (binary) on x axis with each bar broken down by parent classroom choice (binary)
#Natalie Suggestion: ???

AVCB_DIscores <- AVCB_wp_dyads_ID %>% select(Family_ID, 'Single Child ID', Years, Child_Class_Self_choice, Child_Class_Parents_choice, COBRAS_avg, Parent_Class_Self_choice, Class_Self_Rating_CB:Teacher_diff)
write_csv(AVCB_DIscores, "Data/AVCB_DIscores.csv")

#H3: Higher authoritarian values will positively predict more colorblind attitudes.
#For parents: Scatterplot with parent AV score on x axis, parent COBRA score on y axis
#For kids: Barplot with child classroom choice (binary) on x axis and child AV score on y axis
#Natalie's Suggestion: 2 Boxplots of parent AV score comparing kids who choose CC vs. kids who choose CB

AVCB_Bothscores <- AVCB_wp_dyads_ID %>% select(Family_ID, 'Single Child ID', Years, Child_Class_Self_choice, Child_Class_Parents_choice, COBRAS_avg, Parent_Class_Self_choice, Class_Self_Rating_CB:Teacher_diff, Parent_AVscore, Child_AVscore_Self, Child_AVscore_Parent)
write_csv(AVCB_Bothscores, "Data/AVCB_Bothscores.csv")

# Other hyptheses that might require pivot_longer -------------------------------------------------------------------

##Pivoting longer
#Examples below on how I might transform my data from wide to long format for some secondary research Qs/data visualizations:

                                                      #Child data:

##If I wanted to look at relations between child class choice and child perceptions of parent class choice

  #pivot cols Child_CB_Self_choice	and CB_Parents_choice (kids' choice of classroom for themselves and their parent)
  #new col that will store variable names: Class_Choice_Type (either parent or self)
  #new col that will store the values corresponding to the measurements:Class_Choice (either CC or CB)

AVCB_DIscoresa_longer <- AVCB_DIscores %>% pivot_longer (cols = c("Child_Class_Self_choice", "Child_Class_Parents_choice"), 
                                                     names_to = "Child_Class_Choice_For", 
                                                     values_to = "Child_Class_Choice")
#rename values in column to be simpler

#AVCB_DIscoresa_longer <- AVCB_DIscoresa_longer %>%
  #mutate(Child_Class_Choice_For = recode(Child_Class_Choice_For, 
                             #"Child_Class_Self_choice" = "Self",
                             #"Child_Class_Parents_choice" = "Parent"))

#write_csv(AVCB_DIscoresa_longer, "AVCB_DIscoresa_longer.csv")

                                              #Parent data:

##If I wanted to look at relations btw parents' rating of the CB and CC classrooms (how similar is each message to the one you would give?)
#pivot Class_Self_Rating_CB	Class_Self_Rating_CC
#new col that will store variable names: Class_self_rating_type
#new col that will store the values corresponding to the measurements:Class_self_rating_response

#AVCB_DIscoresb_longer <- AVCB_DIscores %>% pivot_longer (cols = c("Class_Self_Rating_CB", "Class_Self_Rating_CC"), 
                                            #names_to = "Class_Self_Rating_Type", 
                                            #values_to = "Class_Self_Rating_Response")
#rename values in column to be simpler

#AVCB_DIscoresb_longer <- AVCB_DIscoresb_longer %>%
  #mutate(Class_Self_Rating_Type = recode(Class_Self_Rating_Type, 
                                         #"Class_Self_Rating_CB" = "CB",
                                         #"Class_Self_Rating_CC" = "CC"))

#write_csv(AVCB_DIscoresb_longer, "AVCB_DIscoresb_longer.csv")

##If I wanted to look at relations btw parents' rating of the CB and CC classrooms (how much do you like each of these teachers' message?)
#pivot CB_Teacher_Rating_CB	CB_Teacher_Rating_CC
#new col that will store variable names: Class_teacher_rating
#new col that will store the values corresponding to the measurements:Class_teacher_rating_response

#AVCB_DIscoresc_longer <- AVCB_DIscores %>% pivot_longer (cols = c("Class_Teacher_Rating_CB", "Class_Teacher_Rating_CC"), 
                                             #names_to = "Class_Teacher_Rating_Type", 
                                             #values_to = "Class_Teacher_Rating_Response")
#rename values in column to be simpler

#AVCB_DIscoresc_longer <- AVCB_DIscoresc_longer %>%
  #mutate(Class_Teacher_Rating_Type = recode(Class_Teacher_Rating_Type, 
                                         #"Class_Teacher_Rating_CB" = "CB",
                                         #"Class_Teacher_Rating_CC" = "CC"))

#write_csv(AVCB_DIscoresc_longer, "AVCB_DIscoresc_longer.csv")


##If I wanted to look at relations btw parents' choice btw class they'd want their child to be in vs. parents choice of teacher's class they prefer (should be super similar)
#pivot cols Parent_Class_Self_choice	and Class_Teacher_choice 
#new col that will store variable names: Class_Choice_Type 
#new col that will store the values corresponding to the measurements:Class_Choice_Response

#AVCB_DIscoresd_longer <- AVCB_DIscores %>% pivot_longer (cols = c("Parent_Class_Self_choice", "Class_Teacher_choice"), 
                                                      #names_to = "Parent_Class_Choice_Type", 
                                                      #values_to = "Parent_Class_Choice_Response")
#rename values in column to be simpler

#AVCB_DIscoresd_longer <- AVCB_DIscoresd_longer %>%
  #mutate(Parent_Class_Choice_Type = recode(Parent_Class_Choice_Type, 
                                          #"Parent_Class_Self_choice" = "Self-children",
                                          #"Class_Teacher_choice" = "Teacher"))

#write_csv(AVCB_DIscoresd_longer, "AVCB_DIscoresd_longer.csv")
