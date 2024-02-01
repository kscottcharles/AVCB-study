library(tidyverse)

## Create your goal tibble to replicate

# Run this line to see what your end product should look like
sw.wrangled.goal <- read_csv("sw-wrangled.csv") %>% 
  mutate(across(c(hair, gender, species, homeworld), factor)) # this is a quick-and-dirty fix to account for odd importing behavior

# View in console
sw.wrangled.goal 

# Examine the structure of the df and take note of data types
# Look closely at factors (you may need another function to do so) to see their levels
str(sw.wrangled.goal) 

#first_name: chr
#last_name: chr
#initials: chr
#height_in: num
#height_cm: num
#mass: num
#hair: factor with 12 levels
#gender: factor with 2 levels
#species: factor with 37 levels
#homeworld: factor with 48 levels
#brown_hair: logi (T/F)


## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled
# If you get stuck, use comments to "hold space" for where you know code needs to go to achieve a goal you're not sure how to execute
sw.wranged <- starwars # %>% ...

#What to do to replicate wrangled data?

  #1 Take out cols: eye_color, skin_color, birth_year, sex, films, vehicles, starships
  #2 Split "name" col to first and last name
  #3 Create "initial" col
  #4 Create height_in and height_cm from "height" col
  #5 Change "hair_color" col to "hair" and use this col to create hair_brown col with T/F in the cells
  #6 Change gender column cells to be m and f
  #7 Capitalize cells in "species" col
  #8 Match data types with goal dataframe
  #9 #87 rows in original dataset, only 81 rows in goal dataset (don't know how to fix yet)


#NAs
    #for(i in 1:ncol(sw.wranged)) {print(which(is.na(sw.wranged[,i])))}
    
    #for(i in 1:ncol(sw.wrangled.goal)) {print(which(is.na(sw.wrangled.goal[,i])))}
    
    
#1 Take out cols: eye_color, skin_color, birth_year, sex, films, vehicles, starships

# Specify the columns to delete using the negative sign with which
columns_to_delete <- which(names(sw.wranged) %in% c("eye_color", "skin_color", "birth_year", "sex", "films", "vehicles", "starships"))

# Delete the specified columns
sw.wranged_inprogress <- sw.wranged[, -columns_to_delete]

#2 Split "name" col to first and last name
  
sw.wranged_inprogress <- separate(sw.wranged_inprogress, name, into = c("first_name", "last_name"), sep = " ", fill = "right")
  
#3 Create "initial" col

sw.wranged_inprogress <- sw.wranged_inprogress %>%
  mutate(initials = paste0(substr(first_name, 1, 1), substr(last_name, 1, 1)))

# Move new col to before height column
sw.wranged_inprogress <- sw.wranged_inprogress %>% 
  relocate(initials, .before = height)

#4 Create height_in and height_cm from "height" col

# Rename height to height_cm
names(sw.wranged_inprogress)[names(sw.wranged_inprogress) == "height"] <- "height_cm"

# Create height_in
# Convert cm to in: Divide cm by 2.54, round to 4 digits
sw.wranged_inprogress <- sw.wranged_inprogress %>%
  mutate(height_in = round(height_cm / 2.54, 4))

#I'm getting a slightly dif number, but I'll leave it

# Move new col to before "height_cm" col
sw.wranged_inprogress <- sw.wranged_inprogress %>% 
  relocate(height_in, .before = height_cm)

#5 Change "hair_color" col to "hair" and use this col to create hair_brown col with T/F in the cells

#Rename "hair_color" col to "hair"
names(sw.wranged_inprogress)[names(sw.wranged_inprogress) == "hair_color"] <- "hair"

#Create new col called hair_brown

  # Specify the string to check for
  target_string <- "brown"

  # Create a new column "hair_brown" with TRUE or FALSE
  sw.wranged_inprogress <- sw.wranged_inprogress %>%
    mutate(hair_brown = str_detect(hair, target_string))

#6 Change gender column cells to be m and f

sw.wranged_inprogress$gender <- dplyr::recode(sw.wranged_inprogress$gender, 
                        "masculine" = "m",
                        "feminine" = "f",
                        .default = NA_character_  # Keep other values (including NAs) unchanged
)


#7 Capitalize cells in "species" col

sw.wranged_inprogress$species <- toupper(sw.wranged_inprogress$species)


#8 Match data types with goal dataframe

#For my reference (goal dataset data types):
#first_name: chr
#last_name: chr
#initials: chr
#height_in: num
#height_cm: num
#mass: num
#hair: factor with 12 levels
#gender: factor with 2 levels
#species: factor with 37 levels
#homeworld: factor with 48 levels
#brown_hair: logi (T/F)

#height_cm
sw.wranged_inprogress$height_cm <- as.numeric(sw.wranged_inprogress$height_cm)

#hair
sw.wranged_inprogress$hair <- as.factor(sw.wranged_inprogress$hair)
  levels (sw.wrangled.goal$hair)
  levels (sw.wranged_inprogress$hair)
#noticing that goal dataset doesn't have "unknown" level like old dataset + has "bald" level unlike the old dataset -- not sure what to do about it tho
  
#gender
sw.wranged_inprogress$gender <- as.factor(sw.wranged_inprogress$gender)
  levels (sw.wrangled.goal$gender)
  levels (sw.wranged_inprogress$gender)
  
#species
sw.wranged_inprogress$species <- as.factor(sw.wranged_inprogress$species)
  levels (sw.wrangled.goal$species)
  levels (sw.wranged_inprogress$species)

#homeworld
sw.wranged_inprogress$homeworld <- as.factor(sw.wranged_inprogress$homeworld)
  levels (sw.wrangled.goal$homeworld)
  levels (sw.wranged_inprogress$homeworld)

#9 Anything else??
  
#homeworld and species cols need to be switched
sw.wranged_inprogress <- sw.wranged_inprogress %>% 
  relocate(species, .before = homeworld)

## Check that your sw.wrangled df is identical to the goal df
# Use any returned information about mismatches to adjust your code as needed
all.equal(sw.wranged_inprogress, sw.wrangled.goal)

#no, not equal. 

#Using dataprep-demo.R

# preview of tbl with first few observations
head(sw.wranged_inprogress) 
head(sw.wrangled.goal) 

# get some basic count info for sanity check purposes
table(sw.wranged_inprogress$hair)
#auburn - 1
#auburn, grey - 1
#auburn, white - 1
#black - 13
#blond - 3
#blonde - 1
#brown - 18
#brown, grey - 1
#grey- 1
#none - 37
#unknown - 1
#white - 4

table(sw.wrangled.goal$hair)
#auburn - 1
#auburn, grey - 1
#auburn, white - 1
#bald - 5
#black - 12
#blond - 3
#blonde - 1
#brown - 15
#brown, grey - 1
#grey - 1
#none - 36
#unknown - 0
#white - 4

#Additional:
  #bald - 5

#Reduced:
  #unknown - 1
  
#Trying to fix the mismatch in hair column

#Find the 1 observation with "unknown" in sw.wranged_inprogress
  # Find rows where hair is "unknown"
  rows_with_unknown <- sw.wranged_inprogress[sw.wranged_inprogress$hair == "unknown", ]

  # Display the result
  print(rows_with_unknown)

#Now finding row in sw.wrangled.goal with first_name = Captain, last_name = Phasma 
  # Find rows where first_name is "Captain"
  rows_with_Captain <- sw.wrangled.goal[sw.wrangled.goal$first_name == "Captain", ]
  
  # Display the result
  print(rows_with_Captain)
  
  #Captain Phasma with the unknown observation in my is sw.wranged_inprogress gone - need to delete this row from dataframe
    # Delete rows where Category is "A"
  sw.wranged_inprogress <- sw.wranged_inprogress[sw.wranged_inprogress$hair != "unknown", ]
    
  # Check to make sure it's gone
    table(sw.wranged_inprogress$hair) #it's gone
    
    
#I don't think this is an efficient way to figure out which rows need to be deleted to look like the goal dataset - is there a better way to figure this out??
#There's stil 86 rows in my dataset, but 81 rows in the goal dataset    
#I'm not very sure what to do from here

all.equal(sw.wranged_inprogress, sw.wrangled.goal)

#I was just realized I didn't use one long pipe... hopefully that's okay for this assignment?

#Update: nAfter seeing Natalie's script

#filter out NAs in height columns
sw.wranged_inprogress <- filter(sw.wranged_inprogress, !is.na(height_cm))
sw.wranged_inprogress <- filter(sw.wranged_inprogress, !is.na(height_in))

#Don't have right number of rows -- not sure what went wrong

##HW 11 (Part 1)

#load data
sw.wrangled <- read.csv("data/sw-wrangled.csv")

#Create plots

#Plot 1

ggplot(sw.wrangled) +    
  geom_histogram(aes(x = height_cm), bins=15) +
  coord_cartesian(ylim = c(0,20)) +
  scale_y_continuous(breaks = seq(0,20, by = 5)) 

#not sure what to set the bin/binwidth to to make it match the pic


#Plot 2

ggplot(data = sw.wrangled, aes(x = fct_infreq(hair))) +
  geom_bar() +
  labs(x = "sorted_hair")


#Plot 3

ggplot(data = sw.wrangled, aes(x = height_in, y = mass)) +
  geom_point(shape=24, fill="black")  +
  coord_cartesian(ylim = c(0,160)) +
  scale_y_continuous(breaks = seq(0,160, by = 40)) 

#not sure how to get 100 not to show on the y axis

