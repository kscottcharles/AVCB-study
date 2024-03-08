# Descriptives ----------------------------------------------------

# Correlation Matrix ------------------------------------------------------

#Selecting relevant columns
AVCB_wp_dyads_corr <- AVCB_wp_dyads |> 
  select(COBRAS_avg, Self_diff, Teacher_diff, Self_sum, Parent_sum, AV_sum)

#Creating AV scores

#Parent score
AVCB_wp_dyads_corr <- AVCB_wp_dyads_corr %>%
  mutate(AV_sum = AV_sum / 4)

names(AVCB_wp_dyads_corr)[names(AVCB_wp_dyads_corr) == "AV_sum"] <- "Parent_AVscore"


#Child-self score
AVCB_wp_dyads_corr <- AVCB_wp_dyads_corr %>%
  mutate(Self_sum = Self_sum / 4)

names(AVCB_wp_dyads_corr)[names(AVCB_wp_dyads_corr) == "Self_sum"] <- "Child_AVscore_Self"

#Child-parent perception score

AVCB_wp_dyads_corr <- AVCB_wp_dyads_corr %>%
  mutate(Parent_sum = Parent_sum / 4)

names(AVCB_wp_dyads_corr)[names(AVCB_wp_dyads_corr) == "Parent_sum"] <- "Child_AVscore_Perception"


# Specify the custom variable names + moving in right position
custom_names <- c("Parent COBRAS Score", "Parent Class Diff Score (Self)", "Parent Class Diff Score (Teacher)", "Child AV Score (Self)", "Child AV Score (Parent-perception)", "Parent AV Score")

colnames(AVCB_wp_dyads_corr) <- custom_names

AVCB_wp_dyads_corr <- AVCB_wp_dyads_corr %>% 
  relocate(`Parent AV Score`, .before = `Child AV Score (Self)`)

# Create the APA-style correlation table 

matr <- apa.cor.table(AVCB_wp_dyads_corr, filename = NA)

# Now, create the APA-style table
apa_table(matr$table.body, caption = matr$table.title, note = matr$table.note, font_size = "footnotesize", row.names = F, placement = "p")


#Creating the correlation matrix
#AVCB_wp_dyads_corr <- cor(AVCB_wp_dyads_corr, use = "complete.obs") # using a matrix-like object (e.g., df)
#AVCB_wp_dyads_corr


# Hypothesis Testing ------------------------------------------------------

#From Pre-reg:

######Prelims

#Parent Diversity ideology:
#Create difference scores: Teacher_dif and Self_dif (CB-CC; lower scores = more color-conscious)
#Look at relations between Teacher and Self classroom choice (binary) and Teacher_dif and Self_dif

#generalized linear model: 
#model <- glm(y ~ x1 + x2 + ..., family = family_type, data = mydata)


######Rel between parent measures

##Rel between diversity ideology measures 
#Look at relations between COBRAS and Teacher_dif and Self_dif + COBRAS and Teacher and Self classroom choice (binary)

#Logistic regression model:

#Self_dif
#Convert to binary variable
AVCB_DIscores <- AVCB_DIscores %>%
  mutate(Parent_Class_Self_choice_num = if_else(Parent_Class_Self_choice == "CC", 1, 0))

# Fit logistic regression model
# The key here is the `family=binomial` argument!!
log_pClassSelf_pCOBRAS <- glm(Parent_Class_Self_choice_num ~ COBRAS_avg, data = AVCB_DIscores, family = binomial)

# Summary of the model
summary(log_pClassSelf_pCOBRAS)

#Teacher_dif
#Convert to binary variable
AVCB_DIscores <- AVCB_DIscores %>%
  mutate(Class_Teacher_choice_num = if_else(Class_Teacher_choice == "CC", 1, 0))

# Fit logistic regression model
# The key here is the `family=binomial` argument!!
log_pClassTeacher_pCOBRAS <- glm(Parent_Class_Self_choice_num ~ COBRAS_avg, data = AVCB_DIscores, family = binomial)

# Summary of the model
summary(log_pClassTeacher_pCOBRAS)

#Linear regression model:

# Fit regression model
pSelfdiff_pCOBRAS_lm <- lm(Self_diff ~ COBRAS_avg, data = AVCB_DIscores)
summary(pSelfdiff_pCOBRAS_lm)

pTeacherdiff_pCOBRAS_lm <- lm(Teacher_diff ~ COBRAS_avg, data = AVCB_DIscores)
summary(pTeacherdiff_pCOBRAS_lm)

##Rel between parent diversity ideology measures and AV measure

#Parent AV + Teacher and Self classroom choice (binary)
#generalized linear mixed effects model:
#We will include response to the teacher measure as the outcome and as predictors: question perspective (Parent-actual, Teacher) and parents’ authoritarian values (and additionally examining the interaction between these factors). 


#pivot_longer - do this longer
#model <- glm(y ~ x1 + x2 + ..., family = family_type, data = mydata)

#Parent AV + COBRAS 

#check cor first
AVCB_Bothscores_no_missing <- na.omit(AVCB_Bothscores[, c("Parent_AVscore", "COBRAS_avg")])
cor.test(AVCB_Bothscores_no_missing$Parent_AVscore, AVCB_Bothscores_no_missing$COBRAS_avg)

#Linear regression model

pAV_pCOBRAS_lm <- lm(Parent_AVscore ~ COBRAS_avg, data = AVCB_Bothscores)
summary(pAV_pCOBRAS_lm)

######Rel between child measures

##Rel between diversity ideology and AV

#Parent-perception (binary) and AV
#Logistic regression model:
#choice on the Classroom measure as the outcome and authoritarian values as the predictor

#Convert to binary variable
AVCB_Bothscores <- AVCB_Bothscores %>%
  mutate(Child_Class_Parents_choice_num = if_else(Child_Class_Parents_choice == "CC", 1, 0))

# Fit logistic regression model
# The key here is the `family=binomial` argument!!
log_cClassParent_cAV <- glm(Child_Class_Parents_choice_num ~ Child_AVscore_Self, data = AVCB_Bothscores, family = binomial)

# Summary of the model
summary(log_cClassParent_cAV)


#Self classroom choice (binary) and AV
#Logistic regression model:
#choice on the Teacher measure as the outcome and authoritarian values as the predictor

#Convert to binary variable
AVCB_Bothscores <- AVCB_Bothscores %>%
  mutate(Child_Class_Self_choice_num = if_else(Child_Class_Self_choice == "CC", 1, 0))

# Fit logistic regression model
# The key here is the `family=binomial` argument!!
log_cClassSelf_cAV <- glm(Child_Class_Self_choice_num ~ Child_AVscore_Self, data = AVCB_Bothscores, family = binomial)

# Summary of the model
summary(log_cClassSelf_cAV)


#Including both in model - will need data in long format?
#Generalized linear mixed effects model with a random intercept for each participant
#Outcome: Response to classroom measure
#Predictors: question perspective (Self, Parent-perception), children’s own authoritarian values, their perception of parents’ authoritarian values (additionally examining relation between these variables).
#Side note: We predict that children’s own authoritarian values will relate to their own racial ideology (we do not have strong predictions about the relation between children’s perceptions of parents’ authoritarian values and their perceptions of parents’ racial ideology). 


######Rel between parent and child measures
#Parent AV and children's AV 
#generalized linear mixed effects model with a random intercept for each dyad
#Outcome: children’s authoritarian values as the outcome
#Predictors: question perspective (Self, Parent-perception) and parents’ authoritarian values (additionally examining the interaction between these factors).

#pivot_longer

AVCB_AVscores_longer <- AVCB_AVscores %>% pivot_longer (cols = c("Child_AVscore_Self", "Child_AVscore_Parent"), 
                                                        names_to = "Child_AVscore_Type", 
                                                        values_to = "Child_AVscore_Choice_For")

AVCB_AVscores_longer$Child_AVscore_Type <- factor(AVCB_AVscores_longer$Child_AVscore_Type)


#generalized linear model: 
#Need to use glmer(): designed for fitting generalized linear mixed-effects models, and it extends the functionality of glm() to handle random effects.

glmer_cAVpAV <- glmer(Child_AVscore_Choice_For ~ Child_AVscore_Type + Parent_AVscore + (1 | Family_ID), 
                      family = binomial, 
                      data = AVCB_AVscores_longer)


sum_glmer_cAVpAV <- summary(glmer_cAVpAV)
sum_glmer_cAVpAV 

#should I add interaction terms now? - ASK GRACE



#Parent diversity ideology and children's diversity ideologies - dont have a graph made
#generalized linear mixed effects model with a random intercept for each dyad
#Outcome: children’s response to the classroom measure
#Predictors: question perspective (Self, Parent-perception), parent CoBRAS, parents’ response to the classroom measure (separately for Teacher and Parent-actual), and additionally examining interactions between these factors). 

AVCB_DIscores_longer <- AVCB_DIscores %>% pivot_longer (cols = c("Child_Class_Self_choice", "Child_Class_Parents_choice"), 
                                                        names_to = "Child_Class_Type", 
                                                        values_to = "Child_Class_Choice_For")

AVCB_DIscores_longer$Child_Class_Type <- factor(AVCB_DIscores_longer$Child_Class_Type)
AVCB_DIscores_longer$Parent_Class_Self_choice <- factor(AVCB_DIscores_longer$Parent_Class_Self_choice)
AVCB_DIscores_longer$Class_Teacher_choice <- factor(AVCB_DIscores_longer$Class_Teacher_choice)

#take out nas: use this code below
AVCB_DIscores_longer_no_missing <- na.omit(AVCB_DIscores_longer[, c("Child_Class_Choice_For", "COBRAS_avg")])

glmer_cDIpDI <- glmer(Child_Class_Choice_For ~ COBRAS_avg + Parent_Class_Self_choice + Class_Teacher_choice + (1 | Family_ID), 
                      family = binomial, 
                      data = AVCB_DIscores_longer)

