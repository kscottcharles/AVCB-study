#Articulate hypotheses/research Qs/mock figures:
 
  #H1: Parents’ authoritarian values will positively predict children’s own authoritarian values and this relation will strengthen with age.    
    #Scatterplot with parent AV score on x axis, child AV score on y axis with child age in different colors (6-10 years) 

  #H2: Parents’ racial ideology will positively predict children’s own diversity ideology, and this relation will strengthen with age.   
    #Barplot with child classroom choice (binary) on x axis and parent COBRA score on y axis, plot age with different colored dots on top of barplot
    #Stacked barchart with child classroom choice (binary) on x axis with each bar broken down by parent classroom choice (binary)

  #H3: Higher authoritarian values will positively predict more colorblind attitudes.
    #For parents: Scatterplot with parent AV score on x axis, parent COBRA score on y axis
    #For kids: Barplot with child classroom choice (binary) on x axis and child AV score on y axis


#Secondary questions/data visualizations:
  #How do kids own classroom choice compare to their perceptions of parents classroom choice
  #How do kids own AV score compare to their perceptions of parents' AV score
  #How do parents classroom choice (binary and scale versions) relate to their COBRA score
  #How do parents beliefs about parent vs. school responsibility to teach about racism vs. DEI relate to diversity ideology
  #How do parents beliefs about parent vs. school responsibility to teach about racism vs. DEI relate to AV score


#Data wrangling:

  #For my main analyses, I don't need to keep the following columns so I would take them out: P1_Race.Eth,P2_Race.Eth, P1_pol,	P2_pol, P1_ed, P2_ed

  #Transform variables - I think everything should be a factor, except anything ending in _num which should be an integer (should I just take out these since they dummy code the other variables?)

  #Make sure variables where values come from a scale are leveled correctly: CB_Self_Rating_CB,	CB_Self_Rating_CC,CB_Teacher_Rating_CB, CB_Teacher_Rating_CC

  #Turn COBRAS variables back to Strongly disagree - Strongly agree? Not sure at what point they were turned into numbers

  #Turn Responsibility variables back to Strongly disagree - Strongly agree? Not sure at what point they were turned into numbers


#Examples below on how I might transform my data from wide to long format for certain research Qs/data visualizations:

                                                
                                                      #Child data:

#pivot cols Child_CB_Self_choice	and CB_Parents_choice (kids' choice of classroom for themselves and their parent)
#new col that will store variable names: Class_Choice_Type (either parent or self)
#new colthat will store the values corresponding to the measurements:Class_Choice (either CC or CB)


#pivot RE_Ind_Self_choice	and RE_Ind_Parents_choice (kids' choice of preferred trait between respect for elders and independence for themselves and their parent)
#new col that will store variable names: RE_Ind_Type (either parent or self)
#new col that will store the values corresponding to the measurements:RE_Ind_Choice (either Independent or Respect for Elders)


#pivot Obed_SR_Self_choice	and Obed_SR_Parents_choice 
#new col that will store variable names: Obed_SR_Type
#new col that will store the values corresponding to the measurements:Obed_SR_Choice 


#pivot GM_Cur_Self_choice	and GM_Cur_Parents_choice 
#new col that will store variable names: GM_Cur_Self_Type 
#new col that will store the values corresponding to the measurements:GM_Cur_Self_Choice 


#pivot WB_Cons_Self_choice	and WB_Cons_Parents_choice 
#new col that will store variable names: WB_Cons_Self_Type 
#new col that will store the values corresponding to the measurements:WB_Cons_Self_Choice 


                                              #Parent data:

#pivot COBRAS_1_num	COBRAS_2_num	COBRAS_3_num	COBRAS_4_num
#new col that will store variable names: COBRAS_QType 
#new col that will store the values corresponding to the measurements:COBRAS_response 


#pivot CB_Self_Rating_CB	CB_Self_Rating_CC
#new col that will store variable names: Class_self_rating
#new col that will store the values corresponding to the measurements:Class_self_rating_response


#pivot CB_Teacher_Rating_CB	CB_Teacher_Rating_CC
#new col that will store variable names: Class_teacher_rating
#new col that will store the values corresponding to the measurements:Class_teacher_rating_response


#pivot Responsibility_Parents_Racism_num,	Responsibility_Schools_Racism_num
#new col that will store variable names: Responsibility_racism_type
#new col that will store the values corresponding to the measurements:Responsibility_racism_response


#pivot Responsibility_Parents_DEI_num,	Responsibility_Schools_DEI_num
#new col that will store variable names: Responsibility_DEI_type
#new col that will store the values corresponding to the measurements:Responsibility_DEI_response


#pivot cols Child_CB_Self_choice	and CB_Parents_choice (kids' choice of classroom for themselves and their parent)
#new col that will store variable names: Class_Choice_Type (either parent or self)
#new col that will store the values corresponding to the measurements:Class_Choice (either CC or CB)


  #Some examples for myself using pivot_longer:

df_test_longer <- AVCB_wp_dyads_ID %>% pivot_longer (cols = c("Child_CB_Self_choice", "CB_Parents_choice"), 
                                                     names_to = "Class_Choice_Type", 
                                                     values_to = "Class_Choice")

df_test_longer2<- df_test_longer %>% pivot_longer (cols = c("RE_Ind_Self_choice", "RE_Ind_Parents_choice"), 
                                                     names_to = "RE_Ind_Type", 
                                                     values_to = "RE_Ind_Choice")

df_test_longer3 <- AVCB_wp_dyads_ID %>% 
  rename(
    child_class_choice = Child_CB_Self_choice,
    parent_class_choice = CB_Parents_choice
  ) %>%  
  pivot_longer (cols = c("child_class_choice", "parent_class_choice"), 
                names_to = "Type_choice", 
                values_to = "Choice")

