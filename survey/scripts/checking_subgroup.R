rm(list = ls())

library(here)
library(readxl)
library(dplyr)
library(tidyr)

df <- read_xlsx(here("survey","clean_data","ICT_clean.xlsx"))

colnames(df)


#No difference between  DB programming and gender 



df |> 
  filter(digit_related %in% c("Specialized & Programming Digital Skills",
                              "Programming Digital Skills")) |> 
  filter(!Prog_DB %in% "Average") |> 
  mutate(Prog_DB = case_when(Prog_DB %in% c("Very Poor","Poor") ~ "Poor",
                             Prog_DB %in% c("Good","Excellent") ~ "Good")) ->df_m



chisq.test(table(df_m$Gender,df_m$Prog_DB))


#No difference between gender and College_work_prep
df |> 
  filter(!College_work_prep %in% "Not Sure")  -> df_m

chisq.test(table(df_m$Gender,df_m$College_work_prep)) -> testt


testt$p.value
testt$observed
testt$expected


#No difference between gender and  College_work_major_prep
df$College_work_major_prep
df |> 
  filter(!College_work_major_prep %in% "Not Sure")  -> df_m

chisq.test(table(df_m$Gender,df_m$College_work_major_prep)) -> testt

testt$p.value
testt$observed
testt$expected


#No difference between education type and  college prepration for school
unique(df$Education_type)

df |> 
  filter(!College_work_major_prep %in% "Not Sure",
         !Education_type %in% "Unkown")  -> df_m


chisq.test(table(df_m$Education_type,
                 df_m$College_work_major_prep)) -> testt


testt$p.value
testt$observed
testt$expected



#There is difference between education type and  College_work_prep

df |> 
  filter(!College_work_prep %in% "Not Sure",
         !Education_type %in% "Unkown")  -> df_m


chisq.test(table(df_m$Education_type,
                 df_m$College_work_prep)) -> testt

df_m$College_work_prep
df_m |> 
  filter(College_work_prep %in% "Yes") -> yes_df

df_m |> 
  filter(College_work_prep %in% "No") -> no_df

t.test(no_df$Age,yes_df$Age)

df_m |> 
  mutate(College_work_prep  = case_when(College_work_prep %in% "Yes"~ 1,
                                        College_work_prep %in% "No"~ 0)) -> df_m

df$Employment_statue
summary(lm(formula = College_work_prep ~ Employment_statue ,
           data = df_m))

testt$p.value
testt$observed
testt$expected

20/67
43/274


