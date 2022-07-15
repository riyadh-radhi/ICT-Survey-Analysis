########################
# BASIC SKILLS EVALUATION
#########################
rm(list = ls())

library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(patchwork)
library(ggtext)
library(ggchicklet)
library(glue)
library(forcats)
library(cowplot)
library(stringr)
library(table1)

df <- read_xlsx(here("survey","clean_data","ICT_clean.xlsx"))

colnames(df)

table1(~ Gender + Age +  Education  + Education_type + Employment_statue + City,
       data=df,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       transpose =  T)



table1(~ Age + Gender + Education + Education_type + Employment_statue | College_work_prep ,
       data=df,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       transpose =  T)
