########################
# Specialized SKILLS Knowledge Source
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

df <- read_xlsx(here("survey","clean_data","ICT_clean.xlsx"))
colnames(df)

df |> 
  filter(digit_related %in% c("Specialized & Programming Digital Skills",
                              "Specialized Digital Skills")) -> df

df |>
  select(Spec_selfStudy,Spec_college,
         Spec_workplace,Spec_trainings) |> 
  pivot_longer(cols = contains("Spec"),
               names_to = "feature",values_to = "evaluation") |> 
  mutate(feature = str_sub(string = feature,start = 6)) |> 
  group_by(feature) |> 
  add_count(evaluation) |> 
  mutate(total = nrow(df)) |> 
  distinct(feature,evaluation,.keep_all = T) |> 
  mutate(perc = (n/total)*100,
         evaluation = factor(evaluation)) |> 
  mutate(evaluation = fct_relevel(evaluation,"Not Applicable",
                                  "Very Poor Benefit","Poor Benefit",
                                  "Average Benefit","Good Benefit",
                                  "Excellent Benefit"),
         feature = case_when(feature %in% "selfStudy" ~ "Self-taught",
                             feature %in% "college" ~ "College",
                             feature %in% "workplace" ~ "Workplace",
                             feature %in% "trainings" ~ "Trainings")) |>
  mutate(score = case_when(evaluation %in% "Not Applicable" ~ n * 0,
                           evaluation %in% "Very Poor Benefit" ~ n * 1,
                           evaluation %in% "Poor Benefit" ~ n * 2,
                           evaluation %in% "Average Benefit" ~ n * 3,
                           evaluation %in% "Good Benefit" ~ n* 4,
                           evaluation %in% "Excellent Benefit" ~ n * 5)) |> 
  group_by(feature) |> 
  mutate(score = sum(score)) |> 
  arrange(desc(score),feature) |> 
  ungroup() |> 
  mutate(feature = fct_reorder(feature,score,max))-> df_bar

df_bar$feature

# KAPITA Colors -----------------------------------------------------------

ka_col <- list(
  green = c("normal"= "#accb46","1" = "#d6e3a3","2" = "#cdde91","3" = "#c4d87f","4" = "#bdd46b",
            "5" = "#b4cf5a","6" = "#9cb73f","7" = "#8ba237","8" = "#788e31","9" = "#687a2a","10" = "#576725"),
  turquaz =c("normal"= "#0098a0","1" = "#81cbcf","2" = "#66c2c6","3" = "#4bb7bc","4" = "#33adb3",
             "5" = "#18a2a9","6" = "#048990","7" = "#007a80","8" = "#006a71","9" = "#005c61","10" = "#004c50"),
  orange = c("normal"= "#f9b036","1" = "#fcd89b","2" = "#fcd086","3" = "#fbc871","4" = "#fac05d",
             "5" = "#fab84b","6" = "#e09e30","7" = "#c78e2a","8" = "#ae7b24","9" = "#956a20","10" = "#7d581b"),
  purple = c("normal"= "#ab3d76","1" = "#d69fbb","2" = "#cd8bad","3" = "#c4779f","4" = "#bd6491",
             "5" = "#b45085","6" = "#9b376a","7" = "#8a315e","8" = "#792b54","9" = "#672547","10" = "#561f3b"),
  title = c("1"= "#3d3d3d"),
  source = c("1" = "#194354")
)

# Plot Theme --------------------------------------------------------------

theme(
  #####################                     TITLES, SUBTITLE, CAPTION               ################
  plot.title = element_markdown(size = 14,hjust =0.5,vjust = -0.7,color = ka_col$title["1"]),
  #plot.title = element_blank(),
  plot.subtitle = element_markdown(size = 14,hjust =0.5,vjust = -0.7,color = ka_col$title["1"]),
  
  plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                  size = 10,hjust = 0,padding = unit(c(10, 0, 0, 0), "pt")),
  
  #####################                     AXIS                                  ################
  #axis.title.x = element_text(color = ka_col$title["1"],size = 12),
  axis.title.y =element_blank(),
  axis.title.x =element_blank(),
  
  axis.text.y=element_text(size=13),
  axis.text.x=element_blank(),
  axis.ticks= element_blank(),
  #####################                     Panel                                  ################
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border  = element_blank(),
  
  
  #####################                     LEGEND                                  ################
  legend.text=element_text(size=12),
  legend.title = element_blank(),
  legend.position = "top"
  
) -> riyadh_theme

# Plot Parameters ---------------------------------------------------------
glue("Specialized Skills Knowledge Source") |> 
  str_to_title() -> title

#subtitle <- glue(" <span style = 'color: {ka_col$turquaz[['10']]}'>Within The Private Sector In General</span>")
subtitle <- ""
caption <- ""
# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- glue("Percentage (%) Out of Total Responders)")
x_title <- ""

#Plotting--------------------------------------------------------------
#plot 1

df_bar |> head()

col_vector <- c(ka_col$title[[1]],ka_col$purple[[2]],ka_col$purple[[4]],
                ka_col$purple[[6]],ka_col$purple[[8]],ka_col$purple[[10]])

names(col_vector) <- levels(df_bar$evaluation)
df_bar$feature
df_bar |> 
  ggplot(aes(x=feature,y= perc, fill = evaluation))+
  geom_chicklet(width = 0.5)+
  scale_fill_manual(values = col_vector)+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  riyadh_theme+
  geom_text(aes(x=feature,
                y= perc,
                label = ifelse(perc < 6,"",glue("{round(perc,2)}%"))),
            size = 4.5,
            color= "#ffffff",
            position = position_stack(reverse = T,vjust = 0.5),
            fontface = "bold")+
  coord_flip()



ggsave(filename = glue("Specialized_skills_knowledge_source_CUTSAMPLE.png"),
       path = file.path(here("survey","results")),
       width=13,height= 6,units="in",dpi=300)

