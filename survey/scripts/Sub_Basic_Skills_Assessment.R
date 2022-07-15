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

# transformation_function <- function(row){
#   results <- case_when(row %in% c("Excellent","Good") ~ "Good Skills",
#                        row %in% c("Very Poor","Poor") ~ "Poor Skills")
#   
#   return(results)
# }
# 
# transformation_function(df$Prog_stats)
# df |> 
#   mutate(across(c("Basic_email", "Basic_spreadsheet",
#                   "Basic_searching","Basic_vedioConf",
#                   "Basic_word","Basic_presentation",
#                   "Spec_design","Spec_data",
#                   "Spec_networking","Spec_digitalMarketing",
#                   "Spec_socialMedia","Spec_projectMangement",
#                   "Spec_communication","Prog_webDev",
#                   "Prog_Desktop","Prog_mobileDev","Prog_DS",
#                   "Prog_DB","Prog_stats","Prog_versionMang"),transformation_function )) -> modified_df
# 
# colnames(df)

#group by here
df$Education_type
df$Employment_statue

df |> 
  group_by(Gender) |> 
  count() |> 
  pivot_wider(names_from = Gender,values_from = n)-> sub_group_count

df |>
  select(Gender,Basic_email,Basic_searching,Basic_vedioConf,Basic_word,
         Basic_spreadsheet,Basic_presentation) |> 
  pivot_longer(cols = contains("Basic"),
               names_to = "feature",values_to = "evaluation") |> 
  mutate(feature = str_sub(string = feature,start = 7)) |> 
  group_by(Gender,feature) |> 
  add_count(evaluation) |> 
  distinct(Gender, feature,evaluation,n) |> 
  arrange(Gender, feature,evaluation) |> 
  mutate(total = case_when(Gender %in% "Male"  ~ sub_group_count$Male,
                           Gender %in% "Female"  ~ sub_group_count$Female)) |> 
  mutate(perc = (n/total)*100,
         evaluation = factor(evaluation)) |> 
  mutate(evaluation = fct_relevel(evaluation,"Very Poor",
                                  "Poor","Average",
                                  "Good","Excellent"),
         feature = case_when(feature %in% "email" ~ "Email",
                             feature %in% "searching" ~ "Web Search",
                             feature %in% "vedioConf" ~ "Video Conference",
                             feature %in% "word" ~ "Word Processing",
                             feature %in% "spreadsheet" ~ "Spreadsheet",
                             feature %in% "presentation" ~ "Presentation")) |>
  mutate(score = case_when(evaluation %in% "Very Poor" ~ n * 1,
                           evaluation %in% "Poor" ~ n * 2,
                           evaluation %in% "Average" ~ n * 3,
                           evaluation %in% "Good" ~ n* 4,
                           evaluation %in% "Excellent" ~ n * 5)) |> 
  group_by(feature) |> 
  mutate(score = sum(score)) |> 
  arrange(desc(score),feature) |> 
  ungroup() |> 
  mutate(feature = fct_reorder(feature,score,max))-> df_bar

  
  
  
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
  axis.title.y =element_text(size=6),
  axis.title.x =element_text(size=6),
  
  axis.text.y=element_text(size=8),
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
  legend.position = "none"
  
) -> riyadh_theme

# Plot Parameters ---------------------------------------------------------
glue("SubGroup Basic Skills Assessment") |> 
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

col_vector <- c(ka_col$turquaz[[2]],ka_col$turquaz[[4]],
                ka_col$turquaz[[6]],ka_col$turquaz[[8]],ka_col$turquaz[[10]])

names(col_vector) <- levels(df_bar$evaluation)
unique(df_bar$feature)

plots <- list()
i <- as.character(unique(df_bar$feature)[[1]])

for(i in unique(df_bar$feature)){
  df_bar |> 
    filter(feature %in% i) |> 
    ggplot(aes(x=Gender,y= perc, fill = evaluation))+
    geom_chicklet(width = 0.5)+
    scale_fill_manual(values = col_vector)+
    ggtitle(label = i,
            subtitle = subtitle)+
    ylab("")+
    xlab("")+
    labs(caption = caption)+
    scale_y_continuous(breaks = pretty_breaks(8))+
    theme_bw()+
    riyadh_theme+
    geom_text(aes(x=Gender,
                  y= perc,
                  label = ifelse(perc < 6,"",glue("{round(perc,2)}%"))),
              size = 2.5,
              color= "#ffffff",
              position = position_stack(reverse = T,vjust = 0.5),
              fontface = "bold")+
    coord_flip() -> p1
  
  plots[[i]] <- p1
  
}

(plots$`Web Search` + plots$`Video Conference` +
    plots$Email )  / (plots$`Word Processing` + plots$Presentation +
                        plots$Spreadsheet)

ggsave(filename = glue("Gender_Sub_group_Basic_Skills_Assessment.png"),
       path = file.path(here("survey","results")),
       width=13,height= 6,units="in",dpi=300)

