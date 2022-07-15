########################
# TRUST in the Collegue System
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
  #plot.title = element_markdown(size = 14,hjust =0.5,vjust = -0.7,color = ka_col$title["1"]),
  plot.title = element_markdown(size = 14,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                margin=margin(0,0,-30,0)),
  plot.subtitle = element_blank(),
  
  plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                  size = 10,hjust = 0,padding = unit(c(10, 0, 0, 0), "pt")),
  
  #####################                     AXIS                                  ################
  #axis.title.x = element_text(color = ka_col$title["1"],size = 12),
  axis.title.y =element_blank(),
  axis.title.x =element_blank(),
  
  axis.text.y=element_text(size=13,hjust = 0.5),
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

subtitle <- ""
glue(" <span style = 'color: {ka_col$turquaz[['10']]}'>Preparing Graduates To Work In The Private Sector</span>") -> title

caption <- ""
# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- glue("Number of Responders (Percentage (%) Out of Total Responders)")
x_title <- ""

#Plotting--------------------------------------------------------------
#plot 1
unique(df$College_work_prep)
df |>
  mutate(College_work_prep = case_when(College_work_prep %in% "Not Sure" ~"Not Sure",
                                       College_work_prep %in% "No" ~"Weak Belief",
                                       College_work_prep %in% "Yes" ~"Strong Belief")) |> 
  select(College_work_prep) |> 
  add_count(College_work_prep) |>
  mutate(obs_count = nrow(df),
         perc = (n/obs_count)*100,
         College_work_prep = factor(College_work_prep),
         College_work_prep = fct_reorder(College_work_prep, n,max)) |> 
  distinct(College_work_prep,.keep_all = T) -> df_bar


df_bar |> 
  ggplot(aes(x=College_work_prep,y= n))+
  geom_chicklet(width = 0.45,aes(fill = College_work_prep))+
  scale_fill_manual(values = c(ka_col$turquaz[[5]],
                               ka_col$turquaz[[1]],
                               ka_col$turquaz[[10]]))+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  riyadh_theme+
  coord_flip()+
  geom_text(aes(x=College_work_prep,
                y= n-30,
                label = glue("{n}({round(perc,2)}%)")),
            nudge_y = c(-10,-10,0),
            size = 4.5,
            color= c(rep("#ffffff",3)),
            fontface = "bold") -> p1

#plot 2

# Plot Parameters ---------------------------------------------------------
subtitle <- ""

title <- 
  glue("<span style = 'color: {ka_col$purple['7']}'>Preparing Graduates To Work Within The Studied Major</span>") 
  

caption <- ""
# caption <-glue("<br></br><span style = 'color: {ka_col$source['1']}'>Source:</span>
#                  {unique(data$platform)} Job Matching Platform
#                  <br></br><span style = 'color: {ka_col$source['1']}'>Note:</span>
#                  Data collection is from the 1st of January 2021 until the 1st of August")

y_title <- glue("Number of Responders (Percentage (%) Out of Total Responders)")
x_title <- ""

unique(df$College_work_major_prep)
df |>
  mutate(College_work_major_prep = case_when(College_work_major_prep %in% "Not Sure" ~"Not Sure",
                                             College_work_major_prep %in% "No" ~"Low Belief",
                                             College_work_major_prep %in% "Yes" ~"High Belief")) |> 
  select(College_work_major_prep) |> 
  add_count(College_work_major_prep) |>
  mutate(obs_count = nrow(df),
         perc = (n/obs_count)*100,
         College_work_major_prep = factor(College_work_major_prep),
         College_work_major_prep = fct_relevel(College_work_major_prep, "High Belief", "Not Sure","Low Belief")) |> 
  distinct(College_work_major_prep,.keep_all = T) -> df_bar

df_bar |> 
  ggplot(aes(x=College_work_major_prep,y= -n))+
  geom_chicklet(width = 0.45,aes(fill = College_work_major_prep))+
  scale_fill_manual(values = c(ka_col$purple[[5]],
                               ka_col$purple[[1]],
                               ka_col$purple[[10]]))+
  ggtitle(label = title,
          subtitle = subtitle)+
  ylab(y_title)+
  xlab(x_title)+
  labs(caption = caption)+
  scale_y_continuous(breaks = pretty_breaks(8))+
  theme_bw()+
  theme(  #####################                     TITLES, SUBTITLE, CAPTION               ################
          plot.title = element_markdown(size = 14,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                        margin=margin(0,0,-30,0)),
          plot.subtitle = element_blank(),
          plot.caption = element_markdown(color = ka_col$title["1"],face = "italic",
                                          size = 10,hjust = 0,padding = unit(c(10, 0, 0, 0), "pt")),
          
          #####################                     AXIS                                  ################
          axis.title =element_blank(),
          
          axis.text=element_blank(),
          axis.ticks= element_blank(),
          #####################                     Panel                                  ################
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border  = element_blank(),
          
          
          #####################                     LEGEND                                  ################
          legend.text=element_text(size=12),
          legend.title = element_blank(),
          legend.position = "none")+
  coord_flip()+
  geom_text(aes(x=College_work_major_prep,
                y= -n+40,
                label = glue("{n}({round(perc,2)}%)")),
            nudge_y = 0,
            size = 4.5,
            color= c(rep("#ffffff",3)),
            fontface = "bold") -> p2

p2
glue("The Belief in the Educational System") |> 
  str_to_title() -> title

p2 + p1 + plot_annotation(title = title,
                          caption = y_title,
                          theme = theme(
                            plot.title = element_markdown(size = 17,hjust =0.5,vjust = 0.2,color = ka_col$title["1"],
                                                          margin=margin(0,0,-20,0)),
                            plot.caption = element_markdown(color = ka_col$title["1"],size = 15,
                                                            hjust = 0.5,margin=margin(0,0,30,0)))
)


# ggsave(filename = glue("Belief_in_Educational_System.png"),
#        path = file.path(here("survey","results")),
#        width=13,height= 6,units="in",dpi=1000)

ggsave(filename = glue("Belief_in_Educational_System.pdf"),
       path = file.path(here("survey","results","magazine")),
       width=13,height= 6,units="in",dpi=1000)

