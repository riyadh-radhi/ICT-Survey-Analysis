rm(list = ls())

library(here)
library(readxl)
library(dplyr)
library(tidyr)

data <- read_xlsx(here("survey","raw_data","ICT survey.xlsx"))

data |> 
  select(-c("Timestamp")) -> data

colnames(data) <- c("Gender","Age","City","Education","Major",
                    "Detailed_major","Education_type","Employment_statue",
                    "Reason_not_to_work","College_work_prep",
                    "College_work_major_prep","Basic_email",
                    "Basic_searching","Basic_vedioConf",
                    "Basic_word","Basic_spreadsheet",
                    "Basic_presentation","Basic_selfStudy",
                    "Basic_college","Basic_workplace","Basic_trainings",
                    "Spec_design","Spec_data","Spec_networking",
                    "Spec_digitalMarketing","Spec_socialMedia",
                    "Spec_projectMangement","Spec_communication",
                    "Spec_selfStudy","Spec_college","Spec_workplace",
                    "Spec_trainings","Prog_webDev","Prog_Desktop",
                    "Prog_mobileDev","Prog_DS","Prog_DB","Prog_stats",
                    "Prog_versionMang","Prog_selfStudy","Prog_college",
                    "Prog_workplace","Prog_trainings","digit_related")


colnames(data)

unique(data$digit_related)

data |> 
  select(Reason_not_to_work) |> 
  count(Reason_not_to_work) |> 
  arrange(desc(n))

data |> 
  mutate(
    Age = readr::parse_number(Age),
    digit_related = case_when(digit_related %in% "نعم مرتبط بمهارات رقمية متخصصة" ~ "Specialized Digital Skills",
                                digit_related %in% "نعم مرتبط بكلاهما" ~ "Specialized & Programming Digital Skills",
                                digit_related %in% "نعم مرتبط بالبرمجة" ~ "Programming Digital Skills",
                                digit_related %in% "كلا غير مرتبط بالمهارات الرقمية" ~ "Not Related"),
    
    Basic_selfStudy = case_when(Basic_selfStudy %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Basic_selfStudy %in% "فائدة جيدة" ~ "Good Benefit",
                                Basic_selfStudy %in% "فائدة متوسطة" ~ "Average Benefit",
                                Basic_selfStudy %in% "فائدة قليلة" ~ "Poor Benefit",
                                Basic_selfStudy %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                                Basic_selfStudy %in% "لا ينطبق" ~ "Not Applicable"),
    
    Basic_college = case_when(Basic_college %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Basic_college %in% "فائدة جيدة" ~ "Good Benefit",
                                Basic_college %in% "فائدة متوسطة" ~ "Average Benefit",
                                Basic_college %in% "فائدة قليلة" ~ "Poor Benefit",
                                Basic_college %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                                Basic_college %in% "لا ينطبق" ~ "Not Applicable"),
    
    Basic_workplace = case_when(Basic_workplace %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Basic_workplace %in% "فائدة جيدة" ~ "Good Benefit",
                                Basic_workplace %in% "فائدة متوسطة" ~ "Average Benefit",
                                Basic_workplace %in% "فائدة قليلة" ~ "Poor Benefit",
                                Basic_workplace %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                                Basic_workplace %in% "لا ينطبق" ~ "Not Applicable"),
    
    Basic_trainings = case_when(Basic_trainings %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Basic_trainings %in% "فائدة جيدة" ~ "Good Benefit",
                                Basic_trainings %in% "فائدة متوسطة" ~ "Average Benefit",
                                Basic_trainings %in% "فائدة قليلة" ~ "Poor Benefit",
                                Basic_trainings %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                                Basic_trainings %in% "لا ينطبق" ~ "Not Applicable"),
    
    Spec_selfStudy = case_when(Spec_selfStudy %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Spec_selfStudy %in% "فائدة جيدة" ~ "Good Benefit",
                                Spec_selfStudy %in% "فائدة متوسطة" ~ "Average Benefit",
                                Spec_selfStudy %in% "فائدة قليلة" ~ "Poor Benefit",
                                Spec_selfStudy %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                                Spec_selfStudy %in% "لا ينطبق" ~ "Not Applicable"),
    
    Spec_college = case_when(Spec_college %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Spec_college %in% "فائدة جيدة" ~ "Good Benefit",
                             Spec_college %in% "فائدة متوسطة" ~ "Average Benefit",
                             Spec_college %in% "فائدة قليلة" ~ "Poor Benefit",
                             Spec_college %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                             Spec_college %in% "لا ينطبق" ~ "Not Applicable"),
    
    
    Spec_workplace = case_when(Spec_workplace %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Spec_workplace %in% "فائدة جيدة" ~ "Good Benefit",
                                Spec_workplace %in% "فائدة متوسطة" ~ "Average Benefit",
                                Spec_workplace %in% "فائدة قليلة" ~ "Poor Benefit",
                                Spec_workplace %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                                Spec_workplace %in% "لا ينطبق" ~ "Not Applicable"),
    
    Spec_trainings = case_when(Spec_trainings %in% "فائدة ممتازة" ~ "Excellent Benefit",
                                Spec_trainings %in% "فائدة جيدة" ~ "Good Benefit",
                                Spec_trainings %in% "فائدة متوسطة" ~ "Average Benefit",
                                Spec_trainings %in% "فائدة قليلة" ~ "Poor Benefit",
                                Spec_trainings %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                                Spec_trainings %in% "لا ينطبق" ~ "Not Applicable"),
  
    Prog_selfStudy = case_when(Prog_selfStudy %in% "فائدة ممتازة" ~ "Excellent Benefit",
                               Prog_selfStudy %in% "فائدة جيدة" ~ "Good Benefit",
                               Prog_selfStudy %in% "فائدة متوسطة" ~ "Average Benefit",
                               Prog_selfStudy %in% "فائدة قليلة" ~ "Poor Benefit",
                               Prog_selfStudy %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                               Prog_selfStudy %in% "لا ينطبق" ~ "Not Applicable"),
    
    Prog_college = case_when(Prog_college %in% "فائدة ممتازة" ~ "Excellent Benefit",
                               Prog_college %in% "فائدة جيدة" ~ "Good Benefit",
                               Prog_college %in% "فائدة متوسطة" ~ "Average Benefit",
                               Prog_college %in% "فائدة قليلة" ~ "Poor Benefit",
                               Prog_college %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                               Prog_college %in% "لا ينطبق" ~ "Not Applicable"),
    
    Prog_workplace = case_when(Prog_workplace %in% "فائدة ممتازة" ~ "Excellent Benefit",
                               Prog_workplace %in% "فائدة جيدة" ~ "Good Benefit",
                               Prog_workplace %in% "فائدة متوسطة" ~ "Average Benefit",
                               Prog_workplace %in% "فائدة قليلة" ~ "Poor Benefit",
                               Prog_workplace %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                               Prog_workplace %in% "لا ينطبق" ~ "Not Applicable"),
    
    Prog_trainings = case_when(Prog_trainings %in% "فائدة ممتازة" ~ "Excellent Benefit",
                               Prog_trainings %in% "فائدة جيدة" ~ "Good Benefit",
                               Prog_trainings %in% "فائدة متوسطة" ~ "Average Benefit",
                               Prog_trainings %in% "فائدة قليلة" ~ "Poor Benefit",
                               Prog_trainings %in% "فائدة ضعيفة جدا او معدومة" ~ "Very Poor Benefit",
                               Prog_trainings %in% "لا ينطبق" ~ "Not Applicable"),
    
    
    Basic_email = case_when(Basic_email %in% "ممتاز" ~ "Excellent",
                            Basic_email %in% "متوسط" ~ "Average",
                            Basic_email %in% "جيد" ~ "Good",
                            Basic_email %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                            Basic_email %in% "ضعيف" ~ "Poor"),
    
    Basic_searching = case_when(Basic_searching %in% "ممتاز" ~ "Excellent",
                            Basic_searching %in% "متوسط" ~ "Average",
                            Basic_searching %in% "جيد" ~ "Good",
                            Basic_searching %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                            Basic_searching %in% "ضعيف" ~ "Poor"),

    Basic_vedioConf = case_when(Basic_vedioConf %in% "ممتاز" ~ "Excellent",
                                Basic_vedioConf %in% "متوسط" ~ "Average",
                                Basic_vedioConf %in% "جيد" ~ "Good",
                                Basic_vedioConf %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                Basic_vedioConf %in% "ضعيف" ~ "Poor"),
    
    Basic_word = case_when(Basic_word %in% "ممتاز" ~ "Excellent",
                            Basic_word %in% "متوسط" ~ "Average",
                            Basic_word %in% "جيد" ~ "Good",
                            Basic_word %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                            Basic_word %in% "ضعيف" ~ "Poor"),
    
    Basic_spreadsheet = case_when(Basic_spreadsheet %in% "ممتاز" ~ "Excellent",
                            Basic_spreadsheet %in% "متوسط" ~ "Average",
                            Basic_spreadsheet %in% "جيد" ~ "Good",
                            Basic_spreadsheet %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                            Basic_spreadsheet %in% "ضعيف" ~ "Poor"),
    
    Basic_presentation = case_when(Basic_presentation %in% "ممتاز" ~ "Excellent",
                            Basic_presentation %in% "متوسط" ~ "Average",
                            Basic_presentation %in% "جيد" ~ "Good",
                            Basic_presentation %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                            Basic_presentation %in% "ضعيف" ~ "Poor"),
    
    Spec_design = case_when(Spec_design %in% "ممتاز" ~ "Excellent",
                            Spec_design %in% "متوسط" ~ "Average",
                            Spec_design %in% "جيد" ~ "Good",
                            Spec_design %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                            Spec_design %in% "ضعيف" ~ "Poor"),
    
    Spec_data = case_when(Spec_data %in% "ممتاز" ~ "Excellent",
                          Spec_data %in% "متوسط" ~ "Average",
                          Spec_data %in% "جيد" ~ "Good",
                          Spec_data %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                          Spec_data %in% "ضعيف" ~ "Poor"),
    
    Spec_networking = case_when(Spec_networking %in% "ممتاز" ~ "Excellent",
                                Spec_networking %in% "متوسط" ~ "Average",
                                Spec_networking %in% "جيد" ~ "Good",
                                Spec_networking %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                Spec_networking %in% "ضعيف" ~ "Poor"),
    
    Spec_digitalMarketing = case_when(Spec_digitalMarketing %in% "ممتاز" ~ "Excellent",
                                      Spec_digitalMarketing %in% "متوسط" ~ "Average",
                                      Spec_digitalMarketing %in% "جيد" ~ "Good",
                                      Spec_digitalMarketing %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                      Spec_digitalMarketing %in% "ضعيف" ~ "Poor"),
    
    Spec_socialMedia = case_when(Spec_socialMedia %in% "ممتاز" ~ "Excellent",
                                 Spec_socialMedia %in% "متوسط" ~ "Average",
                                 Spec_socialMedia %in% "جيد" ~ "Good",
                                 Spec_socialMedia %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                 Spec_socialMedia %in% "ضعيف" ~ "Poor"),
    
    Spec_projectMangement = case_when(Spec_projectMangement %in% "ممتاز" ~ "Excellent",
                            Spec_projectMangement %in% "متوسط" ~ "Average",
                            Spec_projectMangement %in% "جيد" ~ "Good",
                            Spec_projectMangement %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                            Spec_projectMangement %in% "ضعيف" ~ "Poor"),
    
    Spec_communication = case_when(Spec_communication %in% "ممتاز" ~ "Excellent",
                                   Spec_communication %in% "متوسط" ~ "Average",
                                   Spec_communication %in% "جيد" ~ "Good",
                                   Spec_communication %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Spec_communication %in% "ضعيف" ~ "Poor"),
    
    Prog_webDev = case_when(Prog_webDev %in% "ممتاز" ~ "Excellent",
                                   Prog_webDev %in% "متوسط" ~ "Average",
                                   Prog_webDev %in% "جيد" ~ "Good",
                                   Prog_webDev %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Prog_webDev %in% "ضعيف" ~ "Poor"),
    
    Prog_Desktop = case_when(Prog_Desktop %in% "ممتاز" ~ "Excellent",
                                   Prog_Desktop %in% "متوسط" ~ "Average",
                                   Prog_Desktop %in% "جيد" ~ "Good",
                                   Prog_Desktop %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Prog_Desktop %in% "ضعيف" ~ "Poor"),
    
    Prog_mobileDev = case_when(Prog_mobileDev %in% "ممتاز" ~ "Excellent",
                                   Prog_mobileDev %in% "متوسط" ~ "Average",
                                   Prog_mobileDev %in% "جيد" ~ "Good",
                                   Prog_mobileDev %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Prog_mobileDev %in% "ضعيف" ~ "Poor"),
    
    Prog_DS = case_when(Prog_DS %in% "ممتاز" ~ "Excellent",
                                   Prog_DS %in% "متوسط" ~ "Average",
                                   Prog_DS %in% "جيد" ~ "Good",
                                   Prog_DS %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Prog_DS %in% "ضعيف" ~ "Poor"),
    
    Prog_DB = case_when(Prog_DB %in% "ممتاز" ~ "Excellent",
                                   Prog_DB %in% "متوسط" ~ "Average",
                                   Prog_DB %in% "جيد" ~ "Good",
                                   Prog_DB %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Prog_DB %in% "ضعيف" ~ "Poor"),
    
    Prog_stats = case_when(Prog_stats %in% "ممتاز" ~ "Excellent",
                                   Prog_stats %in% "متوسط" ~ "Average",
                                   Prog_stats %in% "جيد" ~ "Good",
                                   Prog_stats %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Prog_stats %in% "ضعيف" ~ "Poor"),
    
    Prog_versionMang = case_when(Prog_versionMang %in% "ممتاز" ~ "Excellent",
                                   Prog_versionMang %in% "متوسط" ~ "Average",
                                   Prog_versionMang %in% "جيد" ~ "Good",
                                   Prog_versionMang %in% "ضعيف جدا او معدوم" ~ "Very Poor",
                                   Prog_versionMang %in% "ضعيف" ~ "Poor"),
    
    
    
    College_work_major_prep = case_when(College_work_major_prep %in% "نعم" ~ "Yes",
                                        College_work_major_prep %in% "كلا" ~ "No",
                                        College_work_major_prep %in% "لست متاكداً" ~ "Not Sure"),
    
    College_work_prep = case_when(College_work_prep %in% "نعم" ~ "Yes",
                                  College_work_prep %in% "كلا" ~ "No",
                                  College_work_prep %in% "لست متاكداً" ~ "Not Sure"),
    
    Employment_statue = case_when(Employment_statue %in% "لا أعمل" ~ "Not Working",
                                  Employment_statue %in% "أعمل ضمن القطاع الخاص (موظف,مؤسس,عامل حر,الخ..)" ~ "Private Sector",
                                  Employment_statue %in% "أعمل ضمن القطاع العام" ~ "Public Sector",
                                  Employment_statue %in% "أعمل ضمن القطاع الخاص والعام" ~ "Public & Private Sector",
                                  TRUE ~ "Others"),
    
    Education_type = case_when(Education_type %in% "حكومي" ~ "Public",
                               Education_type %in% "خاص (أهلي)" ~ "Private",
                       TRUE ~ "Unkown"),
    
    Gender = case_when(Gender %in% "أنثى" ~ "Female",
                            Gender %in% "ذكر" ~ "Male",
                            TRUE ~ "Other"),
         
         City = case_when(City %in% "نينوى"~ "Ninewa",
                          City %in% "كركوك"~ "Kirkuk",
                          City %in% "ديالى"~ "Diyala",
                          City %in% "الأنبار"~ "Anbar",
                          City %in% "بغداد"~ "Baghdad",
                          City %in% "بابل"~ "Babil",
                          City %in% "كربلاء"~ "Karbala",
                          City %in% "واسط"~ "Wassit",
                          City %in% "صلاح الدين"~ "Salah Al-Din",
                          City %in% "النجف"~ "Najaf",
                          City %in% "الديوانية"~ "Diwaniyah",
                          City %in% "ميسان"~ "Maysan",
                          City %in% "البصرة"~ "Basrah",
                          City %in% "المثنى"~ "Muthana",
                          City %in% "أربيل"~ "Erbil",
                          City %in% "دهوك"~ "Dohuk",
                          City %in% "السليمانية"~ "Sulaymaniyah",
                          City %in% "ذي قار"~ "Dhi Qar",
                          TRUE ~ "Outside Iraq"),
         
         Education = case_when(Education %in% "طالب اعدادية" ~ "Highschool Student",
                               Education %in% "طالب بكالوريوس او مايعادله" ~ "Bachelor Student",
                               Education %in% "خريج اعدادية" ~ "Highschool Graduate",
                               Education %in% "خريج بكالوريوس او مايعادله" ~ "Bachelor Graduate",
                               Education %in% "خريج ماجستير او مايعادله" ~ "Master Graduate",
                               Education %in% "طالب ماجستير او مايعادله" ~ "Master Student",
                               Education %in% "خريج دكتواره او مايعادله" ~ "PHD Graduate",
                               Education %in% "طالب ابتدائية او متوسطة" ~ "Primary/Secondary School Student",
                               Education %in% "خريج ابتدائية او متوسطة" ~ "Primary/Secondary School Graduate",
                               Education %in% "طالب دكتواره او مايعادله" ~ "PHD Student")
         ) -> data

data |> 
  mutate(Major  = case_when(Major %in% "هندسة" ~ "Engineering",
                            Major %in% "علوم" ~ "Sciences",
                            Major %in% "ادارة و الاقتصاد" ~ "Administration and Economics",
                            Major %in% "طب" ~ "Medicine",
                            TRUE ~ "Others")) -> data


xlsx::write.xlsx(x = data,
                 file = here("survey","clean_data","ICT_clean.xlsx"))





