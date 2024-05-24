library(tidyverse)
library(dtplyr)
library(stringr)

headers = c("ID",
            "Age",
            "Correct [n=5]",
            "Correct [n=9]",
            "Correct [n=13]",
            "Reaction time [n=5]",
            "Reaction time [n=9]",
            "Reaction time [n=13]",
            "Accuracy [n=5]",
            "Accuracy [n=9]",
            "Accuracy [n=13]",
            "Total correct"
            )


df = read_tsv('./data/_final_mergedfile.csv')
df_long = df %>% select(id, age, pic_num, is_correct, mouse.time ) %>% 
  mutate(is_correct=as.logical(is_correct), mouse.time = as.double(gsub('\\[([0-9]+\\.[0-9]+).*', '\\1', mouse.time))) %>% 
  filter(!is.na(is_correct), !is.na(pic_num)) 

df_wide = df_long %>% 
  group_by(id, pic_num) %>% 
  summarise(correct=sum(is_correct), mean_time=mean(mouse.time), age=mean(age)) %>% 
  pivot_wider(names_from = pic_num, values_from = c(correct, mean_time)) %>% 
  mutate(accuracy5=(correct_5/5)*100 ,
         accuracy9=(correct_9/5)*100 ,
         accuracy13=(correct_13/5)*100 ,
         sum_correct=sum(across(starts_with("correct")))) 

names(df_wide) = headers

write_csv(df_wide, "visual_memory_pivot.csv")
write_csv(df_long, "visual_memory_pivot_long.csv")
