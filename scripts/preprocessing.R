library(tidyverse)

df <- readxl::read_xlsx("data/test.xlsx") %>% as.data.frame()
str(df)

# add a column with cell line name
df <- df %>% mutate(Line = case_when(
  str_detect(Slice, pattern="nsclcmm1") ~ "NSCLCMM1 (AC)",
  str_detect(Slice, pattern="nsclcm1") ~ "NSCLCMM1 (AC)",
  str_detect(Slice, pattern="nsclcmm2") ~ "NSCLCMM2 (SqCC)",
  str_detect(Slice, pattern="nsclcm2") ~ "NSCLCMM2 (SqCC)",
  .default = NULL
))

# add a column with a medium variant
df <- df %>% mutate(Group = case_when(
  str_detect(Slice, pattern="_1_|_1-") ~ "CTRL1",
  str_detect(Slice, pattern="_2_|_2-") ~ "CTRL2",
  str_detect(Slice, pattern="_3_|_3-") ~ "CTRL3",
  str_detect(Slice, pattern="_4_|_4-") ~ "F1",
  str_detect(Slice, pattern="_5_|_5-") ~ "F1T",
  str_detect(Slice, pattern="_6_|_6-") ~ "F2",
  str_detect(Slice, pattern="_7_|_7-") ~ "F2T",
  str_detect(Slice, pattern="_8_|_8-") ~ "F3",
  str_detect(Slice, pattern="_9_|_9-") ~ "F3T",
  str_detect(Slice, pattern="_10_|_10-") ~ "F1 all FGFs",
  str_detect(Slice, pattern="_11_|_11-") ~ "F1T all FGFs",
  str_detect(Slice, pattern="_12_|_12-") ~ "F2 all FGFs",
  str_detect(Slice, pattern="_13_|_13-") ~ "F2T all FGFs",
  str_detect(Slice, pattern="_14_|_14-") ~ "F3 all FGFs",
  str_detect(Slice, pattern="_15_|_15-") ~ "F3T all FGFs",
  str_detect(Slice, pattern="_16_|_16-") ~ "F1 no FGFs",
  str_detect(Slice, pattern="_17_|_17-") ~ "F1T no FGFs",
  str_detect(Slice, pattern="_18_|_18-") ~ "F2 no FGFs",
  str_detect(Slice, pattern="_19_|_19-") ~ "F2T no FGFs",
  str_detect(Slice, pattern="_20_|_20-") ~ "F3 no FGFs",
  str_detect(Slice, pattern="_21_|_21-") ~ "F3T no FGFs",
  .default = "CTRL1"
))

df <- df %>% mutate(Formula = case_when(
  str_detect(Group, pattern="F1$|F1 ") ~ "Formula 1",
  str_detect(Group, pattern="F2$|F2 ") ~ "Formula 2",
  str_detect(Group, pattern="F3$|F3 ") ~ "Formula 3",
  str_detect(Group, pattern="^F1T") ~ "Formula 1 \nTumor plus",
  str_detect(Group, pattern="^F2T") ~ "Formula 2 \nTumor plus",
  str_detect(Group, pattern="^F3T") ~ "Formula 3 \nTumor plus",
  .default = NULL
))

df <- df %>% mutate(Supp = case_when(
  str_detect(Group, pattern = "CTRL") ~ NA,
  str_detect(Group, pattern = "all FGFs") ~ "All FGFs",
  str_detect(Group, pattern = "no FGFs") ~ "No FGFs",
  .default = "Original"
))

df$Formula <- factor(df$Formula, levels = c("Formula 1", "Formula 2", "Formula 3", 
                                            "Formula 1 \nTumor plus", "Formula 2 \nTumor plus", "Formula 3 \nTumor plus"))
df$Supp <- factor(df$Supp, levels = c("Original", "All FGFs", "No FGFs"))

# add a column with a day of measurement
df <- df %>% mutate(Day = case_when(
  str_detect(Slice, pattern="day1_|day1-") ~ 1,
  str_detect(Slice, pattern="day4") ~ 4,
  str_detect(Slice, pattern="day7") ~ 7,
  str_detect(Slice, pattern="day12") ~ 12,
  str_detect(Slice, pattern="day15") ~ 15,
  str_detect(Slice, pattern="day18") ~ 18,
  str_detect(Slice, pattern="day22") ~ 22,
  .default = NULL
))

# drop unnecessary columns
df <- select(df, !c(Count, `Average Size`, `%Area`, Angle))

str(df)

#sanity check
temp <- df %>% group_by(Line, Day, Formula, Supp) %>% summarize(n=n())
