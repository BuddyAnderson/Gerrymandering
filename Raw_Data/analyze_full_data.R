##---------------------------------------------------------------
##                  Analyze Full Gerrymandering Data           --
##---------------------------------------------------------------
library(readxl)
library(MASS)
library(ggplot2)
library(viridis)
library(dplyr)
library(miceadds)
library(stringr)
library(gdata)
library(latex2exp)
library(tidyr)
library(ggpubr)
library(raster)
library("stats")
library(estimatr)
library(lmtest)
library(sandwich)
library(grid)
library(gridExtra)
library("matrixStats")
library(car)
library(plm)
library(stargazer)
library(magrittr)
library(texreg)
library(xtable)
library(tibble)
library(lme4)
library(nlme)
##---------------------------------------------------------------
##                  Remove unnecessary clutter                  --
##---------------------------------------------------------------
rm(list = ls()) # Take out the Environment "trash"
cat("\014")  # Clear console, making error checking easier
while(!is.null(dev.list())) dev.off() # Clear old plots
setwd("~/Desktop/Research/GerryMandering/An_Anderson_Deck/Raw_Data")

# read in all data files for sessions 1 through 8
for(i in 1:16){
  if(i > 9){
    x <- read_excel(paste("Cleaned_Raw_Session_", i, ".xlsx", sep = "")) %>% dplyr::select(
      Period, Subject, Player, partner, LType, Map_Selection = Map, EDG5:EW1, TE1:TE5, pEDG5:pEW1, Win1:Win5
    ) %>% mutate(Session = i)
    nam <- paste("df", i, sep = "_")
    assign(nam, x)
  }else{
    x <- read_excel(paste("Cleaned_Raw_Session_0", i, ".xlsx", sep = "")) %>% dplyr::select(
      Period, Subject, Player, partner, LType, Map_Selection = Map, EDG5:EW1, TE1:TE5, pEDG5:pEW1, Win1:Win5
    ) %>% mutate(Session = i)
    nam <- paste("df", i, sep = "_")
    assign(nam, x)
  }
}

# combine data vertically
df <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12, df_13, df_14, df_15, df_16)
# library(tidyr)
# long_df <- df %>% gather(District, Effort, EDG5:EW1)
df <- df %>% rename(Partner = partner,
                    EDG_5 = EDG5, EDG_4 = EDG4, EDG_3 = EDG3, EDG_2 = EDG2, EDG_1 = EDG1,
                    ELG_5 = ELG5, ELG_4 = ELG4, ELG_3 = ELG3, ELG_2 = ELG2, ELG_1 = ELG1,
                    EW_5  = EW5, EW_4  = EW4, EW_3  = EW3, EW_2  = EW2, EW_1  = EW1,
                    TE_5 = TE5, TE_4 = TE4, TE_3 = TE3, TE_2 = TE2, TE_1 = TE1,
                    pEDG_5 = pEDG5, pEDG_4 = pEDG4, pEDG_3 = pEDG3, pEDG_2 = pEDG2, pEDG_1 = pEDG1,
                    pELG_5 = pELG5, pELG_4 = pELG4, pELG_3 = pELG3, pELG_2 = pELG2, pELG_1 = pELG1,
                    pEW_5  = pEW5, pEW_4  = pEW4, pEW_3  = pEW3, pEW_2  = pEW2, pEW_1  = pEW1
)
long_df <- df %>% dplyr::select(Session, Period, Subject, Player, Partner, LType, EDG_5:EW_1, pEDG_5:pEW_1) %>% gather(District, Effort, EDG_5:pEW_1)

# Orderly data with district, Map, and effort amount for each subject each period in each session

# No need to use long_df_map because already renamed them manually above
df_clean <- long_df %>% separate(District, c("District", "Map")) %>% filter(!grepl("p", District))



##---------------------------------------------------------------
##                  Main Regression Table                      --
##---------------------------------------------------------------
stage_1_regression_data <- df %>% dplyr::select(Session, Period, Subject, Player, TE_1:TE_5) %>%
  filter(Period >= 15 & Period <= 24) %>%
  gather(Map, Effort, TE_1:TE_5)

stage_1_regression_data <- stage_1_regression_data %>% 
  mutate(subject.id = as.factor(as.numeric(Session)*8-(8-as.numeric(Subject))),
          Player_B = ifelse(Player== "B", 1, 0),
          Gerry_B = ifelse(Map == "TE_1", 1, 0),
          Symm_1_1 = ifelse(Map == "TE_2", 1, 0),
          Symm_1_3 = ifelse(Map == "TE_3", 1, 0),
          Symm_3_1 = ifelse(Map == "TE_4", 1, 0),
          Gerry_A = ifelse(Map == "TE_5", 1, 0),
          Adv = ifelse((Map == "TE_1" & Player == "B")|(Map == "TE_5" & Player == "A"), 1,0),
          Disadv = ifelse((Map == "TE_1" & Player == "A")|(Map == "TE_5" & Player == "B"), 1,0),
          Stage_2_indicator = ifelse((Period > 24 & Period < 28), 1, 0) # this is currently useless (all zeros)
         )

# First set of sessions
stage_1_first_sessions <- stage_1_regression_data %>% filter(as.numeric(Session) < 9)
map_impact_on_stage_1_first_sessions_no_learning <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, data = stage_1_first_sessions)
# Look at subject Fixed Effects and standard errors clustered at session level
map_impact_on_stage_1_first_sessions_no_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, data = stage_1_first_sessions)
# What about if there is learning occurring?
stage_1_first_sessions_with_learnng <- stage_1_first_sessions %>% filter(Period >=20)
map_impact_on_stage_1_first_sessions_with_learning <- lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, data = stage_1_first_sessions_with_learnng)
# Look at subject Fixed Effects and standard errors clustered at session level
map_impact_on_stage_1_first_sessions_with_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, data = stage_1_first_sessions_with_learnng)


# Second set of sessions
stage_1_second_sessions <- stage_1_regression_data %>% filter(as.numeric(Session) > 8)
map_impact_on_stage_1_second_sessions_no_learning <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, data = stage_1_second_sessions)
map_impact_on_stage_1_second_sessions_no_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, data = stage_1_second_sessions)
stage_1_second_sessions_with_learnng <- stage_1_second_sessions %>% filter(Period >=20)
map_impact_on_stage_1_second_sessions_with_learning <- lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, data = stage_1_second_sessions_with_learnng)
map_impact_on_stage_1_second_sessions_with_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, data = stage_1_second_sessions_with_learnng)


# Both sessions
stage_1_full_sessions <- stage_1_regression_data
map_impact_on_stage_1_full_sessions_no_learning <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, data = stage_1_full_sessions)
map_impact_on_stage_1_full_sessions_no_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, data = stage_1_full_sessions)
stage_1_full_sessions_with_learnng <- stage_1_full_sessions %>% filter(Period >=20)
map_impact_on_stage_1_full_sessions_with_learning <- lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, data = stage_1_full_sessions_with_learnng)
map_impact_on_stage_1_full_sessions_with_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, data = stage_1_full_sessions_with_learnng)


# Make regression table
stargazer(map_impact_on_stage_1_first_sessions_no_learning_FE,
          map_impact_on_stage_1_first_sessions_with_learning_FE,
          title = "Map Impact on Stage 1 Bidding: First 8 Sessions",
          column.labels = c("w/out learning", "w/ learning"),
          label = "Tab:stage_1_first_sessions_with_and_without_learning_FE_CSE",
          omit = "subject.id", single.row = T)
stargazer(map_impact_on_stage_1_second_sessions_no_learning_FE,
          map_impact_on_stage_1_second_sessions_with_learning_FE,
          title = "Map Impact on Stage 1 Bidding: Second 8 Sessions",
          column.labels = c("w/out learning", "w/ learning"),
          label = "Tab:stage_1_second_sessions_with_and_without_learning_FE_CSE",
          omit = "subject.id", single.row = T)
stargazer(map_impact_on_stage_1_full_sessions_no_learning_FE,
          map_impact_on_stage_1_full_sessions_with_learning_FE,
          title = "Map Impact on Stage 1 Bidding: Full 16 Sessions",
          column.labels = c("w/out learning", "w/ learning"),
          label = "Tab:stage_1_full_sessions_with_and_without_learning_FE_CSE",
          omit = "subject.id", single.row = T)


