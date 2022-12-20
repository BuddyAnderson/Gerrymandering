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
library(fixest)
library(modelsummary)

##---------------------------------------------------------------
##                  Remove unnecessary clutter                 --
##---------------------------------------------------------------
rm(list = ls()) # Take out the Environment "trash"
cat("\014")  # Clear console, making error checking easier
while(!is.null(dev.list())) dev.off() # Clear old plots
setwd("~/Desktop/Research/GerryMandering/An_Anderson_Deck/Raw_Data")
##---------------------------------------------------------------
##                  Load all data                              --
##---------------------------------------------------------------
for(i in 1:16){
  if(i > 9){
    x <- read_excel(paste("Cleaned_Raw_Session_", i, ".xlsx", sep = "")) %>% 
      dplyr::select(
                    Period, Subject, Player, partner, 
                    LType, Map_Selection = Map, EDG5:EW1, 
                    TE1:TE5, pEDG5:pEW1, Win1:Win5,
                    PEQ_7, TimeSubmitPEQ7OK, PEQ_8
    ) %>% mutate(Session = i)
    nam <- paste("df", i, sep = "_")
    assign(nam, x)
  }else{
    x <- read_excel(paste("Cleaned_Raw_Session_0", i, ".xlsx", sep = "")) %>% 
      dplyr::select(
                    Period, Subject, Player, partner, 
                    LType, Map_Selection = Map, EDG5:EW1, 
                    TE1:TE5, pEDG5:pEW1, Win1:Win5,
                    PEQ_7, TimeSubmitPEQ7OK, PEQ_8
    ) %>% mutate(Session = i)
    nam <- paste("df", i, sep = "_")
    assign(nam, x)
  }
}
##---------------------------------------------------------------
##                  Combine all data                           --
##---------------------------------------------------------------
df <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12, df_13, df_14, df_15, df_16)
df <- df %>% rename(Partner = partner,
                    EDG_5 = EDG5, EDG_4 = EDG4, EDG_3 = EDG3, EDG_2 = EDG2, EDG_1 = EDG1,
                    ELG_5 = ELG5, ELG_4 = ELG4, ELG_3 = ELG3, ELG_2 = ELG2, ELG_1 = ELG1,
                    EW_5  = EW5, EW_4  = EW4, EW_3  = EW3, EW_2  = EW2, EW_1  = EW1,
                    TE_5 = TE5, TE_4 = TE4, TE_3 = TE3, TE_2 = TE2, TE_1 = TE1,
                    pEDG_5 = pEDG5, pEDG_4 = pEDG4, pEDG_3 = pEDG3, pEDG_2 = pEDG2, pEDG_1 = pEDG1,
                    pELG_5 = pELG5, pELG_4 = pELG4, pELG_3 = pELG3, pELG_2 = pELG2, pELG_1 = pELG1,
                    pEW_5  = pEW5, pEW_4  = pEW4, pEW_3  = pEW3, pEW_2  = pEW2, pEW_1  = pEW1
                    )


## Pick Which Sessions ##########################################
##                                                             ##
df <- df %>% filter(                                           ##
                    #Session < 9,                               ## comment out this line if you want all Sessions
                    Period < 30)                               ##
peq_response <- df %>%                                         ##
  filter(as.numeric(Period) == 29) %>%                         ##
  dplyr::select(Session, Period, Subject,                      ##
                PEQ_7, TimeSubmitPEQ7OK, PEQ_8) %>%            ##
  mutate(subject.id =                                          ##
           as.numeric(Session)*8-(8-as.numeric(Subject)))      ##
# ###############################################################



##---------------------------------------------------------------
##                  Restructure data                           --
##---------------------------------------------------------------
long_df <- df %>% dplyr::select(Session, Period, Subject, Player, Partner, LType, EDG_5:EW_1, pEDG_5:pEW_1) %>% gather(District, Effort, EDG_5:pEW_1)
df_clean <- long_df %>% separate(District, c("District", "Map")) %>% filter(!grepl("p", District))

df_clean %<>%
  mutate(Session = as.numeric(Session),
         Effort = as.numeric(Effort),
         Subject = as.numeric(Subject),
         Period = as.numeric(Period),
         subject.id = Session*8-(8-Subject))

##---------------------------------------------------------------
##                  Table 2: District Stats                    --
##---------------------------------------------------------------
df_renamed_maps <- df_clean %>%
  mutate(Effort = as.numeric(Effort)) %>%
  filter(Period >= 15, Period <= 24) %>%
  mutate(District.compare = ifelse((District == "EDG" & Player == 'A')|
                                     (District == "ELG" & Player == 'B'), 
                                   "DG Player A to LG Player B",
                                   ifelse((District == "ELG" & Player == 'A')|
                                            (District == "EDG" & Player == 'B'), 
                                          "LG Player A to DG Player B", 
                                          "W for Both Players")),
         Advantage = ifelse((Player == "A" & Map == 5)|
                              (Player == "B" & Map == 1), "Adv",
                            ifelse((Player == "B" & Map == 5)|
                                     (Player == "A" & Map == 1),
                                   "Dis.adv", 
                                   "fair")))

df_renamed_maps <- df_renamed_maps %>%
  mutate(Adv.District.Names = ifelse((Map == 1 & District == "EDG" & Player == "B")|
                                       (Map == 5 & District == "ELG" & Player == "A"), 
                                     "Two Partisan For",
                                     ifelse((Map == 1 & District == "ELG" & Player == "B")|
                                              (Map == 5 & District == "EDG" & Player == "A"), 
                                            "Three Partisan Against",
                                            ifelse((Map == 1 & District == "EW" & Player == "B")|
                                                     (Map == 5 & District == "EW" & Player == "A"),
                                                   "One Partisan For", 
                                                   "Error"))),
         
         Disadv.District.Names = ifelse((Map == 1 & District == "EDG" & Player == "A")|
                                          (Map == 5 & District == "ELG" & Player == "B"), 
                                        "Two Partisan Against",
                                        ifelse((Map == 1 & District == "ELG" & Player == "A")|
                                                 (Map == 5 & District == "EDG" & Player == "B"), 
                                               "Three Partisan For",
                                               ifelse((Map == 1 & District == "EW" & Player == "A")|
                                                        (Map == 5 & District == "EW" & Player == "B"),
                                                      "One Partisan Against", 
                                                      "Error"))))

table_2_data <- df_renamed_maps %>%
  filter(Period >= 15, Period <= 24) %>%
  mutate(Fairness = ifelse((Player == "A" & Map == 5)|
                             (Player == "B" & Map == 1), 
                           "Adv", 
                           ifelse((Player == "B" & Map == 5)|
                                    (Player == "A" & Map == 1),
                                  "Dis.adv", 
                                  "fair")),
         Map.new = ifelse(Map == 1 |Map == 5, "gerry", Map),
         EffortWithNAs = ifelse(Effort == 0, NA, Effort),
         District.compare = ifelse((Map == 1 & Player == "B")|
                                     (Map == 5 & Player == "A"), 
                                   Adv.District.Names,
                                   ifelse((Map == 1 & Player == "A")|
                                            (Map == 5 & Player == "B"), 
                                          Disadv.District.Names, 
                                          District.compare))
  ) 

table_2 <- table_2_data %>%
  group_by(District.compare, Map.new, Fairness) %>%
  summarize(pct.bid.zero = 100*round(sum(is.na(EffortWithNAs))/n(), 2),
            avg.positive.bid = round(mean(EffortWithNAs, na.rm = T)),
            avg.bid          = round(mean(Effort))) %>%
  arrange(Fairness)
#table_2

##---------------------------------------------------------------
##                  Figure 3                                   --
##---------------------------------------------------------------
ts.each.map.each.dist <- df_clean %>% 
  dplyr::select(Session, Period, subject.id, Player, Map, District, Effort) %>% 
  mutate(District.compare = ifelse((District == "EDG" & Player == 'A')|(District == "ELG" & Player == 'B'), 
                                   "DG Player A to LG Player B",
                                   ifelse((District == "ELG" & Player == 'A')|(District == "EDG" & Player == 'B'), 
                                          "LG Player A to DG Player B", 
                                          "W for Both Players")),
         Advantage = ifelse((Player == "A" & Map == 5)|(Player == "B" & Map == 1), 
                            "Adv",
                            ifelse((Player == "B" & Map == 5)|(Player == "A" & Map == 1),
                                   "Dis.adv", 
                                   "fair")))
ts.each.map.each.dist <- ts.each.map.each.dist %>%
  mutate(Adv.District.Names = ifelse((Map == 1 & District == "EDG" & Player == "B")|
                                       (Map == 5 & District == "ELG" & Player == "A"), 
                                     "Two Partisan For",
                                     ifelse((Map == 1 & District == "ELG" & Player == "B")|
                                              (Map == 5 & District == "EDG" & Player == "A"), 
                                            "Three Partisan Against",
                                            ifelse((Map == 1 & District == "EW" & Player == "B")|
                                                     (Map == 5 & District == "EW" & Player == "A"),
                                                   "One Partisan For", 
                                                   "Error"))),
         Disadv.District.Names = ifelse((Map == 1 & District == "EDG" & Player == "A")|
                                          (Map == 5 & District == "ELG" & Player == "B"), 
                                        "Two Partisan Against",
                                        ifelse((Map == 1 & District == "ELG" & Player == "A")|
                                                 (Map == 5 & District == "EDG" & Player == "B"), 
                                               "Three Partisan For",
                                               ifelse((Map == 1 & District == "EW" & Player == "A")|
                                                        (Map == 5 & District == "EW" & Player == "B"),
                                                      "One Partisan Against", "Error"))))

ts.each.map.each.dist <- ts.each.map.each.dist %>%
  filter(Period >= 15, Period <= 24) %>%
  mutate(Fairness = ifelse((Player == "A" & Map == 5)|(Player == "B" & Map == 1), "Adv", 
                           ifelse((Player == "B" & Map == 5)|(Player == "A" & Map == 1),"Dis.adv", "fair")),
         Map.new = ifelse(Map == 1 |Map == 5, "gerry", Map),
         EffortWithNAs = ifelse(Effort == 0, NA, Effort),
         District.compare = ifelse((Map == 1 & Player == "B")|(Map == 5 & Player == "A"), Adv.District.Names,
                                   ifelse((Map == 1 & Player == "A")|(Map == 5 & Player == "B"), Disadv.District.Names, District.compare))
  )

ts.each.map.each.dist <- ts.each.map.each.dist %>%
  mutate(Map = ifelse((Map == 1 & Player == "B")|(Map == 5 & Player =="A"), "Gerry Advantaged",
                      ifelse((Map == 1 & Player == "A")|(Map == 5 & Player == "B"),"Gerry Disadvantaged",
                             ifelse(Map == 2, "Symm_1_1",
                                    ifelse(Map == 3, "Symm_1_3", "Symm_3_1")))),
         District = ifelse(District == "EDG", "Dark Gray",
                           ifelse(District == "ELG", "Light Gray", 
                                  ifelse(District == "EW","White", "Wrong"))))

for.plot.of.ts.each.map.and.district <- ts.each.map.each.dist %>%  
  filter(Period >= 15, Period <=24) %>% 
  group_by(Period, Map, District.compare) %>% 
  summarise(avg.Effort = mean(Effort))

ts.by.map.and.district.gerry.adv <- for.plot.of.ts.each.map.and.district %>%
  filter(Map == "Gerry Advantaged") %>%
  ggplot(aes(x=Period, y=avg.Effort, group = District.compare, color = District.compare)) +
  geom_line(aes(linetype=District.compare)) +
  scale_linetype_manual(values=c("dotted", "solid","solid")) +
  scale_color_manual(values=c('#333333', '#000000','#999999'))+
  xlim(1,10) + ylim(0,60) + ylab("Expenditure") +
  scale_x_discrete(breaks=c(1:10)) +
  theme_bw() +  #drop a mostly white theme on for contrast
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1), legend.title = element_blank(), legend.position="none", text = element_text(size = 12))

ts.by.map.and.district.gerry.disadv <- for.plot.of.ts.each.map.and.district %>%
  filter(Map == "Gerry Disadvantaged") %>%
  ggplot(aes(x=Period, y=avg.Effort, group = District.compare, color = District.compare)) +
  geom_line(aes(linetype=District.compare)) +
  scale_linetype_manual(name = "District",labels = c("White", "Dark Gray", "Light Gray"), values=c("dotted", "solid","solid")) +
  scale_color_manual(guide = F,labels = c("White", "Dark Gray", "Light Gray"), values=c('#333333', '#999999','#000000'))+
  xlim(1,10) + ylim(0,60) + ylab("Expenditure") +
  scale_x_discrete(breaks=c(1:10)) +
  theme_bw() +  #drop a mostly white theme on for contrast
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1), legend.position="right", text = element_text(size = 12)) 

ts.by.map.and.district.sym11 <- for.plot.of.ts.each.map.and.district %>%
  filter(Map == "Symm_1_1") %>%
  ggplot(aes(x=Period, y=avg.Effort, group = District.compare, color = District.compare)) +
  geom_line(aes(linetype=District.compare)) +
  scale_linetype_manual(values=c("solid", "solid","dotted")) +
  scale_color_manual(values=c('#000000', '#333333','#999999'))+
  xlim(1,10) + ylim(0,60) + ylab("Expenditure") +
  scale_x_discrete(breaks=c(1:10)) +
  theme_bw() +  #drop a mostly white theme on for contrast
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1), legend.title = element_blank(), legend.position="none", text = element_text(size = 12))

ts.by.map.and.district.sym13 <- for.plot.of.ts.each.map.and.district %>%
  filter(Map == "Symm_1_3") %>%
  ggplot(aes(x=Period, y=avg.Effort, group = District.compare, color = District.compare)) +
  geom_line(aes(linetype=District.compare)) +
  scale_linetype_manual(values=c("solid", "solid","dotted")) +
  scale_color_manual(values=c('#000000', '#333333','#999999'))+
  xlim(1,10) + ylim(0,60) + ylab("Expenditure") +
  scale_x_discrete(breaks=c(1:10)) +
  theme_bw() +  #drop a mostly white theme on for contrast
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1), legend.title = element_blank(), legend.position="none", text = element_text(size = 12))

ts.by.map.and.district.sym13.alternative <- for.plot.of.ts.each.map.and.district %>%
  filter(Map == "Symm_1_3") %>%
  ggplot(aes(x=Period, y=avg.Effort, group = District.compare, color = District.compare)) +
  geom_line(aes(linetype=District.compare)) +
  scale_linetype_manual(values=c("solid", "solid","dotted")) +
  scale_color_manual(values=c('#000000', '#333333','#999999'))+
  xlim(1,10) + ylim(0,60) + ylab("Expenditure") +
  scale_x_discrete(breaks=c(1:10)) +
  theme_bw() +  #drop a mostly white theme on for contrast
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1), legend.title = element_blank(), legend.position="none", text = element_text(size = 12))

ts.by.map.and.district.sym31 <- for.plot.of.ts.each.map.and.district %>%
  filter(Map == "Symm_3_1") %>%
  ggplot(aes(x=Period, y=avg.Effort, group = District.compare, color = District.compare)) +
  geom_line(aes(linetype=District.compare)) +
  scale_linetype_manual(values=c("solid", "solid","dotted")) +
  scale_color_manual(values=c('#000000', '#333333','#999999'))+
  xlim(1,10) + ylim(0,60) + ylab("Expenditure") +
  scale_x_discrete(breaks=c(1:10)) +
  theme_bw() +  #drop a mostly white theme on for contrast
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1),legend.title = element_blank(), legend.position="none", text = element_text(size = 12))

# saved with width 300 height 175
ts.by.map.and.district.gerry.adv
ts.by.map.and.district.gerry.disadv # only one with 300 and 210
ts.by.map.and.district.sym11
ts.by.map.and.district.sym13
ts.by.map.and.district.sym13.alternative
ts.by.map.and.district.sym31

##----------------------------------------------------------------------------------------------
##                  Table 3: Comparison of Total Expenditure Across Maps                      --
##----------------------------------------------------------------------------------------------
stage_1_regression_data <- df %>% 
  dplyr::select(Session, Period, Subject, Player, TE_1:TE_5) %>%
  filter(Period >= 15 & Period <= 24) %>%
  gather(Map, Effort, TE_1:TE_5) %>%
  mutate(Session = as.numeric(Session),
         Period = as.numeric(Period),
         Subject = as.numeric(Subject),
         Effort = as.numeric(Effort))

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

# Run regressions
map_impact_stage_1 <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, 
                        data = stage_1_regression_data)
map_impact_stage_1_w_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, 
                                         data = stage_1_regression_data)
map_impact_stage_1_w_learning <- lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1, 
                                    data = stage_1_regression_data %>% filter(Period >= 20))
map_impact_stage_1_w_learning_and_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id, 
                                      data = stage_1_regression_data %>% 
                                        filter(Period >= 20))

# Make Table 3
stargazer(map_impact_stage_1_w_FE,
          map_impact_stage_1_w_learning_and_FE,
          title = "Map Impact on Stage 1 Bidding",
          column.labels = c("w/out learning", "w/ learning"),
          label = "Tab:stage_1_first_sessions_with_and_without_learning_FE_CSE",
          omit = "subject.id", single.row = T)

# Add the following clustered standard errors
map_impact_stage_1_w_FE_clustered <- lm.cluster(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id,
                                                cluster = "Session", 
                                                data = stage_1_regression_data)
summary(map_impact_stage_1_w_FE_clustered)
map_impact_stage_1_w_learning_and_FE_clustered <- lm.cluster(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + subject.id,
                                                cluster = "Session", 
                                                data = stage_1_regression_data %>%
                                                  filter(Period >= 20))
summary(map_impact_stage_1_w_learning_and_FE_clustered)

# Verified clustered SE
feols_m1 <- feols(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 | subject.id,
                  cluster = ~Session,
                  data = stage_1_regression_data)
#modelsummary(feols_m1)

# Hypothesis testing coefficients with NO LEARNING
linearHypothesis(map_impact_stage_1_w_FE_clustered, c("Adv = Disadv"))
#linearHypothesis(map_impact_stage_1_w_FE_clustered, c("Symm_1_3 = 0"))
linearHypothesis(map_impact_stage_1_w_FE_clustered, c("Symm_1_3 = 10"))
#linearHypothesis(map_impact_stage_1_w_FE_clustered, c("Symm_3_1 = 0"))
linearHypothesis(map_impact_stage_1_w_FE_clustered, c("Symm_3_1 = 10"))
linearHypothesis(map_impact_stage_1_w_FE_clustered, c("Symm_1_3 = Symm_3_1"))

# Hypothesis testing coefficients WITH LEARNING
linearHypothesis(map_impact_stage_1_w_learning_and_FE_clustered, c("Adv = Disadv"))
#linearHypothesis(map_impact_stage_1_w_learning_and_FE_clustered, c("Symm_1_3 = 0"))
linearHypothesis(map_impact_stage_1_w_learning_and_FE_clustered, c("Symm_1_3 = 10"))
#linearHypothesis(map_impact_stage_1_w_learning_and_FE_clustered, c("Symm_3_1 = 0"))
linearHypothesis(map_impact_stage_1_w_learning_and_FE_clustered, c("Symm_3_1 = 10"))
linearHypothesis(map_impact_stage_1_w_learning_and_FE_clustered, c("Symm_1_3 = Symm_3_1"))

##----------------------------------------------------------------------------------------------
##                  Figure 4: Scatter-Plot of Relative Expenditures in Sym3,1                 --
##----------------------------------------------------------------------------------------------
map_four_bidding <- df %>% dplyr::select(Session, Period, Subject, Player, EDG_4, ELG_4, EW_4)
map_four_bidding %<>%
  mutate(EDG_4 = as.numeric(EDG_4),
         ELG_4 = as.numeric(ELG_4),
         EW_4 = as.numeric(EW_4)) %>%
  mutate(two.bids = ifelse((EDG_4 > 0 & ELG_4 > 0 & EW_4 == 0)|
                             (EDG_4 > 0 & ELG_4 == 0 & EW_4 > 0)|
                             (EDG_4 == 0 & ELG_4 > 0 & EW_4 > 0),1,0),
         one.bids = ifelse((EDG_4 > 0 & ELG_4 == 0 & EW_4 == 0)|
                             (EDG_4 == 0 & ELG_4 > 0 & EW_4 == 0)|
                             (EDG_4 == 0 & ELG_4 == 0 & EW_4 > 0),1,0),
         all.three.bids = ifelse((EDG_4 > 0 & ELG_4 > 0 & EW_4 > 0),1,0),
         all.zeros.bids = ifelse((EDG_4 == 0 & ELG_4 == 0 & EW_4 == 0),1,0)
         ) %>%
  mutate(districts.bid = ifelse(all.three.bids == 1, "Three", 
                                ifelse(two.bids == 1, "Two", 
                                       ifelse(one.bids == 1, "One", "Zero"))),
         max_bid = pmax(EDG_4, ELG_4, EW_4),
         min_bid = pmin(EDG_4, ELG_4, EW_4),
         )
map_four_bidding$median_bid = apply(map_four_bidding[,5:7], 1, median)

map_four_bidding %<>%
  mutate(Spread = ifelse(all.three.bids == 1, max_bid - min_bid, 
                         ifelse(two.bids == 1, max_bid - median_bid, 
                                ifelse(one.bids == 1, max_bid - min_bid, -1))))

map_four_bidding$districts.bid <- factor(map_four_bidding$districts.bid,
                                         levels = c("Zero", "One", "Two", "Three"))

scatter_spread_df <- map_four_bidding %>%
  filter(Period <= 24) %>%
  mutate(subject.id = as.numeric(Session)*8-(8-as.numeric(Subject)),
         total.bid  = (EDG_4 + ELG_4 + EW_4),
         max.over.total = ifelse(total.bid > 0, max_bid/total.bid, 0),
         med.over.total = ifelse(total.bid > 0, median_bid/total.bid, 0)
  )

# Save width = 800 and height = 700
scatter_spread_df %>%
  ggplot(aes(x = med.over.total, y = max.over.total)) +
  geom_point() +
  labs(x = "Median District Expenditure Relative to Total Expenditure", 
                     y = " Maximum District Expenditure Relative to Total Expenditure") +
  geom_jitter(width = 0.025, height = 0.025) +
  xlim(-0.1,0.75) +
  ylim(-0.1,1) +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed() +
  theme_classic()


##----------------------------------------------------------------------------------------------
##               Figure 5: Expenditure Distribution in Comp. District of Gerry Map            --
##----------------------------------------------------------------------------------------------
dissag.df.overlay <- df_clean %>% 
  filter((Map == 1 | Map == 5) & District == "EW") %>% 
  dplyr::select(Period, subject.id, Player, Map, District, Effort) %>% 
  mutate(Advantage = ifelse((Player == "A" & Map == 5)|(Player == "B" & Map == 1), "Adv", "Dis.adv"))

adv.vs.disadv.cdf <- dissag.df.overlay %>%
  filter(Period <= 24) %>%
  ggplot(aes(x=as.numeric(Effort))) +
  scale_color_manual(values = c("black","green"), labels = c("Advantaged", "Disadvantaged")) +
  stat_ecdf(aes(colour=Advantage)) +
  geom_vline(xintercept = 20, linetype = "dashed") + xlim(0,80)+
  guides(size = F)

# Save width = 900 and height = 500
adv.vs.disadv.cdf + 
  theme(legend.title = element_blank()) +
  xlab("Expenditure in White District") +
  ylab("Cumulative Percentage") +
  annotate('text', x=10, y=0.75, label = "Theoretical Prediction") +
  theme_classic()

# Adv vs Dis.adv KS test
ADV.All <- subset(dissag.df.overlay, Advantage == "Adv")[,"Effort"]
Dis.ADV.All <- subset(dissag.df.overlay, Advantage == "Dis.adv")[,"Effort"]
ks.test(as.numeric(unlist(ADV.All)),as.numeric(unlist(Dis.ADV.All)))

##-------------------------------------------------------------
##               Figure 6: Map Preference Stage 2            --
##-------------------------------------------------------------
stage_2 <- df %>% subset(Period > 24 & Period < 28) %>% 
  dplyr::select(Session, Subject, Period, Player, Map_Selection) %>%
  mutate(Session = as.numeric(Session),
         Subject = as.numeric(Subject),
         subject.id = Session*8-(8-Subject))

mode_df <- stage_2 %>% group_by(subject.id, Player) %>% 
  summarise(mode.map = ifelse(length(unique(Map_Selection)) == 3, 
                              Map_Selection[3],modal(Map_Selection, 
                                                     ties = 'random')),
    selection.tie = ifelse(length(unique(Map_Selection)) == 3,1,0))

mode_df.v1 <- mode_df %>%
  mutate(renamed.mode.map = ifelse((mode.map == 1 & Player == "B")|(mode.map==5 & Player == "A"), "Advantaged",
                                   ifelse(mode.map==2, "Symm_1_1",
                                          ifelse(mode.map==3, "Symm_1_3",
                                                 ifelse(mode.map==4,"Symm_3_1", "Disadvantaged")))))

combined.bar <- ggplot(mode_df.v1, aes(x=renamed.mode.map)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),width = 0.5, alpha = 0.5, position="identity", fill = "#666666") +
  labs(
    x = "",
    y = ""
  ) + 
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c('Symm_1_1'= parse(text = TeX('$Sym_{1,1}$')),
                            'Symm_1_3'= parse(text = TeX('$Sym_{1,3}$')),
                            'Symm_3_1'= parse(text = TeX('$Sym_{3,1}$')),
                            'Advantaged'= parse(text = TeX('$Advantaged$')),
                            'Disadvantaged'= parse(text = TeX('$Disadvantaged$'))))

# Save width = 1100 and height = 700
combined.bar + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
                     axis.text.y = element_text(size = 20))

##-------------------------------------------------------------
##              Figure 7: Political Leaning and Gerry        --
##-------------------------------------------------------------
# Join PEQ to gerry table
mode_df$gerry <- ifelse((mode_df$Player == 'A' & mode_df$mode.map == 5)|
                   (mode_df$Player == 'B' & mode_df$mode.map == 1),1,0)

gerry_and_politics <- right_join(mode_df, peq_response, copied = F) %>%
  dplyr::select(Session, subject.id, PEQ_7, PEQ_8, gerry) %>%
  mutate(support_gerry = ifelse(PEQ_8 == 1, "Yes", "No"),
         gerry.character = ifelse(gerry == 1,"Did Gerrymander","Did Not Gerrymander"))

#gerry_and_politics$gerry.character[gerry_and_politics$gerry.character=="Did not Gerrymander"] <- "Did Not Gerrymander"

# Save width = 800 and height = 500 
ggplot(transform(
  gerry_and_politics, gerry.character = factor(gerry.character, 
                                             levels=c("Did Gerrymander", 
                                                      "Did Not Gerrymander"))), 
  aes(x=as.numeric(PEQ_7), fill = as.character(support_gerry))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, alpha = 1, position="identity") +
  facet_wrap(~gerry.character) + 
  xlim(1,9) + 
  ylim(0,12) +
  scale_x_continuous(breaks=c(1:9)) +
  labs(x = "Left to Right Political Identification", y = "") +
  labs(fill = "Do you support gerrymandering?") + 
  scale_y_continuous(labels=scales::percent, n.breaks = 15) +
  scale_fill_manual(values = c("No"="gray","Yes"="black")) +
  theme_classic() +
  theme(legend.position = "top")

##-------------------------------------------------------------
##               Figure 8: Map Preference Stage 3            --
##-------------------------------------------------------------
last_period <- subset(df, Period=="28")

last_period$Map_Selection <- as.character(last_period$Map_Selection)
last_period$Bug <- ifelse(last_period$Player == last_period$LType, "No", "Yes")

last_period.v1 <- last_period %>%
  mutate(renamed.map.selection = ifelse(Map_Selection==1, "Gerry_B",
                                        ifelse(Map_Selection==2, "Symm_1_1",
                                               ifelse(Map_Selection==3, "Symm_1_3",
                                                      ifelse(Map_Selection==4, "Symm_3_1",
                                                             ifelse(Map_Selection == 5, "Gerry_A", "No Selection"))))))

stage3_map <- last_period.v1 %>% filter(renamed.map.selection != "No Selection") %>%
  ggplot(aes(x=renamed.map.selection)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),width = 0.5, alpha = 0.5, position="identity", fill = "#666666") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) +
  xlab("") + ylab("") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c('Symm_1_1'= parse(text = TeX('$Sym_{1,1}$')),
                            'Symm_1_3'= parse(text = TeX('$Sym_{1,3}$')),
                            'Symm_3_1'= parse(text = TeX('$Sym_{3,1}$')),
                            'Gerry_A'= parse(text = TeX('$Gerry_A$')),
                            'Gerry_B'= parse(text = TeX('$Gerry_B$'))))

# Save width = 800 and height = 500
stage3_map + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
                   axis.text.y = element_text(size = 20))

##-------------------------------------------------------------
##            Tables 4, 5, and 6: Effect of Player B         --
##-------------------------------------------------------------
df_all_cdf <- df_clean %>% dplyr::select(Session, Period, subject.id, Player, District, Map, Effort) %>%
  filter(!grepl("p", District))

df_all_cdf <- df_all_cdf %>%
  filter(Period >= 15, Period <= 24) %>%
  mutate(District.compare = ifelse((District == "EDG" & Player == 'A')|(District == "ELG" & Player == 'B'), "DG Player A to LG Player B",
                                   ifelse((District == "ELG" & Player == 'A')|(District == "EDG" & Player == 'B'), "LG Player A to DG Player B","W for Both Players")),
         Advantage = ifelse((Player == "A" & Map == 5)|(Player == "B" & Map == 1), "Adv", 
                            ifelse((Player == "B" & Map == 5)|(Player == "A" & Map == 1),"Dis.adv", "fair")))

df_all_cdf <- df_all_cdf %>%
  mutate(Adv.District.Names = ifelse((Map == 1 & District == "EDG" & Player == "B")|(Map == 5 & District == "ELG" & Player == "A"), "Two Partisan For",
                                     ifelse((Map == 1 & District == "ELG" & Player == "B")|(Map == 5 & District == "EDG" & Player == "A"), "Three Partisan Against",
                                            ifelse((Map == 1 & District == "EW" & Player == "B")|(Map == 5 & District == "EW" & Player == "A"),"One Partisan For", "Error"))),
         Disadv.District.Names = ifelse((Map == 1 & District == "EDG" & Player == "A")|(Map == 5 & District == "ELG" & Player == "B"), "Two Partisan Against",
                                        ifelse((Map == 1 & District == "ELG" & Player == "A")|(Map == 5 & District == "EDG" & Player == "B"), "Three Partisan For",
                                               ifelse((Map == 1 & District == "EW" & Player == "A")|(Map == 5 & District == "EW" & Player == "B"),"One Partisan Against", "Error"))))

df_all_cdf <- df_all_cdf %>%
  filter(Period >= 15, Period <= 24) %>%
  mutate(District.compare = ifelse((District == "EDG" & Player == 'A')|(District == "ELG" & Player == 'B'), "DG Player A to LG Player B",
                                   ifelse((District == "ELG" & Player == 'A')|(District == "EDG" & Player == 'B'), "LG Player A to DG Player B","W for Both Players")),
         Advantage = ifelse((Player == "A" & Map == 5)|(Player == "B" & Map == 1), "Adv", 
                            ifelse((Player == "B" & Map == 5)|(Player == "A" & Map == 1),"Dis.adv", "fair")),
         Renamed.Map = ifelse((Map == 1 & Player == "B")|(Map == 5 & Player =="A"), "Advantaged",
                              ifelse((Map == 1 & Player == "A")|(Map == 5 & Player == "B"),"Disadvantaged",
                                     ifelse(Map == 2, "Sym_1_1",
                                            ifelse(Map == 3, "Sym_1_3", "Sym_3_1")))))

Map <- c("Sym_1_1", "Sym_1_3", "Sym_3_1", "Advantaged", "Disadvantaged")
adv_coef_cdfs <- data.frame()
disadv_coef_cdfs <- data.frame()
fair_coef_cdfs <- data.frame()

adv_coef_cdfs.fixed.effects <- data.frame()
disadv_coef_cdfs.fixed.effects <- data.frame()
fair_coef_cdfs.fixed.effects <- data.frame()

adv_coef_cdfs.random.effects <- data.frame()
disadv_coef_cdfs.random.effects <- data.frame()
fair_coef_cdfs.random.effects <- data.frame()


for(c in Map){
  temp_df <- df_all_cdf %>% 
    filter(Renamed.Map == c) %>%
    mutate(Compare = ifelse(Renamed.Map == "Advantaged", Adv.District.Names,
                            ifelse(Renamed.Map == "Disadvantaged", Disadv.District.Names,
                                   District.compare))) %>%
    dplyr::select(Renamed.Map,
                  Compare,
                  Effort,
                  Player,
                  subject.id
    )
  for(i in unique(temp_df$Compare)){
    temp_model <- lm(Effort ~ Player, temp_df %>% filter(Compare == i))
    temp_summary <- summary(temp_model)
    temp_intercept <- temp_summary[["coefficients"]][1,1]
    temp_intercept_pvalue <- temp_summary[["coefficients"]][1,4]
    temp_est <- temp_summary[["coefficients"]][2,1]
    temp_pvalue <- temp_summary[["coefficients"]][2,4]
    
    dat <- df_all_cdf %>% mutate(Compare = ifelse(Renamed.Map == "Advantaged", Adv.District.Names,
                                                  ifelse(Renamed.Map == "Disadvantaged", Disadv.District.Names,
                                                         District.compare))) %>% filter(Renamed.Map == c, Compare == i)
    dat <- pdata.frame(dat, index=c("subject.id")  )
    # # Random effects; no clustered standard error; probably better way to do this
    temp_model.RE <- plm( Effort ~ Player , data= dat  , model="between")
    temp_summary.RE <- summary(temp_model.RE)
    temp_intercept.RE <- temp_summary.RE[["coefficients"]][1,1]
    temp_intercept_pvalue.RE <- temp_summary.RE[["coefficients"]][1,4]
    temp_est.RE <- temp_summary.RE[["coefficients"]][2,1]
    temp_pvalue.RE <- temp_summary.RE[["coefficients"]][2,4]
    
    if(c == "Advantaged"){
      adv_coef_cdfs[c,paste(i,"intercept",sep = " ")] <- temp_intercept
      adv_coef_cdfs[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue
      adv_coef_cdfs[c,paste(i,"estimate",sep = " ")] <- temp_est
      adv_coef_cdfs[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue
      
      # adv_coef_cdfs[c,paste(i,"intercept",sep = " ")] <- temp_intercept
      # adv_coef_cdfs[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue
      # adv_coef_cdfs[c,paste(i,"estimate",sep = " ")] <- temp_est
      # adv_coef_cdfs[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue
      # 
      adv_coef_cdfs.random.effects[c,paste(i,"intercept",sep = " ")] <- temp_intercept.RE
      adv_coef_cdfs.random.effects[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue.RE
      adv_coef_cdfs.random.effects[c,paste(i,"estimate",sep = " ")] <- temp_est.RE
      adv_coef_cdfs.random.effects[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue.RE
    }else if (c == "Disadvantaged"){
      disadv_coef_cdfs[c,paste(i,"intercept",sep = " ")] <- temp_intercept
      disadv_coef_cdfs[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue
      disadv_coef_cdfs[c,paste(i,"estimate",sep = " ")] <- temp_est
      disadv_coef_cdfs[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue
      
      # disadv_coef_cdfs[c,paste(i,"intercept",sep = " ")] <- temp_intercept
      # disadv_coef_cdfs[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue
      # disadv_coef_cdfs[c,paste(i,"estimate",sep = " ")] <- temp_est
      # disadv_coef_cdfs[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue
      # 
      disadv_coef_cdfs.random.effects[c,paste(i,"intercept",sep = " ")] <- temp_intercept.RE
      disadv_coef_cdfs.random.effects[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue.RE
      disadv_coef_cdfs.random.effects[c,paste(i,"estimate",sep = " ")] <- temp_est.RE
      disadv_coef_cdfs.random.effects[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue.RE
    }else{
      fair_coef_cdfs[c,paste(i,"intercept",sep = " ")] <- temp_intercept
      fair_coef_cdfs[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue
      fair_coef_cdfs[c,paste(i,"estimate",sep = " ")] <- temp_est
      fair_coef_cdfs[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue
      
      # fair_coef_cdfs[c,paste(i,"intercept",sep = " ")] <- temp_intercept
      # fair_coef_cdfs[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue
      # fair_coef_cdfs[c,paste(i,"estimate",sep = " ")] <- temp_est
      # fair_coef_cdfs[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue
      # 
      fair_coef_cdfs.random.effects[c,paste(i,"intercept",sep = " ")] <- temp_intercept.RE
      fair_coef_cdfs.random.effects[c,paste(i,"intercept p-value",sep = " ")] <- temp_intercept_pvalue.RE
      fair_coef_cdfs.random.effects[c,paste(i,"estimate",sep = " ")] <- temp_est.RE
      fair_coef_cdfs.random.effects[c,paste(i,"estimate p-value",sep = " ")] <- temp_pvalue.RE
      
    }
  }
}
# xtable(adv_coef_cdfs)
# xtable(disadv_coef_cdfs)
# xtable(fair_coef_cdfs)

xtable(adv_coef_cdfs.random.effects)

xtable(disadv_coef_cdfs.random.effects)

xtable(fair_coef_cdfs.random.effects)

# Checking model consistency
dat <- df_all_cdf %>% mutate(Compare = ifelse(Renamed.Map == "Advantaged", Adv.District.Names,
                                              ifelse(Renamed.Map == "Disadvantaged", Disadv.District.Names,
                                                     District.compare))) %>% filter(Renamed.Map == "Sym_3_1", Compare == "LG Player A to DG Player B")
m1 <- lm(Effort ~ Player, data = dat)
m2 <- lmer(Effort ~ Player + (1 | subject.id), data = dat)
# m2 <- plm(Effort ~ Player, data = dat, index = c("subject.id","Period"), model = "within")
# m3 <- plm(Effort ~ Player, data = dat, index = "Period", model = "random")
z2 <- pdata.frame(dat, index=c("subject.id")  )    
m3 <- plm( Effort ~ Player , data= z2  , model="between") # matches xtreg , fe
# m4 <- lme(Effort ~ Player, data = dat, random = pdDiag(~subject.id))
#summary(m3)

##-------------------------------------
##            Table 7                --
##-------------------------------------
stage_1to2_regression_data <- df %>% dplyr::select(Session, Period, Subject, Player, TE_1:TE_5) %>%
  filter(Period >= 15 & Period <= 27) %>%
  gather(Map, Effort, TE_1:TE_5)

stage_1to2_regression_data <- stage_1to2_regression_data %>% mutate(subject.id = as.factor(as.numeric(Session)*8-(8-as.numeric(Subject))),
                                                                    Player_B = ifelse(Player== "B", 1, 0),
                                                                    Gerry_B = ifelse(Map == "TE_1", 1, 0),
                                                                    Symm_1_1 = ifelse(Map == "TE_2", 1, 0),
                                                                    Symm_1_3 = ifelse(Map == "TE_3", 1, 0),
                                                                    Symm_3_1 = ifelse(Map == "TE_4", 1, 0),
                                                                    Gerry_A = ifelse(Map == "TE_5", 1, 0),
                                                                    Adv = ifelse((Map == "TE_1" & Player == "B")|(Map == "TE_5" & Player == "A"), 1,0),
                                                                    Disadv = ifelse((Map == "TE_1" & Player == "A")|(Map == "TE_5" & Player == "B"), 1,0),
                                                                    Stage_2_indicator = ifelse((Period > 24 & Period < 28), 1, 0))

map_impact_on_stage_1to2_no_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + Stage_2_indicator + Stage_2_indicator*Adv + Stage_2_indicator*Disadv + Stage_2_indicator*Symm_1_3 + Stage_2_indicator*Symm_3_1 + subject.id, data = stage_1to2_regression_data)

stage_1to2_regression_data_with_learning <- stage_1to2_regression_data %>% filter(Period >= 20)
map_impact_on_stage_1to2_with_learning_FE <-lm(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + Stage_2_indicator + Stage_2_indicator*Adv + Stage_2_indicator*Disadv + Stage_2_indicator*Symm_1_3 + Stage_2_indicator*Symm_3_1 + subject.id, data = stage_1to2_regression_data_with_learning)

stargazer(map_impact_on_stage_1to2_no_learning_FE,
          map_impact_on_stage_1to2_with_learning_FE,
          title = "Map Impact with Stage 2 Indicator (FE and Clustereed SE)",
          column.labels = c("w/out learning", "w/ learning"),
          label = "Tab:stage_1to2_with_and_without_learning_FE_CSE",
          omit = "subject.id", single.row = T)

# Must fill in clustered SE manually; can't figure out how to do this in Stargazer
map_impact_on_stage_1to2_no_learning_FE_cluster <- lm.cluster(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + Stage_2_indicator + Stage_2_indicator*Adv + Stage_2_indicator*Disadv + Stage_2_indicator*Symm_1_3 + Stage_2_indicator*Symm_3_1 + subject.id, cluster = "Session", data = stage_1to2_regression_data)
summary(map_impact_on_stage_1to2_no_learning_FE_cluster)
#summary(reg,cluster = c("class_id"))

map_impact_on_stage_1to2_with_learning_FE_cluster <- lm.cluster(Effort ~ Adv + Disadv + Symm_1_3 + Symm_3_1 + Stage_2_indicator + Stage_2_indicator*Adv + Stage_2_indicator*Disadv + Stage_2_indicator*Symm_1_3 + Stage_2_indicator*Symm_3_1 + subject.id, cluster = "Session", data = stage_1to2_regression_data_with_learning)
summary(map_impact_on_stage_1to2_with_learning_FE_cluster)


