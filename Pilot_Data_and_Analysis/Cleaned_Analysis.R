# Author: Buddy Anderson
# Date: Thu Feb 18 18:37:39 2021
#
# Cleaned Pilot Data Brief Review

library(readxl)
df <- read_excel(
   "Desktop/Research/GerryMandering/An_Anderson_Deck/Pilot_Data_and_Analysis/Cleaned_Pilot_Data.xlsx")

#### Below is information regarding the columns of the condensed data (only including relevant variables)

## For i in 1:5, if Win_i== 1 then the mathced player won the contest for that map
## h is a random number to help determine r_round which is unique to each player
## r_round picks 1,2,3,4, or 5 randomly with equal chance and assigns that as the map on which to be paid
## for Stage 1; AKA, r_round determines which panel to highlight; pr_round = r_round (for Type2 partners)
## Win_practice does nothing...(= Win_i for r_round determined map)
## Map is the button so when players click a button under the map they are assigned that as "Map" value
## pMap is partner's map
## k is a random number used to determine with 50/50 chance whether type 1 or type 2 player's map is used
## that is the "Comp_Map"
## Win_Stage_2 takes on value of Win_i (either 0 or 1) for Comp_Map == i
## t is random number to help reshuffle the types
## LType now determines what player you are for veil of ignorance
## Win_Stage_4 takes on value of Win_i for Comp_Map == i
## PO_i = if (Win_i == 1, Value - TE_i, -TE_i) is the payoff for each participant in Map i

############## Alterations to Data for analysis #################
stage <- rep("1", length(df$Period))
stage[df$Period >= 20 & df$Period < 25] <- "2"
stage[df$Period >= 25] <- "3"
df$Stage <- stage # so now we have each stage
df$Map_Choice <- df$Map # rename the Map as the Map that the subject chose for a given period

# # Let's only keep the relevant stuff:
library(dplyr)
df_small <- select(df,
                      Period,
                      Stage,
                      Subject,
                      Map_Choice,
                      EDG5:EW1
)
df1 <- subset(df, Subject == 1)
df1_small <- select(df1,
                   Period,
                   Stage,
                   Subject,
                   Map,
                   EDG5:EW1
)


# Now, in order to graph effort by district across Map for each subject we need to restructure the data
library(tidyr)
long_df <- df_small %>% gather(District, Effort, EDG5:EW1)

# Below is a chunk that replaces the District value with a "_" separated District value which will then
# allow us to split those values into the district and accompanying map

long_df_map <- data.frame(lapply(long_df, function(x) {
  gsub("EDG5", "EDG_5", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EDG4", "EDG_4", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EDG3", "EDG_3", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EDG2", "EDG_2", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EDG1", "EDG_1", x)
}))
##########################
long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("ELG5", "ELG_5", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("ELG4", "ELG_4", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("ELG3", "ELG_3", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("ELG2", "ELG_2", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("ELG1", "ELG_1", x)
}))
##########################
long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EW5", "EW_5", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EW4", "EW_4", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EW3", "EW_3", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EW2", "EW_2", x)
}))

long_df_map <- data.frame(lapply(long_df_map, function(x) {
  gsub("EW1", "EW_1", x)
}))
# #############################################################################################

df_clean <- long_df_map %>% separate(District, c("District", "Map")) # get district and map seprately

df_clean$Effort <- as.numeric(as.character(df_clean$Effort)) # make sure these values are numeric
df_clean$Period <- as.numeric(as.character(df_clean$Period)) # make sure these values are numeric
df_clean$Map <- as.numeric(as.character(df_clean$Map)) # make sure these values are numeric

# Save this data to the relevant path as Simp_Pilot_Data.csv
Clean_Pilot_Data <- df_clean
write.csv(
  Clean_Pilot_Data,
  "Desktop/Research/GerryMandering/An_Anderson_Deck/Pilot_Data_and_Analysis/Simp_Pilot_Data.csv",
  row.names = FALSE)






