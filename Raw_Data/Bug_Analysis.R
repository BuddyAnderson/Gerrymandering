# Author: Buddy Anderson
# Date: Fri Apr  2 18:43:57 2021
# 
# Looking into code bug

library(readxl)
library(dplyr)
library(ggplot2)
rm(df)
df <- read_excel("Desktop/Research/GerryMandering/An_Anderson_Deck/Raw_Data/Raw_1_to_8.xlsx")
colnames(df)
# we want to compare the player type to LType
df <- df %>% select(Session, Subject, Player, LType, Map)

# There were 30 subjects that were told, for the last round, that they 
# were both Player A and Player B

# Now, let's compare the distribution of map choices, excluding the one subject that didn't
# pick in session 8
map_df <- subset(df, Map > 0)
map_df$gob <- ifelse(map_df$Player == map_df$LType, "Correct", "Incorrect")

map_df %>%
  ggplot(aes(Map, fill = gob)) + geom_density(aes(alpha = 0.5)) + 
  scale_fill_discrete(name="") +
  scale_alpha(guide = 'none')

# Does showing subjects the incorrect screen alter their behavior?
# Ho: map preference does not differ between the correct and incorrect group
# Ha: map preference does differ between the correct and incorrect group
# let alpha = 0.05
df_test <- map_df %>% select(gob, Map)
mdata <- melt(df_test, gob = c("Correct", "Incorrect"))
nextdata <- cast(mdata, gob~value)
colnames(nextdata) <- c("GOB","M1", "M2", "M3", "M4", "M5")
nextdata <- nextdata %>% mutate(
  total = M1 + M2 + M3 + M4 + M5
)

# Expected values
nextdata <- nextdata %>% mutate(
  E1 = total*sum(M1)/sum(total),
  E2 = total*sum(M2)/sum(total),
  E3 = total*sum(M3)/sum(total),
  E4 = total*sum(M4)/sum(total),
  E5 = total*sum(M5)/sum(total)
)

# Test stat
nextdata <- nextdata %>% mutate(
  tstat1 = ((M1 - E1)^(2))/E1,
  tstat2 = ((M2 - E2)^(2))/E2,
  tstat3 = ((M3 - E3)^(2))/E3,
  tstat4 = ((M4 - E4)^(2))/E4,
  tstat5 = ((M5 - E5)^(2))/E5
)

tstat <- sum(nextdata$tstat1) + sum(nextdata$tstat2) + 
  sum(nextdata$tstat3) + sum(nextdata$tstat4) + sum(nextdata$tstat5) # 1.18

dof = (5 -1)*(2-1) # 4

# With alpha = 0.05 and dof = 4 the critical value of chi squared test is
# 9.488 which is much higher than our chi squared stat of 1.18
# Thus, we fail to reject the null and cannot say the two map preferences differ

# The above was pretty useless. What we want to compare is the effort spent in the last round.

dt <- read_excel("Desktop/Research/GerryMandering/An_Anderson_Deck/Raw_Data/Raw_1_to_8.xlsx")

dt <- dt %>% select(Session, Player, LType, 58:72) # just looking at efforts here
dt <- dt %>% mutate(
  GOB = ifelse(Player == LType, "Correct", "Incorrect")
)

dt <- dt[,-(1:3)]

dt <- data.frame(GOB = dt$GOB, dt[,1:15])

# Now we want to collapse the data
agg <- aggregate(list(dt[,2:16]), by = list(dt$GOB), mean)

new <- agg %>% gather(District, Effort, 2:16)

## But really, we only care if those people facing the incorrect setting bid differently in the
## last round relative to the correct setting folks, conditional on their previous bidding behavior...

