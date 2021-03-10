## Pilot Data Analysis; 11/17/2020
library(ggplot2)
library(readxl)
Pilot_Data <- read_excel("Desktop/Research/GerryMandering/An_Anderson_Deck/Pilot_Data_and_Analysis/Cleaned_Pilot_Data.xlsx")

E4 <- Pilot_Data[, -c(3:5,9:17)]

## Let's figure out by Map average efforts in districts (Plots) and do with with and without Player 1

## Map 5
EffortAvg <- aggregate(Pilot_Data[, 3:17], list(Pilot_Data$Period), mean)

Map5 <- ggplot(EffortAvg, aes(x=Group.1)) + 
  geom_line(aes(y = EDG5), color = "darkred") + 
  geom_line(aes(y = ELG5), color="steelblue") + geom_line(aes(y = EW5), color="darkolivegreen")

Map4 <- ggplot(EffortAvg, aes(x=Group.1)) + 
  geom_line(aes(y = EDG4), color = "darkred") + 
  geom_line(aes(y = ELG4), color="steelblue") + geom_line(aes(y = EW4), color="darkolivegreen")

Map3 <- ggplot(EffortAvg, aes(x=Group.1)) + 
  geom_line(aes(y = EDG3), color = "darkred") + 
  geom_line(aes(y = ELG3), color="steelblue") + geom_line(aes(y = EW3), color="darkolivegreen")

Map2 <- ggplot(EffortAvg, aes(x=Group.1)) + 
  geom_line(aes(y = EDG2), color = "darkred") + 
  geom_line(aes(y = ELG2), color="steelblue") + geom_line(aes(y = EW2), color="darkolivegreen")

Map1 <- ggplot(EffortAvg, aes(x=Group.1)) + 
  geom_line(aes(y = EDG1), color = "darkred") + 
  geom_line(aes(y = ELG1), color="steelblue") + geom_line(aes(y = EW1), color="darkolivegreen")

print(Map1)
print(Map2)
print(Map3)
print(Map4)
print(Map5)


