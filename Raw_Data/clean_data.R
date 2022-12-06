##---------------------------------------------------------------
##                  Clean Gerrymandering Data                  --
##---------------------------------------------------------------
library(readxl)
library(writexl)
##---------------------------------------------------------------
##                  Remove unnecessary clutter                  --
##---------------------------------------------------------------
rm(list = ls()) # Take out the Environment "trash"
cat("\014")  # Clear console, making error checking easier
while(!is.null(dev.list())) dev.off() # Clear old plots
setwd("~/Desktop/Research/GerryMandering/An_Anderson_Deck/Raw_Data")

data_files <- c(1:16)
peq_df <- data.frame()
for(i in data_files){
  if(i > 9){
    tmp_df <- read_xlsx(paste0("Raw_Session_", i, ".xlsx"))
    tmp_df <- tmp_df[,1:(ncol(tmp_df)-4)]
    colnames(tmp_df) <- tmp_df[2,]
    tmp_df %<>% filter(subjects == "subjects", Period != "Period") %>%
      mutate(Period = as.numeric(Period)) %>%
      filter(Period > 14, Period < 30)
    write_xlsx(tmp_df, paste0("Cleaned_Raw_Session_", i, ".xlsx"))
    
  }else{
    tmp_df <- read_xlsx(paste0("Raw_Session_0", i, ".xlsx"))
    tmp_df <- tmp_df[,1:(ncol(tmp_df)-4)]
    colnames(tmp_df) <- tmp_df[2,]
    tmp_df %<>% filter(subjects == "subjects", Period != "Period") %>%
      mutate(Period = as.numeric(Period)) %>%
      filter(Period > 14, Period < 30)
    write_xlsx(tmp_df, paste0("Cleaned_Raw_Session_0", i, ".xlsx"))
  }
}
