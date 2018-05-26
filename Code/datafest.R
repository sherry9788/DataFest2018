library(readr)
library(dplyr)
jobs <- read_csv("datafest2018-Updated-April12.csv")

jobs <- mutate_if(jobs, is.character, as.factor)


jobs_norm <- mutate_if(jobs, is.numeric, scale)
jobs_can <- jobs[jobs$country=="CA",]


jobs %>% filter(normTitleCategory=="techsoftware") %>% 
  mutate(num_job = )
  group_by(jobId) %>% mutate(avg_clk_jobid = mean(clicks)) %>% 
  group_by(stateProvince) %>% mutate(avg_clk_state = mean(avg_clk_jobid)) %>%
  
  
prob_mat <- matrix(rep(0, 51*51), nrow = 51)

for (i in 1:51){
  for (j in 1:51){
    prob_mat[i, j] 
  }
}
  
library(readxl)
govn16 <- read_excel("state_M2016_dl.xlsx")
govn17 <- read_excel("state_M2017_dl.xlsx")
govn97 <- read_excel("state_1997_dl.xlsx")

govn16 <- govn16[-which(govn16$ST == "VI"),]
govn17 <- govn17[-which(govn17$ST == "VI"),]
govn97 <- govn97[-which(govn97$st == "VI"),]

comp16 <- govn16[grep("^15", govn16$OCC_CODE),]
comp16_tot <- comp16 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(comp16_tot, "govern_16.csv")

comp17 <- govn17[grep("^15", govn17$OCC_CODE),]
comp17_tot <- comp17 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(comp17_tot, "govern_17.csv")

comp97 <- govn97[grep("^25", govn97$occ_code),]
comp97_tot <- comp97 %>% group_by(st) %>% summarise(sum = sum(as.numeric(tot_emp), na.rm = T))
write.csv(comp97_tot, "govern_97.csv")

sales16 <- govn16[grep("^41", govn16$OCC_CODE),]
sales16_tot <- sales16 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(sales16_tot, "sales_16.csv")

sales17 <- govn17[grep("^41", govn17$OCC_CODE),]
sales17_tot <- sales17 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(sales17_tot, "sales_17.csv")

sales97 <- govn97[grep("^49", govn97$occ_code),]
sales97_tot <- sales97 %>% group_by(st) %>% summarise(sum = sum(as.numeric(tot_emp), na.rm = T))
write.csv(sales97_tot, "sales_97.csv")

mgmt16 <- govn16[grep("^11", govn16$OCC_CODE),]
mgmt16_tot <- mgmt16 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(mgmt16_tot, "mgmt_16.csv")

mgmt17 <- govn17[grep("^11", govn17$OCC_CODE),]
mgmt17_tot <- mgmt17 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(mgmt17_tot, "mgmt_17.csv")

mgmt97 <- govn97[grep("^1", govn97$occ_code),]
mgmt97_tot <- mgmt97 %>% group_by(st) %>% summarise(sum = sum(as.numeric(tot_emp), na.rm = T))
write.csv(mgmt97_tot, "mgmt_97.csv")

food16 <- govn16[grep("^35", govn16$OCC_CODE),]
food16_tot <- food16 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(food16_tot, "food_16.csv")

food17 <- govn17[grep("^35", govn17$OCC_CODE),]
food17_tot <- food17 %>% group_by(ST) %>% summarise(sum = sum(as.numeric(TOT_EMP), na.rm = T))
write.csv(food17_tot, "food_17.csv")

food97 <- govn97[grep("^65", govn97$occ_code),]
food97_tot <- food97 %>% group_by(st) %>% summarise(sum = sum(as.numeric(tot_emp), na.rm = T))
write.csv(food97_tot, "food_97.csv")

final <- read.csv("final_states.csv", header = F)

final <- data.frame(state = comp16_tot$ST, govn_change = comp17_tot$sum/sum(comp17_tot$sum) - comp16_tot$sum/sum(comp16_tot$sum), final = final)
#final$govn_change <- format(final$govn_change, scientific = F)
write.csv(final, "final_compare.csv")
