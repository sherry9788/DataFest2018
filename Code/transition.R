datafest <- read_csv("datafest2018-Updated-April12.csv")


library(dplyr)
US <- datafest %>% filter(country=="US")

US %>% filter(normTitleCategory == "techsoftware") %>% group_by(stateProvince, jobId) %>% 
  select(jobId:stateProvince,clicks) %>% 
  summarise(totalclicks = sum(clicks), totalposting = n()) %>% 
  mutate(avgclick = totalclicks/totalposting) %>% summarise(rate = 1/mean(avgclick))




regiontable <- read.csv("C:/Users/winuser/Desktop/DataFest/us census bureau regions and divisions.csv")
merged <- merge(US, regiontable, by.x = "stateProvince", by.y = "State.Code")



rate <- merged %>% filter(normTitleCategory == "techsoftware") %>% group_by(Division, jobId) %>% 
  select(jobId,Division,clicks) %>% 
  summarise(totalclicks = sum(clicks), totalposting = n()) %>% 
  mutate(avgclick = totalclicks/totalposting) %>% summarise(rate = 1/mean(avgclick))
USrate <- merged %>% filter(normTitleCategory == "techsoftware") %>% group_by(jobId) %>% 
  select(jobId,Division,clicks) %>% 
  summarise(totalclicks = sum(clicks), totalposting = n()) %>% 
  mutate(avgclick = totalclicks/totalposting) %>% summarise(rate = 1/mean(avgclick))


v <- rate$rate
names(v) <- rate$Division
v <- v/USrate$rate[1]





salary <- merged %>% filter(normTitleCategory == "techsoftware") %>% group_by(Division, jobId) %>% 
  select(jobId,Division,estimatedSalary) %>% 
  summarise(totalsalary = sum(estimatedSalary), totalposting = n()) %>% 
  mutate(avgsalary = totalsalary/totalposting) %>% summarise(mediansalary = median(avgsalary))

USsalary <- merged %>% filter(normTitleCategory == "techsoftware") %>% group_by(jobId) %>% 
  select(jobId,Division,estimatedSalary) %>% 
  summarise(totalsalary = sum(estimatedSalary), totalposting = n()) %>% 
  mutate(avgsalary = totalsalary/totalposting) %>% summarise(mediansalary = median(avgsalary))

u <- salary$mediansalary
names(u) <- salary$Division
u <- u/USsalary$mediansalary[1]

t <- v+u

f <- function(i, j) j-i
m <- outer(t,t,f)

sm <- min(m)
for(i in 1:nrow(m)){
  m[i,] <- m[i,]-sm
}

for(i in 1:nrow(m)){
  
  s <- sum(m[i,])
  for(j in 1:nrow(m)){
    m[i,j] <- m[i,j]/s
  }
}

write.csv(m,"transition.csv",row.names = FALSE)
