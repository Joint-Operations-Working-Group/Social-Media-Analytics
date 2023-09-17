setwd("C:/Users/kilbi/Documents/Companies/")
require(plotly)
require(ggplot2)
require(tidyverse)
###########################
getwd()

ALLDATA <- data.frame()

for(i in 1:20){

FILENAME <-paste0("https://raw.githubusercontent.com/Joint-Operations-Working-Group/Social-Media-Analytics/main/Linkedin/Company_",sprintf("%04.0f",i),".csv")
myData_1 <- read_csv(FILENAME)
dim(myData_1)
ALLDATA <- ALLDATA %>% bind_rows(myData_1)
ALLDATA <- ALLDATA %>% select("Company","Date","Followers")
ALLDATA <- ALLDATA %>% arrange(Company,Date) %>% distinct()
}



# myData_11 <- read_csv("https://raw.githubusercontent.com/DragonflyStats/Projects/master/linkedin/Company_2021.csv")
# dim(myData_11)
# ALLDATA <- ALLDATA %>% bind_rows(myData_11)
# ALLDATA <- ALLDATA %>% arrange(Company,Date) %>% distinct()
# dim(ALLDATA)
# 
# 
# myData_12 <- read_csv("https://raw.githubusercontent.com/DragonflyStats/Projects/master/linkedin/Company_2022.csv")
# dim(myData_12)
# ALLDATA <- ALLDATA %>% bind_rows(myData_12)
# ALLDATA <- ALLDATA %>% arrange(Company,Date) %>% distinct()
# dim(ALLDATA)
# 
# FULL_LIST <- unique(ALLDATA$Company) %>% sort()
# 
# 
# myData_12 <- read_csv("https://raw.githubusercontent.com/DragonflyStats/Projects/master/linkedin/Company_2023.csv")
# dim(myData_12)
# ALLDATA <- ALLDATA %>% bind_rows(myData_12)
# ALLDATA <- ALLDATA %>% arrange(Company,Date) %>% distinct()
# dim(ALLDATA)
# 

FULL_LIST <- unique(ALLDATA$Company) %>% sort()
len <- length(FULL_LIST)
STOPS <- ceiling(1:20*(len/20))
STARTS <- c(1, STOPS+1)[1:20]


length(STARTS)
length(STOPS)
for(i in 1:20){
THISLIST <- FULL_LIST[ (STARTS[i]):(STOPS[i])]
THISDATA <- ALLDATA %>% filter(Company %in% THISLIST)  
THISDATA <- THISDATA %>% select("Company","Date","Followers")
dim(THISDATA)
OUTPUTNAME <- paste0("Company_",sprintf("%04.0f",i),".csv")
write.csv(THISDATA,OUTPUTNAME,row.names = FALSE)
}


SpecialList1 <- grep("Julia",unique(ALLDATA$Company),value=T) %>% setdiff("Julian Jewel's AI Bot")
SpecialList1 <- c(SpecialList1,"SciML")
THISDATA <- ALLDATA %>% filter(Company %in% SpecialList1)  %>%
  mutate( Company = case_when(
    grepl("JuliaCon",Company)==TRUE ~ "JuliaCon",
    grepl("JuliaCon",Company)==FALSE ~ Company
  ))
dim(THISDATA)
write.csv(THISDATA,"C:/Users/kilbi/Documents/Companies/JuliaList.csv",row.names = FALSE)
p <- THISDATA %>% ggplot(aes(x=Date,y=Followers,col=Company)) +  geom_line(lwd=1.2)
p <- p + theme_classic()
p
ggsave("JuliaLinkedin.PNG")


SpecialList1 <- grep("PyData",unique(ALLDATA$Company),value=T) 
THISDATA <- ALLDATA %>% filter(Company %in% SpecialList1)  
PyDataList2 <- THISDATA %>% group_by(Company) %>% arrange(desc(Followers)) %>% slice(1) %>% 
  filter(Followers>100) %>% pull(Company)
THISDATA <- ALLDATA %>% filter(Company %in% PyDataList2)
dim(THISDATA)

write.csv(THISDATA,"C:/Users/kilbi/Documents/Companies/PyDataList.csv",row.names = FALSE)
p <- THISDATA %>% ggplot(aes(x=Date,y=Followers,col=Company)) +  geom_line()
ggplotly(p)


SpecialList1 <- c("DataCamp","RStudio","RStudio PBC","Pluralsight","Posit PBC")
THISDATA <- ALLDATA %>% filter(Company %in% SpecialList1)  
dim(THISDATA)

write.csv(THISDATA,"C:/Users/kilbi/Documents/Companies/DSList.csv",row.names = FALSE)
p <- THISDATA %>% ggplot(aes(x=Date,y=Followers,col=Company)) +  geom_line()
ggplotly(p)




SpecialList1 <- c("R User Community","Why R?",
                  "The R Foundation for Statistical Computing",
                  "useR! - The R User Conference","R Consortium","R-Ladies Global")
THISDATA <- ALLDATA %>% filter(Company %in% SpecialList1)  
dim(THISDATA)

write.csv(THISDATA,"C:/Users/kilbi/Documents/Companies/R-List.csv",row.names = FALSE)
p <- THISDATA %>% ggplot(aes(x=Date,y=Followers,col=Company)) +  geom_line()
ggplotly(p)
