#################################### 1.1 ################################################

##Load the companies and rounds data (provided on the previous page) into two data frames
## and name them companies and rounds2 respectively.
##setwd("C:\\Users\\kanupriya.saxena\\Desktop\\Study\\UpGrad\\Case Study")
##library(dplyr)
library(tidyr)

companies <- read.csv("companies.txt", encoding='UTF8', sep = "\t",na.strings=c("", "NA"),header = T,stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F,na.strings=c("", "NA"))

companies_lower <- mutate(companies,permalink=tolower(permalink))
rounds2_lower <- mutate(rounds2,company_permalink=tolower(company_permalink))  
                      
                          
## How many unique companies are present in rounds2?
#length(unique(rounds2$company_permalink))
length(unique(rounds2_lower$company_permalink))

## How many unique companies are present in companies?
length(unique(companies_lower$permalink))

## In the companies data frame, which column can be used as the unique key for each company?
## Write the name of the column.

Permalink

##Are there any companies in the rounds2 file which are not present in companies? 
## Answer yes or no: Y/N

N
#length(which(sapply(rounds2_lower$company_permalink,tolower) %in% sapply(companies_lower$permalink,tolower) == FALSE))
length(which(rounds2_lower$company_permalink %in% companies_lower$permalink == FALSE))

##Merge the two data frames so that all variables (columns) in the companies frame are
#added to the rounds2 data frame. Name the merged frame master_frame. 
##How many observations are present in master_frame?

master_frame <- merge(rounds2_lower,companies_lower, by.x="company_permalink",by.y= "permalink")

#################################### 2.1 ################################################

##Checkpoint 2: Funding Type Analysis
group1<-aggregate(raised_amount_usd~funding_round_type,rounds2,mean)
##Average funding amount of venture type
subset(aggregate(raised_amount_usd~funding_round_type,rounds2,mean),funding_round_type == "venture")

#Average funding amount of angel type	 
subset(aggregate(raised_amount_usd~funding_round_type,rounds2,mean),funding_round_type == "angel")

##Average funding amount of seed type	 
subset(aggregate(raised_amount_usd~funding_round_type,rounds2,mean),funding_round_type == "seed")

##Average funding amount of private equity type
subset(aggregate(raised_amount_usd~funding_round_type,rounds2,mean),funding_round_type == "private_equity")

##Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
subset(aggregate(raised_amount_usd~funding_round_type,rounds2,mean),(raised_amount_usd >5000000 & raised_amount_usd < 15000000))$funding_round_type
#################################### 3.1 ################################################

##For the chosen investment type, make a data frame named top9 with the top nine countries 
##(based on the total investment amount each country has received)
inv_chosen<-subset(aggregate(raised_amount_usd~funding_round_type,rounds2,mean),(raised_amount_usd >=5000000 & raised_amount_usd <= 15000000))$funding_round_type

##master_frame_filtered<- na.omit(filter(master_frame,funding_round_type == inv_chosen))
master_frame_filtered<-subset(master_frame,funding_round_type == inv_chosen)

top9 <- head(arrange(aggregate(raised_amount_usd~country_code,master_frame_filtered,sum),desc(raised_amount_usd)),9)
#countrycode((top9$country_code),"iso3c",destination = "country.name")
top9_eng <- cbind(top9,engIsOfficial=c(1,0,1,1,1,0,0,0,0))
top3<-head(subset(top9_eng,top9_eng$engIsOfficial == 1),3)[-3]

#1. Top English-speaking country
top1_eng<-top3$country_code[1]

#2. Second English-speaking country	
top2_eng<-top3$country_code[2]

#3. Third English-speaking country
top3_eng<-top3$country_code[3]

#################################### 4.1 ################################################

master_frame_filtered_with_primary_sector <- mutate(master_frame_filtered,primary_sector = sapply(strsplit(tolower(master_frame_filtered$category_list),"|",fixed=T),"[",1))

#mappings = read.csv("mapping.csv",stringsAsFactors = F,na.strings = c(""," ","NA"))
mappings = read.csv("mapping.csv",stringsAsFactors = F)

mappings_gathered <- filter(gather(mappings,main_sector,value,2:10),value == "1")[-3]
mappings_lower <- mutate(mappings_gathered,category_list=tolower(category_list))
mappings_cleanse <- mutate(mappings_lower,category_list=ifelse(grepl(".*0.+",mappings_lower$category_list),gsub("0","na",mappings_lower$category_list),category_list))

master_frame_with_sectors <- inner_join(master_frame_filtered_with_primary_sector,mappings_cleanse,by = c("primary_sector" = "category_list")  )

#################################### 5.1 ################################################
master_frame_with_sectors_agg <- do.call(data.frame,aggregate(raised_amount_usd~main_sector,master_frame_with_sectors,FUN = function(x) c(Total_amount_invested = sum(x) , Total_no_of_investments = length(x))))

D1<-subset(master_frame_with_sectors, country_code == top1_eng)
D2<-subset(master_frame_with_sectors, country_code == top2_eng)
D3<-subset(master_frame_with_sectors, country_code == top3_eng)

D1_agg <- do.call(data.frame,aggregate(raised_amount_usd~main_sector,D1,FUN = function(x) c(Total_amount_invested = sum(x) , Total_no_of_investments = length(x))))
D2_agg <- do.call(data.frame,aggregate(raised_amount_usd~main_sector,D2,FUN = function(x) c(Total_amount_invested = sum(x) , Total_no_of_investments = length(x))))
D3_agg <- do.call(data.frame,aggregate(raised_amount_usd~main_sector,D3,FUN = function(x) c(Total_amount_invested = sum(x) , Total_no_of_investments = length(x))))

D1_agg$main_sector<-as.character(D1_agg$main_sector)
D2_agg$main_sector<-as.character(D2_agg$main_sector)
D3_agg$main_sector<-as.character(D3_agg$main_sector)

D1<-inner_join(D1,D1_agg,by="main_sector")
D2<-inner_join(D2,D2_agg,by="main_sector")
D3<-inner_join(D3,D3_agg,by="main_sector")


## 1. Total number of investments (count)
length(D1$company_permalink)
length(D2$company_permalink)
length(D3$company_permalink)

## 2. Total amount of investment (USD)
sum(D1$raised_amount_usd,na.rm = T)
sum(D2$raised_amount_usd,na.rm = T)
sum(D3$raised_amount_usd,na.rm =T)

getSector <- function(df,no,ag) {
#print(ag)
  if(ag == "name"){
    
  print(arrange(df,desc(df$raised_amount_usd.Total_no_of_investments))[no,"main_sector"])  
  }
  else{
      arrange(df,desc(df$raised_amount_usd.Total_no_of_investments))[no,"raised_amount_usd.Total_no_of_investments"]
  }
}
## 3. Top sector (based on count of investments)
#arrange(summarise(group_by(D1,main_sector),length(raised_amount_usd)),desc(`length(raised_amount_usd)`))[1,]
D1_name_1<-getSector(D1_agg,"1","name")
D2_name_1<-getSector(D2_agg,"1","name")
D3_name_1<-getSector(D3_agg,"1","name")

##4. Second-best sector (based on count of investments)
D1_name_2<-getSector(D1_agg,"2","name")
D2_name_2<-getSector(D2_agg,"2","name")
D3_name_2<-getSector(D3_agg,"2","name")

## 5. Third-best sector (based on count of investments)
D1_name_3<-getSector(D1_agg,"3","name")
D2_name_3<-getSector(D2_agg,"3","name")
D3_name_3<-getSector(D3_agg,"3","name")

## 6. Number of investments in the top sector (refer to point 3)
getSector(D1_agg,"1","count")
getSector(D2_agg,"1","count")
getSector(D3_agg,"1","count")

##  7. Number of investments in the second-best sector (refer to point 4)
getSector(D1_agg,"2","count")
getSector(D2_agg,"2","count")
getSector(D3_agg,"2","count")

##8. Number of investments in the third-best sector (refer to point 5)

getSector(D1_agg,"3","count")
getSector(D2_agg,"3","count")
getSector(D3_agg,"3","count")

## 9. For the top sector count-wise (point 3), which company received the highest investment?
arrange(aggregate(raised_amount_usd~name,
filter(D1,main_sector == D1_name_1),sum),desc(raised_amount_usd))[1,1]

arrange(aggregate(raised_amount_usd~name,
filter(D2,main_sector == D2_name_1),sum),desc(raised_amount_usd))[1,1]

arrange(aggregate(raised_amount_usd~name,
filter(D3,main_sector == D3_name_1),sum),desc(raised_amount_usd))[1,1]

##10. For the second-best sector count-wise (point 4), which company received the highest investment?
arrange(aggregate(raised_amount_usd~name,
                  filter(D1,main_sector == D1_name_2),sum),desc(raised_amount_usd))[1,1]

arrange(aggregate(raised_amount_usd~name,
                  filter(D2,main_sector == D2_name_2),sum),desc(raised_amount_usd))[1,1]

arrange(aggregate(raised_amount_usd~name,
                  filter(D3,main_sector == D3_name_2),sum),desc(raised_amount_usd))[1,1]

