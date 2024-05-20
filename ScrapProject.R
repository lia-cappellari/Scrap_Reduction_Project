############################## SETUP ###########################################
library(DBI)
library(dplyr)
library(RPostgres)
library(tidyr)
library(ggplot2)
library(lubridate)
library(formattable)
library(gridExtra)
library(shiny)
library(RColorBrewer)
library(showtext)
library(colorRamp2)
library(htmltools)
library(shinyWidgets)
library(shinyjs)
library(ggtext)
library(writexl)

################################################################################

font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

############################## SCRAP Table #####################################

##setup the server
pw = {"isyethermo"}
con <- dbConnect(RPostgres::Postgres()
                 ,host = "usd-thermo-s23-pg14.c8gp7jmm4l1e.us-west-2.rds.amazonaws.com"
                 ,port='5432'
                 ,dbname='Thermo'
                 ,user='USD_ISYE_KIM'
                 ,password=pw)

qry = paste0("select * from present_scrap_reason;")

##DF called reason
reason = dbGetQuery(con, qry)
################################################################################

#First Glance: Scrap Value per Fiscal Year
Total = reason %>% group_by(fiscal_year_number)%>% summarise(Value =sum(display_value))
formattable(Total)

#Identifying Specific SKUs with High Scrap Values 
HighValue = reason %>% select("sku_number","display_value", "transaction_qty", "fiscal_year_number", "scrap_reason_desc", "branch_code") %>% arrange(-display_value) %>% head(3)

#Creating Tables and Ordering by Scrap Quantity
HighQuantity = reason %>% select("sku_number","display_value", "transaction_qty", "fiscal_year_number", "scrap_reason_desc", "branch_code") %>% arrange(-transaction_qty) %>% head(3)

#Relating Scrapped SKUs to a SKUs Usage to Determine Proportionality
yearList = c(2018:2022)
branchX = "US03"

##CSV with the reasons catagorized
ZZ = read.csv("scrapDescALL.csv")


DF = reason %>%
  mutate(reasoning = case_when(
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="QC"] ~ "QC",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Null"] ~ "Null",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Lot Remnant"] ~ "Lot Remnant",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Overbuilt"] ~ "Overbuilt",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Distribution"] ~ "Distribution",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Overbought"] ~ "Overbought",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Expiration:"] ~ "Expiration:",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Sizing Error"] ~ "Sizing Error",     
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Obselence"] ~ "Obselence",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Forecast"] ~ "Forecast",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Defect"] ~ "Defect",
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Raw Material"] ~ "Raw Material",     
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="Other"] ~ "Other", 
    
    scrap_reason_desc %in% ZZ$Reason[ZZ$Category=="BOM"] ~ "BOM", 
    
  ))

##Filter US03 Branch Only
DF1 = DF %>% filter(branch_code=="US03")

##Select certain columns
DF2 = DF1[ , c("sku_number","wxyz", "product_line_group_code", "sales_rank_code","safety_stock","maximum_reorder_quantity","minimum_reorder_quantity","reorder_quantity","reorder_point_quantity","fiscal_quarter_number","fiscal_year_number","transaction_qty","display_value","reasoning")]  
head(DF2)

##Summarize display value for WXYZ
WXYZ.Value = DF2 %>% filter(sales_rank_code == "C") %>% group_by(wxyz) %>% summarise(n=n(), total = sum(display_value)) %>% arrange(-total)

#filter by ABCD
DF.C = DF2 %>% filter(sales_rank_code == "C")
nrow(DF.C)

##filter by wxyz: 29770 rows
DF.C.Z = DF.C %>% filter(wxyz == "Z-Stranger")

##filter out outlier: 29769 rows

DF.C.Z.Filtered = DF.C.Z %>% filter(!(sku_number %in% c("A33502")))

#Filter to only include top 3 reasons for high display values

DF.C.Z.Filtered %>% group_by(reasoning) %>%  arrange(-display_value)

Top3 = DF.C.Z.Filtered %>% filter(reasoning %in% c("Forecast","BOM","Overbought"))

Top3 %>% group_by(product_line_group_code) %>%  arrange(-display_value) %>% head()

#Create data frames for each fiscal year that summarizes the product group codes total display 
#value and arrange it by descending order. Find the codes that are present in all 5 years using "Yall"

Y = Top3 %>% group_by(product_line_group_code, fiscal_year_number) %>% summarise(Total=sum(display_value)) %>% arrange(-fiscal_year_number,-Total)
Y1 = Y %>% filter(fiscal_year_number==2022) %>% head(10)
Y2 = Y %>% filter(fiscal_year_number==2021) %>% head(10)
Y3 = Y %>% filter(fiscal_year_number==2020) %>% head(10)
Y4 = Y %>% filter(fiscal_year_number==2019) %>% head(10)
Y5 = Y %>% filter(fiscal_year_number==2018) %>% head(10)
Yall = rbind(Y1,Y2,Y3,Y4,Y5)
Yall = left_join(Yall,Yall %>% group_by(product_line_group_code) %>% summarise(n=n()),by="product_line_group_code")
Yall = Yall %>% spread(fiscal_year_number,total) %>% arrange(-n)

#Filter 3 codes that show up in all 5 years:
Top3.Codes = Top3 %>% filter(product_line_group_code %in% c("ANT","FCR","PEX"))
Top3.Codes$Unit_Cost = Top3.Codes$display_value/Top3.Codes$transaction_qty
Top3.Codes %>% arrange(-display_value) %>% head(10)

#See the display values for each group code:
Top3.Codes %>% group_by(reasoning) %>%  filter(product_line_group_code == "FCR") %>% summarise(sum=sum(display_value))
Top3.Codes %>% group_by(reasoning) %>%  filter(product_line_group_code == "PEX") %>% summarise(sum=sum(display_value))
Top3.Codes %>% group_by(reasoning) %>%  filter(product_line_group_code == "ANT") %>% summarise(sum=sum(display_value))

#Choose FCR due to large number of observations and high scrap value
Top3.Codes %>%  group_by(product_line_group_code) %>% summarise(n=n())

#2,457,235
Top3.Codes %>% filter(!(product_line_group_code %in% c("FCR"))) %>% summarise(total=sum(display_value))

#7,863,322
Top3.Codes %>% filter(product_line_group_code == "FCR") %>% summarise(total=sum(display_value))

#FCR coded SKUs make up 76.19% of the total display value sum.
(7863322/(7863322+2457235))*100

#Filter only the FCR product group code
FCR = Top3.Codes %>% filter(product_line_group_code == "FCR")

#Load in usage data:

qry = paste0("select * from sku_usage;")
usage = dbGetQuery(con, qry)

#lubridate the transaction_week_yyyymmdd column to make both scrap and usage able 
#to be seen over time:
usage$Date = ymd(usage$transaction_week_yyyymmdd)
usage$year = year(ymd(usage$transaction_week_yyyymmdd))
usage$Month = month(ymd(usage$transaction_week_yyyymmdd))
usage$yday = yday(usage$Date)
usage$Week = isoweek(usage$Date)
usage$Quarter = ifelse(usage$Week <= 13,1,ifelse(usage$Week > 13 & usage$Week<=26,2,ifelse(usage$Week > 26 & usage$Week<=39,3,4)))

#Load in inventory data
qry = paste0("select * from weekly_inventory_summary_ss;")
inventory = dbGetQuery(con, qry)

#lubridate the capture_date_yyyymmdd column to allow for inventory to be seen over time:
inventory$Date = ymd(inventory$capture_date_yyyymmdd)
inventory$year = year(ymd(inventory$capture_date_yyyymmdd))

#filter SKUs in inventory to only be FCR coded SKUs that were scrapped
inventory = inventory %>% filter(sku_number %in% FCR$sku_number)


#Attempt to relate SKU scrap, usage, and inventory data:
inventory$Year = substr(inventory$capture_date_yyyymmdd,1,4)
table(inventory$Year)

inventory$Date = ymd(inventory$capture_date_yyyymmdd)
inventory$year = year(ymd(inventory$capture_date_yyyymmdd))
inventory$Month = month(ymd(inventory$capture_date_yyyymmdd))
inventory$yday = yday(inventory$Date)
inventory$Week = isoweek(inventory$Date)
inventory$Quarter = ifelse(inventory$Week <= 13,1,ifelse(inventory$Week > 13 & inventory$Week<=26,2,ifelse(inventory$Week > 26 & inventory$Week<=39,3,4)))

inventory.sub = inventory %>% filter(year==2019)

table(inventory$year)

str(usage.sub)
usage.sub$usage = as.numeric(usage.sub$usage)
usage.sub.summary = usage.sub %>% group_by(sku_number, Week) %>% summarise(Usage=sum(usage))
Z=unique(usage.sub.summary$sku_number)[1]
usage.sub.summary %>% filter(sku_number==Z) %>% ggplot(aes(x=Week,y=Usage))+geom_line()

##SCRAP visuals
Prez  = DF %>% filter(sku_number != "A33502")

##Scrap Quantity by WXYZ
Prez %>% filter(fiscal_year_number <= 2022) %>% filter(branch_code=="US03")%>% ggplot(aes(x=fiscal_year_number, y=transaction_qty, fill=wxyz)) + geom_bar(stat="identity") +   theme(axis.text = element_text(face="bold"),axis.title.x = element_text(size=12, face="bold", colour = "black"),axis.title.y = element_text(size=12, face="bold", colour = "black"),legend.title = element_text(face = "bold"))+
  labs(x="Fiscal Year", y = "Scrap Volume")+ labs(fill = "WXYZ")+  ggtitle("Visualizing Scrap Volume for US03 - Filled by WXYZ")+theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  theme(text=element_text(family="Montserrat"))+ scale_fill_manual(values = c("#7fcdbb","#41b6c4","#1d91c0", "#225ea8")) + theme(legend.title.align=0.5) 

##Scrap Value by WXYZ
Prez %>% filter(fiscal_year_number <= 2022) %>% filter(branch_code=="US03")%>% ggplot(aes(x=fiscal_year_number, y=display_value, fill=wxyz)) + geom_bar(stat="identity") +   theme(axis.text = element_text(face="bold"),axis.title.x = element_text(size=12, face="bold", colour = "black"),axis.title.y = element_text(size=12, face="bold", colour = "black"),legend.title = element_text(face = "bold"))+
  labs(x="Fiscal Year", y = "Scrap Value ($)")+ labs(fill = "WXYZ")+  ggtitle("Visualizing Scrap Value for US03 - Filled by WXYZ")+theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  theme(text=element_text(family="Montserrat"))+ scale_fill_manual(values = c("#7fcdbb","#41b6c4","#1d91c0", "#225ea8")) + theme(legend.title.align=0.5) 

colourCount = length(unique(Prez$reasoning))
getPalette = colorRampPalette(brewer.pal(11, "YlGnBu"))

##Scrap Quantity by Reason
Prez %>% filter(fiscal_year_number <= 2022) %>% filter(branch_code=="US03")%>% ggplot(aes(x=fiscal_year_number, y=transaction_qty, fill=reasoning)) + geom_bar(stat="identity") +   theme(axis.text = element_text(face="bold"),axis.title.x = element_text(size=12, face="bold", colour = "black"),axis.title.y = element_text(size=12, face="bold", colour = "black"),legend.title = element_text(face = "bold"))+
  labs(x="Fiscal Year", y = "Scrap Volume")+ labs(fill = "Reason")+  ggtitle("Visualizing Scrap Volume for US03 for Z Strangers - Filled by Reason")+theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  theme(text=element_text(family="Montserrat"))+ scale_fill_manual(values = getPalette(colourCount)) + theme(legend.title.align=0.5) 

##Scrap Value by Reason
Prez %>% filter(fiscal_year_number <= 2022) %>% filter(branch_code=="US03")%>% ggplot(aes(x=fiscal_year_number, y=display_value, fill=reasoning)) + geom_bar(stat="identity") +   theme(axis.text = element_text(face="bold"),axis.title.x = element_text(size=12, face="bold", colour = "black"),axis.title.y = element_text(size=12, face="bold", colour = "black"),legend.title = element_text(face = "bold"))+
  labs(x="Fiscal Year", y = "Scrap Value ($)")+ labs(fill = "Reason")+  ggtitle("Visualizing Scrap Value for US03 for Z Strangers - Filled by Reason")+theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  theme(text=element_text(family="Montserrat"))+ scale_fill_manual(values = getPalette(colourCount)) + theme(legend.title.align=0.5) 




###Overall reasoning for scrap value and qunatity

DF1  = DF %>% filter(sku_number != "A33502")

DF2 = DF1 %>% select("fiscal_year_number","reasoning","display_value","transaction_qty")

#The top three reasons for scrap value were Forecasting, NULL, and Overbought, 
#collectively accounting for over 61% of all scrap value. Similarly, the top three 
#reasons for scrap quantity were Overbought, Raw Material, and Defects, representing 
#quantities exceeding 80 million and accounting for 62% of all scrap quantity. 
DF2.summary = DF2 %>% group_by(reasoning) %>% summarize(TotalVal=sum(display_value),TotalQ=sum(transaction_qty),.groups="drop") 
DF2.summary = DF2.summary %>% mutate(PercVal=TotalVal/sum(TotalVal)*100,PercQ=TotalQ/sum(TotalQ)*100)
DF2.summary.Val = DF2.summary %>% arrange(-PercVal)
DF2.summary.Val = data.frame(DF2.summary.Val)
DF2.summary.Q = DF2.summary %>% arrange(-PercQ)
DF2.summary.Q = data.frame(DF2.summary.Q)

#another method 
DF3 = DF2 %>% group_by(reasoning) %>% summarise(Value=sum(display_value)) %>% arrange(-Value)
DF3 = data.frame(DF3)
DF4 = DF3 %>% mutate(Percentage = scales::label_percent()(Value / sum(Value))) 

#look annually at the reasoning for value and quantity
DF2.summary = DF2 %>% group_by(fiscal_year_number,reasoning) %>% summarize(TotalVal=sum(display_value),TotalQ=sum(transaction_qty),.groups="drop") 
DF2.summary = DF2.summary %>% group_by(fiscal_year_number) %>% mutate(PercVal=TotalVal/sum(TotalVal)*100,PercQ=TotalQ/sum(TotalQ)*100)
DF2.summary.Val = DF2.summary %>% arrange(fiscal_year_number,-PercVal)
DF2.summary.Val = data.frame(DF2.summary.Val)
DF2.summary.Q = DF2.summary %>% arrange(fiscal_year_number,-PercQ)
DF2.summary.Q = data.frame(DF2.summary.Q)

#export excel
write_xlsx(DF2.summary.Val,"Value.xlsx")
write_xlsx(DF2.summary.Q,"Quantity.xlsx")


