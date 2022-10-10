library(dplyr)
library (ggplot2)
library (lubridate)
library(stringr)
library(reshape2)

setwd("D://books//TT//assignment")
covid_dat=as.data.frame( read.csv("anonymized-covid19 data.csv"))


#Q2 Plot userid and msg percent, and check most frequent user


# Group by count using dplyr
userMsgPercent_df <-as.data.frame( covid_dat %>% group_by(User.ID) %>% 
  summarise(percent = 100 * n() / nrow(covid_dat),
            .groups = 'drop'))
userMsgPercent_df

plot(userMsgPercent_df$User.ID,userMsgPercent_df$percent,xlab="User Id",ylab="Messages percent", ,type = "o" ,main="User Msgs Percentages",pch=13)
#The most active user according to graph shows user id 8 as the highest peak is for user id 8.


#Q3 ordered plot according to msgs percentage
ggplot(userMsgPercent_df,aes(x=reorder(User.ID,-percent),y=percent,group=1 ))+ geom_point(col=userMsgPercent_df$percent,size=2)+ geom_line()+labs(title="Descending User Msgs Percentages")+xlab("User id")+ylab("Msg Percentage")


#Q4 Trend after endorsement
covid_dat_march=covid_dat
covid_dat_march$Date <- mdy(covid_dat$Date)
result <- subset(covid_dat_march, format(Date, '%m') == '03')

#Getting all msgs having chloroquine or hydroxychloroquine
chloroquine_hydroxychloroquine_Msgs <- as.numeric(str_detect(tolower(result$Message), "chloroquine|hydroxychloroquine"))
med_df=data.frame(result$Date,chloroquine_hydroxychloroquine_Msgs)

med_msg_count <-as.data.frame( med_df %>% group_by(result.Date) %>% 
                                     summarise(across(everything(), sum),
                                               .groups = 'drop'))
med_msg_count
ggplot(data=med_msg_count, aes(x=result.Date , y=chloroquine_hydroxychloroquine_Msgs)) +
  geom_line()+geom_smooth(method="loess",col="firebrick",size=1)+
  geom_point()+xlab("Month")+ylab("Medicine Messages Count")+title("Medicine Discussion Trend")

#Getting msgs data having elon or trump discussion too 
elon_trump <- as.numeric(str_detect(tolower(result$Message), "elon|trump"))
elon_med_df=data.frame(result$Date,chloroquine_hydroxychloroquine_Msgs,elon_trump)

med_elon_count <-as.data.frame( elon_med_df %>% group_by(result.Date) %>% 
                                      summarise(across(everything(), sum),
                                                .groups = 'drop'))
med_elon_count
d <- melt(med_df, id.vars="result.Date")

# Everything on the same plot medicines and elon and trump data both on same plot
ggplot(d, aes(result.Date,value, col=variable)) + 
  geom_point() + geom_line()+
  stat_smooth()


#Plotting medicine msgs in whole dataset
msgscounts <- as.data.frame(apply(result, 1, function(x) length(which(grepl("Chloroquine",result$Message,ignore.case=TRUE)))))
View(msgscounts)
med_rows=covid_dat %>% filter(grepl("Chloroquine",covid_dat$Message,ignore.case=TRUE) |grepl("Hydroxychloroquine",covid_dat$Message,ignore.case=TRUE) )
med_rows=med_rows[c('Date','Message')]
dates=mdy(med_rows$Date)
med_rows$Date=dates
med_rows_count <-as.data.frame( discussion_rows %>% group_by(Date) %>% 
                                     summarise(Msg_count = n(),
                                               .groups = 'drop'))
med_rows_count
ggplot(data=med_rows_count, aes(x=as.Date(Date) , y=Msg_count)) +
  geom_line()+
  geom_point(col=med_rows_count$Msg_count) +geom_smooth(method="lm",col="firebrick",size=1)+xlab("Month")+ylab("Medicine Messages Count")+title("Medicine Discussion Trend")
