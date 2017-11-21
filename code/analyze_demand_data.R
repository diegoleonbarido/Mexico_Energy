library(ggplot2)

data_so_far <- read.csv('/Users/diego/Desktop/Projects_Code/niuera-data/scraping/MX/scripts/data_cenace.csv')
names(data_so_far) <- c('hour','demand','cnx','generation','forecast','day','month','year','region')


#date
data_so_far$date <- as.Date(with(data_so_far, paste(year, month, day,sep="-")), "%Y-%m-%d")


data1 <- subset(data_so_far,data_so_far$date>= '2016-01-01' &  data_so_far$date < '2017-04-01')
data1 <- subset(data1,data1$demand < 1000000)
length(data1$hour)


##### Aggregates

data1_agg <- aggregate(data1$demand,by=list(data1$region,data1$hour),FUN=mean,na.rm=TRUE)
data1_agg <- subset(data1_agg,data1_agg$Group.1 != 'mexico')

ggplot(data1_agg,aes(Group.2,x,colour=Group.1)) + geom_line(aes(group = Group.1)) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Hour") + ylab("MW")

##### Plot of Months

unique_df <- as.data.frame(unique(data1[,c('month','year'),]))

data_sub <- subset(data1,data1$region!='mexico')
i<-1
this_df <- subset(data_sub,data_sub$month == '3' & data_sub$year == '2017')
data1_agg <- aggregate(this_df$demand,by=list(this_df$region,this_df$hour),FUN=mean,na.rm=TRUE)
ggplot(data1_agg,aes(Group.2,x,colour=Group.1)) + geom_line(aes(group = Group.1)) + theme_bw() +theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + theme(legend.position="bottom") + xlab("Hour") + ylab("MW")


write.csv(data1,'/Users/diego/Desktop/Projects_Code/Mexico/data/data1.csv')





