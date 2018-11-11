library(lubridate)
library(dplyr)
library(ggplot2)
bake <- read.csv('/Users/rendongmin/Desktop/IMC/BreadBasket_DMS.csv',stringsAsFactors = )
bake$Date <- ymd(bake$Date)
bake$Time <- hms(bake$Time)
bake$ts <- with(bake, as.POSIXct(paste(Date, Time)))
bake$h <- hour(bake$Time)
bake$m <- month(bake$Date)
bake$y <- year(bake$Date)
summary(bake)
bake = subset(bake,Item !='NONE')

#Transaction Time Series
bts = bake %>% group_by(y,m) %>% summarise(n_t = length(unique(Transaction)))
tran_ts = bake %>% group_by(Date) %>% summarise(n_t=length(unique(Transaction)))
ts = ts(tran_ts$n_t)
plot(ts)
acf(ts,lag=70)
pacf(ts,lag=70)

#Product Transaction
bfreq = bake %>% group_by(Item) %>% summarise(n = n(), p = n/nrow(bake)) %>% arrange(desc(n))
btop = bfreq[1:12,]
ggplot(btop,aes(x = reorder(Item, n),y=n))+geom_col() + coord_flip() +
  labs(x='Items',y='Number of Sales',title='Total Sales of Bakery Products (Top 12)')

toplist = bfreq$Item[7:12]
btop = bake %>% select(Transaction,Item,h,m,y) %>% 
                filter(Item %in% toplist) %>% filter(m != 10 & m != 4) %>%
                group_by(y,m,Item) %>% summarize(n=n()) %>% arrange(Item, y, m)
btop$date <- ymd(paste(btop$y, btop$m,1, sep="/"))
ggplot(btop,aes(x=date,y=n,color=Item)) + geom_line(lwd=0.8) + geom_point() + 
  facet_wrap(Item~.) + labs(x='Date',y='Number of Sales',title='Product Sales by Months (Top 7-12)')

hour_plot <- function(x){
  title = as.character(x)
  sdata <- subset(bake,Item ==x)
  avgdata <- sdata %>% group_by(Date,h) %>% summarise(n = n()) %>% group_by(h) %>% summarise(sum_n = mean(n))
  plot(avgdata,type='b',main=title)
}

toplist = bfreq$Item[7:12]
sdata <- subset(bake,Item %in% toplist) 
avgdata <- sdata %>% group_by(Date,h,Item) %>% summarise(n = n()) %>% group_by(h,Item) %>% filter(h>=8 & h<=18) %>% summarise(avg_n = mean(n))
ggplot(avgdata,aes(x=h,y=avg_n,color=Item)) + geom_line(lwd=0.8) + geom_point() + 
  facet_wrap(Item~.) + labs(x='Hour',y='Average Number of Sales',title='Product Sales by Hour (Top 7-12)')

library(arules)
library(arulesViz)
library(plyr)
bake1 <- bake %>% select(Date,Transaction,Item) %>% filter(Item != 'Coffee')
#bake1 <- bake
transactionData <- ddply(bake1,c("Transaction","Date"),
                         function(df1)paste(df1$Item,
                                            collapse = ","))
titem <- as.data.frame(transactionData$V1)
colnames(titem) <- c("items")
write.csv(titem,"/Users/rendongmin/Desktop/IMC/bakeitem.csv", quote = FALSE, row.names = TRUE)
tdata <- read.transactions("/Users/rendongmin/Desktop/IMC/bakeitem.csv", format = 'basket', sep=',')

association.rules <- apriori(tdata, parameter = list(supp=0.001, conf=0.4,maxlen=10))
inspect(association.rules)
top10subRules <- head(association.rules, n = 10, by = "lift")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

