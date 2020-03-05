#read csv
data_breaches <- read.csv("C:/Users/Abhi/Downloads/breaches.csv", header=T, sep=",")

#maximum Affected people
max_affected = arrange(data_breaches, desc(Individuals_Affected))
max_affected

#filter for year and State
t1 = data_breaches %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(year =="2009")
t1_ca = t1%>%select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(State %in% c("CA","TX"))
t2 = data_breaches %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(year=="2010")
t2_ca = t2%>%select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(State %in% c("CA","TX"))
t3 = data_breaches %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(year=="2013")
t3_ca = t3%>%select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(State %in% c("CA","TX"))
t3_ca


#histogram 
histogram_year = ggplot(data  = data_breaches, aes( x = data_breaches$year)) + geom_histogram(bins = 30,col="red",fill="darkgreen") + xlim(2008,2015)
histogram_year + labs(title = "Reported incident per year" , x ="Year" , y="Number of incidents")

#bar plot
p = ggplot(t1, aes(x = "" , fill = factor(Location_of_Breached_Information))) + geom_bar(color="black")
p
p + labs(title = "Gear types", x = "Values of cyl" , y = "Values of total gear")

#adding new column
mydata1 = mutate(t1_ca, Affected_people_increased2010=(sum(t2_ca$Individuals_Affected)-sum(t1_ca$Individuals_Affected)))
mydata2 = mutate(mydata1, Affected_people_increased2013=(sum(t3_ca$Individuals_Affected)-sum(t2_ca$Individuals_Affected)))
mydata2


t = max_affected %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information) %>% filter(Location_of_Breached_Information =="Network Server")
t1 = data_breaches %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year) %>% filter(year
 =="2009")



#pie chart
p = ggplot(t1, aes(x = "" , fill = factor(Location_of_Breached_Information))) + geom_bar(color="black")
p
p + labs(title = "Gear types", x = "Values of cyl" , y = "Values of total gear")
pie <- p + coord_polar("y", start=0)
pie

#pie chart2
t2 = data_breaches %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year) %>% filter(year=="2014")
p2 = ggplot(t2, aes(x = "" , fill = factor(Location_of_Breached_Information))) + geom_bar(color="black")
p2
pie2 <- p2 + coord_polar("y", start=0)
pie2

#pie chart3
t4a = data_breaches %>% select(Type_of_Breach,year) %>% filter(year=="2014")
p2a = ggplot(t4a, aes(x = "" , fill = factor(Type_of_Breach))) + geom_bar(color="black")
p2a
pie3a <- p2a + coord_polar("y", start=0)
pie3a

#pie chart 4
t5a = data_breaches %>% select(Type_of_Breach,year) %>% filter(year=="2009")
p3a = ggplot(t5a, aes(x = "" , fill = factor(Type_of_Breach))) + geom_bar(color="black")
p3a
pie4a <- p3a + coord_polar("y", start=0)
pie4a

p3 = ggplot(data_breaches, aes(x = data_breaches$State  )) + geom_bar(color="blue")
p3
pie_state <- p3 + coord_polar("y", start=0)
pie_state

#filter
t5 = data_breaches %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(State %in% c("TX","CA","GA"))
t52009 = t5 %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(year %in% c("2009","2014"))
t52014 = data_breaches %>% select(Name_of_Covered_Entity, Individuals_Affected, Date_of_Breach, Location_of_Breached_Information,year,State) %>% filter(year %in% c("2014"))

t50914 = mutate(t5, data_dif= sum(t52014$Individuals_Affected)-sum(t52009$Individuals_Affected))
t50914 = mutate(t50914, data_dif2014= sum(t52014$Individuals_Affected)-sum(t52009$Individuals_Affected))
t50914

by_cyl <- data_breaches %>% group_by(State)

state_count = mydata2 %>% summarise(
  disp = sum(Individuals_Affected),
)

#bar chart with x and y
p3 = ggplot(t52009, aes(x = t52009$State, y = t52009$Individuals_Affected,fill=round(t52009$year)))+ geom_col()
p3
m10 <- max_affected[0:10,]
ggplot(data=m10, aes(x=m10$Name_of_Covered_Entity)) + geom_bar(color="red") + ylim(0,2)
p4
p + geom_bar(stat="identity", position=position_dodge())

p5 = ggplot(max_affected[0:10,], aes(x = max_affected[0:10,]$State, y = max_affected[0:10,]$Individuals_Affected))+ geom_col()
p5

#bar plot 2010 vs 2012
test1=data_breaches %>% select(State,year) %>% filter(State %in% c("TX","CA","NY","GA","VA","NJ","FL")) %>% filter(year %in% c("2012")) %>% group_by(State) %>% summarise(count=n())
test2=data_breaches %>% select(State,year) %>% filter(State %in% c("TX","CA","NY","GA","VA","NJ","FL")) %>% filter(year %in% c("2010")) %>% group_by(State) %>% summarise(count=n())

test1a=mutate(test1,((test2$count-test1$count)/test1$count)*100)
ptest = ggplot(test1a, aes(x = test1a$State, y = test1a$`((test2$count - test1$count)/test1$count) * 100`))+ geom_col()
ptest + labs(title = "2012 vs 2010", x = "States" , y = "Percentage increase or decrease in breaches")


