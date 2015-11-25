###Final Project Prototype
###Julia Bowling
###Data Visualization Nov. 24, 2015
library("ggplot2")
library("ggplot2")
library("tidyr")
library("dplyr")
###Deportations: http://www.dhs.gov/publication/yearbook-immigration-statistics-2013-enforcement-actions
###Imprisonment: http://www.bjs.gov/index.cfm?ty=nps
migrant <- read.csv("~/Desktop/juliacbowling.github.io/migrant.csv")
View(migrant)
head(migrant)
migrant$northborder <- migrant$BP.North+migrant$HS.North+migrant$ER.North
migrant$southborder <- migrant$BP.South+migrant$HS.South+migrant$ER.South
head(migrant)
plot1 <- ggplot(migrant, aes(x=Year)) + 
  geom_line(aes(y=PROGRAM.Total), color="#E63227") + 
  geom_line(aes(y=northborder), color="#0C5B15") + 
  geom_line(aes(y=southborder), color="#293BE5") + ggtitle("Undocumented People Deported 2004-2013") + 
  theme(plot.title = element_text(family="Helvetica", face ="bold", size=18, hjust=0))
print(plot1)
ggsave("border.pdf", plot1)
plot2 <- ggplot(migrant, aes(x=Year)) + 
  geom_line(aes(y=Total), color="#E63227") + 
  geom_line(aes(y=North.America), color="#0C5B15") + 
  geom_line(aes(y=South.America), color="#293BE5") + ggtitle("Nationalities of Undocumented People Deported 2004-2013") + 
  theme(plot.title = element_text(family="Helvetica", face ="bold", size=18, hjust=0))
print(plot2)
ggsave("nation.pdf", plot2)
migrant <- mutate(migrant, na_percent = (North.America/Total))
migrant <- mutate(migrant, sa_percent = (South.America/Total))
migrant <- mutate(migrant, af_percent = (Africa/Total))
migrant <- mutate(migrant, as_percent = (Asia/Total))
migrant <- mutate(migrant, eu_percent = (Europe/Total))
migrant <- mutate(migrant, oc_percent = (Oceania/Total))
migrant <- mutate(migrant, ukn_percent = (Unknown.1/Total))
migrant <- mutate(migrant, am_percent = ((North.America+South.America)/Total))
migrant2013 <- subset(migrant, Year==2013, drop=FALSE)
head(migrant2013)
plot3 <- ggplot(migrant2013, aes(x=Year), colorRamp()) + 
  geom_bar(stat="identity", aes(y=1), fill='grey') +
  geom_bar(stat="identity", aes(y=na_percent), fill='red') + coord_flip() + ggtitle("Origins of Undocumented People Deported 2004-2013") + 
  theme(plot.title = element_text(family="Helvetica", face ="bold", size=16, hjust=0))
print(plot3)
###na_percent == 0.9507957
ggsave("percent_northamerican.pdf", plot3, width = 8, height = 4)
plot4 <- ggplot(migrant2013, aes(x=Year), colorRamp()) + 
  geom_bar(stat="identity", aes(y=1), fill='grey') +
  geom_bar(stat="identity", aes(y=am_percent), fill='red') + coord_flip() + ggtitle("Origins of Undocumented People Deported 2004-2013") + 
  theme(plot.title = element_text(family="Helvetica", face ="bold", size=16, hjust=0))
print(plot4)
ggsave("percent_american.pdf", plot4, width = 8, height = 4)
###am_percent == 0.9696022
plot6 <- ggplot(migrant, aes(x=Year)) + 
  geom_area(aes(y=Mexico), color="purple") + 
  geom_area(aes(y=Guatemala), color="green") +
  geom_area(aes(y=Honduras), color="yellow") + 
  geom_area(aes(y=El.Salvador), color="blue") + 
  geom_area(aes(y=Ecuador), color="red") + ggtitle("Deportations: Top Countries of Origin, 2004-2013") + 
  theme(plot.title = element_text(family="Helvetica", face ="bold", size=16, hjust=0))
print(plot6)
ggsave("top_deporations.pdf", plot6)
plot7 <- ggplot(migrant, aes(x=Year)) + 
  geom_area(aes(y=Guatemala), color="green") +
  geom_area(aes(y=Honduras), color="yellow") + 
  geom_area(aes(y=El.Salvador), color="blue") + 
  geom_area(aes(y=Ecuador), color="red") + ggtitle("Deportations: Top Rising Countries of Origin, 2004-2013") + 
  theme(plot.title = element_text(family="Helvetica", face ="bold", size=16, hjust=0))
print(plot7)
ggsave("top_deporations_nomex.pdf", plot7)
removal_history <- read.csv("~/Desktop/juliacbowling.github.io/removal_history.csv")
View(removal_history)
head(removal_history)
removal1978 <- subset(removal_history, Year>1977, drop = FALSE)
plot5 <- ggplot(removal1978, aes(x=Year)) + 
  geom_point(aes(y=Removals), color="purple")+ geom_smooth(aes(y=Removals), color="purple") + geom_point(aes(y=Returns), color="green")+geom_smooth(aes(y=Returns), color="green") + 
  geom_point(aes(y=im_count), color="blue") + geom_smooth(aes(y=im_count), color="blue") + ggtitle("Imprisonment & Deportation 2004-2013") + 
  theme(plot.title = element_text(family="Helvetica", face ="bold", size=16, hjust=0))
print(plot5)
ggsave("im_dep.pdf", plot5)