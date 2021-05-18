# creatig covid table that will be used to create graphs
Covid<- read.csv("/Users/Michael/Library/Mobile Documents/com~apple~CloudDocs/Download.Data/Covid/Country_vaccinations.csv")
Covid<- Covid[,c("country", "total_vaccinations", "date", "people_vaccinated", "daily_vaccinations_raw",
                "people_vaccinated_per_hundred", "daily_vaccinations_per_million", "vaccines")]
Covid$date <- as.Date(Covid$date)

Covid$total_vaccinations[which(is.na(Covid$total_vaccinations))] <-0
Covid$people_vaccinated[which(is.na(Covid$people_vaccinated))] <-0
Covid$daily_vaccinations_raw[which(is.na(Covid$daily_vaccinations_raw))] <-0
Covid$people_vaccinated_per_hundred[which(is.na(Covid$people_vaccinated_per_hundred))] <-0
Covid$daily_vaccinations_per_million[which(is.na(Covid$daily_vaccinations_per_million))] <-0



head <- Covid[sample(1:nrow(Covid),5), ]
head[order(head$Covid),]

Covid$month <- month(Covid$date)
Covid$weekday <- weekdays(Covid$date)
Covid$percent_people <- Covid$people_vaccinated_per_hundred/100

# comapring US,Israel, South K
US <- Covid %>% group_by(date,country) %>% filter(country=="United States")
Israel <- Covid %>% group_by(date, country) %>% filter(country=="Israel")
Russia<- Covid %>% group_by(date, country) %>% filter(country=="Russia")
ggplot()+
  geom_line(data=Covid, aes(date, daily_vaccinations_raw, group= country),size=.6, colour= " grey71" )+
  geom_line(data=Israel, aes(date,daily_vaccinations_raw),size= .8, colour= "red2")+
  geom_line(data = US,aes(date,daily_vaccinations_raw), size= .8,colour= "blue3")+
  geom_line(data = Russia, aes(date, daily_vaccinations_raw),size=.8, colour="cyan1")

