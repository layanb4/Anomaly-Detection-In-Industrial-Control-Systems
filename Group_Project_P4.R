getwd()
setwd("/Users/CMPT 318")

set.seed(2)

library("depmixS4")
library("dplyr")
library('ggbiplot')
library('lubridate')
library('stats')
library("zoo")

# Clean dataset
df <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
df[df == "?"] <- NA


# Turn into numerics so that the approx() fn works
# approx() interpolates information
df$Global_active_power <- as.numeric(df$Global_active_power)
df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
df$Voltage <- as.numeric(df$Voltage)
df$Global_intensity <- as.numeric(df$Global_intensity)
df$Sub_metering_1 <- as.numeric(df$Sub_metering_1)
df$Sub_metering_2 <- as.numeric(df$Sub_metering_2)
df$Sub_metering_3 <- as.numeric(df$Sub_metering_3)

df$Global_active_power <- na.approx(df$Global_active_power)
df$Global_reactive_power <- na.approx(df$Global_reactive_power)
df$Voltage <- na.approx(df$Voltage)
df$Global_intensity <- na.approx(df$Global_intensity)
df$Sub_metering_1 <- na.approx(df$Sub_metering_1)
df$Sub_metering_2 <- na.approx(df$Sub_metering_2)
df$Sub_metering_3 <- na.approx(df$Sub_metering_3)

df[is.na(df)] <- 0
df = df %>% mutate_at(c('Global_active_power', 'Global_reactive_power', 'Voltage', 'Global_intensity', 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), ~(scale(.) %>% as.vector))


# Part 2

# Format date_times as seconds since Jan. 1, 1970
# Have to set the timezone for POSIX as some date_times 
# will not exist for certain times zones (daylight savings)
df$date_time <- as.POSIXct(paste(df$Date, df$Time), 
                           format="%d/%m/%Y %H:%M:%S",
                           tz = "America/Chicago")
# Temporary fix since idk the time zone and can't be asked to find it out
df <- na.omit(df)

print(which(rowSums(is.na(df)) > 0))
df$date_time <- as.numeric(df$date_time)
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
df$day_type <- weekdays(df$Date)
df$Time.numeric <- as.numeric(hms(df$Time))

# Only take Saturday and only take from times 5:30pm - 7:30pm
df <- df[df$day_type == "Saturday",]
df <- df[df$Time.numeric >= as.numeric(hms(("17:30:00"))) & df$Time.numeric < as.numeric(hms(("19:30:00"))),]

# Partition data, and order the training data
df.train <- df[df$date_time < as.numeric(as.POSIXct("2010-01-01")),]
df.test <- df[df$date_time >= as.numeric(as.POSIXct("2010-01-01")),]

ntimesvector <- rep(120, 159)
ntimesfortest <- rep(120, 47)


#best model
modeltraining <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                 data = df.train,
                 nstates = 13,
                 family = list(gaussian(), gaussian()),
                 ntimes = ntimesvector
)

fittedmodel<- fit(modeltraining)
logliketrain <- forwardbackward(fittedmodel, df.train, return.all = FALSE, useC = FALSE)
logliketraining <- logliketrain$logLike / 19080
print(logliketraining)


modNew <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1), 
                  data=df.test, nstates= 13,
                  family= list(gaussian(),gaussian()), ntimes= ntimesfortest)


modNew <- setpars(modNew, getpars(fittedmodel))
loglikefortest <- forwardbackward(modNew, df.test, return.all = FALSE, useC = FALSE)
print(loglikefortest$logLike/5640)

#getting all of the 10 subsets
df.subtest1 <- df.test[df.test$date_time >= as.POSIXct("2010-01-01") & 
                         df.test$date_time <= as.POSIXct("2010-01-31"), ]


df.subtest2 <- df.test[df.test$date_time > as.POSIXct("2010-02-01") & 
                         df.test$date_time <= as.POSIXct("2010-03-7"), ]


df.subtest3 <- df.test[df.test$date_time > as.POSIXct("2010-03-13") & 
                         df.test$date_time <= as.POSIXct("2010-04-11"), ]

df.subtest4 <- df.test[df.test$date_time > as.POSIXct("2010-04-16") & 
                         df.test$date_time <= as.POSIXct("2010-05-16"), ]



df.subtest5 <- df.test[df.test$date_time > as.POSIXct("2010-05-16") & 
                         df.test$date_time <= as.POSIXct("2010-06-20"), ]



df.subtest6 <- df.test[df.test$date_time > as.POSIXct("2010-06-20") & 
                         df.test$date_time <= as.POSIXct("2010-07-25"), ]


df.subtest7 <- df.test[df.test$date_time > as.POSIXct("2010-07-25") & 
                         df.test$date_time <= as.POSIXct("2010-08-29"), ]




df.subtest8 <- df.test[df.test$date_time > as.POSIXct("2010-08-29") & 
                         df.test$date_time <= as.POSIXct("2010-10-03"), ]


df.subtest9 <- df.test[df.test$date_time > as.POSIXct("2010-10-03") & 
                         df.test$date_time <= as.POSIXct("2010-11-07"), ]


df.subtest10<- df.test[df.test$date_time > as.POSIXct("2010-11-07") & 
                         df.test$date_time <= as.POSIXct("2010-11-21"), ]



ntimesformostweeks <- rep(120, 5)
ntimesforlastweek <- rep(120, 2)

anomlylogs <- c()
modelanom1 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest1,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom1 <- setpars(modelanom1, getpars(fittedmodel))
lgl1 <- forwardbackward(modelanom1, df.subtest1, return.all = FALSE, useC = FALSE)
print(lgl1$logLike /600)
anomlylogs <- c(anomlylogs, lgl1$logLike/600)

modelanom2 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest2,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom2 <- setpars(modelanom2, getpars(fittedmodel))
lgl2 <- forwardbackward(modelanom2, df.subtest2, return.all = FALSE, useC = FALSE)
print(lgl2$logLike /600)
anomlylogs <- c(anomlylogs, lgl2$logLike/600)


modelanom3 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest3,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom3 <- setpars(modelanom3, getpars(fittedmodel))
lgl3 <- forwardbackward(modelanom3, df.subtest3, return.all = FALSE, useC = FALSE)
print(lgl3$logLike/600)
anomlylogs <- c(anomlylogs, lgl3$logLike/600)



modelanom4 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest4,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom4 <- setpars(modelanom4, getpars(fittedmodel))
lgl4 <- forwardbackward(modelanom4, df.subtest4, return.all = FALSE, useC = FALSE)
print(lgl4$logLike /600)
anomlylogs <- c(anomlylogs, lgl4$logLike/600)




modelanom5 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest5,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom5 <- setpars(modelanom5, getpars(fittedmodel))
lgl5 <- forwardbackward(modelanom5, df.subtest5, return.all = FALSE, useC = FALSE)
print(lgl5$logLike /600)
anomlylogs <- c(anomlylogs, lgl5$logLike/600)




modelanom6 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest6,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom6 <- setpars(modelanom6, getpars(fittedmodel))
lgl6 <- forwardbackward(modelanom6, df.subtest6, return.all = FALSE, useC = FALSE)
print(lgl6$logLike /600)
anomlylogs <- c(anomlylogs, lgl6$logLike/600)



modelanom7 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest7,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom7 <- setpars(modelanom7, getpars(fittedmodel))
lgl7 <- forwardbackward(modelanom7,df.subtest7, return.all = FALSE, useC = FALSE)
print(lgl7$logLike /600)
anomlylogs <- c(anomlylogs, lgl7$logLike/600)



modelanom8 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest8,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)

modelanom8 <- setpars(modelanom8, getpars(fittedmodel))
lgl8 <- forwardbackward(modelanom8, df.subtest8, return.all = FALSE, useC = FALSE)
print(lgl8$logLike /600)
anomlylogs <- c(anomlylogs, lgl8$logLike/600)


modelanom9 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest9,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesformostweeks
)
View(df.subtest9)

modelanom9 <- setpars(modelanom9, getpars(fittedmodel))
lgl9 <- forwardbackward(modelanom9, df.subtest9, return.all = FALSE, useC = FALSE)
print(lgl9$logLike /600)
anomlylogs <- c(anomlylogs, lgl9$logLike/600)



modelanom10 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                     data = df.subtest10,
                     nstates = 13,
                     family = list(gaussian(),gaussian()),
                     ntimes = ntimesforlastweek
)

modelanom10 <- setpars(modelanom10, getpars(fittedmodel))
lgl10 <- forwardbackward(modelanom10, df.subtest10, return.all = FALSE, useC = FALSE)
print(lgl10$logLike /240)
anomlylogs <- c(anomlylogs, lgl10$logLike/240)

View(anomlylogs)


logsfortestsubets <- data.frame("Subset Number" = c(1,2,3,4,5,6,7,8,9,10),
                               "Log Likelhood Value" = anomlylogs)

View(logsfortestsubets)
print(logliketraining)


anomlyvariance<- c()
for(i in 1:10){
  anomlydifference<-(-1.1376420)- anomlylogs[i]
  anomlyvariance<- c(anomlyvariance,  anomlydifference)
  
  
}
View(anomlyvariance)

differencelist<- c()
for(i in 1:10){
  differenceabs<- abs(anomlyvariance[i])
  differencelist<- c(differencelist,differenceabs )
}


loglikdifference <- data.frame("Subset Number" = c(1,2,3,4,5,6,7,8,9,10),
                     Variance = differencelist)

View(loglikdifference)


trainvstestloglikelhood <-data.frame("States" = c(11,12,13,14),
                                     Trainloglikelhood = c(logliketraining1,logliketraining2,
                                                           logliketraining,logliketraining4),
                                      Testloglikelhood = c(loglikefortest1$logLike/5640,
                                                           loglikefortest2$logLike/5640,
                                                           loglikefortest$logLike/5640,
                                                           loglikefortest4$logLike/5640))
View(trainvstestloglikelhood)

                     
# 
library(ggplot2)
anomlydataframe <- data.frame(set = c("1", "2", "3","4", "5", "6", "7", "8", "9","10"), loglikelhood = anomlylogs)
ggplot(anomlydataframe, aes(x = set, y = loglikelhood)) +
  labs(title = "Loglikelhood Anomalies",
        x = "Dataset",
        y = "Loglikelhoodvalues",) +
  geom_hline(yintercept = -1.1376420, color = "red")+

  geom_point()



