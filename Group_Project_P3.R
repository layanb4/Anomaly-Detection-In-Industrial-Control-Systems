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
View(df)

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
df <- df[df$Time.numeric >= as.numeric(hms(("17:30:00"))) 
         & df$Time.numeric < as.numeric(hms(("19:30:00"))),]

# Partition data
df.train <- df[df$date_time < as.numeric(as.POSIXct("2010-01-01")),]
df.test <- df[df$date_time >= as.numeric(as.POSIXct("2010-01-01")),]
print(nrow(df.train))
print(nrow(df.test))

ntimesvector <- rep(120, 159)
train_results <- data.frame(states = integer(), BIC = numeric(), log_lik = numeric())
fittedlist <- list()
df.test <- df.test[df.test$Global_intensity <= 8.3,]
# Loop through each number of states
# Only every 4 to save time
for (n in seq(4,20,4)) {
  model <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                  data = df.train,
                  nstates = n,
                  family = list(gaussian(), gaussian()),
                  ntimes = ntimesvector
                  )
  fitted_model <- fit(model)
  fitted_model.data = data.frame(states = n,
                                 BIC = BIC(fitted_model),
                                 log_lik = logLik(fitted_model)
                                 )

  train_results <- rbind(train_results, fitted_model.data)
}

View(train_results)

# # Generate the plot
plot(seq(4,20,4), train_results$BIC, type = "b", ylim = range(c(train_results$BIC, train_results$log_lik)),
     xlab = " Number of States", ylab = "BIC/Log-Likelihood", main = "BIC and Log-Likelihood Comparison")

# Add log-likelihood values to the plot
points(seq(4,20,4), train_results$log_lik, col = "red", type = "b")

# Add legend in the top left corner
# legend("topright", legend = c("BIC", "Log-Likelihood"), col = c("black", "red"), pch = 1)

# After iterating through each state, find the best few (elbow) and test them
# 12 states was the winner, check around it to make sure

for (n in 11:14) {
  model <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                  data = df.train,
                  nstates = n,
                  family = list(gaussian(), gaussian()),
                  ntimes = ntimesvector
  )
  fitted_model <- fit(model)
  fitted_model.data = data.frame(states = n,
                                 BIC = BIC(fitted_model),
                                 log_lik = logLik(fitted_model)
  )
  fittedlist <- c(fittedlist, fitted_model)
  
  train_results <- rbind(train_results, fitted_model.data)
}

View(train_results)

# # Generate the plot
plot(11:14, train_results$BIC, type = "b", ylim = range(c(train_results$BIC, train_results$log_lik)),
     xlab = " Number of States", ylab = "BIC/Log-Likelihood", main = "BIC and Log-Likelihood Comparison")

# Add log-likelihood values to the plot
points(11:14, train_results$log_lik, col = "red", type = "b")

logliks.test <- list()
ntimesvector <- rep(120, 47)
View(df.test)
View(df.train)


# From a seperate run before, we found that the elbow occurs 
# somewhere between states 11-13
for (n in 1:4) {
  model <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1),
                  data = df.test,
                  nstates = n + 10,
                  family = list(gaussian(), gaussian()),
                  ntimes = ntimesvector
  )
  
  View(getpars(fittedlist[[n]]))
  View(getpars(model))
  
  modified_model <- setpars(model, getpars(fittedlist[[n]]))
  
  logliks.test <- c(logliks.test, 
                        forwardbackward(modified_model, 
                                        df.test, return.all = FALSE, 
                                        useC = FALSE
                                        )
                    )
}

View(train_results)
View(logliks.test)

# Hard coded LogLiks comparisons from what I found

LogLiks <- data.frame(states = c(11, 12, 13, 14), 
                      TrainLogLiks = c(15982.13, 18713.85, 21706.21, 22614.4), 
                      TestLogLiks = c(6074.363, 6846.17, 7428.991, 8118.954)
)

LogLiks$TrainLogLiks <-LogLiks$TrainLogLiks / 19080
LogLiks$TestLogLiks <- LogLiks$TestLogLiks / 5640

View(LogLiks)
View(train_results)

