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

View(df)

set.seed(1)
pca <- prcomp(~ Global_active_power + Global_reactive_power + Voltage + 
                Global_intensity + Sub_metering_1 + Sub_metering_2 + Sub_metering_3, 
              data = df)
summary(pca)

## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main= "Energy Consumption PCA", names = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7"), xlab="Principal Component", ylab="Percent Variation",
        ylim = c(0, 50))

pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data

biplot(pca,
       xlabs=c(rep(".", nrow(df))),
       xlim = c(-0.04, 0.02), ylim = c(-0.02, 0.02)
       )

loading_scores <- pca$rotation[,1]
variable_scores <- abs(loading_scores) ## get the magnitudes
variable_scores_ranked <- sort(variable_scores, decreasing=TRUE)

variable_scores_ranked

