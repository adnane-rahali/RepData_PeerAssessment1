library(dplyr)
library(ggplot2)

dt <-read.csv("./activity/activity.csv")

proc_data <- dt %>%
        group_by(date) %>%
        summarise(total = sum(steps))

View(proc_data)

hist_steps <- ggplot(proc_data, aes(total)) + 
        geom_histogram()
hist_steps

mean_and_median <- proc_data %>%
        summarise(mean = mean(total, na.rm = TRUE), 
                  median=median(total, na.rm = TRUE))
cat("mean:  ", mean_and_median$mean, "\n", "median:  ", mean_and_median$median)

steps_per_interv <- dt %>%
        group_by(interval) %>%
        summarise(average=mean(steps, na.rm=TRUE))

steps_plot <- ggplot(steps_per_interv, aes(x=interval, y=average)) +
        geom_line() +
        xlab("5min intervals") +
        ylab("avg number of steps") + 
        theme_grey()
steps_plot

steps_per_interv$interval[steps_per_interv$average == max(steps_per_interv$average)]


sum(is.na(dt$steps))

sum(apply(dt, 1, anyNA))

replaceNa <- function(i){
        if (is.na(i["steps"])){
                temp <- steps_per_interv$average[steps_per_interv$interval==as.numeric(i["interval"])]
                i["steps"]<- round(as.numeric(temp))
        }
        else{
                i["steps"] = as.numeric(i["steps"])
        }
}


dt <-read.csv("./activity/activity.csv")

new_data <- data.frame(dt)
new_data$steps <- apply(new_data, 1, replaceNa)

proc_data2 <- new_data %>%
        group_by(date) %>%
        summarise(total = sum(steps))

hist_steps2 <- ggplot(proc_data2, aes(total)) + 
        geom_histogram(bins=20)
hist_steps2

str(new_data)

new_data <- new_data %>%
        mutate(day = ifelse(weekdays(as.Date(date, "%Y-%m-%d")) %in% 
                                    c("Sunday","Saturday"),"weekend","weekday")) %>%
        mutate(day = as.factor(day))

library(lattice)
attach(steps_per_interv_per_day)

steps_per_interv_per_day <- new_data %>%
        group_by(day, interval) %>%
        summarise(average=mean(steps, na.rm=TRUE))


day.f<-factor(steps_per_interv_per_day$day,levels=c("weekend", "weekday"),
              labels=c("weekend", "weekday"))

xyplot(average~interval|day.f, type='l', layout=c(1, 2),
       ylab="avg  number  of  steps  taken", xlab="5min  intervals")



