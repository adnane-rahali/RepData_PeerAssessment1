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
                temp <- steps_per_interv$average[steps_per_interv$interval==i["interval"]]
                #i["steps"]<-temp[1]
                print(temp[1])
        }
        else{
                i["steps"] = i["steps"]
        }
}

dt$steps <- apply(dt, 1, replaceNa)

tst <- dt[1:10, ]
tst$steps <- apply(tst, 1, replaceNa)


iii <= tst[1, ]
steps_per_interv$average[steps_per_interv$interval==i["interval"]]