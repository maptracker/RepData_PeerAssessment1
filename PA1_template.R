### I use emacs' ESS mode for editting, and it's a pain to work on
### .Rmd files (they are focused on Markdown, not R). This is just the
### R code cleanly separated for ease of editting and cut-and-paste
### testing.


input <- "activity.csv"
iCls  <- c("integer", "character", "integer")
data  <- read.csv( file = input, stringsAsFactors = FALSE,
    colClasses = iCls)
data$date <- as.Date( data$date )

## Fix the goofy intervals. They jump by 100 instead of 60
## Make a properly-continuous minute interval:
data$minutes <- vapply( data$interval, function(x) {
    hr = as.integer(x / 100)
    min = x - hr * 100
    hr * 60 + min
}, 0)
## Also generate pretty-print versions of the time (H:MM)
data$time <- vapply( data$interval, function(x) {
    hr = as.integer(x / 100)
    min = x - hr * 100
    sprintf("%d:%02d", hr, min)
}, "")
## Finally add a weekend flag
data$DayType <- factor(ifelse(weekdays(data$date) %in%
                              c("Saturday", "Sunday"),
                              "Weekend", "Weekday"),
                       levels = c("Weekday","Weekend" ))






## ggplot can automatically aggregate for us. But this makes
## computing the mean easier. I think.
stepsByDay <- aggregate( steps ~ date + DayType, data, sum )
p <- ggplot(stepsByDay, aes(date)) +
    geom_bar( aes(weight = steps, fill = DayType )) +
    labs(x = "Date", y = "Total Number of Steps")
p + theme(legend.position = c(0.1,0.9))

datSum <- summary(stepsByDay$steps, na.rm = TRUE)

sprintf("Average steps = %d, median = %d", datSum["Mean"], datSum["Median"])






stepsByInterval <- aggregate( steps ~ minutes + time + interval,
                             data, mean, na.rm = TRUE )
## Hourly markers for labels
stepsByInterval$Hour <- vapply( stepsByInterval$minutes, function(x) {
    ifelse(x %% 60, "", as.character(x / 60))
}, "")

## http://docs.ggplot2.org/current/annotation_raster.html
## Put in bars on the background to highlight hours
colorHours <- rep(c("red","green", "blue","yellow"), 6)
## Increase the transparency so the bars are not so stark
transCH <- rgb(t(col2rgb(colorHours)), alpha = 40, maxColorValue = 255)

p <- ggplot(stepsByInterval, aes(x = minutes, y = steps)) +
    ## The hour bars:
    annotation_raster( matrix(transCH, nrow = 1), 0,
                      max(stepsByInterval$minutes), -Inf, Inf) +
    geom_line( )  +
    labs(x = "Minutes from Midnight\nColored bars mark off hours",
        y = "Average Number of Steps")

## Find the time that the most steps occurs at
with( stepsByInterval, {
    maxInd  <<- which.max(steps) 
    maxTime <<- time[ maxInd ]
    maxInt  <<- interval[ maxInd ]
} )

## Add in the 24 hour labels and the time at max steps
p  + geom_text(label = stepsByInterval$Hour, y = -5 ) +
    geom_label(label = maxTime, x = stepsByInterval$minutes[maxInd],
               y = stepsByInterval$steps[maxInd] + 5 )

sprintf("Maximum steps occur at interval %d, at %s", maxInt, maxTime)




## We will use the mean broken out by weekend/weekday and interval to
## fill in missing values:
imputationAggregation <-
    aggregate( steps ~ DayType + interval, data, mean, na.rm = TRUE )

## I am skeptical that subsetting will be efficient here. I am going
## to make a hash to store these values
library(hash)
imputationLookup <- hash()
for (i in 1:nrow(imputationAggregation)) {
    imputationLookup[[ paste(imputationAggregation$DayType[i],
                             imputationAggregation$interval[i]) ]] <-
        ## Round to keep it an integer
        round(imputationAggregation$steps[i])
}

imputed <- data
for (i in 1:nrow(imputed)) {
    if (is.na(imputed$steps[i])) imputed$steps[i] <-
        imputationLookup[[ paste(imputed$DayType[i],
                                 imputed$interval[i]) ]]
}

sprintf("%d rows had missing data, imputation has added values for %d rows",
        sum(!complete.cases(imputed)),
        sum(complete.cases(imputed)) - sum(complete.cases(data)))





impByDay <- aggregate( steps ~ date + DayType, imputed, sum )
p <- ggplot(impByDay, aes(date)) +
    geom_bar( aes(weight = steps, fill = DayType )) +
    labs(x = "Date", y = "Total Number of Steps",
         main = "Steps by day, using imputed data")
p + theme(legend.position = c(0.1,0.9))

# RAW: 41    8841   10760   10770   13290   21190

impSum <- summary(impByDay$steps, na.rm = TRUE)

sprintf("Average steps = %d (%+d), median = %d (%+d)",
        impSum["Mean"], impSum["Mean"] - datSum["Mean"],
        impSum["Median"], impSum["Median"] - datSum["Median"])



## Directly compare the two plots. We need to turn the NAs into zeros:
dInds <- 1:length(data$steps)
repInds <- dInds[ is.na(data$steps) ]
plot(x = data$date, y = log10(imputed$steps -
                              replace(data$steps, repInds, 0)+1),
     xlab = "Date", ylab = "Steps Added by Imputation (log10)")




## We'll use the imputed values for this:
stepsByIntType <- aggregate( steps ~ minutes + DayType,
                             imputed, mean, na.rm = TRUE )
## Hourly markers for labels
stepsByIntType$Hour <- vapply( stepsByIntType$minutes, function(x) {
    ifelse(x %% 60, "", as.character(x / 60))
}, "")


## the polygon connects the first and last points, which is kind of
## irritating. I don't know a simple way to just drop the shading to
## the x-axis.
p <- ggplot(stepsByIntType, aes(x = minutes, y = steps)) +
    geom_polygon( aes(alpha = 0.1, fill = DayType, col = DayType))  +
    labs(x = "Minutes from Midnight",
        y = "Average Number of Steps")

## Add in the 24 hour labels and the time at max steps
p  + geom_text(label = stepsByIntType$Hour, y = -5 )

