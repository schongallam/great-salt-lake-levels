# Great Salt Lake Water Levels

library(readr)
library(dplyr)
library(tidyr)

# Import & clean up north-south GSL data ----
# note, the 2nd line of data needs to be commented out for each file
gsl.N <- read_tsv("GSL_North.tsv",
                 col_types = cols(datetime = col_date("%Y-%m-%d")),
                 comment="#")
gsl.S <- read_tsv("GSL_South.tsv",
                 col_types = cols(datetime = col_date("%Y-%m-%d")),
                 comment="#")

colnames(gsl.N) <- c("AgencyName", "SiteNum", "Date", "Feet", "unused")
colnames(gsl.S) <- c("AgencyName", "SiteNum", "Date", "Feet", "unused")

gsl.N <- gsl.N %>%
 select(-unused) %>%
 mutate(Location="North")
gsl.S <- gsl.S %>%
 select(-unused) %>%
 mutate(Location="South")

# Set up a data frame for ggplot ----
# Gather annual min and max levels

gsl.N$Year <- as.numeric(format(gsl.N$Date, '%Y'))
gsl.S$Year <- as.numeric(format(gsl.S$Date, '%Y'))

gsl.N.years <- unique(gsl.N$Year)
gsl.S.years <- unique(gsl.S$Year)

gsl.N.maxlevel <- sapply(gsl.N.years, \(x) max(gsl.N[gsl.N$Year==x,]$Feet, na.rm=TRUE))
gsl.N.minlevel <- sapply(gsl.N.years, \(x) min(gsl.N[gsl.N$Year==x,]$Feet, na.rm=TRUE))

gsl.S.maxlevel <- sapply(gsl.S.years, \(x) max(gsl.S[gsl.S$Year==x,]$Feet, na.rm=TRUE))
gsl.S.minlevel <- sapply(gsl.S.years, \(x) min(gsl.S[gsl.S$Year==x,]$Feet, na.rm=TRUE))

gsl.N.minmax <- data.frame(Year=gsl.N.years, Max=gsl.N.maxlevel, Min=gsl.N.minlevel)
gsl.S.minmax <- data.frame(Year=gsl.S.years, Max=gsl.S.maxlevel, Min=gsl.S.minlevel)
colnames(gsl.N.minmax)[colnames(gsl.N.minmax) == 'Max'] <- 'N_Max'
colnames(gsl.N.minmax)[colnames(gsl.N.minmax) == 'Min'] <- 'N_Min'
colnames(gsl.S.minmax)[colnames(gsl.S.minmax) == 'Max'] <- 'S_Max'
colnames(gsl.S.minmax)[colnames(gsl.S.minmax) == 'Min'] <- 'S_Min'

gsl.minmax <- full_join(gsl.N.minmax, gsl.S.minmax)


# Visualize water level vs year ----
# Full time series, and "recent" time series (see README)

library(ggplot2)

gsl.minmax.pivot <- pivot_longer(gsl.minmax,
                     cols = c("N_Max", "N_Min", "S_Max", "S_Min"),
                     names_to = "Location",
                     values_to = "Height")

titlestr <- sprintf("Great Salt Lake Water Level (%d to %d)",
                    min(gsl.minmax.pivot$Year),
                    max(gsl.minmax.pivot$Year))

group.colors <- c(N_Max = "red",
                  N_Min = "darkred",
                  S_Max = "blue",
                  S_Min = "darkblue")

GSL_full <- ggplot(gsl.minmax.pivot, aes(Year, Height, color = Location)) +
 geom_line() +
 ggtitle(titlestr) +
 xlab("Year") +
 ylab("Height (feet)") +
 scale_color_manual(name="Location",
                      breaks=c("N_Max", "N_Min", "S_Max", "S_Min"),
                      labels=c("North (Max)",
                               "North (Min)",
                               "South (Max)",
                               "South (Min)"),
                      values=group.colors)
GSL_full
ggsave("GSL_levels.svg",
       plot=GSL_full,
       width = 2400,
       height = 1800,
       units="px")

gsl.minmax.recent <- gsl.minmax[gsl.minmax$Year > 1965,]
gsl.minmax.recent.pivot <- pivot_longer(gsl.minmax.recent,
                                 cols = c("N_Max", "N_Min", "S_Max", "S_Min"),
                                 names_to = "Location",
                                 values_to = "Height")
titlestr.recent <- sprintf("Great Salt Lake Water Level (%d to %d)",
                           min(gsl.minmax.recent.pivot$Year),
                           max(gsl.minmax.recent.pivot$Year))
GSL_recent <- ggplot(gsl.minmax.recent.pivot,
                     aes(Year, Height, color = Location)) +
 geom_line() +
 ggtitle(titlestr.recent) +
 xlab("Year") +
 ylab("Height (feet)") +
 scale_color_manual(name="Location",
                    breaks=c("N_Max", "N_Min", "S_Max", "S_Min"),
                    labels=c("North (Max)",
                             "North (Min)",
                             "South (Max)",
                             "South (Min)"),
                    values=group.colors)
GSL_recent
ggsave("GSL_levels_recent.svg",
       plot=GSL_recent,
       width = 2400,
       height = 1800,
       units="px")


# Time Series Analysis ----

library(xts)

gsl.N.xts <- xts(gsl.N$Feet, gsl.N$Date)
gsl.S.xts <- xts(gsl.S$Feet, gsl.S$Date)
gsl.xts <- merge(gsl.N.xts, gsl.S.xts)

acf(gsl.N.xts, na.action=na.pass)

gsl.N.xts.annual <- to.yearly(gsl.N.xts) # effectively only goes back as far as 1966
gsl.S.xts.annual <- to.yearly(gsl.S.xts)


# Look for autocorrelation within the last few decades ----
# Chose 1980s-present because that's when the trend appears sometime in the 80s
# that we need to de-trend.  1981 specifically was cherry-picked for showing
# robust autocorrelation with a 12-13 year period.
# Somewhat arbitrarily, picking North GSL annual highs.  Using lows gives
# a similar result.

# For ease of comparing different time series starting points
lake.acf <- function(time.series, start.year, stop.year) {
 block <- time.series[sprintf("%s/%s",start.year,stop.year)]
 block.level <- coredata(block$gsl.N.xts.Low)
 block.year <- index(block)
 block.m <- lm(block.level ~ block.year)
 block.m.dt <- xts(resid(block.m), block.year)
 block.m.dt.plot <- plot(block.m.dt,
  main="De-trended lake level",
  sub=sprintf("%s - %s", start.year, stop.year))
 plot(block.m.dt.plot)
 block.m.dt.acf <- acf(block.m.dt, lag.max = 400, ylim=c(-0.6, 1.0), plot=FALSE)
 plot(block.m.dt.acf,
      main="Autocorrelation function for de-trended lake data",
      #sub=sprintf("%s - %s", start.year, stop.year),
      xlab="Lag (years)")
 mtext(side=3, line=0.5, sprintf("%s - %s", start.year, stop.year))
 return(list(block.m.dt, block.m.dt.acf))
}

years = seq(1983, 1988, 1)
sapply(years, lake.acf, time.series=gsl.N.xts.annual, stop.year=2024)

# Sample of interest for figure generation
lake.acf.86 <- lake.acf(gsl.N.xts.annual, 1986, 2024)

svg("Detrend1986-2024.svg", width=11, height=8)
plot(lake.acf.86[[1]],
     main="De-trended Lake Level")
dev.off()
    
svg("ACF1986-2024.svg", width=11, height=8)
plot(lake.acf.86[[2]],
     main="Autocorrelation function for de-trended lake level time series")
dev.off()


# Check for mean reversion ----

library(tseries)
gsl.recent.adf <- adf.test(na.omit(gsl.xts['1987/2024']$gsl.N.xts))
adf.title <- "Great Salt Lake water levels, 1987-2024"
adf.sub <- sprintf("ADF p-value = %f",gsl.recent.adf$p.value)

year.recent <- index(gsl.xts['1987/2024']$gsl.N.xts)
level.recent <- coredata(gsl.xts['1987/2024']$gsl.N.xts)
gsl.recent.m <- lm(level.recent ~ year.recent)

plot(level.recent ~ year.recent,
     type="l",
     main=adf.title,
#     sub=adf.sub,
     xlab="Year",
     ylab="Water level (feet)")
mtext(side=3, line=0.5, adf.sub)
abline(gsl.recent.m, lty=2)

library(fUnitRoots)
adfTest(na.omit(coredata(gsl.xts['1986/2024']$gsl.N.xts)), type='nc')


# ARIMA modeling, just for fun ----
# Not particularly representative of anything, however

pacf(na.omit(gsl.N.xts["1987/2024"]))

library(forecast)
gsl.N.arima <- auto.arima(gsl.N.xts['1987/2024'])
checkresiduals(gsl.N.arima)
plot(gsl.N.arima)
confint(gsl.N.arima)
# small Ljung-Box p value, lots of autocorrelation, not good
# let's try it anyway

gsl.N.arima.fcst <- forecast(gsl.N.arima, h=365*4)
plot(gsl.N.arima.fcst)
