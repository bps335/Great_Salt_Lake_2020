###################### Wind data and wave modeling ########################

## This script loads and processes wind data from the Mesowest database. 
## Any station data above the first header line should be deleted in the
## csv file beforehand. The script assumes the header information has not
## been altered from the typical Mesowest headers. 

## The script uses the lubridate, circular, dplyr, and openair packages to 
## average data hourly. 

####################### Load data and packages ###########################

## Clear any variables from the global environment.

  remove(list= ls())

## Load the following packages

  library(ggplot2)
  library(lubridate)
  library(stringr)
  library(dplyr)
  library(openair)

## Load data from the formatted .csv file

  data <- read.csv("HATUT_for_export.csv")

####################### Convert text to dates  ###########################

## Convert and clean text characters in the Date_Time so that R 
## recognizes them as dates. 

  x <-data$Date_Time
  
  x %>% str_replace_all(c("T" = " ", "Z" = " "))

  date <- ymd_hms(x, tz = "GMT")
  
  ymd <- as_date(date)

################# Find maximum wind speeds by hour  ######################
  
## CITATION:
## Carslaw DC, Ropkins K (2012). "openair - An R package for air quality 
## data analysis." Environmental Modelling & Software, 27-28(0), 52-61. 
## ISSN 1364-8152, doi: 10.1016/j.envsoft.2011.09.008.

## Create a data frame of dates, wind speeds, and wind directions with
## missing or zero values omitted.Two important notes: first, the height
## of the station is important for calculating wind speed. If the station
## is not at 10m, then the wind speeds need to be adjusted. This can also
## be done in the wind speed module, but I have included it here. The 
## second note is that, for some reason, the wind module takes inputs as
## mph and converts them to m/s, so we create an additional "speed_mph"
## column.
  
  height = 3   ##height of the wind station in meters
  
  ## This uses the conversion forumla listed in Rohwder et al., (2012) 
  ## to adjust for station height and a conversion of 1 m/s = 2.24 mph
  
  wind <- data.frame(date,ymd,data$wind_direction_set_1*(10/height)^(1/7),
                     data$wind_speed_set_1,data$wind_speed_set_1*2.24)
  
  ## Omit missing values and rename the columns 
  
  wind<-na.omit(wind)
  
  colnames(wind) <-c("date","ymd","direction", "speed", "speed_mph")
  
## For testing only: Subset data to look at a specific interval.This is 
## commented out by default. 
  
  #wind <- wind%>%filter(ymd >as.Date("2011-01-01") & ymd 
                    #< as.Date("2014-01-01"))
  #wind <- wind%>%filter(ymd ==as.Date("2000-05-16"))
  
## Use the "openair" package to create hourly averages with missing values
## omitted. The averaging interval of choice is 4 hours, which allows us 
## to resolve short storm intervals. The data threshold is 75%, i.e., at 
## least 3 records of 15 minutes need to be present in each hour for an 
## average to be calculated.
  
  hourly_wind<-timeAverage(wind, avg.time = '4 hour', 
                           data.thresh = 75, interval = '15 min')
  
  hourly_wind<-na.omit(hourly_wind)
  
  hourly_wind$ymd <-ymd(as.Date(hourly_wind$date))
  
## Check the number of days for which there is data and calculate the data
## coverage. For Hat Island, there is one large gap in the data, but 
## otherwise the data coverage is relatively complete
  
  all_days<-unique(hourly_wind$ymd)
  
  dimensions <-dim(hourly_wind)
  
  total_time_period<-as.numeric(difftime( hourly_wind$date[dimensions[1]],
                            hourly_wind$date[1], units = "days"))
  
  data_coverage <-(length(all_days)/total_time_period)
  
## Optional: Plot wind speed and direction data. Not advised unless the 
## data has been subsetted. 
  
 #ggplot(data = hourly_wind, aes(x = date, y = speed_mph)) + geom_line() + 
 #geom_point(data = wind, aes(x = date, y = speed_mph))
  
  
############### Plot hourly averages in a rose diagram ###################
  
## Make the rose diagram in Fig. 2A from the main textusing the "rose" 
## function in the openair package.
  
rose <-openair::windRose(hourly_wind, ws = 'speed_mph', wd = 'direction', 
                            paddle = F, breaks = c(0,5,10,15,20))

#################### Bin wind direction data  ############################
  
## This section assumes that the fetch maps generated in arc are binned 
## into 20 degree intervals centered around (10, 30... 350). The wave 
## program fails if you provide it a wind direction that  does not 
## correspond EXACTLY to an azimuth in a fetch map. This section of the 
## code re-writes the azimuths into the nearest bins so that can be read 
## correctly. 
  
  x <-length(hourly_wind$direction)
  
  hourly_binned <-hourly_wind
  
  for (step in 1:x)
  {
    if      (hourly_binned$direction[step] >=0 
            && hourly_binned$direction[step] <20) 
            {hourly_binned$direction[step] = 10}
    
    
    else if (hourly_binned$direction[step] >=20 
             && hourly_binned$direction[step] <40) 
             {hourly_binned$direction[step] = 30}
    
    else if (hourly_binned$direction[step] >=40 
             && hourly_binned$direction[step] <60) 
             {hourly_binned$direction[step] = 50}
    
    else if (hourly_binned$direction[step] >=60 
             && hourly_binned$direction[step] <80) 
            {hourly_binned$direction[step] = 70}
    
    else if (hourly_binned$direction[step] >=80 
             && hourly_binned$direction[step] <100) 
            {hourly_binned$direction[step] = 90}
      
    else if (hourly_binned$direction[step] >=100 
             && hourly_binned$direction[step] <120) 
            {hourly_binned$direction[step] = 110}
    
    else if (hourly_binned$direction[step] >=120 
             && hourly_binned$direction[step] <140) 
            {hourly_binned$direction[step] = 130}
    
    else if (hourly_binned$direction[step] >=140 
             && hourly_binned$direction[step] <160) 
            {hourly_binned$direction[step] = 150}
    
    else if (hourly_binned$direction[step] >=160 
             && hourly_binned$direction[step] <180) 
            {hourly_binned$direction[step] = 170}
    
    else if (hourly_binned$direction[step] >=180 
             && hourly_binned$direction[step] <200) 
            {hourly_binned$direction[step] = 190}
      
    else if (hourly_binned$direction[step] >=200 
             && hourly_binned$direction[step] <220) 
            {hourly_binned$direction[step] = 210}
      
    else if (hourly_binned$direction[step] >=220 
             && hourly_binned$direction[step] <240) 
            {hourly_binned$direction[step] = 230}
      
    else if (hourly_binned$direction[step] >=240 
             && hourly_binned$direction[step] <260) 
            {hourly_binned$direction[step] = 250}
      
    else if (hourly_binned$direction[step] >=260 
             && hourly_binned$direction[step] <280) 
            {hourly_binned$direction[step] = 270}
      
    else if (hourly_binned$direction[step] >=280 
             && hourly_binned$direction[step] <300) 
            {hourly_binned$direction[step] = 290}
      
    else if (hourly_binned$direction[step] >=300 
             && hourly_binned$direction[step] <320) 
            {hourly_binned$direction[step] = 310}
      
    else if (hourly_binned$direction[step] >=320 
             && hourly_binned$direction[step] <340) 
            {hourly_binned$direction[step] = 330}
    
    else if (hourly_binned$direction[step] >=340 
             && hourly_binned$direction[step] <=360) 
            {hourly_binned$direction[step] = 350}
  }
  
########################## Site A analysis ###############################
  
## This section computes significant transport events at site A in Fig 1c. 
## The cutoffs for wind speed from each direction are taken from shear 
## velocity maps for each 20 degree interval. 
  
  subset_10 <-subset(hourly_binned, hourly_binned$direction ==10 & 
                       hourly_binned$speed_mph >= 100 )
  
  subset_30 <-subset(hourly_binned, hourly_binned$direction ==30 & 
                       hourly_binned$speed_mph >= 100 )
  
  subset_50 <-subset(hourly_binned, hourly_binned$direction ==50 & 
                       hourly_binned$speed_mph >= 100 )
  
  subset_70 <-subset(hourly_binned, hourly_binned$direction ==70 & 
                       hourly_binned$speed_mph >= 100 )
  
  subset_90 <-subset(hourly_binned, hourly_binned$direction ==90 & 
                       hourly_binned$speed_mph >= 90 )
  
  subset_110 <-subset(hourly_binned, hourly_binned$direction ==110 & 
                        hourly_binned$speed_mph >= 70 )
  
  subset_130 <-subset(hourly_binned, hourly_binned$direction ==130 & 
                        hourly_binned$speed_mph >= 60 )
  
  subset_150 <-subset(hourly_binned, hourly_binned$direction ==150 & 
                        hourly_binned$speed_mph >= 50 )
  
  subset_170 <-subset(hourly_binned, hourly_binned$direction ==170 & 
                        hourly_binned$speed_mph >= 50 )
  
  subset_190 <-subset(hourly_binned, hourly_binned$direction ==190 & 
                        hourly_binned$speed_mph >= 50 )
  
  subset_210 <-subset(hourly_binned, hourly_binned$direction ==210 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_230 <-subset(hourly_binned, hourly_binned$direction ==230 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_250 <-subset(hourly_binned, hourly_binned$direction ==250 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_270 <-subset(hourly_binned, hourly_binned$direction ==270 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_290 <-subset(hourly_binned, hourly_binned$direction ==290 & 
                        hourly_binned$speed_mph >= 30)
  
  subset_310 <-subset(hourly_binned, hourly_binned$direction ==310 & 
                        hourly_binned$speed_mph >= 20)
  
  subset_330 <-subset(hourly_binned, hourly_binned$direction ==330 & 
                        hourly_binned$speed_mph >= 20)
  
  subset_350 <-subset(hourly_binned, hourly_binned$direction ==350 & 
                        hourly_binned$speed_mph >= 50)
  
  total_R <-rbind(subset_10, subset_30, subset_50, subset_70, 
                  subset_90, subset_110, subset_130, subset_150, 
                  subset_170, subset_190, subset_210, subset_230, 
                  subset_250, subset_270, subset_290, subset_310, 
                  subset_330, subset_350)
  
  
  hourly_culled <- arrange(total_R,ymd) 
  
  intervals <-hourly_culled%>% mutate(time_diff = ymd - lag(ymd))
  intervals_R <-subset(intervals, intervals$time_diff>1 
                       & intervals$time_diff<548, na.rm = TRUE)
  
  ggplot(intervals_R, aes(x = time_diff)) +
    geom_histogram()
  
  intervals_R$site<-c("R")
  intervals_S$site<-c("S")
  

########################## Site B analysis ###############################
  
## This section computes significant transport events at site B in Fig 1c.
## The procedure is the same as above. 
  
  subset_10 <-subset(hourly_binned, hourly_binned$direction ==10 & 
                       hourly_binned$speed_mph >= 90 )
  
  subset_30 <-subset(hourly_binned, hourly_binned$direction ==30 & 
                       hourly_binned$speed_mph >= 100 )
  
  subset_50 <-subset(hourly_binned, hourly_binned$direction ==50 & 
                       hourly_binned$speed_mph >= 100 )
  
  subset_70 <-subset(hourly_binned, hourly_binned$direction ==70 & 
                       hourly_binned$speed_mph >= 100 )
  
  subset_90 <-subset(hourly_binned, hourly_binned$direction ==90 & 
                       hourly_binned$speed_mph >= 90 )
  
  subset_110 <-subset(hourly_binned, hourly_binned$direction ==110 & 
                        hourly_binned$speed_mph >= 70 )
  
  subset_130 <-subset(hourly_binned, hourly_binned$direction ==130 & 
                        hourly_binned$speed_mph >= 50 )
  
  subset_150 <-subset(hourly_binned, hourly_binned$direction ==150 & 
                        hourly_binned$speed_mph >= 50 )
  
  subset_170 <-subset(hourly_binned, hourly_binned$direction ==170 & 
                        hourly_binned$speed_mph >= 40 )
  
  subset_190 <-subset(hourly_binned, hourly_binned$direction ==190 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_210 <-subset(hourly_binned, hourly_binned$direction ==210 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_230 <-subset(hourly_binned, hourly_binned$direction ==230 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_250 <-subset(hourly_binned, hourly_binned$direction ==250 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_270 <-subset(hourly_binned, hourly_binned$direction ==270 & 
                        hourly_binned$speed_mph >= 30 )
  
  subset_290 <-subset(hourly_binned, hourly_binned$direction ==290 & 
                        hourly_binned$speed_mph >= 30)
  
  subset_310 <-subset(hourly_binned, hourly_binned$direction ==310 & 
                        hourly_binned$speed_mph >= 20)
  
  subset_330 <-subset(hourly_binned, hourly_binned$direction ==330 & 
                        hourly_binned$speed_mph >= 20)
  
  subset_350 <-subset(hourly_binned, hourly_binned$direction ==350 & 
                        hourly_binned$speed_mph >= 30)
  
  total_S <-rbind(subset_10, subset_30, subset_50, subset_70, 
                  subset_90, subset_110, subset_130, subset_150, 
                  subset_170, subset_190, subset_210, subset_230, 
                  subset_250, subset_270, subset_290, subset_310, 
                  subset_330, subset_350)
  
  hourly_culled <- arrange(total_S,ymd) 
  
  intervals <-hourly_culled%>% mutate(time_diff = ymd - lag(ymd))
  intervals_S <-subset(intervals, intervals$time_diff>1 
                       & intervals$time_diff<548, na.rm = TRUE)
  
  ggplot(intervals_S, aes(x = time_diff)) +
    geom_histogram()
  
############################# Plot Data ##################################
  
## Combine outputs from the previous sections. 
  
  plot<-rbind(intervals_R, intervals_S)
  table <-with(plot, table(time_diff, site))
  table<-as.data.frame((table))

## Choose an interval range. If there's an error, the maximum interval 
## range may be too low.
  
  breaks = seq(from = 0, to = 200, by = 20)
  ranges = paste(head(breaks,-1), breaks[-1], sep="-")
  
## Transform interval data into histograms and write them to tables. 

  freq_R <-hist(as.vector(intervals_R$time_diff), 
                breaks =breaks, include.lowest = T, plot = F)
  
  freq_S <-hist(as.vector(intervals_S$time_diff), 
                breaks =breaks, include.lowest = T, plot = F) 

  table_R <-data.frame(ranges = ranges, 
                       frequency = freq_R$counts, site = "R")
  
  table_R$ranges<-factor(table_R$ranges, levels = ranges)
  
  table_S <-data.frame(ranges = ranges, 
                       frequency = freq_S$counts, site = "S")
  
## Set factor levels so that the intervals are in the corrct order. 
  
  table_S$ranges<-factor(table_S$ranges, levels = ranges)

  plot<-rbind(table_R, table_S)

  plot$range <-factor(plot$ranges, levels = ranges)
  
## Plot data and save plot to external file. This generates the histograms
## in Fig. 7c. 

  ggplot(data = plot, aes( x = range, y = frequency, fill = site)) + 
    geom_bar(position = "dodge", stat = "identity")

  ggsave("frequency_plot.pdf", plot = last_plot(), scale = 3, width = 4, 
         height = 2.5, units = "cm", dpi = 300)  
   
