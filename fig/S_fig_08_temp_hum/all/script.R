rm(list=ls())
library(data.table)
library(ggplot2)
library(fasttime)
library(ggetho)
library(behavr)
library(maptools)
library(patchwork)
library(gratia)
library(smacof)
library(ggrepel)

source('../helpers.R')

dt <- make_dt()


pdf('plots.pdf', w=10, h=10)

common_layers <- function(ncol=1, wrap=TRUE){
  o <-                       list(
                        theme_bw(),
                        scale_y_continuous(name=expression(Capture~(h^-1%.%device^-1))))
  if(wrap)
    o[[3]] = facet_wrap( ~ taxon, ncol=ncol, scales = 'free_y')
  o
}

p1 <- ggplot(dt[xmv(label_itc)==3],
	   aes(y=temp, x=as.POSIXct(t, origin="1970-01-01"))) +
	stat_summary_bin(binwidth=mins(30), geom='line')

p2 <- ggplot(dt[xmv(label_itc)==3],
	   aes(y=hum, x=as.POSIXct(t, origin="1970-01-01"))) +
	stat_summary_bin(binwidth=mins(30), geom='line')


sunset_rise <- unique(dt[xmv(label_itc)==3 , sun_rise_set(datetime, LATITUDE, LONGITUDE)])
sunset_rise[,sunrise_n := c(sunrise[2:.N], NA) ]

p3 <- ggplot(sunset_rise, aes(xmin=sunrise, xmax=sunset, ymin=10, ymax=5)) +
      geom_rect(fill="white") +
      geom_rect(data= sunset_rise, aes(xmin=sunset, xmax=sunrise_n, ymin=10, ymax=5), fill="black")




print(p1/p2/p3)
	
dev.off()




temp_dt <- dt[ xmv(label_itc) == 3, ]
temp_dt[, t_f := hours(1) * floor(t / hours(1))]
dev_temp_hour_dt = temp_dt[, .(temp = mean(temp),
                               hum = mean(hum)),by=.(id,t_f)]
mean(dev_temp_hour_dt[, sd(temp),by=t_f]$V1)
mean(dev_temp_hour_dt[, sd(hum),by=t_f]$V1)