rm(list=ls())
library(behavr)
library(data.table)
library(ggetho)
library(zeitgebr)
source('./cplot.R')

PALETTE <- c(LD='#000000', LL='#cc0000', DD='#003399')

excluded_zones = data.table(
				device=c('7168f343', '7168f343'), 
				start=c('2020-02-27_23-25-00', '2020-02-29_17-40-00'), 
				end=c('2020-02-28_04-18-00', '2020-02-29_18-10-00'))
				
				
excluded_zones[, start := as.POSIXct(start, format="%Y-%m-%d_%H-%M-%S", tz='UTC')]
excluded_zones[, end := as.POSIXct(end, format="%Y-%m-%d_%H-%M-%S", tz='UTC')]


ndt = fread("detection_results.csv.gz")[, -"image"]
ndt[ , id := as.factor(series_id)]
ndt[ , series_id := NULL]
ndt <- na.omit(ndt)
setkey(ndt, id)

meta = fread("metadata.csv")
meta[, id := paste(device,start,end,sep='.')]
meta[ , ref_datetime := as.POSIXct(ref_datetime, format="%Y-%m-%d_%H-%M-%S", tz='UTC')]
setkey(meta, id)
 
dt = behavr(ndt, meta)
dt[ , datetime := as.POSIXct(datetime, tz= 'UTC')]

foo <- function(i){
	e = excluded_zones[i]
	dt[, which(datetime %between% c(e[,start], e[,end]) & device == e[,device])]
}

l = lapply(1:nrow(excluded_zones), foo)
s <- as.vector(unlist(l))
dt <- dt[!s]


dt[, t := as.numeric(datetime-xmv(ref_datetime),units="secs")]
dt[, N := runmed(N,5), by=key(dt)]
dt[, dN := c(0,diff(N) / diff(t)) * hours(1), by=key(dt)]
k = 5
dt[, dN := filter(dN, rep(1/k, each=k)), by=key(dt)]
dt <- dt[t > days(-0.5) & t <days(3)]
dt[,condition := factor(condition,levels = names(PALETTE)),meta=T]
cplot(dt, '20200223.pdf', PALETTE)

