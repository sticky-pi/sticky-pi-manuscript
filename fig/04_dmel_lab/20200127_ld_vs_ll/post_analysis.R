rm(list=ls())
library(behavr)
library(data.table)
library(ggetho)
library(zeitgebr)


source('./cplot.R')

PALETTE <- c(LD='#000000', LL='#cc0000', DD='#003399')

excluded_zones = data.table(
				device='7168f343', 
				start='2020-02-06_18-15-00', 
				end='2020-02-06_20-00-00')
				
				
excluded_zones[, start := as.POSIXct(start, format="%Y-%m-%d_%H-%M-%S", tz='UTC')]
excluded_zones[, end := as.POSIXct(end, format="%Y-%m-%d_%H-%M-%S", tz='UTC')]


ndt = fread("detection_results.csv")[, -"image"]
ndt[ , id := as.factor(series_id)]
ndt[ , series_id := NULL]
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

s = sapply(1:nrow(excluded_zones), foo)
s <- as.vector(s)

dt <- dt[!s]
# 
# 
# dt[, t := as.numeric(datetime-xmv(ref_datetime),units="secs")]
# 
# dt[, N := runmed(N,5), by=key(dt)]
# dt[, dN := c(0,diff(N) / diff(t)) * hours(1), by=key(dt)]
# #dt[, dN := c(0,diff(N))/c(+Inf,diff(t)), by=key(dt)]
# 
# 
# dt <- dt[t > days(1.5)]



dt[, t := as.numeric(datetime-xmv(ref_datetime),units="secs")]
dt[, N := runmed(N,5), by=key(dt)]
dt[, dN := c(0,diff(N) / diff(t)) * hours(1), by=key(dt)]
k = 5
dt[, dN := filter(dN, rep(1/k, each=k)), by=key(dt)]
dt <- dt[, t := t-days(2)]
dt <- dt[t > days(- 0.5) & t <days(3)]

dt[,condition := factor(condition,levels = names(PALETTE)),meta=T]
cplot(dt, '20200127.pdf', PALETTE)

per_dt <- periodogram(dN, na.omit(dt), FUN = ac_periodogram, resample_rate = 1/mins(10))
per_dt <- find_peaks(per_dt)

summary_dt <- rejoin(per_dt[period==days(1)])
summary_dt

ggplot(summary_dt, aes(period_group, period, fill= period_group)) +
        geom_boxplot(outlier.colour = NA) +
        geom_jitter(aes(size=power -  signif_threshold), alpha=.5) +
        scale_y_hours(name = "Period")

rejoin(dt[, .(mean(dN)), by=id])[,.(mean(V1), sd(V1)),  by=condition]


summary_dt <- rejoin(per_dt[period==days(1)])

