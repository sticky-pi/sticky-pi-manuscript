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

# 
# pdf(w=12,h=8, '20200127.pdf')
#             
#             
# pl = ggetho(dt, aes(y=N, color = start, group=id)) +
#     stat_ld_annotations() +
#     stat_pop_etho() +facet_wrap(~condition, ncol=1) + geom_point()
# print(pl)          
#             
# pl = ggetho(dt, aes(y=dN, fill = condition)) +
#       stat_ld_annotations() +
#     stat_pop_etho() + facet_wrap(~condition, ncol=1)+
#     scale_y_continuous(name=expression(frac(dN, dt)~(h^-1)))
# 
# print(pl)
# 
# pl = ggetho(dt, aes(y=dN, fill = condition),
#             time_wrap = hours(24), summary_time_window=mins(30), time_offset = hours(6)
#        ) + 
#     stat_ld_annotations() +
#     stat_pop_etho()  +
#     scale_y_continuous(name=expression(frac(dN, dt)~(h^-1)))
# 
# 
# print(pl)
# 
# 
# pl <- ggetho(dt, aes(x=t, z=dN), 
#              multiplot = 2,
#              multiplot_period = hours(24) 
#              ) + 
#   stat_bar_tile_etho() + facet_wrap(~ condition, ncol=1)
#   
# 
# per_dt <- periodogram(dN, na.omit(dt), FUN = ac_periodogram, resample_rate = 1/mins(10))
# per_dt
# 
# pl = ggperio(per_dt, aes(period, power, colour=start, group=id)) +
#     geom_line() + facet_wrap( ~ condition, ncol=1) + scale_y_continuous(name='r')
# 
# print(pl)
# 
# 
# pl = ggperio(per_dt, aes(period, power, colour=condition)) + 
#   stat_pop_etho()
# 
# pl
# 
# 
# per_dt <- periodogram(dN, na.omit(dt), FUN = chi_sq_periodogram, resample_rate = 1/mins(10))
# per_dt <- find_peaks(per_dt)
# 
# pl = ggperio(per_dt, aes(period, power-signif_threshold, colour=start, group=id)) +
#     geom_line() + facet_wrap( ~ condition, ncol=1)
# 
# print(pl)
# 
# pl = ggperio(per_dt, aes(period, power - signif_threshold, colour=condition)) + 
#     stat_pop_etho()
#     
#     
# print(pl)
# dev.off()
