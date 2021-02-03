rm(list=ls())
library(data.table)
library(ggplot2)
library(fasttime)
library(ggetho)
library(behavr)
library(maptools)
library(patchwork)
library(gratia)
source('../helpers.R')

dt <- make_dt()

{
pdf('plots.pdf', w=10, h=10)

common_layers <- function(ncol=1, wrap=TRUE){
  o <-                       list(
                        theme_bw(),
                        scale_y_continuous(name=expression(Capture~(h^-1%.%device^-1))))
  if(wrap)
    o[[3]] = facet_wrap( ~ taxon, ncol=ncol, scales = 'free_y')
  o
}

p1 <- ggetho(dt[xmv(label_itc)==3], aes(y=temp, x=wt), summary_time_window = hours(1)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + scale_y_continuous(name=expression(Temperature~(degree*C))) +
  ggtitle('Average Temperture vs Datetime')



p2 <- ggetho(dt[xmv(label_itc)==3], aes(y=hum,x=wt), summary_time_window = hours(1)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + scale_y_continuous(limits = c(0,100), name='Relative Humidity (%)') +
  ggtitle('Average Humidity vs Datetime')

p3 <- ggetho(dt[xmv(label_itc)==3], aes(y=light_intensity,x=wt), summary_time_window = hours(1)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + scale_y_continuous(name='Light Intensity (a.u.)') +
  ggtitle('Average Light Intensity vs Datetime')

print(p1/p2/p3)
summary_dt <- rejoin(dt[, .(N=sum(N)), by=id])
n_per_tax = summary_dt [,.(N_total=sum(N)), by=taxon]

p <- ggplot(rejoin(dt[, .(N=sum(N)), by=id]), aes(device, hours(1)*N/days(7))) +
		geom_boxplot() + common_layers(ncol=3) +
		geom_point()
print(p)

p <- ggetho(dt, aes(x=wt,y=N), summary_time_window = days(1)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + common_layers(ncol=3) + ggtitle('Average Capture rate vs Datetime') +
  geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")
print(p)

p <- ggetho(dt, aes(x=wt,y=N), summary_time_window = hours(1), time_wrap = hours(24)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  scale_x_hours(name='WZT')+
  # stat_summary(aes(linetype=as.character(start_datetime)), geom='line')+
  stat_pop_etho() +common_layers(ncol=2) + ggtitle('Average Capture rate vs Time of day') +
  geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")
print(p)
dev.off()
}



#test_dt <- dt[xmv(pred) %in% c(3, 6, 8,7)]
test_dt <- dt
tmp_dt <- cbind(sun_rise_set(test_dt[,start_datetime, meta=T], LATITUDE, LONGITUDE),
      start_datetime = test_dt[,start_datetime, meta=T])

test_dt[, wstart_datetime := tmp_dt[,wzt(start_datetime, sunrise,sunset)], meta=T]

pdf(w=10,h=18, 'misc.pdf')

ggetho(test_dt, aes(x=wt,y=N, colour=height),
             summary_time_window = hours(24)) +
                #geom_vline(data=test_dt[, meta=T], aes(xintercept=wstart_datetime), col='blue',linetype=2)+
                stat_pop_etho() +
                common_layers(ncol = 3) 



ggetho(test_dt, aes(x=wt,y=N, colour=height),
           time_wrap = hours(24),
           time_offset = hours(6), summary_time_window = hours(1) ) +
              stat_pop_etho()  +
              common_layers(ncol=3) +
            stat_ld_annotations()

dev.off()



##### covariance between coincidental insect capture rates?

tmp_dt <- dt
tmp_dt 

cova_dt <- dt[xmv(label_itc) == 9, .(N_Sciaridae=N, wt)][rejoin(dt), on=c("wt")]
cova_dt[, day := floor(t/days(1)) * days(1)]
cova_dt

plot_dt <- cova_dt[, .(N=mean(N), N_Sciaridae=mean(N_Sciaridae)), by=.(day, taxon)]

ggplot(plot_dt, aes(N, N_Sciaridae)) + geom_point() + facet_wrap( ~ taxon)


##### temp affects e. rosa daily rhythm
test_dt <- dt[xmv(label_itc)==2]
test_dt[, t := floor(wt/hours(1)) * hours(1)]
test_dt = test_dt[, .SD[,.( N = sum(N),
                            temp=mean(temp),
                            hum=mean(hum),
                            li=mean(light_intensity)

                          ), by=t], by=id]


test_dt[, device := xmv(device)]
test_dt[, height := xmv(height)]
test_dt[, zt := t %% hours(24)]

test_dt[, day := floor(t/days(1)) * days(1)]
test_dt = test_dt[, .SD[,.(day_mean_temp = mean(temp),
                           day_mean_hum = mean(hum),
                           day_mean_li = mean(li)
                           ), by=day][.SD, on='day'], by=id]

test_dt = test_dt[, last_24h_temp := rollmean(temp, 
                                              k=24, 
                                              align='right',
                                              na.pad = TRUE),
                  by=device]
test_dt = test_dt[, last_24h_hum := rollmean(hum, 
                                              k=24, 
                                              align='right',
                                              na.pad = TRUE),
                  by=device]

test_dt = test_dt[, last_24h_li := rollmean(li, 
                                             k=24, 
                                             align='right',
                                             na.pad = TRUE),
                  by=device]
test_dt[, id_fact := as.factor(id)]
test_dt[, height := as.factor(height)]


mod <- gam(N ~ s(zt, bs="cp" ) + 
           s(day_mean_temp) +
             s(day_mean_hum) +
             s(day_mean_li) +
           te(zt, last_24h_temp) +
             te(zt, last_24h_hum) +
             te(zt, last_24h_li) +
           s(height,   bs='re'), # include trap height nested in id!?
           # s(height,  by=id_fact, bs='re'), # include trap height nested in id!?
           data=test_dt,
           family = 'poisson', select = TRUE)
summary(mod)
AIC(mod)



vis.gam(mod,  type='response', theta=-35, phi=10)
vis.gam(mod,  type='response', theta=-130, phi=10)


ggplot(test_dt[, .(temp=mean(temp), last_24h_temp = mean(last_24h_temp)), by=t], aes(temp, last_24h_temp)) + geom_point() 

ggetho(test_dt, aes(t, last_24h_temp)) + stat_pop_etho()
ggetho(test_dt, aes(t, N),summary_time_window = days(1)) + stat_pop_etho()
ggetho(test_dt, aes(t, N),summary_time_window = days(1)) + stat_pop_etho()



test_dt[, N_dusk := ifelse(zt > hours(10) & zt < hours(14), N, 0)]
test_dt[, N_dawn := ifelse(zt > hours(22) | zt < hours(02), N, 0)]

ggetho(test_dt, aes(t, N),
       summary_time_window = days(1)) +
  stat_pop_etho()

ggetho(test_dt, aes(t, N_dawn),
       summary_time_window = days(1)) +
  stat_pop_etho()

ggetho(test_dt, aes(t, N_dusk),
       summary_time_window = days(1)) +
  stat_pop_etho()

ggetho(test_dt, aes(t, N_dusk),
       summary_time_window = days(1)) +
  stat_pop_etho()


ggplot(test_dt[,.(temp = mean(temp), 
                  N_dusk = mean(N_dusk),
                  N_dawn = mean(N_dawn)) ,by='day'], 
       aes(temp, N_dawn)) + geom_point() + geom_smooth()

ggetho(test_dt, aes(t, N), time_wrap = hours(24), summary_FUN = sum) + stat_pop_etho()

test_dt[, high_temp := last_24h_temp > 20]
ggetho(test_dt[high_temp == TRUE], aes(t, N), time_wrap = hours(24), summary_FUN = mean) + stat_pop_etho() + 
  coord_cartesian(ylim = c(0,.5))

ggetho(test_dt[high_temp == FALSE], aes(t, N), time_wrap = hours(24), summary_FUN = mean) + stat_pop_etho() +
  coord_cartesian(ylim = c(0,.5))









test_dt <- dt[xmv(label_itc)==2]
test_dt[, t := wt]

test_dt[, device := xmv(device)]
test_dt[, height := xmv(height)]
test_dt[, zt := t %% hours(24)]

test_dt[, day := floor(t/days(1)) * days(1)]
test_dt = test_dt[, .SD[,.(day_mean_temp = mean(temp)), by=day][.SD, on='day'], by=id]


test_dt[, high_temp := day_mean_temp > 18]
ggetho(test_dt[high_temp == TRUE], aes(t, N), 
       time_wrap = hours(24), 
       summary_FUN = mean, 
       summary_time_window = hours(1)) + stat_pop_etho() + 
  coord_cartesian(ylim = c(0,.15))

ggetho(test_dt[high_temp == FALSE], aes(t, N), time_wrap = hours(24),
       summary_time_window = hours(1)) + 
  stat_pop_etho() +
  coord_cartesian(ylim = c(0,.15))



# test_dt = test_dt[, last_24h_temp := rollmean(temp, 
#                                               k=24, 
#                                               align='right',
#                                               na.pad = TRUE),
#                   
# N_12h_before
# test_dt[,
#         .(N=.SD[t %in% t-hours(12)), N]),
#   by=id]

# warnings()

# test_dt[day]
# 
# pred_dt <- as.data.table(expand.grid(zt = hours(0:(48))/2,
#                                      temp=seq(from=10, to=30, by=5)
#                                      #light_intensity=seq(from=0, to=1200, by=100)
# )
# )
# 
# 
# predict(mod, type='response', exclude= 's(id_factor)')
# 
# pred_dt[, N_pred := predict(mod, type='response')]
# ggplot(pred_dt, aes(y=N_pred, x=zt, col=temp, group=temp)) + geom_line(size=1.5) +
#   #facet_wrap(~light_intensity) +
#   scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
#   scale_x_hours() + stat_summary_bin(test_dt, mapping = aes(zt, N), inherit.aes = FALSE)
# 
# ggplot(pred_dt, aes(y=N_pred, x=temp, col=zt, group=zt)) + geom_line() + 
#   facet_wrap(~hum) + 
#   scale_colour_gradient(low = "blue", high = "red", na.value = NA)
# 
#   varImpPlot(d)
# d

# dt_day = bin_apply_all(data=dt, y=N_rel, x=t, FUN = sum, x_bin_length = days(1))
# 
# dt[, day := round(t / hours(24))]
# dt[, taxon:=xmv(taxon)]
# dt_day = dt[,.(N_rel = sum(N_rel), temp=mean(temp)),by=c(key(dt), 'day', 'taxon')]
# ggplot(dt_day, aes(temp, N_rel), alpha=.1) + geom_point() +
#   facet_wrap( ~ taxon)
# 
# ggetho(dt, aes(y=temp, colour=taxon), summary_time_window = hours(1), time_wrap = hours(24)) +
#   stat_pop_etho() +
#   facet_wrap( ~ taxon)
# # 



## daily variance in E. rosae /scria

library(mgcv)
test_dt <- dt
ggetho(test_dt, aes(wt, N), time_wrap = hours(24)) + stat_pop_etho() +
  geom_vline(xintercept = hours(c(6,13)))


# test_dt = test_dt[, .SD[,.(temp=mean(temp))], by=id]
test_dt[, t := floor(wt/days(1))]
test_dt = rejoin(test_dt)[, .SD[,.( N = sum(N),
                            temp=mean(temp),
                            # temp_dusk = mean(temp[wzt %between% hours(c(10,13))]),
                            hum=mean(hum),
                            # hum_dusk = mean(hum[wzt %between% hours(c(10,13))]),
                            li=mean(light_intensity)
                            # li_dusk = mean(light_intensity[wzt %between% hours(c(10,13))])
                            
), by=t], by='id,taxon,height']

# test_dt = test_dt[, dtemp := c(0,diff(temp)), by=id]
test_dt[, id_fact := as.factor(id)]

# p1 <- ggplot(test_dt, aes(t, N)) + stat_summary(geom='ribbon') 
# p1 <- ggplot(test_dt, aes(t, N)) + stat_summary(geom='ribbon') 

# ggplot(test_dt[ ,.(temp_dusk=mean(temp_dusk), temp=mean(temp)), by=t], 
#        aes(temp_dusk, temp)) + geom_point() + geom_smooth(method='lm')
# 
# ggplot(test_dt[ ,.(hum_dusk=mean(hum_dusk), hum=mean(hum)), by=t], 
#        aes(hum_dusk, hum)) + geom_point() + geom_smooth(method='lm')
# 
# ggplot(test_dt[ ,.(hum=mean(hum), temp=mean(temp)), by=t], 
#        aes(hum, temp)) + geom_point() + geom_smooth(method='lm')
# ggplot(test_dt[ ,.(li=mean(li), temp=mean(temp)), by=t], 
#        aes(li, temp)) + geom_point() + geom_smooth(method='lm')



ggplot(test_dt[ ,.(N=mean(N), x=mean(temp)), by='t,taxon,height'], aes(x,N, colour=height)) + geom_point() + 
  geom_smooth(method='lm') + 
  facet_wrap(~ taxon, scales = "free_y")

ggplot(test_dt[ ,.(N=mean(N), x=mean(hum)), by='t,taxon,height'], aes(x,N, colour=height)) + geom_point() + 
  geom_smooth(method='lm') + 
  facet_wrap(~ taxon, scales = "free_y")

ggplot(test_dt[ ,.(N=mean(N), x=mean(li)), by='t,taxon,height'], aes(x,N, colour=height)) + geom_point() + 
  geom_smooth(method='lm') + 
  facet_wrap(~ taxon, scales = "free_y")


tax = 'Sciaridae'
tax = 'Edwardsiana'
tax = 'Muscoidea'
summary(
  lm( N ~   li +hum + temp, 
      test_dt[ taxon==tax ,.(N=mean(N), 
                                     li=mean(li),
                                     hum=mean(hum),
                                     temp=mean(temp)), by='t,taxon'])
)


ggplot(test_dt[ ,.(N=mean(N), x=mean(hum)), by=t], aes(x,N)) + geom_point() + geom_smooth(method='lm')
ggplot(test_dt[ ,.(N=mean(N), x=mean(hum_dusk)), by=t], aes(x,N)) + geom_point() + geom_smooth(method='lm')
ggplot(test_dt[ ,.(N=mean(N), x=mean(li)), by=t], aes(x,N)) + geom_point() + geom_smooth(method='lm')
ggplot(test_dt[ ,.(N=mean(N), x=mean(li_dusk)), by=t], aes(x,N)) + geom_point() + geom_smooth(method='lm')



# edwa:
# does env alter phase?


library(randomForest)
rf = 
  randomForest( N ~ hum_dusk * temp_dusk * li_dusk, 
    test_dt[ ,.(N=mean(N), hum_dusk=mean(hum), temp_dusk=mean(temp), li_dusk=mean(li)), by=t])
rf


summary(
  lm( N ~ hum_dusk * temp_dusk, 
      test_dt[ ,.(N=mean(N), hum_dusk=mean(hum_dusk), temp_dusk=mean(temp_dusk)), by=t])
)

