rm(list=ls())
library(data.table)
library(ggplot2)
library(fasttime)
library(ggetho)
library(behavr)
library(maptools)
library(patchwork)
library(mgcv)
library(gratia)
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

# todo, here, we should only use one label otherwise data is repeated
p1 <- ggetho(dt, aes(y=temp, x=wt), summary_time_window = hours(1)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + scale_y_continuous(name=expression(Temperature~(degree*C))) +
  ggtitle('Average Temperture vs Datetime')


p2 <- ggetho(dt, aes(y=hum,x=wt), summary_time_window = hours(1)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + scale_y_continuous(limits = c(0,100), name='Relative Humidity (%)') +
  ggtitle('Average Humidity vs Datetime')

p3 <- ggetho(dt, aes(y=light_intensity,x=wt), summary_time_window = hours(1)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + scale_y_continuous(name='Light Intensity (a.u.)') +
  ggtitle('Average Light Intensity vs Datetime')


print(p1/p2/p3)

summary_dt <- rejoin(dt[, .(N=sum(N)), by=id])

n_per_tax = summary_dt [,.(N_total=sum(N)), by=taxon]

p <- ggplot(summary_dt, aes(device, N), colour=taxon) +
  geom_boxplot()	 +
  facet_wrap( ~ taxon, ncol=3, scales = 'free_y')
print(p)

p <- ggetho(dt, aes(x=wt,y=N,colour=vinegar_bait), summary_time_window = days(1)) +
  # stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  stat_pop_etho() + common_layers(ncol=3) + ggtitle('Average Capture rate vs Datetime') +
  geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")
print(p)  

p <- ggetho(dt, aes(x=wt,y=N, colour=vinegar_bait), summary_time_window = hours(1), time_wrap = hours(24)) +
  stat_ld_annotations(height = 1, alpha=.2, outline=NA) +
  scale_x_hours(name='WZT')+
  # stat_summary(aes(linetype=as.character(start_datetime)), geom='line')+  
  stat_pop_etho() +common_layers(ncol=3) + ggtitle('Average Capture rate vs Time of day') +
  geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")
print(p)

dev.off()

####
# Susukii

test_dt <- dt[xmv(label_itc) %in% c(4,5,15)]


tmp_dt <- cbind(sun_rise_set(test_dt[,start_datetime, meta=T], LATITUDE, LONGITUDE),
      start_datetime = test_dt[,start_datetime, meta=T])

test_dt[, wstart_datetime := tmp_dt[,wzt(start_datetime, sunrise,sunset)], meta=T]


summary_dt <- rejoin(test_dt[, .(N=sum(N)), by=id])
n_per_tax = summary_dt [,.(N_total=sum(N)), by=taxon]
n_per_tax_bait_id = summary_dt [,.(N=sum(N)), by='taxon,vinegar_bait,id']
n_per_tax_bait_id[, mean(N), by='taxon,vinegar_bait']


pdf(w=8,h=5, 'D.suzukii.pdf')
p1 <- ggetho(test_dt, aes(x=wt,y=N, colour=vinegar_bait),
             summary_time_window = hours(24)) +
                geom_vline(data=test_dt[, .(start_time=floor(min(wt)/days(1)) * days(1) ), by=start_datetime], aes(xintercept=start_time), col='blue',linetype=2)+
                stat_pop_etho() +
                common_layers() +
  geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")
p1

p2 <- ggetho(test_dt, aes(x=wt,y=N, colour=vinegar_bait),
           time_wrap = hours(24),
           time_offset = hours(6), summary_time_window = hours(1)) +
              stat_pop_etho()  +
              common_layers() +
              stat_ld_annotations() +
           geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward") 


print(p1 | p2)
dev.off()


## vs baiting renewal
dt2 <- copy(dt)
dt2[,t_in_week := as.numeric(datetime -start_datetime, unit='secs') ]
dt2[, device := xmv(device)]
dt2 <- dt2[, .(all_insects = sum(N)), by='t,device'][dt2, on=c('t', 'device')]
setkey(dt2, id)
dt2 <- behavr(dt2, meta(dt))
dt2[,cum_insects := cumsum(all_insects), by=id]
dt2[,cum_N := cumsum(N), by=id]
dt2[,vinegar_bait := as.factor(xmv(vinegar_bait))]
dt2[, id_fact := as.factor(id)]
dt2[, day := floor(t/days(1)) - min(floor(t/days(1)))]


sdt <- dt2[, .(n_day_02 = sum(N[t_in_week < days(2)]),
               n=sum(N),
               sum_all_insects = sum(all_insects)),
           by=id]

sdt[, prop_day_02 :=  n_day_02/n]

tdt <- rejoin(sdt[xmv(label_itc) %in% 4:5 & xmv(vinegar_bait) == 'Y'])
tdt[, .(sd(prop_day_02), mean(prop_day_02))]
m <- as.matrix(tdt[,.(n_day_02, n- n_day_02)])
mod <- glm( cbind(n_day_02, n- n_day_02) ~ sum_all_insects + taxon,data=tdt,  family=binomial)
summary(mod)

test_dt
p0 <- ggetho(dt2[xmv(label_itc) == 5 ], aes(x=wt, y=cum_insects, 
                                            colour=vinegar_bait,
                                            group=interaction(start_datetime,
                                                              vinegar_bait)), summary_time_window = days(1)) + 
  stat_pop_etho() + OUR_THEME +
  scale_x_days(limits = days(c(18485,18535)))  +theme(legend.position="bottom")
p0  
p1 <- ggplot(tdt, aes(as.numeric(start_datetime, unit='secs') / days(1),  n= n, n_day_02=n_day_02,
                      prop_day_02, colour=taxon)) + 
  geom_smooth(
    method="glm",
    method.args=list(family="binomial"),
    formula = cbind(n_day_02, n- n_day_02) ~ x
  ) +
  geom_jitter(height = 0, width=1.5, alpha=.7) +
  scale_color_manual(values=c( "#042530", "#808000")) +
  OUR_THEME + scale_x_continuous(limits = c(18485,18535)) +theme(legend.position="bottom")

p1
p2 <- ggplot(tdt, aes(sum_all_insects, prop_day_02, colour=taxon, n= n, n_day_02=n_day_02)) + 
  geom_point() +
  scale_color_manual(values=c( "#042530", "#808000")) +
  geom_smooth(
    method="glm",
    method.args=list(family="binomial"),
    formula = cbind(n_day_02, n- n_day_02) ~ x
  ) + OUR_THEME +theme(legend.position="bottom")

pdf(w=8,h=5, 'D.suzukii-bait_decay.pdf')
print((p0/p1) |p2)
dev.off()


wilcox.test(n ~ vinegar_bait,rejoin(sdt[xmv(label_itc) %in% 4 ]))

wilcox.test(n ~ vinegar_bait,rejoin(sdt[xmv(label_itc) %in% 5]))

wilcox.test(n ~ vinegar_bait,rejoin(sdt[xmv(label_itc) %in% 15 ]))




test_dt <- dt[xmv(label_itc) == 4]
tmp_dt <- cbind(sun_rise_set(test_dt[,start_datetime, meta=T], LATITUDE, LONGITUDE),
      start_datetime = test_dt[,start_datetime, meta=T])
test_dt[, wstart_datetime := tmp_dt[,wzt(start_datetime, sunrise,sunset)], meta=T]
crep_dt <- test_dt[, .( crepu  = sum(N[wzt > hours(10) & wzt < hours(14) | wzt < hours(2) | wzt > hours(22)]), N=sum(N)) ,by=id]

crep_dt[,sum(crepu)/sum(N)]
quantile(replicate(10000, {crep_dt[sample(1:.N, replace=T)][,sum(crepu)/sum(N)]}), p=c(0.05, 0.95))

test_dt <- dt[xmv(label_itc) == 5]
crep_dt <- test_dt[, .( crepu  = sum(N[wzt > hours(10) & wzt < hours(14) | wzt < hours(2) | wzt > hours(22)]), N=sum(N)) ,by=id]
crep_dt[,sum(crepu)/sum(N)]
quantile(replicate(10000, {crep_dt[sample(1:.N, replace=T)][,sum(crepu)/sum(N)]}), p=c(0.05, 0.95))



test_dt <- dt[xmv(label_itc) == 15]
diurn_dt <- test_dt[, .( crepu  = sum(N[wzt < hours(12)]), N=sum(N)) ,by=id]
diurn_dt[,sum(crepu)/sum(N)]
quantile(replicate(10000, {diurn_dt[sample(1:.N, replace=T)][,sum(crepu)/sum(N)]}), p=c(0.05, 0.95))


# test_dt <- dt[xmv(taxon)  == "Edwardsiana"]
test_dt <- dt[xmv(label_itc)  == 15]

p1 <- ggetho(test_dt, aes(t, temp), summary_time_window=days(1)) + stat_pop_etho()
p2 <- ggetho(test_dt, aes(t, hum), summary_time_window=days(1)) + stat_pop_etho()
p3 <- ggetho(test_dt, aes(t,N), summary_time_window=days(1)) + stat_pop_etho()

p1/p2/p3


# test_dt <- dt[xmv(label_itc)  == 15][xmv(start_datetime) > "2020-09-10 00:00:00"]


f <- function(sdt){
  out = copy(sdt)
  out[, day := round(t/days(1))]
  day_out = out[, .(N = sum(N),
                    temp = mean(temp),
                    hum = mean(hum)
  ),by=day]

 # day_out[, d_temp := c(NA, diff(temp))]
 #  day_out[, d_hum := c(NA, diff(hum))]
 # day_out[, d_N := c(NA, diff(N))]
 # na.omit((day_out))
}

st_dt <- test_dt[, f(.SD), by=id]

ggplot(st_dt, aes(temp, N)) + geom_point()
ggplot(st_dt, aes(hum, N)) + geom_point()


# 
# ggetho(dt2[xmv(label_itc) == 5 ], aes(x=wt, y=N, colour=vinegar_bait), summary_time_window = days(1)) + stat_pop_etho()
# 
# ggplot(dt2[xmv(label_itc) == 4 & xmv(vinegar_bait) =='Y' ], aes(x=t_in_week, y=cum_insects))  +
#   geom_line(aes(y=cum_N, group=id), 
#             col='red', 
#             data = dt2[xmv(label_itc) %in% c(5) & xmv(vinegar_bait) =='Y']) +
#   geom_line(aes(group=id))  +  facet_grid( device ~ as.factor(start_datetime))
# 
# 
# 
# 
# ggplot(dt2[xmv(label_itc) == 8 & xmv(vinegar_bait) =='N' ], aes(x=t_in_week, y=cum_insects))  +
#   geom_line(aes(group=id))  +  facet_grid( device ~ as.factor(start_datetime))
# 
# 
# 
# 
# 
# p1 <- ggetho(dt2[xmv(label_itc) == 4 ], aes(x=wt, y=N, colour=vinegar_bait), summary_time_window = days(1)) + stat_pop_etho()
# p2 <- ggetho(dt2[xmv(label_itc) == 2 ], aes(x=wt, y=N, colour=vinegar_bait), summary_time_window = days(1)) + stat_pop_etho() 
# p3 <- ggetho(dt2[xmv(label_itc) == 5 ], aes(x=wt, y=cum_insects, colour=vinegar_bait, 
#                                       group=interaction(start_datetime,vinegar_bait)), summary_time_window = days(1)) + stat_pop_etho()
# p1/p2/p3
# 
# mod = bam(N ~ 
#             s(wzt, bs="cp") + 
#             cum_insects+
#             t_in_week +
#             s(id_fact,  bs='re'), 
#           data=dt2[xmv(label_itc) %in% c(4)  &  xmv(vinegar_bait) == 'Y'],
#           family = 'poisson', select = TRUE, nthreads = 4)
# 
# summary(mod)
# vis.gam(mod, view=c("cum_insects","wzt"), type='response', phi=40, theta=80)
# mod = bam(all_insects ~ 
#             s(wzt, bs="cp") + 
#             cum_insects+
#             t_in_week +
#             s(id_fact,  bs='re'), 
#           data=dt2[xmv(label_itc) %in% c(5)  &  xmv(vinegar_bait) == 'Y'],
#           family = 'poisson', select = TRUE, nthreads = 4)
# summary(mod)
# vis.gam(mod, view=c("cum_insects","t_in_week"), type='response', phi=40, theta=20)
# # plot.gam(mod)
# ggplot(sdt, aes(N, s_day02, colour=s_day01)) + geom_point() + facet_wrap( ~ vinegar_bait)
# 
# p1 <- ggetho(test_dt[xmv(vinegar_bait)=='N' & xmv(label_itc) == 4], aes(x=wt,y=N,clour=device),
#              summary_time_window = hours(24)) +
#             geom_vline(data=test_dt[, .(start_time=floor(min(wt)/days(1)) * days(1) ), by=start_datetime], aes(xintercept=start_time), col='blue',linetype=2)+
#           stat_pop_etho() +
#   common_layers() +
#   geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
#              mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")
# p1
# test_dt[xmv(vinegar_bait)=='N' & xmv(label_itc) == 5 ]
# 
# 
# sdt <- dt2[, .(n_day_02 = sum(N[t_in_week < days(2)]),
#            n=sum(N),
#            sum_all_insects = sum(all_insects)),
# by=id]
# 
# sdt[, prop_day_02 :=  n_day_02/n]
# 
# tdt <- rejoin(sdt[xmv(label_itc) %in% 4:5 & xmv(vinegar_bait) == 'Y'])
# 
# tdt[, .(sd(prop_day_02), mean(prop_day_02))]
# m <- as.matrix(tdt[,.(n_day_02, n- n_day_02)])
# mod <- glm( cbind(n_day_02, n- n_day_02) ~ sum_all_insects + taxon,data=tdt,  family=binomial)
# 
# 
# p0 <- ggplot(tdt, aes(start_datetime, sum_all_insects, colour=taxon))  + geom_jitter()
# p0
# p1 <- ggplot(tdt, aes(start_datetime, prop_day_02, colour=taxon)) + geom_jitter() + geom_smooth(method='lm')
# p2 <- ggplot(tdt, aes(sum_all_insects, prop_day_02, colour=taxon, n= n, n_day_02=n_day_02)) + 
#           geom_jitter() + geom_smooth(
#             method="glm",
#             method.args=list(family="binomial"),
#             formula = cbind(n_day_02, n- n_day_02) ~ x
#           )
#   
#   # geom_ribbon(aes(sum_all_insects, ymin=y_pred-y_pred_sd,
#   #                                 ymax=y_pred +y_pred_sd , 
#   #                                 fill=taxon), pred_dt, inherit.aes = F)
# p1/p2
# 
