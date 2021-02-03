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
{
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
}
####
# Susukii


test_dt <- dt[xmv(label_itc) %in% c(4)]
test_dt_droso <- dt[xmv(label_itc) %in% c(5)]
test_dt[, device := xmv(device)]
test_dt_droso[, device := xmv(device)]

fid_dt <- dt[xmv(label_itc) %in% c(15)]
fid_dt[, device := xmv(device)]

test_dt <- bind_behavr_list(list(fid_dt, test_dt,test_dt_droso))

tmp_dt <- cbind(sun_rise_set(test_dt[,start_datetime, meta=T], LATITUDE, LONGITUDE),
      start_datetime = test_dt[,start_datetime, meta=T])

test_dt[, wstart_datetime := tmp_dt[,wzt(start_datetime, sunrise,sunset)], meta=T]


summary_dt <- rejoin(test_dt[, .(N=sum(N)), by=id])
n_per_tax = summary_dt [,.(N_total=sum(N)), by=taxon]

pdf(w=6,h=4, 'D.suzukii.pdf')
ggetho(test_dt, aes(x=wt,y=N, colour=vinegar_bait),
             summary_time_window = hours(24)) +
                geom_vline(data=test_dt[, meta=T], aes(xintercept=wstart_datetime), col='blue',linetype=2)+
                stat_pop_etho() +
                common_layers() +
  geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")



ggetho(test_dt, aes(x=wt,y=N, colour=vinegar_bait),
           time_wrap = hours(24),
           time_offset = hours(6), summary_time_window = hours(1)) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations() +
  geom_label(data=n_per_tax, x=+Inf, y=+Inf, 
             mapping=aes(label=sprintf('N_total = %i',N_total)), inherit.aes = F, vjust = "inward", hjust = "inward")


dev.off()


# what is the intraday variation in baited traps?
# Can we predict it vs other factors?


test_dt <- dt[xmv(label_itc) %in% c(4)]
test_dt_droso <- dt[xmv(label_itc) %in% c(5)]
test_dt[, device := xmv(device)]
test_dt_droso[, device := xmv(device)]
test_dt[, N:= N+test_dt_droso[test_dt,N, on=c('device', 'datetime')]]
test_dt = test_dt[xmv(vinegar_bait) == "Y",]

test_dt

ggetho(test_dt, aes(x=wt,y=N, colour=vinegar_bait),
             summary_time_window = hours(24)) +
                geom_vline(data=test_dt[, meta=T], aes(xintercept=wstart_datetime), col='blue',linetype=2)+
                stat_pop_etho() +
                common_layers() +
                stat_ld_annotations()


ggetho(test_dt, aes(x=wt,y=N, colour=vinegar_bait),
           time_wrap = hours(24),
           time_offset = hours(6) ) +
              stat_pop_etho()  +
              common_layers()

p1 <- ggetho(test_dt,
             aes(x=wt,y=temp), summary_time_window = days(1)) +
              stat_pop_etho()  +
              common_layers()
p2 <- ggetho(test_dt,
             aes(x=wt,y=N), summary_time_window = days(1)) +
              stat_pop_etho()  +
              common_layers()

p1 <- ggetho(test_dt,
             aes(x=wt,y=temp),
           time_wrap = hours(24),
           time_offset = hours(6) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations() + scale_y_continuous(name=expression(Temperature~(C)))
p2 <- ggetho(test_dt,
             aes(x=wt,y=N),
           time_wrap = hours(24),
           time_offset = hours(6), summary_time_window=hours(1) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations()

p1/p2

 ggplot(test_dt,  aes(x=wzt,y=temp, z=N)) +
   stat_summary_2d(fun = sum, binwidth = c(hours(1),3), drop = 0) + scale_x_hours()

test_dt[, id_fact := as.factor(id)]
test_dt[, day := floor(wt/days(1)) * days(1)]
test_dt = test_dt[, .SD[,.(day_mean_temp = mean(temp),
                           day_mean_hum = mean(hum),
                           day_mean_li = mean(light_intensity)
                           ), by=day][.SD, on='day'], by=id]

ggplot(test_dt,  aes(x=wzt,y=day_mean_temp, z=N)) +
   stat_summary_2d(fun = sum, binwidth = c(hours(1),3), drop = 0) + scale_x_hours()



ggplot(test_dt,  aes(x=wzt,y=day_mean_temp, z=N)) +
   stat_summary_2d(fun = mean, binwidth = c(hours(1),3), drop = 0) + scale_x_hours()


test_dt


pdf(w=6,h=4, 'temp.pdf')
p1 <- ggetho(test_dt,
             aes(x=wt,y=temp)) +
              stat_pop_etho()  +
              common_layers()

p2 <- ggetho(test_dt, aes(x=wt,y=temp),
           time_wrap = hours(24),
           time_offset = hours(6) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations()

p1/p2
dev.off()

pdf(w=6,h=4, 'E.rosae.pdf')
test_dt <- dt[xmv(label_itc) == 2]
tmp_dt <- cbind(sun_rise_set(test_dt[,start_datetime, meta=T], LATITUDE, LONGITUDE),
                start_datetime = test_dt[,start_datetime, meta=T])
test_dt[, wstart_datetime := tmp_dt[,wzt(start_datetime, sunrise,sunset)], meta=T]

p1 <- ggetho(test_dt[w_datetime <= '2020-08-30'],
             aes(x=wt,y=N),
           time_wrap = hours(24),
           time_offset = hours(6) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations()

p2 <- ggetho(test_dt[w_datetime > '2020-08-30'], aes(x=wt,y=N),
           time_wrap = hours(24),
           time_offset = hours(6) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations()
p1/p2

test_dt[ , week:=as.character(start_datetime),meta=T]

ggetho(test_dt, aes(x=wt,y=N, colour=week),
           time_wrap = hours(24),
           time_offset = hours(6),summary_time_window = hours(1) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations() +
            facet_wrap( ~ week, ncol=1, scales = 'free_y')

ggplot(test_dt,  aes(x=wzt,y=day_mean_temp, z=N)) +
   stat_summary_2d(fun = sum, binwidth = c(hours(1),3), drop = 0) + scale_x_hours()


dev.off()


test_dt[, id_fact := as.factor(id)]
test_dt[, day := floor(wt/days(1)) * days(1)]
test_dt = test_dt[, .SD[,.(day_mean_temp = mean(temp),
                           day_mean_hum = mean(hum),
                           day_mean_li = mean(light_intensity)
                           ), by=day][.SD, on='day'], by=id]

mod <- bam(N ~ ti(wzt, bs="cp") +
            ti(day) + ti(wzt,day) +
            #s(bait, bs='fs')+
            #s(day_mean_hum) +
            s(day_mean_temp) +
            #s(day_mean_li) +
            s(id_fact,  bs='re'),
           data=test_dt,
           family = 'poisson', select = TRUE, nthreads = 4)


summary(mod)
AIC(mod)
gratia::draw(mod)
vis.gam(mod,  type='response', theta=60, phi=30)

test_dt[, N_pred:=predict(mod, test_dt, type='response')]



p1 <- ggetho(test_dt, aes(x=wt,y=N_pred, colour=week),
           time_wrap = hours(24),
           time_offset = hours(6),summary_time_window = hours(1) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations() +
            facet_wrap( ~ week, ncol=1, scales = 'free_y')

p2 <- ggetho(test_dt, aes(x=wt,y=N, colour=week),
           time_wrap = hours(24),
           time_offset = hours(6),summary_time_window = hours(1) ) +
              stat_pop_etho()  +
              common_layers() +
            stat_ld_annotations() +
            facet_wrap( ~ week, ncol=1, scales = 'free_y')


p1 | p2



##### temp affects e. rosa daily rhythm
test_dt <- dt[xmv(label_itc)==2]
test_dt[, t := floor(wt/hours(1)) * hours(1)]
test_dt = test_dt[, .SD[,.( N = sum(N),
                            temp=mean(temp)
), by=t], by=id]


test_dt[, device := xmv(device)]
test_dt[, zt := t %% hours(24)]

test_dt[, day := floor(t/days(1)) * days(1)]
test_dt = test_dt[, .SD[,.(day_mean_temp = mean(temp)), by=day][.SD, on='day'], by=id]
test_dt = test_dt[, last_24h_temp := rollmean(temp, 
                                              k=24, 
                                              align='right',
                                              na.pad = TRUE),
                  by=device]
test_dt[, id_fact := as.factor(id)]
test_dt[, height := as.factor(height)]


mod <- gam(N ~ s(zt, bs="cp" ) +  s(day_mean_temp) +
             te(zt, day_mean_temp) +
             s(id_fact,   bs='re'), # include trap height nested in id!?
           # s(height,  by=id_fact, bs='re'), # include trap height nested in id!?
           data=test_dt,
           family = 'poisson', select = TRUE)
summary(mod)
AIC(mod)



vis.gam(mod,  type='response', theta=-35, phi=10)
vis.gam(mod,  type='response', theta=-20, phi=20)
vis.gam(mod,  type='response', theta=-130, phi=10)
