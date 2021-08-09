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

p1 <- ggetho(dt[xmv(label_itc)==3], aes(y=temp, x=wt), 	y_time_window = hours(1)) +
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




#test_dt <- dt[xmv(pred) %in% c(3, 6, 8,7)]
test_dt <- dt
tmp_dt <- cbind(sun_rise_set(test_dt[,start_datetime, meta=T], LATITUDE, LONGITUDE),
      start_datetime = test_dt[,start_datetime, meta=T])

test_dt[, wstart_datetime := tmp_dt[,wzt(start_datetime, sunrise,sunset)], meta=T]

pdf(w=18,h=8, 'misc.pdf')

ggetho(test_dt, aes(x=wt,y=N),
             summary_time_window = hours(24)) +
                stat_pop_etho() +
                common_layers(ncol = 43) 
print(p)
p <- ggetho(test_dt, aes(x=wt,y=N),
           time_wrap = hours(24),
           time_offset = hours(6), summary_time_window = hours(1) ) +
              stat_pop_etho()  +
              common_layers(ncol=4) +
            stat_ld_annotations()
print(p)
dev.off()


pdf(w=4,h=4, 'mds.pdf')
start_dt <- rejoin(behavr::bin_apply_all(dt, N, x=t, x_bin_length=hours(1), wrap_x_by=days(1)))
start_dt <- start_dt[label_itc %in% c(2,6,7,8,9,10,16, 17)]
start_dt = start_dt[, .(N=mean(N)),keyby=.(label_itc,t)]

labs <- sort(unique(start_dt[,label_itc]))


dist_mat <- matrix(0, ncol=length(labs), nrow=length(labs),dimnames=list(labs,labs))
combinations <- combn(as.character(labs),2)
apply(combinations, 2, function(x){
  i = x[1]
  j = x[2]
  # d  <- mutinformation(start_dt[label_itc == i]$N, start_dt[label_itc == j]$N)
  d  <- 1 -cor(start_dt[label_itc == i]$N, start_dt[label_itc == j]$N)
  dist_mat[i,j] <<- d
  dist_mat[j,i] <<- d
  NULL
})
diag(dist_mat) <- 0
rownames(dist_mat) <- LABEL_MAP[labs]
colnames(dist_mat) <- LABEL_MAP[labs]


fit <- cmdscale(dist_mat,eig=TRUE, k=2) # k is the number of dim
d <- as.data.table(fit$points)
d[, taxon :=  rownames(fit$points)]
p <- ggplot(d, aes(-V1, -V2))  + 
  geom_point() +
  geom_label_repel(aes(label =d$taxon), size = 3.5) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) + OUR_THEME + 
  scale_x_continuous(name = 'Dimension 1') +
  scale_y_continuous(name = 'Dimension 2')
print(p)
dev.off()

rep_fun = function(i, init="torgerson"){
	print(i)
	dt_rep <- copy(dt)
	bci_rep_tool <- function(sdd){
	  v = sdd[, rep(t, N)]
	  spl <- data.table(t=as.integer(sample(v, size=length(v),replace = T)))
	  spl <- spl[, .(N_boot = .N), by=t]
	  o <- spl[sdd, on='t']
	  o[, N_boot := ifelse(is.na(N_boot), 0, N_boot)]
  	  o[, N := N_boot]
	  o
	}
	if(i >1)
		dt_rep <- dt_rep[,bci_rep_tool(.SD), by=id]

	start_dt <- rejoin(behavr::bin_apply_all(dt_rep, N, x=t, x_bin_length=hours(1), wrap_x_by=days(1)))
	start_dt <- start_dt[label_itc %in% c(2,6,7,8,9,10,16, 17)]
	start_dt = start_dt[, .(N=mean(N)),keyby=.(label_itc,t)]

	labs <- sort(unique(start_dt[,label_itc]))


	dist_mat <- matrix(0, ncol=length(labs), nrow=length(labs),dimnames=list(labs,labs))
	combinations <- combn(as.character(labs),2)
	apply(combinations, 2, function(x){
	  i = x[1]
	  j = x[2]
	  # d  <- mutinformation(start_dt[label_itc == i]$N, start_dt[label_itc == j]$N)
	  d  <- 1 -cor(start_dt[label_itc == i]$N, start_dt[label_itc == j]$N)
	  dist_mat[i,j] <<- d
	  dist_mat[j,i] <<- d
	  NULL
	})
	fit <-  mds(dist_mat, init=init) # k is the number of dim
}


set.seed(678934)
l = list(rep_fun(1))
l <- c(lapply(2:500, rep_fun, init=torgerson(l[[1]]$confdist)))


ref = l[[1]]


pc_res <- lapply( 2:length(l),function(i){
	x = l[[i]]
	x_pc <- Procrustes(ref$conf, x$conf)$Y
	d <- as.data.table(x_pc)
	d[, id :=  rownames(x_pc)]
	d[, rep := i]
	d
})


d <- as.data.table(ref$conf)
d[, id :=  rownames(ref$conf)]
d[, rep := 1]
pc_res[[length(pc_res) + 1]] <- d

d <- rbindlist(pc_res)
d[, taxon:=LABEL_MAP[id]]

p <- ggplot(d, aes(-D1, -D2, colour=taxon))  + 
  geom_point(size=.1, alpha=.2, shape=16) +
    geom_point(data=d[rep==1], shape=20) +
  scale_shape_manual(values=1:length(unique(d$taxon))) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) + OUR_THEME + 
  scale_x_continuous(name = 'Dimension 1') +
  scale_y_continuous(name = 'Dimension 2') + 
  stat_ellipse()

pdf(w=4,h=4, 'mds.pdf')
print(p)
dev.off()

# 


