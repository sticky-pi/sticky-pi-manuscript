# Title     : TODO
# Objective : TODO
# Created by: quentin
# Created on: 25/11/2020

library(data.table)
library(ggplot2)
library(patchwork)
library(jsonlite)
source('../helpers.R')

l <- jsonlite::fromJSON('./validation_results/results.json')
dt <- as.data.table(l)

dt[, .(precision = sum(in_gt & in_im)/ sum(in_im),
		recall = sum(in_gt & in_im)/ sum(in_gt)),
	]
 #
 #dt[, .(recall = sum(in_gt & in_im)/ sum(in_gt),
	#	precision   = sum(in_gt & in_im) / sum(in_im)),
	#	by=filename]


#recall
# ggplot(dt[in_gt==T & !in_im], aes(area, as.numeric(in_gt & in_im))) + geom_smooth() + geom_point(alpha=.1)+ scale_x_log10()

#P(false_neg)
p1 <- ggplot(dt[in_gt==T], aes(area, as.numeric(!in_im))) +
					geom_smooth() + geom_point(alpha=.1)+ scale_x_log10()
#p1
p2 <- ggplot(dt[in_gt==T], aes(area)) +
					geom_histogram() + scale_x_log10()

# fixme, here use a GAM to fit the smooth

x_limits = c(30, 2e5)
y_hist_limits = c(0,1000)

layers = function(y_name){list(
	geom_smooth(method="gam", method.args = list(family = "binomial")),
	geom_point(alpha=.1, shape='|'),
	scale_x_log10(name = 'Insect area (px)', limits=x_limits,
	              labels =  scientific_10),
	scale_y_continuous(name=y_name), OUR_THEME
)}
hist_layers = function(){list(
		geom_histogram(),
		scale_x_log10(name = NULL, labels = NULL, limits=x_limits) ,
		scale_y_continuous(limits = y_hist_limits, breaks = c(0,500,1000)),
		OUR_THEME
)}

pdf('plots.pdf', w=4, h=4)
#recall
hist_pl <- ggplot(dt[in_gt==T], aes(area)) + hist_layers()
p <- ggplot(dt[in_gt==T], aes(area, as.numeric(in_gt & in_im))) + layers(y_name='Recall')
hist_pl/ p  + plot_layout(height = c(1, 3))

hist_pl <- ggplot(dt[in_im==T], aes(area)) + hist_layers()
p <- ggplot(dt[in_im==T], aes(area, as.numeric(in_gt & in_im))) +
		layers(y_name='Precision')
hist_pl/ p  + plot_layout(height = c(1, 4))
dev.off()

# 75% of insects are > 1000px
mean(dt[in_gt==T,area > 1000])

# recall is .1 better for object > 1000 px
dt[area > 1000, .(precision = sum(in_gt & in_im)/ sum(in_im),
		recall = sum(in_gt & in_im)/ sum(in_gt)),
	]


