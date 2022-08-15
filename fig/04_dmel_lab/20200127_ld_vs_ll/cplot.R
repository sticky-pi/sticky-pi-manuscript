

cplot <- function(dt, out, palette){
  pdf(w=10,h=6, out)


  pl = ggetho(dt, aes(y=N, color =condition, group=id), summary_time_window = mins(10)) +
    stat_ld_annotations() +
    stat_pop_etho() +facet_wrap(~condition*start) + geom_point(size=.5) +
    scale_color_manual(values=palette) +
    coord_cartesian(ylim=c(0,500))
  print(pl)          
  
  pl = ggetho(dt, aes(y=dN, fill = condition),
              time_wrap = hours(24), summary_time_window=mins(30), time_offset = hours(6)
  ) + 
    stat_ld_annotations() +
    stat_pop_etho()  +
    scale_y_continuous(name=expression(frac(dN, dt)~(h^-1))) +
    scale_color_manual(values=palette)+   scale_fill_manual(values=palette)+
    coord_cartesian(ylim=c(0,17.5)) + facet_wrap(~ condition, ncol=1)
  print(pl)
  
  per_dt <- periodogram(dN, na.omit(dt), FUN = ac_periodogram, resample_rate = 1/mins(10))
  per_dt

  
  pl = ggperio(per_dt, aes(period, power, colour=condition)) + 
    geom_line(aes(group=id), alpha=.5) +
    geom_hline(yintercept=0, linetype=2, col='green') + 
    stat_pop_etho()  +
    scale_color_manual(values=palette)+   scale_fill_manual(values=palette)+
    scale_y_continuous(name='ACF')+
    coord_cartesian(ylim=c(-.2,.6)) + facet_wrap(~ condition, ncol=1) 
  
  print(pl)
  dev.off()
}
