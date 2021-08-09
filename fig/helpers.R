LATITUDE = 49.2433713
LONGITUDE = -121.8459807

OUR_THEME <- theme_bw()

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

LABEL_MAP =  c(
    "0" = "BG",
    "1" = "Insecta",
    "2" = "Edwardsiana",
    "3" = "Other Cicadellidae",
    "4" = "Drosophila suzukii",
    "5" = "Other Drosophilidae",
    "6" = "Psychodidae",
    "7" = "Culicidae",
    "8" = "Muscoidea",
    "9" = "Sciaridae",
    "10" = "Syrphidae",
    "11" = "Curculionidae",
    "12" = "Coccinellidae",
    "13" = "Elateridae",
    "14" = "Other Coleoptera",
    "15" = "Figitidae",
    "16" = "Halictidae",
    "17" = "Lepidoptera")

# 
# LABEL_MAP =  {c( '3'='Insecta.Hemiptera.Cicadellidae.Edwardsiana.*',
#                 '4'='Insecta.Diptera.Drosophilidae.Drosophila.suzukii',
#                 '5'='Insecta.Diptera.Drosophilidae.*',
#                 '6'='Insecta.Diptera.Psychodidae.*',
#                 '7'='Insecta.Diptera.Culicidae.*',
#                 '8'='Insecta.Diptera.Muscidae.*',
#                 '9'='Insecta.Diptera.Sciaridae.*',
#                 '10'='Insecta.Coleoptera.Curculionidae.*',
#                 '11'='Insecta.Coleoptera.Coccinellidae.*',
#                 '12'='Insecta.Coleoptera.*',
#                 '13'='Insecta.Hymenoptera.Figitidae.*',
#                 '14'='Insecta.Hymenoptera.*',
#                 '2'='Insecta')}

round_time <- function(t, r = behavr::mins(30)){
  as.POSIXct(round(as.numeric(t) / r) * r, origin='1970-01-01', tz='UTC')
}

compute_light_intensity <- function(et, bv, iso){
  log10(1+bv)/et
}

sun_rise_set <- function(datetime, lat, lng, direction=c('sunrise', 'sunset')){
  payload = data.table(datetime, lat, lng)
  payload[, date := as.POSIXct(as.Date(datetime))]
  # only retrieve one sunset/rise per day rather than uselessly
  # call in the function multiple times per day
  f = function(date, lat, lng, x){

              crds=matrix(c(lng, lat), nrow=1)
              o <- lapply(x,function(x){
                  maptools::sunriset(crds,
                                        direction=x,
                                        dateTime=date,
                                        POSIXct.out=TRUE)$time[1]
              })
              names(o) <- x
              o
  }

  unique_payload = unique(payload[,-'datetime'], by=c('date','lat','lng'))
  unique_payload[, row:=.I]
  results = unique_payload[, f(date,lat, lng, direction), by=row]
  unique_payload = unique_payload[results, on='row']
  unique_payload[payload, on=c('date','lat','lng')][, ..direction]
}

wzt <- function(t, day_onset, night_onset, next_day_onset=NULL, output_as_zt=FALSE){
  # In case t is a POSIXct (datetime), we will want to return also a POSIXct
  posix_input <- ifelse("POSIXct" %in% class(t), TRUE, FALSE)
  # if next day onset is not provided,
  # we assume it is 24h after the onset of the current day
  if(is.null(next_day_onset))
    next_day_onset = day_onset + hours(24)
  # all times are converted in days
  t <- as.numeric(t) / days(1)
  day_onset <- as.numeric(day_onset) / days(1)
  night_onset <- as.numeric(night_onset) / days(1)
  next_day_onset <- as.numeric(next_day_onset) / days(1)
  day_length = (night_onset - day_onset)
  night_length = (next_day_onset - night_onset)
  if(any(day_length < 0)) stop('Night onset must be after day onset')
  if(any(night_length < 0)) stop('Next day onset must be after night onset')
  # We ensure that day and night onsets are expressed within [t, t+1]
  day_onset <- floor(t) + day_onset %% 1
  night_onset  <- day_onset + day_length
  next_day_onset <- floor(t+1) + next_day_onset  %% 1

  # Not used for now, assume next sunrise is 24h after previous
  z_next_day_onset = next_day_onset - day_onset

  # the times modulo 24h (ZT), where zt = 0 => sunrise
  z = (t - day_onset) %% 1
  a = 1/(2 * day_length)
  a_prime = 1/(2 * (1 - day_length))
  b_prime =   1 - a_prime
  wzt <- ifelse(z < day_length , z * a , z * a_prime + b_prime)
  wzt = wzt %% 1
  if(output_as_zt)
    return(wzt * days(1) )
  out = wzt +  floor(t - day_onset) + floor(t)
  out = out * days(1)
  if(posix_input)
    out <- as.POSIXct(out, origin='1970-01-01', tz='GMT')
  out
}


make_dt <- function(){
  # expand metadata to all taxa studied
  metadata <- fread('metadata.csv')
  metadata[, start_datetime := fasttime::fastPOSIXct(start_datetime, tz='UTC')]
  metadata[, end_datetime := fasttime::fastPOSIXct(end_datetime, tz='UTC')]
  
  image_meta_dt <- fread('db_snapshot/images.csv')
  image_meta_dt = image_meta_dt[,.(datetime,device,  
                                   temp,hum, no_flash_shutter_speed, no_flash_bv, 
                                   no_flash_iso, no_flash_exposure_time)]
  image_meta_dt[, light_intensity := compute_light_intensity(no_flash_exposure_time, no_flash_bv, no_flash_iso)]
  image_meta_dt[, datetime := fasttime::fastPOSIXct(datetime, tz='UTC')]
  
  tmp_metadata = metadata[, .(device, start_datetime)]
  tmp_metadata[, datetime := start_datetime]
  setkey(image_meta_dt,  device, datetime)
  setkey(tmp_metadata, device, datetime)
  
  
  image_meta_dt <- tmp_metadata[image_meta_dt, roll=TRUE, nomatch=0]
  #sometimes temp is missing and encoded as 1e-6
  image_meta_dt[temp < 1e-3, temp := NA]
  
  
  tub_dt = fread('db_snapshot/itc_labels.csv')
  tub_dt = tub_dt[!is.na(label_itc)]
  
  setnames(tub_dt, c('device_series', 'start_datetime'),c('device', 'datetime'))
  n_insect_dt = tub_dt[ ,.N, keyby=c('label_itc', 'device','datetime', 'start_datetime_series')]
  n_insect_dt[, datetime := fasttime::fastPOSIXct(datetime, tz='UTC')]
  
  dt = n_insect_dt[, .SD[image_meta_dt,on=c('device', 'datetime')], by='label_itc']
  
  dt[is.na(N), N:=0]
  dt = dt[!label_itc %in% 0:1]
  dt[, id:=paste(label_itc,device,as.numeric(start_datetime), sep='.')]
  
  dt <- cbind(dt, dt[ , sun_rise_set(datetime, LATITUDE, LONGITUDE)])
  dt[, w_datetime := wzt(datetime, sunrise,sunset)]
  
  dt[ ,label_itc:= NULL]
  dt[ ,device:= NULL]
  dt[ ,start_datetime_series:= NULL]
  dt = dt[, .SD, keyby=id]
  
  # expand metadata to all taxa studied
  metadata = rbindlist(lapply(unique(tub_dt[,label_itc]), function(x)cbind(label_itc=x, metadata)))
  metadata[, id:=paste(label_itc,device,as.numeric(start_datetime), sep='.')]
  metadata = metadata[, .SD, keyby=id]
  
  dt = behavr(dt, metadata)
  dt[, taxon := factor(LABEL_MAP[as.character(label_itc)], levels=LABEL_MAP), meta=TRUE]
  dt[, label_itc := factor(label_itc, levels=names(LABEL_MAP)), meta=TRUE]
  
  
  dt[,t:=as.numeric(datetime, units='secs')]
  dt[, zt := t %% days(1)]
  
  dt[,wt:=as.numeric(w_datetime, units='secs')]
  dt[, wzt := wt %% days(1)]
  
}