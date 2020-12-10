LATITUDE = 49.2433713
LONGITUDE = -121.8459807

OUR_THEME <- theme_bw()

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

LABEL_MAP =  {c( '3'='Insecta.Hemiptera.Cicadellidae.Edwardsiana.*',
                '4'='Insecta.Diptera.Drosophilidae.Drosophila.suzukii',
                '5'='Insecta.Diptera.Drosophilidae.*',
                '6'='Insecta.Diptera.Psychodidae.*',
                '7'='Insecta.Diptera.Culicidae.*',
                '8'='Insecta.Diptera.Muscidae.*',
                '9'='Insecta.Diptera.Sciaridae.*',
                '10'='Insecta.Coleoptera.Curculionidae.*',
                '11'='Insecta.Coleoptera.Coccinellidae.*',
                '12'='Insecta.Coleoptera.*',
                '13'='Insecta.Hymenoptera.Figitidae.*',
                '14'='Insecta.Hymenoptera.*',
                '2'='Insecta')}

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

  # Not used for now, assume next sunrise is 24h  after previous
  z_next_day_onset = next_day_onset - day_onset

  # the times modulo 24h (ZT), where zt=0 => sunrise
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

