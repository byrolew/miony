track <- function(momentum){
  end.of.bigger.field <- 3.5
  dist1 <- 4
  dist2 <- 5
  dist3 <- 6
  distances <- c(dist1, dist2, dist3)
  r1 <- (momentum/(4))*(10/3) #Teraz powinny byc tu metry, ale moze Pan sprawdzic
  r2 <- 2*r1
  center1 <- c(0, r1)
  cos.angle.between.radius1 <- ((2*r1^2 - end.of.bigger.field^2)/(2*r1^2))
  sin.angle.between.radius1 <- sqrt(1 - cos.angle.between.radius1^2)
  center2 <- center1 + c((r1+r2)*sin.angle.between.radius1, -(r1+r2)*cos.angle.between.radius1)
  odl.center2 <- sqrt(center2[1]^2 + center2[2]^2)
  angle.center2 <- atan(center2[2]/center2[1])
  measurements <- c(momentum)
  idx <- 2
  for(i in distances){
    cos.angle <- (i^2 + odl.center2^2 - r2^2)/(2*i*odl.center2)
    measurements[idx] <- round((4096/(2*pi))*(acos(cos.angle) + angle.center2))
    idx <- idx + 1
  }
  return(measurements)
}

standard.error <- function(measurements){
  errors <- round(rnorm(3, 0, 1/(5*measurements[1]))*(4096/(2*pi)))
  #jako, ze i tak bedziemy zmieniac rozklad, z jakiego korzystamy, to uznalam,
  #ze moge na razie nie bawic sie i dac stala
  for(i in c(2:4)){
    measurements[i] <- measurements[i] + errors[1]
  }
  for(i in c(3, 4)){
    measurements[i] <- measurements[i] + errors[2]
  }
  measurements[4] <- measurements[4] + errors[3]
  return(measurements)
}

change.direction <- function(measurements){
  dir = round(runif(1, 0, 4096))
  measurements[2:4] = (measurements[2:4] + dir)%%4096
  return(measurements)
}

get.single.track <- function(m) {
  t <- track(m)
  s <- standard.error(t)
  change.direction(s)
}

sym <- t(sapply(runif(1e6, 2.1, 100), get.single.track))
sym <- data.frame(sym)
colnames(sym) <- c('momentum', 'loc1', 'loc2', 'loc3')
write.csv(sym, 'symulacja-20170323.csv', row.names=F)






