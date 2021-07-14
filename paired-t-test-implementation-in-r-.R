#with default r function
before <-c(10,9,11,11,5,9)
after <-c(15,9,10,19,17,15)
t.test(after,before,paired = T)
#-------------------calculations start here--------------------------
#with calculations
before <-c(10,9,11,11,5,9)
after <-c(15,9,10,19,17,15)
vector1 <- c()
vector2 <- c()
#calculate D
for (i in 1:length(before)) {
  vector1[i] <- (after[[i]] - before[[i]])
}
vector1
d <- sum(vector1)
d
#calculate D²
for (i in 1:length(before)) {
  vector2[i] <- (after[[i]] - before[[i]]) * (after[[i]] - before[[i]])
}
d2 <- sum (vector2)
d2
#calculate md
md <- d / length(before)
md
#calculate SS
ss <- d2 - d * d / length(before)
ss
#calculate s²
s2 <- ss / (length(before)-1)
s2
#calculate SMD
smd <- sqrt(s2/ length(before)       )
smd
#calculate t according to null hypothesis
t <- (md - 0) / smd
t

