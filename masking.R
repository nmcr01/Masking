#The basic Leonhardt model
p_m<-1/6
p_nm<-2/6

s<-seq(0,20,1)

S_m<-(1-p_m)^s
S_nm<-(1-p_nm)^s
plot(s,S_m, col="blue", lwd=3,ty="b",
     xlab="social situations encountered", ylab="probability healthy")
 points(s,S_nm, col="red", lwd=3, ty="b")
 legend("topright", inset=0.015, lty=c(1,1), col=c("blue","red"),
        legend=c("masked","not masked"))


#What's the advantage over the course of encounters from masking? 
adv_m<-S_m-S_nm
plot(s,adv_m, col="black", lwd=3,ty="b",
      ylim=c(0,0.3),
      xlab="social situations encountered", ylab="gain from masking")
 
# Those were expected value results.  What does it look like if we do
# something stochastic.  Follow cohorts of 100 agents of each type.

# Generate some random numbers from the uniform distribution on
# the interval [0,1]

risk<-vector(length = 2000)
set.seed(12345)
risk<-runif(2000,0,1)

# Make some matrices to define the cohorts. Put the chance
# data from the uniform into the first one.
luck<-matrix(nrow=100, ncol=20, byrow = TRUE,
             data=risk)
ind_out_m<-matrix(nrow=100, ncol=20)
ind_out_nm<-matrix(nrow=100, ncol=20)

# Use the chance data and Leonhardt's dice numbers to decide the
# fate of each member of the two cohorts and coerce the results
# of the coin flips to be numbers not logicals.
ind_out_m[,]<-as.numeric(luck[,]>p_m)
ind_out_nm[,]<-as.numeric(luck[,]>p_nm)

# Make vectors to store the outcomes for each individual for each
# cohort
h_dur_m<-vector(length = 100)
h_dur_nm<-vector(length = 100)

# Use the "match" function to find the first occurrence of 
# 0 in each person's time line - indicating the event
# at which they contract COVID

for (i in 1:100){
  h_dur_m[i]<-match(0,ind_out_m[i,])
  h_dur_nm[i]<-match(0,ind_out_nm[i,])
}
# For individuals who never contract COVID, ther are
# no zero values, so match returns NA.
# Get rid of NA values. Replace them with 21 (i.e.
# the next largest value compared with the max
# of the sequence.)

m_frame<-as.data.frame(h_dur_m)
m_frame[is.na(m_frame)]<-21
h_dur_m<-as.vector(m_frame)

# hist(h_dur_m$h_dur_m)
# hist(h_dur_nm)
# summary(h_dur_nm)
# summary(h_dur_m$h_dur_m)

# Pull the data back into vectors if it 
# has been in a data frame to clear NA
# as above.
surv_nm<-h_dur_nm
surv_m<-h_dur_m$h_dur_m

# Sort the data by the event number at which COVID
# happens, to plot the empirical cumulative distribution
# of cases.

sort_surv_m<-sort(surv_m, decreasing = FALSE)
sort_surv_nm<-sort(surv_nm, decreasing = FALSE)
counting<-seq(1,100,1)
plot(sort_surv_m, counting, ty="l", col="blue", lwd=3,
     xlab="number of social exposure events", ylab="cumulative infected (%)")
 lines(sort_surv_nm, counting, col="red", lwd=3)
 legend("bottomright", inset=0.05, lty=c(1,1), col=c("blue","red"),
        legend=c("masked","not masked"))
 abline(h=50, lty=2, col="gray", lwd=3)
 

 
# Example with reduced risk exposure and social interaction for
# maskers.  We repeat all of the above for maskers, but now
# assume that they cut their number of interactions in half
# and they avoid high risk situations.
 
 new_risk<-vector(length = 1000)
 set.seed(12345)
 new_risk<-runif(1000,0.1,1)
 
 luck2<-matrix(nrow=100, ncol=10, byrow = TRUE,
              data=new_risk)
 ind_out_m2<-matrix(nrow=100, ncol=10)
 ind_out_m2[,]<-as.numeric(luck2[,]>p_m)
 # Get rid of NA values. Replace them with 21 (i.e.
 # the next largest value compared with the max
 # of the sequence.)
 
 
 h_dur_m2<-vector(length = 100)
 for (i in 1:100){
   h_dur_m2[i]<-match(0,ind_out_m2[i,])
 }
 m_frame2<-2*(as.data.frame(h_dur_m2))
 m_frame2[is.na(m_frame2)]<-21
 h_dur_m2<-as.vector(m_frame2)
 surv_m2<-h_dur_m2$h_dur_m2
 sort_surv_m2<-sort(surv_m2, decreasing = FALSE)
 count2<-seq(2,20,2)
 
 plot(sort_surv_m2, counting, ty="l", col="blue", lwd=3,
      xlab="number of potential social exposure events", ylab="cumulative infected (%)")
 lines(sort_surv_nm, counting, col="red", lwd=3)
 legend("bottomright", inset=0.05, lty=c(1,1), col=c("blue","red"),
        legend=c("masked","not masked"))
 abline(h=50, lty=2, col="gray", lwd=3)
 