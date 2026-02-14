# This version uses Accept Reject methods to generate multiple sampling distributions,
# each with an increase to the mean of one standard deviation. This can of course be reversed,
# i.e. decrease in sd.
# Edward G. Brown 2026

rm(list=ls())
# Clear all plots and suppress errors if no plots exist
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)
gc() # garbage collection
set.seed(2026)

#----
# Accept Reject method, starting with observations from your process
#----

# observed deployments so far
deployed_actual <- sample(10:100,20)

dp_mean <- mean(deployed_actual)
dp_sd <- sd(deployed_actual)

months_left <- 12
to_be_deployed <- 1000
min_deployed <- min(deployed_actual)
max_deployed <- 150 # the max deployed goal

l <- min_deployed
r <- max_deployed
n <- 10^4

library(AcceptReject)
library(cowplot)
library(tictoc)

# this method will move the mean across the range of realistic values
simulationA <- function(n, l, r, m, s) {
  # as.vector(
  AcceptReject::accept_reject(
    n = n,
    f = dnorm,
    continuous = TRUE,
    args_f = list(mean = m, sd = (s+1)),
    xlim = c(l-s, r+s),
    parallel = FALSE
  )
  # )
}

ourSim0 <- simulationA(n,min_deployed,max_deployed,dp_mean+(0*dp_sd), dp_sd) # k = 0
ourSim1 <- simulationA(n,min_deployed,max_deployed,dp_mean+(1*dp_sd), dp_sd) # k = 1
ourSim2 <- simulationA(n,min_deployed,max_deployed,dp_mean+(2*dp_sd), dp_sd) # k = 2
ourSim3 <- simulationA(n,min_deployed,max_deployed,dp_mean+(3*dp_sd), dp_sd) # k = 3
ourSim4 <- simulationA(n,min_deployed,max_deployed,dp_mean+(4*dp_sd), dp_sd) # k = 4

a <- plot(ourSim0)
b <- plot(ourSim1)
c <- plot(ourSim2)
d <- plot(ourSim3)
e <- plot(ourSim4)

plot_grid(a, b, c, d, e, nrow = 2L)

#----
# Main function and MC method
#----

pcrsfunc <- function(m,l,u,t,r,d,rmeans,k) {
  
  months <- m # months left to complete project
  lower <- l  # lowest number of tasks observed/expected to be completed
  upper <- u  # highest number of tasks observed/expected to be completed
  threshold <- t # goal is to have zero tasks remaining at project end time
  remaining <- r # number of tasks remaining at start of simulation
  returns <- d # data to return, either one value (1) or multiple (2) in a 2 column matrix
  rolling <- rmeans # use rolling means technique
  kindex <- k
  
  xbar.completed <- rep(NA,months)
  xbar.remaining <- rep(NA,months)
  
  for (i in 1:months)
    if (rmeans == 1) {
      if (kindex == 1) {
        xbar.completed[[i]] <- sample(ourSim1, 1, replace=TRUE)
      } else if(kindex == 2) {
        xbar.completed[[i]] <- sample(ourSim2, 1, replace=TRUE)
      } else if(kindex == 3) {
        xbar.completed[[i]] <- sample(ourSim3, 1, replace=TRUE)
      } else if(kindex == 4) {
        xbar.completed[[i]] <- sample(ourSim4, 1, replace=TRUE)
     }
    } else {
      xbar.completed[[i]] <- sample(lower:upper, 1, replace=TRUE)
    }
  
  xbar.remaining[[1]] = remaining # number of tasks remaining at start of simulation
  
  for (i in 1:months)
    xbar.remaining[[i+1]] = ifelse ((xbar.remaining[i] - xbar.completed[i]) <= 0, 0, (xbar.remaining[i] - xbar.completed[i]) ) # remaining tasks
  
  if (returns == 2) {
    # Let's output both the median completed per month and the remaining per month
    # returning both can be useful for further testing (including sensitivity analysis)
    # output <- cbind(median(xbar.completed[i]),xbar.remaining[i])
    output <- cbind(median(xbar.completed[]),max(xbar.completed[]),min(xbar.completed[]),  xbar.remaining[i])
    return(output)
  } else {
    return(xbar.remaining[i])
    # return(xbar.completed[i])
    
  }
  
} # end of function

reps <- 10^4 # number of bootstrap replicates, increase/decrease as desired

# bresults <- rep(NA,reps) # for storing the results (use this if just one number)
bresults <- matrix(0,nrow=reps,ncol=4) # use if returning the matrix

tic()
for (i in 1:reps)
  # bresults[i] <- pcrsfunc(23,48,200,0,3361,1,0,NULL) # use this if just one number
  bresults[i,] <- pcrsfunc(months_left,min_deployed,max_deployed,0,to_be_deployed,2,0,NULL)

toc()

# the following summarize, plot, and provide 95% CI for number of tasks remaining
# if returning just one value, remove the [,*]
summary(bresults[,4])
hist(bresults[,4])
summary(bresults[,1])
hist(bresults[,1])

# store in a data frame
dataview <- as.data.frame(bresults)

# the following apply if we returned a matrix
colnames(dataview)[colnames(dataview) == "V1"] <- "median completed"
colnames(dataview)[colnames(dataview) == "V2"] <- "max completed"
colnames(dataview)[colnames(dataview) == "V3"] <- "min completed"
colnames(dataview)[colnames(dataview) == "V4"] <- "remaining"

# the following calculates an estimate of the proportion of failure across all bootstrap replicates
# prop_failure <- length(dataview$bresults[dataview$bresults > 0])/reps # use this if returning 1 variable
prop_failure <- length(dataview$remaining[dataview$remaining > 0])/reps # use this if returning the matrix
prop_failure


# this matrix will store our results when we iterate the function over a range for the lower (or upper) boundary value
# it's important to set the last argument to be the correct column size

mean_count <- 4 # the number of means we want to simulate (see accept reject above)

overall <- matrix(0,reps,mean_count)

tic()

for(k in 1:4)
  for (i in 1:reps)
    overall[i,k] <- pcrsfunc(months_left,max_deployed,max_deployed,0,to_be_deployed,1,1,k) # moving means

toc()

# data frame if desired
# overallview <- as.data.frame(overall)

# proportion of failure by column (in this example, by the lower boundary, beginning with 99 all the way down to 40)
prop_failure_overall <- apply(overall,2, function(x) length(x[x > 0])/reps)

# the moving mean version
prop_failure_overallview <- as.data.frame(prop_failure_overall)
prop_failure_overallview$Model <- c("Mean +1sd","Mean +2sd","Mean +3sd","Mean +4sd")


# finally, select out the range that aligns with an acceptable failure proportion (e.g., 0.05)
library(ggplot2)
df <- as.data.frame(deployed_actual)

boxplot(prop_failure_overall ~ Model, data=prop_failure_overallview)

hplot4 <- ggplot(prop_failure_overallview, aes(x=Model, y=prop_failure_overall)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Model")
hplot4

#----
# End 
#----
