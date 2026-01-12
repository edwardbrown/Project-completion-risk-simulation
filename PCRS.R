# This code can be used to simulate project completion failur risk.
# The first section uses deSolve as an exemplar and provides a chart, though it can be extended.
# The second section provides the bootsrapping and stores the number of project tasks not completed
# and calculates an estimate of the proportion of failures that occur. 
# Edward G. Brown 2025

library(deSolve)

pcrs_d <- function(t, state, parameters) {
  with(as.list(c(state)), {
    # the replace argument isn't needed as we are generating only 1 number, but remember to leave it if you alter to sample > 1
    db <-  (sample(80:100,1, replace=TRUE) ) # project tasks completed [lower/upper see below]
    da <-  ifelse ((a - b) < 0, 0, (a - b) ) # tasks left to be completed at end of month
    list(c(db, da))
  })
}

# initial state and control for number and interval of iterations
state      <- c(b = 80, a = 1000) # b is the initial tasks completed, a is remaining tasks
times      <- seq(0, 12, by = 1) # 12 us number of months remaining

out <- ode(y = state, times = times, func = pcrs_d, parms = NULL, method="iteration")

matplot.deSolve(out) #generates a plot
plot(out, type="line", which="a")

# Uncomment to display the remaining tasks.
# out[,3]


# Uncomment the next section to do multiple single runs with a comparative chart.
# You will need to change the upper/lower values and b above prior to each run. Alternatively,
# you could modify the function to accept parameters.

# out2 <- ode(y = state, times = times, func = pcrs_d, parms = NULL, method="iteration")
# out3 <- ode(y = state, times = times, func = pcrs_d, parms = NULL, method="iteration")
# 
# plot(out,out2,out3, type="l", which="a",
#      ylab = c("Remaining Tasks"),
#      xlab = "Time (d)", main = c("Three Runs: 40, 60, 80 to 100"),
#      col = c("red", "blue", "darkred"))



# below is the primary function to calculate proportion of time the project task goal is not met
pcrsfunc <- function(m,l,u,t,r,d) {
  
  months <- m # months left to complete project
  lower <- l  # lowest number of tasks observed/expected to be completed
  upper <- u  # highest number of tasks observed/expected to be completed
  threshold <- t # goal is to have zero tasks remaining at project end time
  remaining <- r # number of tasks remaining at start of simulation
  returns <- d # data to return, either one value (1) or multiple (2) in a 2 column matrix
  
  xbar.completed <- rep(NA,months)
  xbar.remaining <- rep(NA,months)
  
  for (i in 1:months)
    # the replace argument isn't needed as we are generating only 1 number, but remember to leave it if you alter to sample > 1
    xbar.completed[[i]] <-sample(lower:upper, 1, replace=TRUE)
  
  xbar.remaining[[1]] = remaining # number of tasks remaining at start of simulation
  
  for (i in 1:months)
    xbar.remaining[[i+1]] = ifelse ((xbar.remaining[i] - xbar.completed[i]) <= 0, 0, (xbar.remaining[i] - xbar.completed[i]) ) # remaining tasks
  
  if (returns == 2) {
    # Let's output both the median completed per month and the remaining per month
    # returning both can be useful for further testing (including senstivity analysis)
    output <- cbind(median(xbar.completed[]),xbar.remaining[i])
    return(output)
  } else {
    return(xbar.remaining[i])
    
    # alternatively, rather than return tasks remaining you can return success/failure 
    # over_threshold <- ifelse (xbar.remaining[months] > threshold,1,0) # 1 is failure
    # return(over_threshold)
    
    # another way is to just do the calc in one go, but you won't be able to store/return information from
    # individual runs if need be and it won't simulate the real situation as closely so you won't be able to step
    # through the process, though the end results may be the same. 
    # To use this method, uncomment the following lines, and comment all the lines xbar lines and for loops above
    
    # completed <- sum(sample(lower:upper, months, replace=TRUE))
    # remaining <- ifelse (remaining - completed <= 0,0,(remaining-completed))
    # return(remaining)
  }
  
} # end of function

reps <- 1000 # number of bootstrap replicates, increase/decrease as desired

# bresults <- rep(NA,reps) # for storing the results (use this if just one number)
bresults <- matrix(0,nrow=reps,ncol=2) # use if returning the matrix

for (i in 1:reps)
  # bresults[i] <- pcrsfunc(12,80,100,0,1000,1) # use this if just one number
  bresults[i,] <- pcrsfunc(12,80,100,0,1000,2)

# the following summarize, plot, and provide 95% CI for number of tasks remaining
# if returning just one value, remove the [,*]
sd(bresults[,2])
summary(bresults[,2])
hist(bresults[,2])
# the 95% CI
quantile(bresults[,2], 0.025)
quantile(bresults[,2], 0.975)

# store in a data frame
dataview <- as.data.frame(bresults)

# the following calculates an estimate of the proportion of failure across all bootstrap replicates
# prop_failure <- length(dataview$bresults[dataview$bresults > 0])/reps # use this if returning 1 variable
prop_failure <- length(dataview$V2[dataview$V2 > 0])/reps # use this if returning the matrix
prop_failure




# this matrix will store our results when we iterate the function over a range for the lower (or upper) boundary value
# it's important to set the last argument to be the correct column size
overall <- matrix(0,reps,60)

# iterate over random lower bounds (e.g., 99, 98, 97 ... 40) while keeping upper bound fixed
for (j in 1:60) 
  for (i in 1:reps)
    overall[i,j] <- pcrsfunc(12,(100-j),100,0,1000,1)

# data frame if desired
# overallview <- as.data.frame(overall)

# proportion of failure by column (in this example, by the lower boundary, beginning with 99 all the way down to 40)
prop_failure_overall <- apply(overall,2, function(x) length(x[x > 0])/reps)

# a couple of plots
plot(prop_failure_overall)
hist(prop_failure_overall)

# data frame if desired. Set the number below to match the upper boundary used above (e.g., 100)
# You should end up with a data frame showing the proportion of failure for each lower/upper combo you simulated
prop_failure_overallview <- as.data.frame(prop_failure_overall)
prop_failure_overallview <- as.data.frame(prop_failure_overall)
prop_failure_overallview$lower <- (100-seq.int(nrow(prop_failure_overallview)))
prop_failure_overallview$upper <- 100

# a plot showing proportion of failure by the lower boundary
plot(prop_failure_overallview$prop_failure_overall,prop_failure_overallview$lower, type="l")


# finally, select out the range that aligns with an acceptable failure proportion (e.g., 0.05)
library(dplyr)
acceptable_range <- prop_failure_overallview %>%
  filter(prop_failure_overall <= 0.05)

