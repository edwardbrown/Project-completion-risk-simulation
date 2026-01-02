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



# below is the primary function to calculate percentage of time the project task goal is not met

pcrsfunc <- function() {
  
  months <- 12 # months left to complete project
  lower <- 80 # lowest number of tasks observed/expected to be completed
  upper <- 100 # highest number of tasks observed/expected to be completed
  
  threshold <- 0 # goal is to have zero tasks remaining at project end time
  
  xbar.completed <- rep(NA,months)
  xbar.remaining <- rep(NA,months)

  for (i in 1:months)
    # the replace argument isn't needed as we are generating only 1 number, but remember to leave it if you alter to sample > 1
    xbar.completed[[i]] <-sample(lower:upper, 1, replace=TRUE) 
  
  xbar.remaining[[1]] = 1000 # number of tasks remaining at start of simulation
  
  for (i in 1:months)
    xbar.remaining[[i+1]] = ifelse ((xbar.remaining[i] - xbar.completed[i]) <= 0, 0, (xbar.remaining[i] - xbar.completed[i]) ) # remaining tasks

  # alternatively, rather than return tasks remaining you can return success/failure 
  # over_threshold <- ifelse (xbar.remaining[months] > threshold,1,0) # 1 is failure
  # return(over_threshold)

    return(xbar.remaining[i])
} # end of function


reps <- 1000 # number of bootstrap replicates, increase/decrease as desired
bresults <- rep(NA,reps) # for storing the bootsrap results

# run the bootsrap
for (i in 1:reps)
  bresults[[i]] <- pcrsfunc()

# the following summarize, plot, and provide 95% CI for number of tasks remaining
sd(bresults)
summary(bresults)
hist(bresults)
# the 95% CI
quantile(bresults, 0.025)
quantile(bresults, 0.975)

# store in a data frame
dataview <- as.data.frame(bresults)

# the following calculates an estimate of the proportion of failure across all bootstrap replicates
prop_failure <- length(dataview$bresults[dataview$bresults > 0])/reps
prop_failure
