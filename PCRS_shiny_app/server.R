library(deSolve)

function(input, output, session) {

    output$plot <- renderPlot({

    pcrs_d <- function(t, state, parameters) {
      with(as.list(c(state)), {

        # the replace argument isn't needed as we are generating only 1 number, but remember to leave it if you alter to sample > 1
        db <-  (sample(input$lower:input$upper,1, replace=TRUE) ) # project tasks completed [lower/upper see below]
        da <-  ifelse ((a - b) < 0, 0, (a - b) ) # tasks left to be completed at end of month
        list(c(db, da))
      })
    }
    
    # initial state and control for number and interval of iterations
    state      <- c(b = input$lower, a = input$remaining) # b is the initial tasks completed, a is remaining tasks
    times      <- seq(0, input$months, by = 1) # number of months remaining

    out <- ode(y = state, times = times, func = pcrs_d, parms = NULL, method="iteration")
    
    # matplot.deSolve(out) #generates a plot
    plot(out, type="b", which="a",
         xlim=c(0,input$months), ylim=c(0,input$remaining),
         ylab="remaining", xlab="completed"
         , main="completed task simulation - one run"
         , col="red")
        
  })

  # TO DO
  # output$summary <- renderPrint({
  #   summary(cars)
  # })
  # 
  # output$table <- DT::renderDT({
  #   DT::datatable(db)
  # })
}
