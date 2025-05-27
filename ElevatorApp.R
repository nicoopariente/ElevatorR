library(shiny)
library(bslib)

# Define UI for app
ui <- page_sidebar(

  title = "Hello Shiny!",

  sidebar = sidebar(
  
    sliderInput("bins", "Slider", 
                min = 0, max = 100, value = 50),
  ),
  plotOutput(outputId = "distPlot")
)

# Define server logic
server <- function(input, output) {
  
  numberOfElevators <- 3 #input from user
  numberOfPeople <- 10 #input from user
  numberOfFloors <- 3 #input from user
  secondsOnADay <- 50400 #seconds from 6am to 10pm
  secondCount <- 0
  positionElevator <- rep(0, numberOfElevators)
  requestElevatorInstances <- sample(0:50400, numberOfPeople * 4, replace = TRUE)
  peopleWaiting <- list()
  stopFloor <- vector("list", numberOfElevators)
  peopleOnElevator <- numeric(numberOfElevators)
  peopleMovingToElevator <- rep(0, numberOfElevators)
  print(requestElevatorInstances)
  
  for (i in 1:numberOfElevators) {
    stopFloor[[i]] <- numeric()
  }
  
  waitStats <- data.frame(
    second = integer(),
    totalWaitingTime = numeric(),
    peopleWaiting = integer(),
    avgWaitTime = numeric()
  )
  
  while (secondCount < secondsOnADay) {
    peopleAddedToWaiting <- sum(requestElevatorInstances == secondCount)
    
    if(peopleAddedToWaiting > 0){
      for (i in 1:peopleAddedToWaiting) {
        #people waiting will contain vectors with two random floors, 0 or 1 depending if an elevator is on the way, and the last element will be a waiting time tracker
        peopleWaiting[[length(peopleWaiting) + 1]] <- c(sample(0:numberOfFloors, 2), 0, 0)
        priorityElevatorDirection <- sign(peopleWaiting[[1]][1] - peopleWaiting[[1]][2])
      }
    }
    
    
    for (j in 1:numberOfElevators) {
      
      if(length(stopFloor[[j]]) > 0 & peopleOnElevator[j] < 6 & peopleMovingToElevator[j] == 0){
        for (i in seq_along(peopleWaiting)) {
          direction <- peopleWaiting[[i]][1] - peopleWaiting[[i]][2]
          directionElevator <- positionElevator[j] - stopFloor[[j]][1]
          if(direction < 0 & directionElevator < 0 & positionElevator[j] < peopleWaiting[[i]][1] & !peopleWaiting[[i]][1] %in% stopFloor[[j]] & peopleWaiting[[i]][3] == 0){
            stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][1])
            stopFloor[[j]] <- sort(stopFloor[[j]])
            peopleOnElevator[j] <- peopleOnElevator[j] + 1
            peopleWaiting[[i]][3] <- 1
          }else if(direction > 0 & directionElevator > 0 & positionElevator[j] > peopleWaiting[[i]][1] & !peopleWaiting[[i]][1] %in% stopFloor[[j]] & peopleWaiting[[i]][3] == 0){
            stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][1])
            stopFloor[[j]] <- sort(stopFloor[[j]] , decreasing = TRUE)
            peopleOnElevator[j] <- peopleOnElevator[j] + 1
            peopleWaiting[[i]][3] <- 1
          }
        }
        
        if(stopFloor[[j]][1] > positionElevator[j]){
          positionElevator[j] <- positionElevator[j] + 0.5
          
        }else if(stopFloor[[j]][1] < positionElevator[j]){
          positionElevator[j] <- positionElevator[j] - 0.5
          
        }else{
          toBeDeleted <- c()
          for (i in seq_along(peopleWaiting)){
            if(peopleWaiting[[i]][1] == positionElevator[j]){
              if(!peopleWaiting[[i]][2] %in% stopFloor[[j]]){
                stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][2])
                if(stopFloor[[j]][1] > positionElevator[j]){
                  stopFloor[[j]] <- sort(stopFloor[[j]])
                }else{
                  stopFloor[[j]] <- sort(stopFloor[[j]], decreasing = TRUE)
                }
                
              }
              
              toBeDeleted <- c(toBeDeleted, i)
              peopleMovingToElevator[j] <- 10
            }
          }
          for(i in sort(toBeDeleted, decreasing = TRUE)){
            peopleWaiting <- peopleWaiting[-i]
          }
          stopFloor[[j]] <- stopFloor[[j]][-1]
          peopleOnElevator[j] <- peopleOnElevator[j] - length(toBeDeleted)
        }
        
      }else if(length(peopleWaiting) > 0 & peopleOnElevator[j] < 6 & peopleMovingToElevator[j] == 0){
        priorityRequest <- peopleWaiting[[1]][1]
        priorityTargetFloor <- peopleWaiting[[1]][2]
        
        if(priorityRequest > positionElevator[j]){
          positionElevator[j] <- positionElevator[j] + 0.5
          
        }else if(priorityRequest < positionElevator[j]){
          positionElevator[j] <- positionElevator[j] - 0.5
          
        }else{
          peopleMovingToElevator[j] <- 10
          stopFloor[[j]] <- c(stopFloor[[j]], priorityRequest)
          peopleWaiting <- peopleWaiting[-1]
          peopleOnElevator[j] <- peopleOnElevator[j] + 1
          if(length(peopleWaiting) > 1){
            toDelete <- c()
            for (i in 2:length(peopleWaiting)) {
              if(peopleWaiting[[i]][1] == priorityRequest & sign(priorityRequest - priorityTargetFloor) == sign(peopleWaiting[[i]][1] - peopleWaiting[[i]][2])){
                stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][2])  
                if(stopFloor[[j]][1] > positionElevator[j]){
                  stopFloor[[j]] <- sort(stopFloor[[j]])
                }else{
                  stopFloor[[j]] <- sort(stopFloor[[j]], decreasing = TRUE)
                }
                toDelete <- c(toDelete, i)
              }
            }
            for (i in sort(toDelete, decreasing = TRUE)) {
              peopleWaiting <- peopleWaiting[-i]
              peopleOnElevator[j] <- peopleOnElevator[j] + 1
            }
          }
        }
        
      }
      
      peopleMovingToElevator[j] <- peopleMovingToElevator[j] - 1
      
    }
    totalWaitingTime <- 0
    peopleWaitingNow <- 0
    
    for (k in seq_along(peopleWaiting)) {
      peopleWaiting[[k]][4] <- peopleWaiting[[k]][4] + 1
      totalWaitingTime <- totalWaitingTime + peopleWaiting[[k]][4]
      peopleWaitingNow <- peopleWaitingNow + 1
    }
    
    waitStats <- rbind(waitStats, data.frame(
      second = secondCount,
      totalWaitTime = totalWaitingTime,
      peopleWaiting = peopleWaitingNow,
      avgWaitTime = ifelse(peopleWaitingNow > 0, totalWaitingTime / peopleWaitingNow, 0)
      
    ))
    
    secondCount <- secondCount + 1
  }

  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

probabilityPick <- function(numberOfPeople){
  
  totalRequests <- numberOfPeople * 4
  morningPeak <- 3600:14400
  eveningPeak <- 36000:43200
  offPeak <- c(0:3599, 14401:35999, 43201:50400)
  
  numMorning <- round(totalRequests * 0.3)
  numEvening <- round(totalRequests * 0.3)
  
}


shinyApp(ui = ui, server = server)