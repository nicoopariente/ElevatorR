library(shiny)
library(bslib)

# Define UI for app
ui <- page_sidebar(

  title = "ElevatorR",

  sidebar = sidebar(
  
    numericInput( 
      "floors", 
      "Number Of Floors", 
      value = 1, 
      min = 1, 
      max = 50 
    ),
    numericInput( 
      "people", 
      "Number Of People", 
      value = 1, 
      min = 1, 
      max = 50000 
    ),
    numericInput( 
      "elevators", 
      "Number Of Elevators", 
      value = 1, 
      min = 1, 
      max = 50 
    ),
    actionButton("simulate", "Run Simulation")
  ),
  plotOutput(outputId = "distPlot")
)

# Define server logic
server <- function(input, output) {
  
 simulationResults <-  eventReactive(input$simulate,
                {
                  withProgress(message = "Running simulation...", value = 0, {
                    numberOfElevators <- input$elevators #input from user
                    numberOfPeople <- input$people #input from user
                    numberOfFloors <- input$floors #input from user
                    secondsOnADay <- 50400 #seconds from 6am to 10pm
                    secondCount <- 0
                    positionElevator <- rep(0, numberOfElevators)
                    requestElevatorInstances <- probabilityPick(numberOfPeople)
                    #requestElevatorInstances <- c(10, 20, 25, 40, 43, 43, 60)
                    peopleWaiting <- list()
                    #peopleWaiting <- list(c(0,6,0,0), c(1,5,0,0))
                    stopFloor <- vector("list", numberOfElevators)
                    peopleOnElevator <- rep(0, numberOfElevators)
                    peopleMovingToElevator <- rep(0, numberOfElevators)
                    priorityRequests <- vector("list", numberOfElevators)
                    
                    for (i in 1:numberOfElevators) {
                      stopFloor[[i]] <- numeric()
                      priorityRequests[[i]] <- numeric()
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
                          #priorityElevatorDirection <- sign(peopleWaiting[[1]][1] - peopleWaiting[[1]][2])
                          
                        }
                      }
                      
                    
                     
                      #print(peopleWaiting)
                      #print(positionElevator)
                      #print(peopleMovingToElevator)
                      #print(stopFloor)
                      #print(priorityRequests)
                      print(peopleOnElevator)
                      
                      for (j in 1:numberOfElevators) {
                        
                       
                        if(length(stopFloor[[j]]) > 0 & peopleMovingToElevator[j] == 0){
                        
                          for (i in seq_along(peopleWaiting)) {
                            direction <- peopleWaiting[[i]][1] - peopleWaiting[[i]][2]
                            directionElevator <- positionElevator[j] - stopFloor[[j]][1]
                            if(direction < 0 & directionElevator < 0 & positionElevator[j] < peopleWaiting[[i]][1] & peopleWaiting[[i]][3] == 0){
                              if(!peopleWaiting[[i]][1] %in% stopFloor[[j]]){
                                stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][1])
                                stopFloor[[j]] <- sort(stopFloor[[j]])
                              }
                              
                              
                              peopleWaiting[[i]][3] <- 1
                            }else if(direction > 0 & directionElevator > 0 & positionElevator[j] > peopleWaiting[[i]][1] & peopleWaiting[[i]][3] == 0){
                              if(!peopleWaiting[[i]][1] %in% stopFloor[[j]]){
                                stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][1])
                                stopFloor[[j]] <- sort(stopFloor[[j]] , decreasing = TRUE)
                              }
                             
                            
                              peopleWaiting[[i]][3] <- 1
                            }
                          }
                          
                          if(stopFloor[[j]][1] > positionElevator[j]){
                            positionElevator[j] <- positionElevator[j] + 1
                            
                          }else if(stopFloor[[j]][1] < positionElevator[j]){
                            positionElevator[j] <- positionElevator[j] - 1
                            
                          }else{
                            toBeDeleted <- c()
                            
                            for (i in seq_along(peopleWaiting)){
                              if(peopleWaiting[[i]][1] == positionElevator[j] & sign(positionElevator[j] - stopFloor[[j]][1]) == sign(peopleWaiting[[i]][1] - peopleWaiting[[i]][2])){
                                if(!peopleWaiting[[i]][2] %in% stopFloor[[j]]){
                                  stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][2])
                                  if(stopFloor[[j]][1] > positionElevator[j]){
                                    stopFloor[[j]] <- sort(stopFloor[[j]])
                                  }else{
                                    stopFloor[[j]] <- sort(stopFloor[[j]], decreasing = TRUE)
                                  }
                                  
                                }
                                
                                
                                toBeDeleted <- c(toBeDeleted, i)
                                peopleMovingToElevator[j] <- 2
                              }
                            }
                            stopFloor[[j]] <- stopFloor[[j]][-1]
                            for(i in sort(toBeDeleted, decreasing = TRUE)){
                              peopleWaiting <- peopleWaiting[-i]
                            }
                            
                            
                            
                            
                            
                            
                            
                            
                          }
               
                          
                        }else if(length(peopleWaiting) > 0 & peopleMovingToElevator[j] == 0){
                          
                          if(length(priorityRequests[[j]]) == 0){
                            
                            for(t in 1:length(peopleWaiting)){
                             
                                if(peopleWaiting[[t]][3] == 0){
                                  priorityRequests[[j]] <- peopleWaiting[[t]]
                                  peopleWaiting[[t]][3] <- 1
                                }
                              
                            }
                          }
                          
                          
                          
                          
                         
                        
                          if(length(priorityRequests[[j]]) > 0 ){
                            priorityTargetFloor <- priorityRequests[[j]][2]
                              priorityRequest <- priorityRequests[[j]][1]
                            
                            if(priorityRequest > positionElevator[j]){
                              positionElevator[j] <- positionElevator[j] + 1
                              
                            }else if(priorityRequest < positionElevator[j]){
                              positionElevator[j] <- positionElevator[j] - 1
                              
                            }else{
                              
                              peopleMovingToElevator[j] <- 2
                              stopFloor[[j]] <- stopFloor[[j]][-1]
                              stopFloor[[j]] <- c(stopFloor[[j]], priorityTargetFloor)
                              peopleWaiting <- peopleWaiting[-1]
                              priorityRequests[[j]] <- numeric()
                              
                              
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
                                  
                                }
                              }
                            }
                            
                          }
                          
                         
                          
                          
                        }else{
                          if(peopleMovingToElevator[j] > 0){
                            peopleMovingToElevator[j] <- max(0, peopleMovingToElevator[j] - 1)
                          }
                          
                        }
                        
                        
                        
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
                      incProgress(1/secondsOnADay, detail = paste("Step ", secondCount, " of ", secondsOnADay))
                    }
                    
                    return(waitStats)
                    
                  })
                 
                }
                
                )
  


  
  output$distPlot <- renderPlot({
    data <- simulationResults()
    plot(data$second, data$avgWaitTime, type = "l", col = "blue",
         xlab = "Second", ylab = "Average Wait Time",
         main = "Elevator Wait Time Over Time",
         xaxt = "n"
         )
    
  
    axis(1, at = seq(0, 50400, by = 3600), labels = c( "06:00","07:00", "08:00", "09:00", "10:00", "11:00",
                                                       "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                                                       "18:00", "19:00", "20:00"))
  })
  
}

probabilityPick <- function(numberOfPeople){
  
  totalRequests <- numberOfPeople * 2
  morningPeak <- 0:10800
  eveningPeak <- 39600:50400
  offPeak <- c(10801:39599)
  
  numMorning <- round(totalRequests * 0.4)
  numEvening <- round(totalRequests * 0.4)
  numOffPeak <- totalRequests - numMorning - numEvening
  
  return(c(sample(morningPeak, numMorning, replace = TRUE), sample(eveningPeak, numEvening, replace = TRUE), sample(offPeak, numOffPeak, replace = TRUE)))
  
  
  
}


shinyApp(ui = ui, server = server)