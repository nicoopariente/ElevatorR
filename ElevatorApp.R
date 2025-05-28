library(shiny)
library(bslib)


#ElevatorR is an app that will allow users to simulate a normal day in a building, to determine the elevator delay depending on different factors.

# Define UI for app
ui <- page_sidebar(

  title = "ElevatorR",

  #represents a sidebar components in the UI
  sidebar = sidebar(
  
    #numeric input for users to introduce building's number of floors
    numericInput( 
      "floors", 
      "Number Of Floors", 
      value = 1, 
      min = 2, 
      max = 50 
    ),
    #numeric input for users to introduce building's number of people
    numericInput( 
      "people", 
      "Number Of People", 
      value = 1, 
      min = 1, 
      max = 50000 
    ),
    #numeric input for users to introduce building's number of elevators
    numericInput( 
      "elevators", 
      "Number Of Elevators", 
      value = 1, 
      min = 1, 
      max = 50 
    ),
    #button that will trigger the simulation based on the data input by the user
    actionButton("simulate", "Run Simulation")
  ),
  #component that will contain the display output when generated
  plotOutput(outputId = "distPlot")
)

# Define server logic
server <- function(input, output) {
  
  #simulationResults variable will contain the whole eventReactive function that will be triggered when actionButton is clicked.
  #input$simulate is the ID associated with the Run button
 simulationResults <-  eventReactive(input$simulate,
                  #code of the elevator simulation                   
                {
                  #withProgress will show a pop-up with progress status of the simulation, as we run a 50400 loop through out an entire day
                  withProgress(message = "Running simulation...", value = 0, {
                    
                    #variables that depend on user input. 
                    numberOfElevators <- input$elevators 
                    numberOfPeople <- input$people 
                    numberOfFloors <- input$floors 
                    
                    #seconds from 6am to 8pm
                    secondsOnADay <- 50400 
                    #track of the second we are in
                    secondCount <- 0
                    #track of elevators positions. Vector as we could have multiple elevators
                    positionElevator <- rep(0, numberOfElevators)
                    #variable to store result from probabilityPick function. It generates a vector with random points in time simulating request to the elevator
                    requestElevatorInstances <- probabilityPick(numberOfPeople)
                    
                    #in case testing with fewer instances is needed, please check next comment line
                    #requestElevatorInstances <- c(10, 20, 25, 40, 43, 43, 60)
                    
                    #track of people in the queue waiting for the elevator
                    peopleWaiting <- list()
                    
                    #in case testing with specific direction and floor is needed, please check next comment line
                    #peopleWaiting <- list(c(0,6,0,0), c(1,5,0,0))
                    
                    #track the queue of each elevator, which floor requests have been added after people get inside the elevator
                    stopFloor <- vector("list", numberOfElevators)
                    
                    #intended for future enhancements. Add a limit of people that can get inside each elevator
                    #peopleOnElevator <- rep(0, numberOfElevators)
                    
                    #track of delay of people getting into the elevators
                    peopleMovingToElevator <- rep(0, numberOfElevators)
                    #track the queue of each elevator, which floor request has been added before people get inside the elevator.
                    priorityRequests <- vector("list", numberOfElevators)
                    
                    #initialize stopFloor and priorityRequest elements as empty numeric vectors
                    for (i in 1:numberOfElevators) {
                      stopFloor[[i]] <- numeric()
                      priorityRequests[[i]] <- numeric()
                    }
                    
                    #dataframe that will store information by second, related to the time people wait for the elevator
                    #app final result
                    waitStats <- data.frame(
                      second = integer(),
                      totalWaitingTime = numeric(),
                      peopleWaiting = integer(),
                      avgWaitTime = numeric()
                    )
                    
                    
                    #loop through out the entire day second by second
                    while (secondCount < secondsOnADay) {
                      
                      #number of requests generated for that specific second
                      peopleAddedToWaiting <- sum(requestElevatorInstances == secondCount)
                      
                      #if there was a number of requests generated greater than 0, we iterate on each instance
                      if(peopleAddedToWaiting > 0){
                        for (i in 1:peopleAddedToWaiting) {
                          #peopleWaiting variable will contain vectors with two random floors,then 0 or the number of the elevator depending if an elevator is on the way, and the last element will be a waiting time tracker
                          peopleWaiting[[length(peopleWaiting) + 1]] <- c(sample(0:numberOfFloors, 2), 0, 0)
                        }
                      }
                      
                      #loop through each elevator to update position and request queues
                      for (j in 1:numberOfElevators) {
                        
                       #the status of the elevator was divided into two main parts. 
                        #1 - The elevator can be currently be taking a person to a specific floor
                        #2 - The elevator is empty and is looking for a place to go
                        #We will check the status in that order, as if the elevator is taking people to a floor, we will try to add as many people as
                        #we can in the way
                        #If the elevator is empty, we will focus on getting that one first request
                       #There is a third status, when people are moving to inside the elevator, this will have a delay before the elevator moves.
                        
                        #Status 1. Elevator is taking people to a floor and there is no people moving to the elevator
                        if(length(stopFloor[[j]]) > 0 & peopleMovingToElevator[j] == 0){
                        
                          #loop to doublecheck current pending requests. If the request goes to the same direction as the elevator
                          #and there is no other elevator already going there, we are adding the floor to the elevator queue
                          for (i in seq_along(peopleWaiting)) {
                            #direction of request, and elevator
                            direction <- peopleWaiting[[i]][1] - peopleWaiting[[i]][2]
                            directionElevator <- positionElevator[j] - stopFloor[[j]][1]
                            #code for request and elevator going UP
                            if(direction < 0 & directionElevator < 0 & positionElevator[j] < peopleWaiting[[i]][1] & peopleWaiting[[i]][3] == 0){
                              #avoding duplicate floor request
                              if(!peopleWaiting[[i]][1] %in% stopFloor[[j]]){
                                stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][1])
                                stopFloor[[j]] <- sort(stopFloor[[j]])
                              }
                              
                              #request set as taken by an elevator
                              peopleWaiting[[i]][3] <- j
                              
                            #code for request and elevator going DOWN
                            }else if(direction > 0 & directionElevator > 0 & positionElevator[j] > peopleWaiting[[i]][1] & peopleWaiting[[i]][3] == 0){
                              #avoding duplicate floor request
                              if(!peopleWaiting[[i]][1] %in% stopFloor[[j]]){
                                stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][1])
                                stopFloor[[j]] <- sort(stopFloor[[j]] , decreasing = TRUE)
                              }
                             
                              #request set as taken by an elevator
                              peopleWaiting[[i]][3] <- j
                            }
                          }
                          
                          #Update the position of the elevator going to an specific requested floor
                          if(stopFloor[[j]][1] > positionElevator[j]){
                            positionElevator[j] <- positionElevator[j] + 1
                            
                          }else if(stopFloor[[j]][1] < positionElevator[j]){
                            positionElevator[j] <- positionElevator[j] - 1
                            
                          #if the elevator gets to the floor
                          }else{
                            
                            #variable to store indexes of people that will move to the elevator, so we can delete them from the peopleWaiting queue
                            toBeDeleted <- c()
                            
                            #loop to check if there is people waiting on that floor
                            for (i in seq_along(peopleWaiting)){
                              #in order to move to the elevator, the direction needs to match
                              if(peopleWaiting[[i]][1] == positionElevator[j] & sign(positionElevator[j] - stopFloor[[j]][1]) == sign(peopleWaiting[[i]][1] - peopleWaiting[[i]][2])){
                                #avoding duplicate floor request
                                if(!peopleWaiting[[i]][2] %in% stopFloor[[j]]){
                                  #add floor number to the elevator queue
                                  stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][2])
                                  #sort the queue depending on the direction
                                  if(stopFloor[[j]][1] > positionElevator[j]){
                                    stopFloor[[j]] <- sort(stopFloor[[j]])
                                  }else{
                                    stopFloor[[j]] <- sort(stopFloor[[j]], decreasing = TRUE)
                                  }
                                  
                                }
                                
                                #store the index of the peopleWaiting that is moving to the elevator
                                toBeDeleted <- c(toBeDeleted, i)
                                #add movement delay
                                peopleMovingToElevator[j] <- 2
                              }
                            }
                            
                            #the request for the specific floor can be delted
                            stopFloor[[j]] <- stopFloor[[j]][-1]
                            #elimination of the peopleWaiting that is no longer waiting.
                            for(i in sort(toBeDeleted, decreasing = TRUE)){
                              peopleWaiting <- peopleWaiting[-i]
                            }
                          }
               
                          
                        #Status 2. Elevator is or will be going towards a floor to pick people
                        }else if(length(peopleWaiting) > 0 & peopleMovingToElevator[j] == 0){
                          
                          #validate if it is a new request, or the elevator is already moving towards a floor
                          if(length(priorityRequests[[j]]) == 0){
                            
                            #check current people waiting
                            for(t in 1:length(peopleWaiting)){
                             
                                #check if there is no other elevator already attending the request
                                if(peopleWaiting[[t]][3] == 0){
                                  #assign the request to the elevator
                                  priorityRequests[[j]] <- peopleWaiting[[t]]
                                  #set the request as taken
                                  peopleWaiting[[t]][3] <- j
                                  break
                                }
                              
                            }
                          }
                          
                        #doublecheck if the request assignment was successfull
                          if(length(priorityRequests[[j]]) > 0 ){
                            
                            #Extracting information from the request to move the elevator
                            priorityTargetFloor <- priorityRequests[[j]][2]
                            priorityRequest <- priorityRequests[[j]][1]
                            
                            if(priorityRequest > positionElevator[j]){
                              positionElevator[j] <- positionElevator[j] + 1
                              
                            }else if(priorityRequest < positionElevator[j]){
                              positionElevator[j] <- positionElevator[j] - 1
                            
                            #Elevator gets to the floor
                            }else{
                              
                              #add delay for people entering the elevator
                              peopleMovingToElevator[j] <- 2
                              
                              #delete the peopleWaiting instance associated to the priorityRequest.
                              #loop to find the specific instance, and avoid deleting the instance of another elevator
                              for(m in 1:length(peopleWaiting)){
                                if(peopleWaiting[[m]][3] == j){
                                  peopleWaiting <- peopleWaiting[-m]
                                  #add the target floor to the elevator queue
                                  stopFloor[[j]] <- c(stopFloor[[j]], priorityTargetFloor)
                                  break
                                }
                              }
                              priorityRequests[[j]] <- numeric()
                              
                              #if there is more people on the same floor, that goes on the same direction, we will pick them, despite other
                              #elevator may be going on that way
                              if(length(peopleWaiting) > 1){
                                
                                #variable to store indexes of people that will move to the elevator, so we can delete them from the peopleWaiting queue
                                toDelete <- c()
                                
                                #loop to check if there is more people waiting on the same floor
                                for (i in 1:length(peopleWaiting)) {
                                  #if people on the same floor goes to the same direction, we add them to the elevator queue
                                  if(peopleWaiting[[i]][1] == positionElevator[j] & sign(positionElevator[j] - stopFloor[[j]][1]) == sign(peopleWaiting[[i]][1] - peopleWaiting[[i]][2])){
                                    stopFloor[[j]] <- c(stopFloor[[j]], peopleWaiting[[i]][2]) 
                                    
                                    #sort the queue depending on if the elevator goes UP or DOWN
                                    if(stopFloor[[j]][1] > positionElevator[j]){
                                      stopFloor[[j]] <- sort(stopFloor[[j]])
                                    }else{
                                      stopFloor[[j]] <- sort(stopFloor[[j]], decreasing = TRUE)
                                    }
                                    #add the index to the toDelete variable
                                    toDelete <- c(toDelete, i)
                                  }
                                }
                                #use the toDelete variable to eliminate the people waiting that already is on the elevator
                                for (i in sort(toDelete, decreasing = TRUE)) {
                                  peopleWaiting <- peopleWaiting[-i]
                                  
                                }
                              }
                            }
                            
                          }
                          
                        #Status 3. If people is moving to the elevator, we decrease the delay by one on each second  
                        }else if(peopleMovingToElevator[j] > 0){
                            peopleMovingToElevator[j] <- max(0, peopleMovingToElevator[j] - 1)
                        }
                        
                        
                        
                      }
                      
                      #variables to store waiting time calculations
                      totalWaitingTime <- 0
                      peopleWaitingNow <- 0
                      
                      #loop to extract the waiting time information of the people waiting in the specific second
                      for (k in seq_along(peopleWaiting)) {
                        peopleWaiting[[k]][4] <- peopleWaiting[[k]][4] + 1
                        totalWaitingTime <- totalWaitingTime + peopleWaiting[[k]][4]
                        peopleWaitingNow <- peopleWaitingNow + 1
                      }
                      
                      #adding a line to the dataframe with the sec information
                      waitStats <- rbind(waitStats, data.frame(
                        second = secondCount,
                        totalWaitTime = totalWaitingTime,
                        peopleWaiting = peopleWaitingNow,
                        avgWaitTime = ifelse(peopleWaitingNow > 0, totalWaitingTime / peopleWaitingNow, 0)
                        
                      ))
                      
                      #increasing the secondCount by one
                      secondCount <- secondCount + 1
                      
                      #code to show real visual progress of the simulation
                      incProgress(1/secondsOnADay, detail = paste("Step ", secondCount, " of ", secondsOnADay))
                    }
                    
                    #return the dataframe after finishing the while loop
                    return(waitStats)
                    
                  })
                 
                }
                
                )
  


  #output variable that displays the result to the UI. We are rendering a plot from the data of the simulation dataframe
  output$distPlot <- renderPlot({
    data <- simulationResults()
    #the plot will contain the second and avgWaitTime data from the dataframe. Adding color, and labels
    plot(data$second, data$avgWaitTime, type = "l", col = "blue",
         xlab = "Business Day Hours", ylab = "Average Wait Time",
         main = "Elevator Wait Time Over Time",
         xaxt = "n"
         )
    
    #setting configuration for the x axis, by breaking the 50400 seconds into 3600 second hours. Adding labels
    axis(1, at = seq(0, 50400, by = 3600), labels = c( "06:00","07:00", "08:00", "09:00", "10:00", "11:00",
                                                       "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                                                       "18:00", "19:00", "20:00"))
  })
  
}

#function that will receive a number of people, and generate a vector with random numbers inside the 50400 second window
probabilityPick <- function(numberOfPeople){
  
  #we understand people will request the elevator at least two times on a day
  totalRequests <- numberOfPeople * 2
  
  #selecting ranges of peak time during the day
  morningPeak <- 0:10800
  eveningPeak <- 39600:50400
  offPeak <- c(10801:39599)
  
  #setting the percentage of requests on each time range
  numMorning <- round(totalRequests * 0.3)
  numEvening <- round(totalRequests * 0.3)
  numOffPeak <- totalRequests - numMorning - numEvening
  
  #return the vector with random numbers related to each range 
  return(c(sample(morningPeak, numMorning, replace = TRUE), sample(eveningPeak, numEvening, replace = TRUE), sample(offPeak, numOffPeak, replace = TRUE)))
  
  
  
}


#app is initialized
shinyApp(ui = ui, server = server)