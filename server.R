library(shiny)
heartattackRisk <- function(sex, age, chol, hdl, cig) 
      {
      p1 <- 0     # points for age
      p2 <- 0     # points for cholesterol level
      p3 <- 0     # points if non-smoker
      p4 <- 0     # points for HDL cholesterol level
      predpoint <- 0 # prediction score for the risk
      predperc <- 0  # prediction percentage for the risk
      
      if(sex == 1)  # if subject is Male
      {
         if (age == 1) p1 <- -9
         if (age == 1) p1 <- -4
         if (age == 1) p1 <- 0
         if (age == 1) p1 <- 3
         if (age == 1) p1 <- 6
         if (age == 1) p1 <- 8
         if (age == 1) p1 <- 10
         if (age == 1) p1 <- 11
         if (age == 1) p1 <- 12
         if (age == 1) p1 <- 13
         
         if (age == 1 || age == 2) {
               if (chol == 1) p2 <- 0
               else if (chol == 2) p2 <- 4
               else if (chol == 3) p2 <- 7
               else if (chol == 4) p2 <- 9
               else if (chol == 5) p2 <- 11
               if (cig == 1) p3 <- 8 # change p2 points if smoker
         }
         else if (age == 3 || age == 4) {
               if (chol == 1) p2 <- 0
               else if (chol == 2) p2 <- 3
               else if (chol == 3) p2 <- 5
               else if (chol == 4) p2 <- 6
               else if (chol == 5) p2 <- 8
               if (cig == 1) p3 <- 5 # change p2 points if smoker
         }
            else if (age == 5 || age == 6) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 2
                  else if (chol == 3) p2 <- 3
                  else if (chol == 4) p2 <- 4
                  else if (chol == 5) p2 <- 5
                  if (cig == 1) p3 <- 3 # change p2 points if smoker
            }
            else if (age == 7 || age == 8) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 1
                  else if (chol == 3) p2 <- 1
                  else if (chol == 4) p2 <- 2
                  else if (chol == 5) p2 <- 3
                  if (cig == 1) p3 <- 1 # change p2 points if smoker
            }
            else if (age == 9 || age == 10) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 0
                  else if (chol == 3) p2 <- 0
                  else if (chol == 4) p2 <- 1
                  else if (chol == 5) p2 <- 1
                  if (cig == 1) p3 <- 1 # change p2 points if smoker
            }
         if (hdl == 1) p4 <- -1
         if (hdl == 2) p4 <- 0
         if (hdl == 3) p4 <- 1
         if (hdl == 4) p4 <- 2
         
         predpoint <- p1 + p2 + p3 + p4            # prediction points
         if (predpoint < 9) predperc = "<1%"      # prediction percentage 
         if (predpoint > 8 && predpoint < 13) predperc = "1%"
         if (predpoint > 12 && predpoint < 15) predperc = "2%"
         if (predpoint == 15) predperc = "3%"
         if (predpoint == 16) predperc = "4%"
         if (predpoint == 17) predperc = "5%"
         if (predpoint == 18) predperc = "6%"
         if (predpoint == 19) predperc = "8%"
         if (predpoint > 19) predperc = ">10%"
      }
      
      else if(sex == 2)
      {
            if (age == 1) p1 <- -7
            if (age == 1) p1 <- -3
            if (age == 1) p1 <- 0
            if (age == 1) p1 <- 3
            if (age == 1) p1 <- 6
            if (age == 1) p1 <- 8
            if (age == 1) p1 <- 10
            if (age == 1) p1 <- 12
            if (age == 1) p1 <- 14
            if (age == 1) p1 <- 16
            if (age == 1 || age == 2) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 4
                  else if (chol == 3) p2 <- 8
                  else if (chol == 4) p2 <- 11
                  else if (chol == 5) p2 <- 13
                  if (cig == 1) p3 <- 9 # change p2 points if smoker
            }
            else if (age == 3 || age == 4) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 3
                  else if (chol == 3) p2 <- 6
                  else if (chol == 4) p2 <- 8
                  else if (chol == 5) p2 <- 10
                  if (cig == 1) p3 <- 7 # change p2 points if smoker
            }
            else if (age == 5 || age == 6) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 2
                  else if (chol == 3) p2 <- 4
                  else if (chol == 4) p2 <- 5
                  else if (chol == 5) p2 <- 7
                  if (cig == 1) p3 <- 4 # change p2 points if smoker
            }
            else if (age == 7 || age == 8) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 1
                  else if (chol == 3) p2 <- 2
                  else if (chol == 4) p2 <- 3
                  else if (chol == 5) p2 <- 4
                  if (cig == 1) p3 <- 2 # change p2 points if smoker
            }
            else if (age == 9 || age == 10) {
                  if (chol == 1) p2 <- 0
                  else if (chol == 2) p2 <- 1
                  else if (chol == 3) p2 <- 1
                  else if (chol == 4) p2 <- 2
                  else if (chol == 5) p2 <- 2
                  if (cig == 1) p3 <- 1 # change p2 points if smoker
            }
            if (hdl == 1) p4 <- -1
            if (hdl == 2) p4 <- 0
            if (hdl == 3) p4 <- 1
            if (hdl == 4) p4 <- 2
            
            predpoint <- p1 + p2 + p3 + p4            # prediction points
            if (predpoint == 0) predperc = "<1%"      # prediction percentage 
            if (predpoint > 0 && predpoint < 5) predperc = "2%"
            if (predpoint > 4 && predpoint < 7) predperc = "3%"
            if (predpoint == 7) predperc = "4%"
            if (predpoint == 8) predperc = "5%"
            if (predpoint == 9) predperc = "6%"
            if (predpoint == 10) predperc = "8%"
            if (predpoint == 11) predperc = "10%"
            if (predpoint > 12) predperc = ">10%"
      }
      
      predperc
}

shinyServer(
      function(input, output) {
            output$prediction <- renderPrint({heartattackRisk(input$sex, input$age,
                                                              input$chol, input$hdl, 
                                                              input$cig)})
      }
)