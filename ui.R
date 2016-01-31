shinyUI(fluidPage(
      titlePanel("Heart Disease Risk Prediction"),
      
      sidebarLayout(
            sidebarPanel(
                  helpText("This App provides a risk prediction for heart disease 
                            based on a simplified version of Framingham algorithm.
                           According to this methos, the risk is highly correlated 
                           to gender, age, total cholesterol, HDL cholesterol 
                           and smoking habits. Please input your information to assess
                           the risk."),
                  
                  selectInput("sex", label = h4("Sex"), 
                              choices = list("Male" = 1, "Female" = 2, " " = NA), 
                              selected = NA),
                  
                  selectInput("age", label = h4("Age"), 
                              choices = list("20-34" = 1, "35-39" = 2, "40-44" = 3,
                                             "45-49" = 4, "50-54" = 5, "55-59" = 6,
                                             "60-64" = 7, "65-69" = 8, "70-74" = 9,
                                             "75-79" = 10, " " = NA), 
                              selected = NA),
                  
                  selectInput("chol", label = h4("Total cholesterol (mg/dL)"), 
                              choices = list("<160" = 1, "160-199" = 2, 
                                             "200-239" = 3, "240-279" = 4,
                                             ">280" = 5, " " = NA), 
                              selected = NA),
                  
                  selectInput("hdl", label = h4("HDL cholesterol (mg/dL)"), 
                              choices = list(">60" = 1, "50-59" = 2, 
                                             "40-49" = 3, "<40" = 4, " " = NA), 
                              selected = NA),
                  
                  selectInput("cig", label = h4("Cigarette smoker"), 
                              choices = list("Yes" = 1, "No" = 2, " " = NA), 
                              selected = NA),

                  submitButton('Submit')
            ),
            mainPanel(
                  h3('Results of prediction'),
                  h4('Based on your input your 10-year risk is: '),
                  verbatimTextOutput("prediction")
            )
      )
))