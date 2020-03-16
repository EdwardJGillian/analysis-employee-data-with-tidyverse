####################################
#### Messy Data - server.R ###
####################################

shinyServer(function(input, output) { # server is defined within these parentheses
  
  ######################### Task 1 - calculate statistics & plot graphs #######################

  # perform chisq for Task 1
  chisq1 <- chisq_function(cs_es_table)
  
  # set up Title and data for corrplot for Task 1
  Title1 <- "Current salary vs. Salary expectations"
  data1 <- chisq1
  
  # calculate the contribution (in %) of a given cell to the total Chi-square score
  contrib1 <- 100*chisq1$residuals^2/chisq1$statistic
  
  # function to visualize Pearson residuals using the package corrplot:
  # tile legend (tl) oriented at 45 degrees
  # a wider color legend (cl) with numbers right aligned
  # create the main title for corrplot
  # mar sets the parameters for ensuring the main title is in the window 
  
  corrplot_function <- function(cp_title, cp_data, cp_method, bg_colour) {
    corrplot(cp_data$residuals, method = cp_method, bg = bg_colour, is.cor = FALSE, tl.srt=45, cl.ratio = 1.5, cl.align = "r", 
             title = cp_title,  mar=c(0,0,1,0))
  }
  
  # create the output for corrplot
  output$Task1corrplot <- renderPlot({
    
    # checks to see if the user requested squares and change the corrplot graph 
    if(input$square1){
      
      cp_method1 <- "square"
      
    } else {
      cp_method1 <- "circle"
    }
    
    # checks to see if the user requested grey background colour and change the corrplot graph 
    if(input$bg_colour1){
      
      cp_bg1 <- "yellow"
      
    } else {
      cp_bg1 <- "white"
    }
    # create corrplot for Task 1
    corrplot_function(Title1, data1, cp_method1, cp_bg1)
  })
  
  # create text box with question and answers based on corrplot
  output$textDisplay1_1 <- renderText({ 
    
    paste("Question 1.", 
          "What is the distribution of the expected net income in relation to the current net income?",
          "Answer 1.1", 
          "According to the correlation plot, the people earning < 1000 Euros have negative salary expectations.",
          "The people earning > 2000 Euros have positive salary expectations.", 
          "It can be seen that the most contributing cells to the Chi-square are Current Salary/<1000 Euro", 
          round(contrib1[1,1], 2), 
          "Expected Salary/<1000 Euro", 
          round(contrib1[1,2], 2), 
          sep = "\n"
    )
    
  })
  
  # create contigency table data for ggplot2 for Task 1
  dt1 <- ggplot_prep_function(cs_es_table)
  
  # set up x_label, y_label, and data for corrplot for Task 1
  x_label <- "Salary Levels (Euros)"
  y_label <- " "
  
  # Graph with ggplot
  p1 <- ggplot(dt1, aes(x =Var1, y = Var2)) 
  
  # create the output for balloonplot for Task 1
  output$Task1balloonplot <- renderPlot({
    # Create and orientate title and labels for balloonplot
    p1+geom_point( aes(size=Freq), shape=21, colour="black", fill="skyblue") + 
      theme(panel.background=element_blank(), 
      panel.border = element_rect(colour = "blue", fill=NA, size=1), 
      axis.text.x=element_text(angle = -45, hjust = 0), 
      plot.title = element_text(face = "bold", hjust = 0.5)) + 
      labs(x=x_label, y=y_label) +
      ggtitle(Title1)
  })
  
  # create text box with question and answers based on balloonplot
  output$textDisplay1_2 <- renderText({ 
    
    paste("Question 1.", 
          "What is the distribution of the expected net income in relation to the current net income?",
          "Answer 1.2.", 
          "According to the balloon plot for expected salary, the highest frequency of people is between 2001-3000 Euros.",
          "Also, the largest frequencies of people are expecting between 1001 Euros and 4000 Euros.",
          "For current salary, the freqeuncy decreases from the smallest category (<1000 Euro) to the largest category (4000-5000 Euros)",
          sep = "\n"
    )
    
  })
  
  ######################### Task 2 - calculate statistics & plot graphs #######################
  # perform chisq for Task 2
  chisq2 <- chisq_function(es_ab_table)
  
  # set up title and data for corrplot for Task 2
  Title2 <- "Ability to Commute vs. Salary expectations"
  data2 <- chisq2
  
  # calculate the contribution (in %) of a given cell to the total Chi-square score
  contrib2 <- 100*chisq2$residuals^2/chisq2$statistic
  
  
  # create the output for the corrplot for Task 2
  output$Task2corrplot <- renderPlot({
    
    # checks to see if the user requested a square and change the corrplot graph 
    if(input$square2){
      
      cp_method2 <- "square"
      
    } else {
      cp_method2 <- "circle"
    }
    
    # checks to see if the user requested grey background colour and change the corrplot graph 
    if(input$bg_colour2){
      
      cp_bg2 <- "yellow"
      
    } else {
      cp_bg2 <- "white"
    }
    
    # create corrplot for Task 2
    corrplot_function(Title2, data2, cp_method2, cp_bg2)
  })
  
  # create text box with question and answers based on corrplot
  output$textDisplay2_1 <- renderText({ 
    
    paste("Question 2.", 
          "What is the relationship between the desire to earn more and the possibility to commute longer?",
          "Answer 2.1", 
          "According to the correlation plot, the people earning < 1500 Euros reported the ability to commute of < 10 kms.",
          "The people earning between 3000 and 6000 Euros reported the ability to commute between 21 and >100 kms.",
          "It can be seen that the most contributing cells to the Chi-square are 5000-6000 Euro/>100 km", 
          round(contrib2[6,6], 2), 
          "4000-5000 Euro/>100 km", 
          round(contrib2[6,5], 2), 
          sep = "\n"
    )
    
  })
  
  # create contingency table data for ggplot for Task 2
  dt2 <- ggplot_prep_function(es_ab_table)
  
  # set up x_label, y_label, and data for balloonplot for Task 2
  x_label <- "Commute"
  y_label <- "Salary"
  
  # Graph with ggplot
  p2 <- ggplot(dt2, aes(x =Var1, y = Var2)) 
  
  # create the output for balloonplot for Task 2
  output$Task2balloonplot <- renderPlot({
    # Create and orientate title and labels for balloonplot
    p2+geom_point( aes(size=Freq), shape=21, colour="black", fill="skyblue") + 
      theme(panel.background=element_blank(), 
      panel.border = element_rect(colour = "blue", fill=NA, size=1), 
      axis.text.x=element_text(angle = -45, hjust = 0), 
      plot.title = element_text(face = "bold", hjust = 0.5)) + 
      labs(x=x_label, y=y_label) +
      ggtitle(Title2)
  })
  
  
  # create text box with question and answers based on balloonplot
  output$textDisplay2_2 <- renderText({ 
    
    paste("Question 2.", 
          "What is the relationship between the desire to earn more and the possibility to commute longer?",
          "Answer 2.2", 
          "According to the balloon plot, the highest frequencies were people earning between 1001 Euros and 4000 Euros and commuting < 21 kms.",
          sep = "\n"
    )
    
  })
 
  ######################### Task 3 - plot text and graphs #######################
  
  # create the output for Logistic regression
  output$textDisplay3_1 <- renderText({ 
    
    paste("Question 3.", 
          "What are the key factors that determine the tendency to move closer to the work place?",
          "Answer 3.1", "The key factors that are significantly associated to the outcome.",
          "The most significant factors i.e. p<.001 include:", "city size <20 k, age > 50, current salary.",
          "The other significant factors i.e. p.<01 include:", "city size 50 - 99 k, male, ages 25-30, ages 37-40",
          "The performance of the model is not high.",
          "For example, the AIC score - the measure of fit which penalizes model for the number of model coefficients - is",
          round(model2$aic, 2),
          "Also, the null Deviance - the response predicted by a model with nothing but an intercept - is:",
          round(model2$null.deviance, 2),
          "Finally, the residual deviance - the response predicted by a model on adding independent variables - is",
          round(model2$deviance, 2),
           sep = "\n"
    )
    
  })
  # display the output from logistic regression using the tidy function from the broom package
  output$Evaluation <- renderTable({
    broom::tidy(model2)
  })
  
  # set up parameters for:
  # Plot ROC curve - Level 2 training set
  plot_dataset <- ROCRperf2 
  plot_title <- "ROC curve for keenness to move"
  plot_subtitle <- "Level 2 training set"
  
  # create the output for the AUC plot
  output$Task3AUCplot <- renderPlot({
    # Plot ROC curve - Level 2 training set 
    plot_ROC_function(plot_dataset, plot_title, plot_subtitle)
  })
  
  # create the
  output$textDisplay3_2 <- renderText({ 
    
    paste("Question 3.", 
          "Assessing the predictive ability of the model",
          "Answer 3.2", 
          "The model when applied to the test set has a lower AUC",
          round(auc, 2),
          "Digging deeper, if the threshold is set at .6,  the TPR is ",
          round(Test_6_2_df[1,], 2), 
          "and the FPR is",
          round(Test_6_2_df[2,], 2),
          "This indicates the model is not particularly sensitive nor specific.",
          "This reinforces the performance of the logistic model.",
          "Also, these lower scores may be dependent on the small size of the data set and the manual split of the data.",
          sep = "\n"
    )
    
  })
  
})