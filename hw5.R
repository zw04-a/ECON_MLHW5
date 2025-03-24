#---------------------------------------------------
#----- Homework 06 Template
#---------------------------------------------------


#############################################
##----- 0. Prepare the environment
#############################################


  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear memory
  cat("\f")       # Clear the console
  options(scipen = 4) # Remove scientific notation for numbers
  # Prepare needed libraries
  packages <- c("ggplot2" # Best plotting
                , "gridExtra"
                , "sampleSelection" # To load RAND dataset
                , "ISLR"
                , "car"  # For quick residuals plots
                , "MASS" # For sumulation of multivariate normals
                , "boot" # Arranging multiple plots
                )
  for (i in 1:length(packages)) {
    if (!packages[i] %in% rownames(installed.packages())) {
      install.packages(packages[i], dependencies = TRUE)
    }
    library(packages[i], character.only=TRUE)
  }
  rm(packages)

  
#############################################
##----- Q1.
#############################################

#-------------------------------------
#----- Q1.1. Set up the model and generate our sample
#-------------------------------------
  
  # Model's parameters
  q1.n <- 100
  q1.beta0 <- 5 
  q1.beta1 <- 0.5
  q1.beta2 <- -1
  q1.beta3 <- 1
  var.x1 <- 1
  var.x2 <- 1
  var.x3 <- 1
  cov.x1x2 <- 0.6
  cov.x1x3 <- 0.6
  cov.x2x3 <- 0.2
  u.alpha <- 1.5
  u.beta <- 4
  u.shift <- 2
  rg.seed <- 300
  
  # Generate x variables 
  set.seed(rg.seed)
  q1.data <- mvrnorm(n = q1.n # Generate 100 observations
                     , c(3, 3, 3) # With equal means for all 3 X variables
                     , Sigma = matrix(c(var.x3, cov.x1x2, cov.x1x3
                                        , cov.x1x2, var.x3, cov.x2x3
                                        , cov.x1x3, cov.x2x3, var.x3
                                        ) # And covariance matrix Sigma
                                      , nrow = 3, ncol = 3
                                      )
                     )
  q1.data <- as.data.frame(q1.data)
  names(q1.data) <- c("x1", "x2", "x3")
  
  # Generate error term
  set.seed(rg.seed)
  q1.data$u <- c(rbeta(q1.n/2, u.alpha, u.beta, u.shift), -rbeta(q1.n/2, u.alpha, u.beta, u.shift))
  q1.data$epsilon <- q1.data$x1*q1.data$u
  # It is a bi-modal distribution
  q1.chart1 <- ggplot(q1.data, aes(x = epsilon)) +
                geom_histogram(color = "black", fill = "grey", bins = 40) + 
                theme_bw()
  q1.chart1
  
  # Finally, generate y
  q1.data$y <- q1.beta0 + q1.beta1*q1.data$x1 + q1.beta2*q1.data$x2 + 
              q1.beta3*q1.data$x3 + q1.data$epsilon
  
  # As we can see, no strong relationship between y and x1
  q1.chart2 <- ggplot(q1.data, aes(x = x1, y = y)) + geom_point() + theme_bw()
  q1.chart2
  
  # Export charts
  #png(filename = "q1a.png", width = 1920, height = 1200, res = 216)
  #  grid.arrange(q1.chart1, q1.chart2, nrow = 1, ncol = 2)
  #dev.off()

#-------------------------------------
#----- Q1.2. Estimate the OLS model
#-------------------------------------
  
  # Linear regression
  model <- as.formula(paste("y ~", paste(c("x1", "x2", "x3"), collapse = " + ")))
  q1.reg <- lm(model, q1.data)
  summary(q1.reg)
  confint(q1.reg)
  
  #'By observation, beta1 is not close to the true value of 0.5 but it's significant
  #'at 10% level. 
  
#-------------------------------------
#----- Q1.3. Compare OLS vs true vs BS inference
#-------------------------------------
  q1.results <- data.frame(type = c("OLS", "True", "Boot")
                           , beta1.hat = NA
                           , sd = NA
                           , ci.l = NA
                           , ci.u = NA
                           )
  
  #assign values 
  q1.results[1,] <- c("OLS"
                          , coef(summary(q1.reg))["x1",1]
                          , coef(summary(q1.reg))["x1",2]
                          , confint(q1.reg) ["x1",1]
                          , confint(q1.reg) ["x1",2])
  
#-------------------------------------
#----- Q1.4. Set up simulation and bootstrap 
#-------------------------------------  
  # Now let's repeat this process 1000 times
  n.sim <- 1000
  # Create a dataframe to store both sim and bootstrap samples
  q1.data.sim <- data.frame(beta1.hat = rep(NA, 2*n.sim)
                            , type = c(rep("True", n.sim)
                                       , rep("Boot", n.sim)
                                       )
                            , sim.id = rep(1:n.sim, 2)
                            )
#-------------------------------------
#----- Q1.5. Simulate true distribution of beta1.hat
#------------------------------------- 
  for (i in 1:n.sim) {
    
    set.seed(100*i)
    
    ###data gen
    
    # Model's parameters
    sim.n <- 100
    sim.beta0 <- 5 
    sim.beta1 <- 0.5
    sim.beta2 <- -1
    sim.beta3 <- 1
    var.x1 <- 1
    var.x2 <- 1
    var.x3 <- 1
    cov.x1x2 <- 0.6
    cov.x1x3 <- 0.6
    cov.x2x3 <- 0.2
    u.alpha <- 1.5
    u.beta <- 4
    u.shift <- 2
    rg.seed <- 300
    
    # Generate x variables 
    sim.data <- mvrnorm(n = sim.n,              # Generate 100 observations
                        mu = c(3, 3, 3),        # Equal means for all 3 X variables
                        Sigma = matrix(c(var.x3, cov.x1x2, cov.x1x3,
                                         cov.x1x2, var.x3, cov.x2x3,
                                         cov.x1x3, cov.x2x3, var.x3),
                                       nrow = 3, ncol = 3)  # Covariance matrix
    )
    
    sim.data <- as.data.frame(sim.data)
    names(sim.data) <- c("x1", "x2", "x3")
    
    # Generate error term
    sim.data$u <- c(rbeta(sim.n/2, u.alpha, u.beta, u.shift), -rbeta(sim.n/2, u.alpha, u.beta, u.shift))
    sim.data$epsilon <- sim.data$x1 * sim.data$u
    
    # Finally, generate y
    sim.data$y <- sim.beta0 + sim.beta1 * sim.data$x1 + sim.beta2 * sim.data$x2 + 
      sim.beta3 * sim.data$x3 + sim.data$epsilon
    
    # linear regression 
    sim.reg <- lm(model, sim.data)

    
    # Assign values 
    q1.data.sim[i,1] <- coef(summary(sim.reg))[2,"Estimate"]
  }
  

  
#-------------------------------------
#----- Q1.6. True distribution of beta1.hat - chart
#-------------------------------------   
  q1.chart3 <- ggplot(q1.data.sim[q1.data.sim$type == "True",], aes(x = beta1.hat)) + 
                geom_histogram(color = "black", fill = "grey", bins = 40) + 
                geom_vline(xintercept = q1.beta1, linetype = "dashed"
                           , color = "red", size = 1
                           ) + 
                theme_bw()
  q1.chart3
  #png(filename = "q1b.png", width = 1920, height = 1200, res = 216)
  #  q1.chart3
  #dev.off()
  
#-------------------------------------
#----- Q1.7. True distribution of beta1.hat - SD and CI
#-------------------------------------   
  q1.results[2, (2:5)] <- c(mean(q1.data.sim$beta1.hat[1:1000])
                     , sd(q1.data.sim$beta1.hat[1:1000])
                     , quantile(q1.data.sim$beta1.hat[1:1000], probs = 0.025)
                     , quantile(q1.data.sim$beta1.hat[1:1000], probs = 0.975)
                     )
  

#-------------------------------------
#----- Q1.8. Bootstrap the distribution of beta1.hat
#-------------------------------------  
  
  for (i in 1:n.sim) {
    # Modify RG seed to get different samples
    set.seed(100*i)
    # Select which observations to sample
    index <- sample(c(1:nrow(q1.data)), nrow(q1.data), replace = TRUE)
    # Create dataframe to store bootstrap data
    boot.data <- q1.data[index, ]
    boot.reg <- lm(model, boot.data)
    # Put the estimated beta1 coefficient value into q1.data.sim
    q1.data.sim$beta1.hat[1000 + i] <-  coef(summary(boot.reg))[2,"Estimate"]
  }

#-------------------------------------
#----- Q1.9. Bootstraped  distribution - chart
#-------------------------------------  
  
  q1.chart4 <- ggplot(q1.data.sim[q1.data.sim$type == "Boot", ], aes(x = beta1.hat)) + 
                geom_histogram(color = "black", fill = "grey", bins = 40) + 
                geom_vline(xintercept = q1.beta1, linetype = "dashed"
                           , color = "red", size = 1
                           ) + 
                theme_bw()
  q1.chart4
  #png(filename = "q1c.png", width = 1920, height = 1200, res = 216)
  #  q1.chart4
  #dev.off()

#-------------------------------------
#----- Q1.10. Bootstraped  distribution - SD and CI
#-------------------------------------  

  q1.results[3, (2:5)] <- c(mean(q1.data.sim$beta1.hat[1001:2000])
                            , sd(q1.data.sim$beta1.hat[1001:2000])
                            , quantile(q1.data.sim$beta1.hat[1001:2000], probs = 0.025)
                            , quantile(q1.data.sim$beta1.hat[1001:2000], probs = 0.975)
  )
  
#-------------------------------------
#----- Q1.11. Compare all results
#-------------------------------------  
  q1.results

#'By comparing the results, we can see that both OLS and 
#'bootstrapped regressions deviates from the true beta1. Also, 
#'they might both fail to reject the null hypothesis when doing 
#'hypothesis testing. 
#'However, the bootstrap standard error is much closeer to the 
#'true standard error compared to OLS.  

#############################################
##----- Q2.
#############################################
  
  # Import data
  q2.data <- Wage
  
#-------------------------------------
#----- Q2.1. Estimate first model
#------------------------------------- 
  
  # Let's try estimating a model that explains wage
  var_x <- c("age", "maritl", "race", "education", "jobclass", "health", "health_ins")
  formula1 <- as.formula(paste("wage~", paste(var_x, collapse = "+")))
  # Estimate the model 
  q2.reg1 <- lm(formula1, Wage)
  
#-------------------------------------
#----- Q2.2. Prepare CV dataframe
#-------------------------------------   
  residualPlots(q2.reg1)
  
  q2.cv.fit <- data.frame(poly = seq.int(from = 1, to = 5, by = 1)
                          , mse = NA
                          )

#-------------------------------------
#----- Q2.3. Run CV loop
#-------------------------------------   
  formula1 <- deparse(formula1)
  for (i in 1:5) { # The loop cycles through polynomials
    # Adjust the formula
    formula.cv <- paste0(formula1 , " + poly(age, degree =", i, ", raw = TRUE)")
    formula.cv <- as.formula(formula.cv)
    
    # Estimate the model
    model.cv <- glm(formula.cv, data = Wage)
    #  Calculate MSE from CV
    mse <- cv.glm(data = Wage, glmfit = model.cv, K = 10)$delta[1]
    # Put MSE back into main dataframe
    q2.cv.fit$mse[q2.cv.fit$poly == i] <- mse
  }
  
#-------------------------------------
#----- Q2.4. Compare results
#-------------------------------------     
  print(paste("The number of polynomials with the smallest MSE is:", 
              q2.cv.fit$poly[q2.cv.fit$mse == min(q2.cv.fit$mse)]))
  
  # Looks like 3nd order is the best fit:
  formula2 <- paste0(formula1, " + I(age^2) + I(age^3)")
  formula2 <- as.formula(formula2)
  q2.reg2 <- lm(formula2, data = Wage)
  summary(q2.reg2)
  residualPlots(q2.reg2)
  coefficients(q2.reg2)[names = c("age", "I(age^2)", "I(age^3)")]
  
#-------------------------------------
#----- Q2.5. Calculate average marginal effect of age
#-------------------------------------       
  # Now let's try figuring out the average marginal effect of age
  # Since the effect comes from 3rd order polynomial,
  # it depends on actual value of age for each person
  # One of the ways around it is to use median values of age
  # (similar to how margins() command calculates logit effects)
  
  # Lets write a function that calculates it for us
  # And we'll write it in a form that can be used with boot() function
  age.me <- function(data, index) {
    
    #create boot.data
    boot.data <- data[index, ]
    
    #run regressions 
    boot.model <- lm(formula2, data = boot.data)
    
    #get necessary coefs
    coefs <- coefficients(boot.model)[names = c("age","I(age^2)","I(age^3)")]
    b0 <- coefs[1]
    b1 <- coefs[2]
    b2 <- coefs[3]
    
    # calculate with the formula
    med.age <- median(boot.data$age)
    result.me <- b0 + 2* b1 * med.age + 3* b2 * med.age^2
    print(med.age)
    return(result.me)
    
  }
  # Check that function works correctly
  age.me(q2.data, 1:nrow(q2.data))
  
#-------------------------------------
#----- Q2.6. Get boostrap SD for AME of age
#-------------------------------------         
  
  q2.boot <- boot(
    data      = q2.data,     
    statistic = age.me,     
    R         = 1000        
  )
  summary(q2.boot)
  # T-statistic
  summary(q2.boot)[2]/summary(q2.boot)[4]
  # CLT CI based on bootstrap standard error
  summary(q2.boot)[2] - 1.96*summary(q2.boot)[4]
  summary(q2.boot)[2] + 1.96*summary(q2.boot)[4]
  
  