rm(list=ls())

setwd("X:/Projects/Segregation Metrics Toolkit/")

require(shiny)
require(MASS)
#library(CARBayes)
require(coda)
require(Rcpp)
require(truncdist)

source('Dcompute.R')
sourceCpp('cppfunctions.cpp')
source('binomial.iarCAR.R')

##########################
#### Set up the simulation
##########################
#### Create the study region
length <- 20
x <- seq(0,1, length.out=length)
y <- seq(0,1, length.out=length)
grid <- expand.grid(x,y)
colnames(grid) <- c("X", "Y")
n.area <- nrow(grid)
Dist <- as.matrix(dist(grid))
Sigma <- exp(-Dist*0.9)

#### Create the neighbourhood matrix
common.dist <- grid$X[2] - grid$X[1]
W <- array(0, c(n.area,n.area))
for(i in 1:n.area)
{
  for(k in 1:n.area)
  {
    temp1 <- abs((grid$X[i] - grid$X[k])) + abs((grid$Y[i] - grid$Y[k]))
    if(round(temp1,3)==round(common.dist,3))
    {
      W[i,k] <- 1
      W[k,i] <- 1
    }else
    {
      #W[i,k] <- 0
      #W[k,i] <- 0                    
    }
  }
}     




#logit.probs <- mvrnorm(n=1, mu=rep(-1.73, n.area), Sigma=Sigma)  
#probs <- exp(logit.probs) / (1 + exp(logit.probs))
#y <- rbinom(n=n.area, size=rep(1000,n.area), prob=probs)
#model <- binomial.iarCAR(formula=y~1, trials=rep(1000,n.area), W=W, burnin=1000, n.sample=2000) 
#posterior.D <- array(NA, c(1000))
#for(k in 1:1000)
#{
#  p.current <- exp(model$samples$phi[k ,] + model$samples$beta[k,1])   / (1 + exp(model$samples$phi[k ,] + model$samples$beta[k,1])) 
#  p.current.overall <- sum(p.current * rep(1000,n.area)) / sum(rep(1000,n.area))
#  posterior.D[k] <- sum(rep(1000),n.area) * abs(p.current - p.current.overall)) / (2 * sum(rep(1000,n.area)) * p.current.overall * (1-p.current.overall))     
#}


# Define server logic for random distribution application
shinyServer(function(input, output) {
  output$sidebartitle <- renderText({"Input controls"})
   output$plottitle <- renderText({"Summary of the minority proportion data"})
   output$summarytitle <- renderText({"True and estimated dissimilarity indices and 95% uncertainty intervals"})


     
     # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  probs <- reactive({
  set.seed(input$run)
  mean.logit <- log(input$mean / (1 - input$mean)) 
  logit.probs <- mvrnorm(n=1, mu=rep(mean.logit, n.area), Sigma=(input$sd*Sigma))  
  probs <- exp(logit.probs) / (1 + exp(logit.probs))
  })

     
  data <- reactive({
  y <- rbinom(n=n.area, size=rep(input$n.population,n.area), prob=probs())
  })
     
     
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the 'data' reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  
  
    output$plot <- renderPlot({
    zmat <- matrix(data()/input$n.population, nrow=length)
    filled.contour(x,y, zmat, zlim=c(0,1), xlab="Easting", ylab="Northing", main="Spatial map of the minority proportion")
    }, width=500, height=450)
  
  
  # Summarise the data
  output$summary1 <- renderPrint({
    summary(data()/input$n.population)
  })
  
  
  #### Compute the true and estimated values of the dissimilarity index
  output$summary2 <- renderPrint({
  ## Compute the true value
  N <- rep(input$n.population,n.area)
  x.true <- round(probs() * N, 0)
  probs.overall <- sum(x.true) / sum(N)
  Dtrue <- sum(N * abs(probs() - probs.overall)) / (2 * sum(N) * probs.overall * (1-probs.overall))    

  ## Run the classical method
  Dclassical <- round(Dissimilarity.compute(data(), N, n.boot=1000)$D.estimate,4)

  ## Run the Bayesian model
  model <- binomial.iarCAR(formula=data()~1, trials=N, W=W, burnin=1000, n.sample=2000) 
  posterior.D <- array(NA, c(1000))
     for(k in 1:1000)
     {
     p.current <- exp(model$samples$phi[k ,] + model$samples$beta[k,1])   / (1 + exp(model$samples$phi[k ,] + model$samples$beta[k,1])) 
     p.current.overall <- sum(p.current * rep(input$n.population,n.area)) / sum(rep(input$n.population,n.area))
     posterior.D[k] <- sum(rep(input$n.population,n.area) * abs(p.current - p.current.overall)) / (2 * sum(rep(input$n.population,n.area)) * p.current.overall * (1-p.current.overall))     
     }
  Dbayes <- round(quantile(posterior.D, c(0.5, 0.025, 0.975)),4)

  ## Save the results
  results2 <- array(NA, c(2,3))
  rownames(results2) <- c("Classical results", "Bayesian results")
  colnames(results2) <- c("", "", "")
  results2[1 , ] <- Dclassical
  results2[2 , ] <- Dbayes
  results2 <- round(results2, 4)
       
  results1 <- Dtrue    
  results <- list(results1, results2)
  names(results) <- c("True value of D", "Estimated values of D")
  results     
  })

})
