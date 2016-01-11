# You have two steps in the process: cashiers & baggers (some slow, some fast).  
# The customer would have the option of leaving the queue (because of impatience).  
# The number of items purchased affect cashier & bagger speed; they additionally 
# affect the store revenue.  Ultimately the goal of the store is to
# maximize profit = revenue – salaries.  
# What is the optimal number of checkout stations? 

# Customers arrive at rate A (poisson distributed)

# Customers have an average of n items (normally distributed)
# Each item costs an average of d dollars (normally distributed)

# Cashiers and baggers
# Two rates : 
# 1) per item rate: (Ci and Bi) (poisson -> they finish x items in one minute)
# total number of per item minutes for one customer = n/Ci or n/Bi
# and greeting/paying/other rate (Co and Bo) (normal -> takes them an average of x minutes per customer)
# Total time for cashiers = (n/Ci) + Co
# Total time for baggers = (n/Bi) + Bo
# Total time for one customer to be served = (n/Ci) + (n/Bi) + Co + Bo

# Customers leaving the line: exponential, average time a customer will wait before they leave is w 
# (exponential with mean 1/w)

# Cost of cashier: $cost.cashier/hr, cost of bagger: $cost.bagger/hr

# profit = revenue - salaries 
# profit = (customers who don't leave * n number of items * d cost per item) - (number of cashiers * cost per cashier + number of baggers * cost per bagger)

# number of cashiers and baggers from 1-50, do a simulation with each one and see
# find which maximizes profit 


A <- 100       # Customers arrive at rate A, average of 300 per hour
items.mean <- 20   # Customers buy an average of 10 items,
items.sd <- 5      # with st dev of 5 items
revenue.mean <- 0.5     # Average revenue made on an item is $0.5,
revenue.sd <- 0.1       # With st dev of $0.1
Ci <- 5   # Cashiers work at an average rate of 5 items per minute
Bi <- 7   # Baggers work at an average rate of 7 items per minute
Co.mean <- 1     # Average time for "other" for the cashier is 1 minute,
Co.sd <- 0.2     # with st dev of 0.2 minutes
Bo.mean <- 0.5   # Average time for "other" for the bagger is 0.5 minutes,
Bo.sd <- 0.1     # with st dev of 0.1 minutes
W <- 1/5  #customer will wait on an average of 5 minutes
cost.cashier <- 10
cost.bagger <- 10

line.hour <- function(o)
{
  profit = rep(0, 50)
  for(k in 1:50)
  {
    C <- k    #simulate for each possible number of checkout stations from 0 - 50
    K <- rpois(1,A)               # number of customers that arrive within the hour
    P <- matrix(0,K,4)
    P[,1] <- sort(runif(K,0,H))    # time (in seconds) within the hour that customers arrive
    
    D <- matrix(0,C,2)   # array that keeps track of how many customers are in line at each register
    
    Prices <- rep(0,N)
    Time.cashier <- rep(0, N)
    Time.bagger <- rep(0, N)
    
    revenue <- rep(0,K)
    
    for(i in 1:K) 
    {
      N = rnorm(1, items.mean, items.sd)  # number of items the customer buys
      for(j in 1:N)
      {
        Revenue.item = rnorm(1, revenue.mean, revenue.sd)    #cost of each item
        Prices[j] = Revenue.item
        
        TC = rexp(1, Ci)
        TB = rexp(1, Bi)
        Time.cashier[j] = TC
        Time.bagger[j] = TB
      }
      
      customer.revenue = sum(Prices)
      
      TOC = rnorm(1, Co.mean, Co.sd)
      TOB = rnorm(1, Bo.mean, Bo.sd)
      customer.time = sum(TC) + sum(TB) + TOC + TOB
      P[i, 3] = customer.time
      
      m <- which.min(D[,1])
      P[i,4] <- m
      D[m,1] <- D[m,1] + 1
      P[i,2] <- max(P[i,1],D[m,2])
  
      wait.limit = rexp(1, W)
      wait.time = P[i,2] - P[i,1]
      if(wait.limit < wait.time)
      {
        time.leave = P[i,1] + wait.limit
        revenue[i] = 0
        D[m,1] = D[m,1] - 1
      }
      else
      {
        revenue[i] = customer.revenue
        D[m,2] <- P[i,2] + P[i,3]
      }
    }
    revenue.total = sum(revenue)
    revenue.total
    
    salaries.total = C * (cost.cashier + cost.bagger)
    
    profit.current = revenue.total - salaries.total
    profit[k] = profit.current
  }
  optimal <- which.max(profit)
  optimal
}

X <- sapply(1:5, function(o) line.hour())
mean(X)

