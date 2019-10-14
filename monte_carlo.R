library(DBI)
library(RMySQL)
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 

SP500<-"SELECT `trade_date`,`index_code`,`index_name`,`close`
FROM `global_index`.`daily_quote`
WHERE index_code='001006'
ORDER BY `trade_date` ASC"
dbGetQuery(mydb,SP500)
SP500x <- dbGetQuery(mydb,SP500)

SP500x<-SP500x[-1:-14148,]
SP500x<-SP500x[,-2:-3]
SPX <- SP500x[2]
SPX_lag <- rbind(0,SPX)
SPX <-rbind( SP500x[2],0)
SPX_megre <- cbind(SPX_lag,SPX)
SPX_megre <- SPX_megre[-2295,]
RATE <- (SPX_megre[2]-SPX_megre[1])/SPX_megre[1]
RATE.1 <- RATE[-1,]
#RATE.LN <- log(RATE.1+1)
mean(RATE.1)
sd(RATE.1)
rate.1 <- RATE.1
mean_index_return <- 
  mean(rate.1)
stddev_index_return <- 
  sd(rate.1)

simulated_monthly_returns <- rnorm(2520,
                                   mean_index_return,
                                   stddev_index_return)
head(simulated_monthly_returns)

simulated_daily_returns <- rnorm(2520,
                                   mean_index_return,
                                   stddev_index_return)
head(simulated_daily_returns)
tail(simulated_daily_returns)

library("magrittr")
library("tidyverse")
simulated_returns_add_1 <- 
  tibble(c(1,1 + simulated_daily_returns))%>%
  `colnames<-`("returns")

head(simulated_returns_add_1)

simulated_growth <- 
simulated_returns_add_1 %>%
  mutate(growth1 = accumulate(returns,function(x,y)x * y),
         growth2 = accumulate(returns,`*`),
         growth3 = cumprod(returns))%>%
  select(-returns)

tail(simulated_growth)

# compund annual growth rate
cagr <- 
  ((simulated_growth$growth1[nrow(simulated_growth)]^
      (1/10))-1)*100

# first growth simulation function
simulation_accum_1 <- function(init_value, N, mean, stdev){
  tibble(c(init_value, 1 + rnorm(N, mean, stdev)))%>%
  `colnames<-`("returns")%>%
  mutate(growth = 
           accumulate(returns,
                      function(x, y)x * y))%>%
  select(growth)
}

# second growth simulation function
simulation_accum_2 <- function(init_value, N, mean, stdev){
  tibble(c(init_value, 1 + rnorm(N, mean, stdev)))%>%
    `colnames<-`("returns")%>%
  mutate(growth = accumulate(returns,`*`))%>%
  select(growth)
}

# a simulation function using cumprod()
simulation_cumprod <- function(init_value, N, mean, stdev){
  tibble(c(init_value, 1 + rnorm(N, mean, stdev)))%>%
    `colnames <- `("returns")%>%
  mutate(growth = cumprod(returns))%>%
  select(growth)
}

#a function that uses those three previous functions
simulation_confirm_all <- function(init_value, N, mean, stdev){
  tibble(c(init_value, 1 + rnorm(N, mean, stdev)))%>%
    `colnames<-`("returns")%>%
    mutate(growth1 = accumulate(returns, function(x, y)x * y),
           growth2 = accumulate(returns,`*`),
           growth3 = cumprod(returns))%>%
    select(-returns)
}

simulation_confirm_all_test <- 
  simulation_confirm_all(1,2520,
                         mean_index_return,stddev_index_return)
tail(simulation_confirm_all_test)

# Running Multiple Simulations
sims <- 51
starts <- 
  rep(1,sims)%>%
  set_names(paste("sim",1:sims,sep = ""))
head(starts)
tail(starts)

monte_carlo_sim_51 <- 
  map_dfc(starts,simulation_accum_1,
          N = 2520,
          mean = mean_index_return,
          stdev = stddev_index_return)

tail(monte_carlo_sim_51 %>%
       select(growth1, growth2,
              growth49, growth50),3)

monte_carlo_sim_51 <- 
  monte_carlo_sim_51%>%
  mutate(day = seq(1:nrow(.)))%>%
  select(day,everything())%>%
  `colnames<-`(c("day",names(starts)))%>%
   mutate_all(funs(round(.,2)))

tail(monte_carlo_sim_51%>% select(day, sim1, sim2,
                                  sim49, sim50),3)

reruns <- 51

monte_carlo_sim_51 <- 
rerun(.n = returns,
      simulation_accum_1(1,
                         2520,
                         mean_index_return,
                         stddev_index_return))%>%
  simplify_all()%>%
  `names<-`(paste("sim",1:reruns, sep = " "))%>%
  as_tibble()%>%
  mutate(day = seq(1:nrow(.)))%>%
  select(day,everything())

tail(monte_carlo_sim_51%>%
       select(`sim1`,`sim2`,
              `sim49`,`sim50`),3)

#Visualizing Simulations with ggplot
monte_carlo_sim_51%>%
  gather(sim,growth,-day)%>%
  group_by(sim)%>%
  ggplot(aes(x = day, y = growth, color = sim)) +
  geom_line() +
  theme(legend.position = "none")

#max,min,median
sim_summary <- 
monte_carlo_sim_51%>%
  gather(sim, growth, -day)%>%
  group_by(sim)%>%
  summarise(final = last(growth))%>%
  summarise(
            max = max(final),
            min = min(final),
            median = median(final))
sim_summary

monte_carlo_sim_51%>%
  gather(sim,growth,-day)%>%
  group_by(sim)%>%
  filter(
    last(growth) == sim_summary$max ||
    last(growth) == sim_summary$median ||
    last(growth) == sim_summary$min) %>%
  ggplot(aes(x = day, y = growth)) +
  geom_line(aes(color = sim))


library("highcharter")
mc_gathered <- 
  monte_carlo_sim_51 %>%
  gather(sim, growth, -day) %>%
  group_by(sim)

hchart(mc_gathered,
       type = 'line',
       hcaes(y = growth,
             x = day,
             group = sim)) %>%
  hc_title(text = "51 simulations")
hc_xAxis(title = list(text = "days")) %>%
  hc_yAxis(title = list(text = "dollar growth"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)