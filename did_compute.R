
library(dplyr)
library(foreign)

path = '.'
dir = paste0(path, "/fastfood.dta")

# import dataset
dat <- read.dta(dir)

dat <- dat %>% 
  mutate(FTE = nmgrs + empft + (0.5 * emppt), 
         FTE2 = nmgrs2 + empft2 + (0.5 * emppt2))

# city subsets
dat_NJ <- subset(dat, state == 1)
dat_PA <- subset(dat, state == 0)

# reproduce table 1
options = c(0,1,2,3)

table1_NJ = matrix(0, length(options), 1)
table1_PA = matrix(0, length(options), 1)

for( i in 1:length(options)){
  
  app1 <- dat_NJ %>% 
    filter(status2  == options[i]) %>% 
    count(status2) %>% 
    select(n) 
  
  app2 <- dat_PA %>% 
    filter(status2  == options[i]) %>% 
    count(status2) %>% 
    select(n)
  
  table1_NJ[i, ] = if(length( app1$n ) == 0){0}else{app1$n}
  
  table1_PA[i, ] = if(length( app2$n ) == 0){0}else{app2$n}
  
}

app = cbind(table1_NJ + table1_PA,
               table1_NJ, 
               table1_PA)

tot =  cbind( nrow(dat_NJ) + nrow(dat_PA), 
              nrow(dat_NJ), 
              nrow(dat_PA) )

table1 = rbind( tot, app )

colnames(table1) = c("All", "NJ", "PA")
rownames(table1) = c("Stores", "Refusals", "Interviewed", 
                     "Renovations", "Closed")

save.image(file = paste0(path, "/results.RData"))



