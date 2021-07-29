
###################################################################################################
## depthdamage.R: creates depthdamage.Rdata
## Corinne Bowers
## 11/23/2020
###################################################################################################

## packages
require(dplyr)
require(ggplot2)
require(pracma)

## functions
toNumber <- function(x) as.numeric(paste(x))
mft <- 3.28084

## setup
setwd('D:/Research/')


###################################################################################################

## get HAZUS damage curves
hazus <- read.csv('./_data/depthdamage/HAZUS.csv')
hazus <- hazus %>% 
  subset(Curve != 'San Francisco' & Type == 'Struct') %>% 
  mutate(FIA = toNumber(gsub('FIA ', '', Curve))) %>% 
  subset(FIA > 1970) %>% 
  t %>% data.frame %>% 
  select(-X8) %>% 
  setNames(c('x1','x2','x3','x4','x5','x6')) %>% 
  subset(!((1:nrow(.)) %in% 1:5)) %>% 
  subset(1:nrow(.) != 20) %>% 
  apply(2, toNumber) %>% data.frame %>% 
  mutate(ft = -8:10) 
add <- 11:50
hazus <- hazus[nrow(hazus), -ncol(hazus)] %>% 
  apply(2, function(x) rep(x, length(add))) %>% 
  cbind(ft = add) %>% 
  rbind(hazus, .)
hazus_names <- c('One Story - No Basement', 'One Story - Basement', 
                 'Split Story - No Basement', 'Split Story - Basement', 
                 '2+ Stories - No Basement', '2+ Stories - Basement')

## get FLEMO damage curves
flemo <- read.csv('./_data/depthdamage/flemo.csv')
flemo$ft <- flemo$cm/100*mft
add <- seq(260, ceiling(50*2.54*12/10)*10, 10)
flemo <- flemo[nrow(flemo), -(1:2)] %>% 
  apply(2, function(x) rep(x, length(add))) %>% 
  cbind(cm = add, ft = add/2.54/12, .) %>% 
  rbind(flemo, .)

## get beta distribution parameters
wing2020 <- data.frame(water_ft = c(1:5,7), 
                       alpha = c(0.42, 0.48, 0.49, 0.53, 0.68, 0.80),
                       beta = c(0.80, 0.65, 0.52, 0.41, 0.42, 0.38))
f.alpha <- lm(alpha ~ water_ft, data = wing2020)
f.beta <- nls(beta ~ yf + (y0-yf)*exp(-alpha*water_ft), 
              data = wing2020, 
              start = list(y0 = 1, yf = 0, alpha = 1))
beta.dist <- data.frame(water_ft = 1:50) %>% 
  mutate(alpha = predict(f.alpha, data.frame(water_ft)), 
         beta = predict(f.beta, data.frame(water_ft)), 
         mu = alpha / (alpha + beta), 
         sd = sqrt((alpha*beta) / ((alpha+beta)^2 * (alpha + beta + 1)))) %>% 
  rbind(data.frame(water_ft = 0, alpha = NA, beta = NA, mu = 0, sd = 0), .)
g1 <- ggplot() +
  geom_line(data = beta.dist, aes(x = water_ft, y = alpha)) +
  geom_point(data = wing2020, aes(x = water_ft, y = alpha)) +
  labs(x = 'Water Depth (ft)', y = 'alpha') +
  theme_classic()
g2 <- ggplot() +
  geom_line(data = beta.dist, aes(x = water_ft, y = beta)) +
  geom_point(data = wing2020, aes(x = water_ft, y = beta)) +
  labs(x = 'Water Depth (ft)', y = 'beta') +
  theme_classic()
gridExtra::grid.arrange(g1, g2, ncol = 2)


###################################################################################################

## combine into one dataframe (using the SFH no basement curve)
df <- data.frame(flood_ft = seq(0, 50, 0.1))
df$flood_ft_rounded <- round(df$flood_ft)
df$hazus <- interp1(x = hazus$ft, y = hazus$x1, xi = df$flood_ft)
df$flemo <- interp1(x = flemo$ft, y = flemo$PQ_SFH, xi = df$flood_ft)

ggplot() + 
  geom_ribbon(data = beta.dist, aes(x = water_ft, ymin = (mu-sd)*100, ymax = (mu+sd)*100), fill = 'grey95') + 
  geom_line(data = beta.dist, aes(x = water_ft, y = mu*100, color = 'Beta', linetype = 'Beta'), size = 1) + 
  geom_line(data = df, aes(x = flood_ft, y = hazus, color = 'HAZUS', linetype = 'HAZUS'), size = 1) + 
  geom_step(data = df, aes(x = flood_ft, y = flemo, color = 'FLEMO', linetype = 'FLEMO'), size = 1) + 
  scale_linetype_manual(name = 'Curve Type', values = c('dotted', 'solid', 'longdash')) + 
  scale_color_manual(name = 'Curve Type', values = c('grey30', 'grey60', 'black')) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + 
  ggtitle('Depth-Damage Relationships') + 
  labs(x = 'Water Depth (ft)', y = 'Damage Ratio (%)') + 
  coord_cartesian(xlim = c(NA, 15), ylim = c(0, 100)) + 
  theme_classic() 


###################################################################################################

## save
save(hazus, flemo, beta.dist, file = './_data/depthdamage/depthdamage.Rdata')
