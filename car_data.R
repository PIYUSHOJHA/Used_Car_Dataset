library(data.table)
library(lubridate)
cars <- data.table::fread('autos.csv', na.strings = "", stringsAsFactors = TRUE, 
                          drop = 2)

# Convert date columns to POSIXct:
date_cols <- c("dateCrawled", "dateCreated", "lastSeen")
for(c in date_cols) set(cars, 
                        j = c,
                        value = parse_date_time(cars[[c]], "%Y-%m-%d %H:%M:%S"))
head(cars)
summary(cars$price)
cars <- cars[price <= 200000 &
               !(price > 100000 & 
                   vehicleType %in% c('sedan', 'kleinwagen', 
                                      'estate', 'people carrier', 'andere')), ]
summary(cars$price)
cars[price > 75000, unique(brand)] 
cars[price > 75000 & 
       brand %in% c("volkswagen", "seat", "ford", "opel", "renault",
                    "smart", "nissan", "mitsubishi")]

cars <- cars[price <= 75000 |
               brand %in% c("chevrolet", "porsche", "other", "mercedes_benz", 
                            "bmw", "audi", "jaguar", "land-rover") |
               model %in% c("mustang", "touareg"), 
             ]
cars <- cars[price >= 100,]
require(ggplot2)
library(scales)
library(ggthemes)
library(gridExtra)
library(GGally)
#Price of cars
ggplot(data = subset(cars, !is.na(price)), aes(x = price)) + 
  geom_histogram(fill = 'deepskyblue4', bins = 200) +
  scale_x_continuous(trans = "log10", 
                     breaks = c(100, 300, 1000, 3.e3, 1.e4, 3.e4, 1.e5, 3.e5)) +
  geom_vline(xintercept = median(cars$price), colour = 'blue', size = 1) +
  geom_vline(xintercept = mean(cars$price), colour = 'brown', size = 1) 
#Type of vehicle:
ggplot(data = cars, aes(x = vehicleType)) +
  geom_bar(fill = 'deepskyblue4') + coord_flip()
#year of registration
summary(cars$yearOfRegistration)
cars <- cars[yearOfRegistration >= 1960 & yearOfRegistration <= 2016]
ggplot(data = subset(cars, !is.na(yearOfRegistration)),
       aes(x = yearOfRegistration)) +
  geom_histogram(binwidth = 1, fill = 'red') +
  scale_x_continuous(breaks = seq(1960, 2016, 5)) +
  geom_vline(xintercept = mean(cars$yearOfRegistration),
             size = 1, colour = 'green') +
  geom_vline(xintercept = median(cars$yearOfRegistration),
             size = 1, colour = 'orange')
# month of registration
summary(cars$monthOfRegistration)
ggplot(data = subset(cars, !is.na(monthOfRegistration)),
       aes(x = monthOfRegistration)) +
  geom_histogram(binwidth = 0.5, fill = 'deepskyblue4') +
  scale_x_continuous(breaks = 1:12)
# transmission type
ggplot(data = cars, aes(x = gearbox)) +
  geom_bar(fill = 'deepskyblue4') 
# engine power
cars <- cars[powerPS >= 25 & powerPS <= 600, ]

ggplot(data = subset(cars, !is.na(powerPS)),
       aes(x = powerPS)) +
  geom_histogram(binwidth = 10, fill = 'deepskyblue4') +
  scale_x_continuous(breaks = seq(0, 600, 20)) +
  geom_vline(xintercept = mean(cars$powerPS), size = 1, colour = 'brown') +
  geom_vline(xintercept = median(cars$powerPS), size = 1, colour = 'blue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# model
model_count <- cars[, .N, by = model]
model_count <- model_count[order(model_count$N, decreasing = TRUE), ]


ggplot(data = model_count[1:10, ], 
       aes(x = reorder(model, -N), y = N)) +
  geom_bar(stat = 'identity', fill = 'deepskyblue4') + coord_flip()
# milege
summary(cars$kilometer)
ggplot(data = cars, aes(x = kilometer)) +
  geom_histogram(binwidth = 5000, fill = 'deepskyblue4') +
  geom_vline(xintercept = mean(cars$kilometer), size = 1, colour = 'brown') +
  geom_vline(xintercept = median(cars$kilometer), size = 1, colour = 'blue')
cars <- cars[, km_cat := factor(kilometer)]
# fuel type
ggplot(data = cars, aes(x = fuelType)) +
  geom_bar(fill = 'deepskyblue4') 
# Brand
brand_count <- cars[, .N, by = brand]
brand_count <- brand_count[order(brand_count$N, decreasing = TRUE), ]


ggplot(data = brand_count[1:10, ], 
       aes(x = reorder(brand, -N), y = N)) +
  geom_bar(stat = 'identity', fill = 'deepskyblue4') + coord_flip()
other <- c('lada', 'trabant', 'other')
budget <- c('chevrolet', 'daewoo', 'dacia')
budget_plus <- c('hyundai', 'kia', 'skoda', 'daihatsu')
mid_minus <- c('chrysler', 'fiat', 'ford', 'citroen', 'mitsubishi', 'opel', 
               'rover', 'seat', 'suzuki')
mid_range <- c('nissan', 'peugeot', 'renault', 'toyota')
mid_plus <- c('honda', 'mazda', 'smart', 'subaru', 'volkswagen')
premium_minus <- c('alfa_romeo', 'lancia', 'saab', 'jeep', 'volvo', 'mini')
premium <- c('audi', 'bmw', 'jaguar', 'land_rover', 'mercedes_benz', 'porsche')

cars[brand %in% other, brand_cat := 'other']
cars[brand %in% budget, brand_cat := 'budget']
cars[brand %in% budget_plus, brand_cat := 'budget_plus']
cars[brand %in% mid_minus, brand_cat := 'mid_minus']
cars[brand %in% mid_range, brand_cat := 'mid_range']
cars[brand %in% mid_plus, brand_cat := 'mid_plus']
cars[brand %in% premium_minus, brand_cat := 'premium_minus']
cars[brand %in% premium, brand_cat := 'premium']

cars$brand_cat <- ordered(cars$brand_cat, 
                          levels = c('budget', 'budget_plus',
                                     'mid_minus', 'mid_range',
                                     'mid_plus', 'premium_minus',
                                     'premium', 'other'),
                          labels = c('budget', 'budget_plus',
                                     'mid_minus', 'mid_range',
                                     'mid_plus', 'premium_minus',
                                     'premium', 'other'))
ggplot(data = cars, aes(x = brand_cat)) +
  geom_bar(fill = 'deepskyblue4') + coord_flip()
# unrepaired Damage:
ggplot(data = cars, aes(x = notRepairedDamage)) +
  geom_bar(fill = 'deepskyblue4') 
# price vs power
cars$log_price <- log10(cars$price)
ggplot(data = cars, aes(x = powerPS, y = log_price)) + 
  geom_point(fill = "deepskyblue1",
             shape = 21, alpha = .15, size = 1, position = 'jitter') +
  geom_smooth()
# price vs power vs fuel type
ggplot(data = subset(cars, fuelType %in% c("benzin", "diesel")),
       aes(x = powerPS, y = log_price, fill = fuelType)) +
  geom_point(shape = 21, alpha = .15, size = 1, position = 'jitter') +
  scale_fill_brewer(type = 'qual',
                    guide = guide_legend(reverse = TRUE,
                                         override.aes = list(alpha = 1,
                                                             size = 2))) +
  geom_smooth(aes(colour = fuelType))
ggplot(data = subset(cars, fuelType %in% c("benzin", "diesel")),
       aes(x = log(powerPS), y = log_price, fill = fuelType)) +
  geom_point(shape = 21, alpha = .15, size = 1, position = 'jitter') +
  scale_fill_brewer(type = 'qual',
                    guide = guide_legend(reverse = TRUE,
                                         override.aes = list(alpha = 1,
                                                             size = 2))) +
  geom_smooth(aes(colour = fuelType))
# price vs power vs fuel type vs gear box
ggplot(data = subset(cars, 
                     fuelType %in% c("benzin", "diesel") & !is.na(gearbox)),
       aes(x = log(powerPS), y = log_price)) +
  geom_point(shape = 21, fill = 'deepskyblue1', 
             alpha = .05, position = 'jitter') +
  geom_smooth() +
  facet_grid(gearbox ~ fuelType)
cars[, .(log_price_cor = cor(log_price, log(powerPS)))]
cars[fuelType %in% c("benzin", "diesel") & !is.na(gearbox),
     .(log_price_cor = cor(log_price, log(powerPS))), by = .(fuelType, gearbox)]

# price vs year of registration
ggplot(data = subset(cars, !is.na(yearOfRegistration)),
       aes(x = yearOfRegistration, y = price)) +
  geom_point(fill = 'deepskyblue4', shape = 21, alpha = .05, 
             position = 'jitter') +
  scale_y_continuous(
    breaks = c(300, 1000, 3000, 10000, 30000, 100000, 300000)) + 
  coord_trans(x = 'identity', y = 'log10', limx = c(1960, 2016))
cars <- cars[yearOfRegistration < 2016, ]
ggplot(data = subset(cars, !is.na(yearOfRegistration)),
       aes(x = yearOfRegistration, y = log_price)) +
  geom_point( 
             shape = 21, alpha = .05, 
             position = 'jitter') +
  scale_fill_brewer(type = 'qual',
                    guide = guide_legend(reverse = TRUE,
                                         override.aes = list(alpha = 1,
                                                             size = 5))) +
  geom_smooth(data = subset(cars,
                            !is.na(yearOfRegistration) &
                              yearOfRegistration < 2016 )) +
  geom_smooth(data = subset(cars,
                            !is.na(yearOfRegistration) &
                              yearOfRegistration < 2016 )) +
  scale_x_continuous(breaks = seq(1960, 2015, 5))
# price vs milege
ggplot(data = subset(cars, 
                     !is.na(kilometer)),
       aes(x = kilometer, y = log_price)) +
  geom_point(colour = 'lightskyblue3', shape = '.', alpha = .25, 
             position = 'jitter') +
  scale_x_continuous(breaks = c(0, as.numeric(levels(cars$km_cat)))) +
  geom_smooth()
# year of registration vs milege
ggplot(data = subset(cars, !is.na(kilometer) & yearOfRegistration < 2016), 
       aes(x = yearOfRegistration, y = kilometer)) +
  geom_point(colour = 'lightskyblue3', position = 'jitter', shape = '.',
             alpha = 0.25) +
  scale_x_continuous(breaks = seq(1960, 2015, 5), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 150000, 10000)) +
  geom_smooth()
# linear regression
require(dplyr)
cars_no_nas <- na.omit(cars[!(yearOfRegistration %in% seq(1990, 2010) 
                              & kilometer == 5000),
                            ])

fit <- lm(log_price ~  (powerPS : fuelType +
                          I(log(powerPS)) : fuelType +
                          gearbox +      
                          yearOfRegistration + 
                          kilometer + 
                          vehicleType + 
                          brand_cat) : notRepairedDamage,
          data = cars_no_nas)

cbind(Predictor = summary(fit)$dimnames, p_value = summary(fit)$coefficients[, 4])
print(paste("R-squared:", summary(fit)$r.squared))
test_examples <- cars_no_nas[sample(nrow(cars_no_nas), 1000),]
pred_price <- 10^predict(fit, newdata = test_examples)
test_examples <- cbind(pred_price, test_examples)
print(head(test_examples, 20))
ggplot(data = test_examples,
       aes(x = price, y = pred_price)) +
  geom_point(colour = 'deepskyblue4') +
  geom_abline(slope = 1., intercept = 0., colour = 'red', size = 1)

