# Code for Recipes for Data Processing by Max Kuhn (RStudio) 
# at useR 2018 in Brisbane

# setup ----------------------------------------------------------

library(tidyverse)
library(scales)

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

l10_breaks <- scales::trans_breaks("log10", function(x) 10^x)
l10_labels <- scales::trans_format("log10", scales::math_format(10^.x))

# slide 5 --------------------------------------------------------

## contains("Sepal")
## 
## # instead of
## 
## c("Sepal.Width", "Sepal.Length")

## merged <- inner_join(a, b)
## 
## # is equal to
## 
## merged <- a %>%
##   inner_join(b)

# slide 6 --------------------------------------------------------

ames <- 
  read_delim("http://bit.ly/2whgsQM", delim = "\t") %>%
  rename_at(vars(contains(' ')), funs(gsub(' ', '_', .))) %>%
  rename(Sale_Price = SalePrice) %>%
  filter(!is.na(Electrical)) %>%
  select(-Order, -PID, -Garage_Yr_Blt)

ames %>%
  group_by(Alley) %>%
  summarize(mean_price = mean(Sale_Price/1000),
            n = sum(!is.na(Sale_Price)))

# slide 7 --------------------------------------------------------

ggplot(ames, 
       aes(x = Garage_Type,
           y = Sale_Price)) + 
  geom_violin() + 
  coord_trans(y = "log10") + 
  xlab("Garage Type") + 
  ylab("Sale Price") 

# slide 8 --------------------------------------------------------

# summarize via purrr::map
by_alley <- split(ames, ames$Alley)
is_list(by_alley)

map(by_alley, nrow)

# or better yet:
map_int(by_alley, nrow)

# works on non-list vectors too
ames %>%
  mutate(Sale_Price = Sale_Price %>%
           map_dbl(function(x) x / 1000)) %>%
  select(Sale_Price, Yr_Sold) %>%
  head(4)

# slide 13 -------------------------------------------------------
library(AmesHousing)
ames <- make_ames()
nrow(ames)

library(rsample)

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

# slide 16 -------------------------------------------------------
ggplot(ames_train, aes(x = Neighborhood)) + 
  geom_bar() + 
  coord_flip() + 
  xlab("")

# slide 20 -------------------------------------------------------

library(recipes)
mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)

# slide 21 -------------------------------------------------------
mod_rec <- recipe(Sale_Price ~ Longitude + Latitude + Neighborhood, 
                  data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

# slide 22 -------------------------------------------------------
mod_rec_trained <- prep(mod_rec, training = ames_train, 
                        retain = TRUE, verbose = TRUE)

# slide 23 -------------------------------------------------------
mod_rec_trained

# slide 24 -------------------------------------------------------
ames_test_dummies <- bake(mod_rec_trained,newdata = ames_test)
names(ames_test_dummies)

# slide 26 -------------------------------------------------------
price_breaks <- (1:6)*(10^5)

ggplot(ames_train, aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
    scale_x_log10() + 
  scale_y_continuous(breaks = price_breaks, trans = "log10" ) +
  geom_smooth(method = "loess")

# slide 27 -------------------------------------------------------
library(MASS) # to get robust linear regression model

ggplot(ames_train, 
       aes(x = Year_Built, 
           y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
  scale_y_continuous(breaks = price_breaks, 
                     trans = "log10") + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm")

# slide 28 -------------------------------------------------------
mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air, data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built: Central_Air, data = ames_train)
anova(mod1, mod2)

# slide 29 -------------------------------------------------------
recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train, retain = TRUE) %>%
  juice %>%
  # select a few rows with different values
  slice(153:157)

# slide 35 -------------------------------------------------------
set.seed(2453)
cv_splits <- 
  vfold_cv(ames_train, v = 10, strata = "Sale_Price")

cv_splits[1:3,]

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% nrow()
cv_splits$splits[[1]] %>% assessment() %>% nrow()

# slide 36 -------------------------------------------------------
ames_rec <- recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
                     Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
                     Central_Air + Longitude + Latitude,
                   data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

# slide 37 -------------------------------------------------------
ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5)
  ) + 
  scale_y_log10()

# slide 38 -------------------------------------------------------
ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5)
  ) + 
  scale_y_log10()


# slide 39 -------------------------------------------------------
prepper

cv_splits <- cv_splits %>% 
  mutate(ames_rec = map(splits, prepper, recipe = ames_rec, retain = TRUE))

# slide 40 -------------------------------------------------------
lm_fit_rec <- function(rec_obj, ...) 
  lm(..., data = juice(rec_obj))

cv_splits <- cv_splits %>% 
  mutate(fits = map(ames_rec, lm_fit_rec, Sale_Price ~ .))
library(broom)
glance(cv_splits$fits[[1]])

# slide 41 -------------------------------------------------------
assess_predictions <- function(split_obj, rec_obj, mod_obj) {
  raw_data <- assessment(split_obj)
  proc_x <- bake(rec_obj, newdata = raw_data, all_predictors())
  # Now save _all_ of the columns and add predictions. 
  bake(rec_obj, newdata = raw_data, everything()) %>%
    mutate(
      .fitted = predict(mod_obj, newdata = proc_x),
      .resid = Sale_Price - .fitted,  # Sale_Price is already logged by the recipe
      # Save the original row number of the data
      .row = as.integer(split_obj, data = "assessment")
    )
}

# slide 42 -------------------------------------------------------
cv_splits <- cv_splits %>%
  mutate(
    pred = 
      pmap(
        lst(split_obj = cv_splits$splits, 
            rec_obj = cv_splits$ames_rec, 
            mod_obj = cv_splits$fits),
        assess_predictions 
      )
  )

# slide 43 -------------------------------------------------------
library(yardstick)

# Compute the summary statistics
map_df(cv_splits$pred, metrics, truth = Sale_Price, estimate = .fitted) %>% 
  colMeans

# slide 44 -------------------------------------------------------
assess_pred <- bind_rows(cv_splits$pred) %>%
  mutate(Sale_Price = 10^Sale_Price,
         .fitted = 10^.fitted) 

ggplot(assess_pred,
       aes(x = Sale_Price, y = .fitted)) + 
  geom_abline(lty = 2) + 
  geom_point(alpha = .4)  + 
  geom_smooth(se = FALSE, col = "red")
# slide 47 -------------------------------------------------------
ames_rec_norm <- 
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

# slide 48 -------------------------------------------------------
converted_resamples <- 
  rsample2caret(cv_splits)

ctrl <- trainControl(method = "cv", 
                     savePredictions = "final")
ctrl$index <- converted_resamples$index
ctrl$indexOut <- converted_resamples$indexOut

knn_grid <- expand.grid(
  kmax = 1:9,
  distance = 2,
  kernel = c("rectangular", "triangular", "gaussian")
  )

knn_fit <- train(
  ames_rec_norm, data = ames_train,
  method = "kknn", 
  tuneGrid = knn_grid,
  trControl = ctrl
)

# slide 49 -------------------------------------------------------
getTrainPerf(knn_fit)

ggplot(knn_fit)

# slide 50 -------------------------------------------------------
knn_pred <- knn_fit$pred %>%
  mutate(Sale_Price = 10^obs,
         .fitted = 10^pred) 

ggplot(knn_pred,
       aes(x = Sale_Price, y = .fitted)) + 
  geom_abline(lty = 2) + 
  geom_point(alpha = .4)  + 
  geom_smooth(se = FALSE, col = "red") 

