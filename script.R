library(plotly)
library(ggplot2)
install.packages("vctrs")
update.packages()
install.packages("ggplot2")
df <- read.csv('sf_clean.csv')
df2 <- read.csv('raw_sf_scrape.csv')
head(df)

# Simple Linear Regression 
plot(price ~ sqft, data = df, 
     pch = 18, 
     cex = 0.7, 
     col = '#69b3a2', 
     xlab = 'Square-footage of the Listing', 
     ylab = 'Price of Listing ($/Month)', 
     main = 'Rental Rate in San Francisco')

slr <- lm(price ~ sqft, data = df)
summary(slr)
# y = 2.72293x + 935.36946
yfit <- slr$fitted.values
plot(price ~ sqft, data = df, 
     pch = 18, 
     cex = 0.7, 
     col = '#69b3a2', 
     xlab = 'Square-footage of the Listing', 
     ylab = 'Price of Listing ($/Month)', 
     main = 'Rental Rate in San Francisco')
abline(coefficients(slr))

# Residual Histogram Chart. 
p <- ggplot(data=df, aes(slr$residuals)) + 
  geom_histogram(color = 'black', binwidth = 1 ) 
p + ggtitle("Histogram for Model Residuals")

# Use the model to predict housing prices based on sqft
set.seed(1)
random_sqft <- as.data.frame(
    matrix(
    round(
      runif(n = 30,
            min = 500,
            max = 5000)
    )
  )
)
colnames(random_sqft) <- c('sqft')

predicted_price <- predict(slr, newdata = random_sqft)
predicted_model_df <- cbind(predicted_price, random_sqft)
summary(predicted_model_df)

# Plot the prediction
ggplot(data = predicted_model_df,
       aes(x = sqft, y = predicted_price)) + 
  geom_point(shape = 18, color = 'blue', size = 7) +
  scale_y_continuous(breaks = c(1500, 3000, 4500, 6000, 7500, 9000, 10500, 12000, 13500)) + 
  labs(x = 'SQFT', y = 'Predicted Monthly Rent', title = 'Predicted Rental Rates in San Francisco')

# Multiple Linear Regression 
mlr <- lm(price ~ sqft + hood_district, data = df)
summary(mlr)
fig <- plot_ly(df, 
               x = sqft, 
               y = price,
               text = paste("Disctrict: ", hood_district),
               color = hood_district,
               size = df$sqft) %>%
  layout(title = 'Rental Rates in San Francisco',
         plot_bgcolor = "#e5ecf6", 
         xaxis = list(title = 'Square Footage (sqft)'), 
         yaxis = list(title = 'Price ($/month)'),
         align = "right",
         valign = 'top',
         legend = list(title=list(text='District')))
fig


typeof(df$hood_district)
