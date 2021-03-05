# Predicting-Number-of-Customers
Predicting Number of Customers at Food and Beverage Stores
Time Series and Forecasting
Theresa Tiurma

1. Case Introduction
The Food and Beverage Dataset for this case was provided by Dattabot. The data consist of detailed transactions of several outlets from December 2017 to the middle of February 2018. Time series study is used to processes numerical data with certain time intervals. Prediction of future values based on the past data in time series analysis is named forecasting. For this case, forecasting was used to investigate the number of visitors (customers) to help the business owners on justifying business decisions in 2018.

2. Data Preprocessing
2.1 Loading Libraries
library(lubridate) # for date
library(dplyr) # for data wrangling
library(forecast) # time series library
library(ggplot2) # data visualization
library(scales) # scaling
library(tidyverse) # faster processing
library(TTR)  # simple moving average function
library(MLmetrics) # calculate error
library(tseries) # adf test
library(fpp) # forecasting, principle, and practices
library(TSstudio) # beautify timeseries visualization
library(padr) # for padding

2.2 Checking Data Structure
FnB <- read_csv("Capstone_ML/data-train.csv")
glimpse(FnB)
#> Rows: 137,748
#> Columns: 10
#> $ transaction_date <dttm> 2017-12-01 13:32:46, 2017-12-01 13:32:46, 2017-12...
#> $ receipt_number   <chr> "A0026694", "A0026694", "A0026695", "A0026695", "A...
#> $ item_id          <chr> "I10100139", "I10500037", "I10500044", "I10400009"...
#> $ item_group       <chr> "noodle_dish", "drinks", "drinks", "side_dish", "d...
#> $ item_major_group <chr> "food", "beverages", "beverages", "food", "beverag...
#> $ quantity         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1,...
#> $ price_usd        <dbl> 7.33, 4.12, 2.02, 5.60, 3.01, 4.86, 6.34, 7.58, 4....
#> $ total_usd        <dbl> 7.33, 4.12, 2.02, 5.60, 3.01, 4.86, 6.34, 7.58, 4....
#> $ payment_type     <chr> "cash", "cash", "cash", "cash", "cash", "cash", "c...
#> $ sales_type       <chr> "dine_in", "dine_in", "dine_in", "dine_in", "dine_...
The Dataset includes information about:

Transaction_date: The time stamp of a transaction.
Receipt_number: The ID of a transaction.
Item_id: The ID of an item in a transaction.
Item_group: The group ID of an item in a transaction.
Item_major_group: The major-group ID of an item in a transaction.
Quantity: The quantity of purchased item.
Price_usd: The price of purchased item.
Total_usd: The total price of purchased item.
Payment_type: The payment method.
Sales_type: The sales method.

2.3 Rounding Transaction Time to Hourly Basis
Time rounding was used to simplify the model and analysis, the idea was to get hourly based data by grouping
transactions in different minutes and seconds but in the same hour.

FnB <- FnB%>% 
  mutate(datetime = ymd_hms(transaction_date))
FnB_clean <- FnB%>% 
  mutate(datetime = floor_date(datetime, unit = "hour"))
head(FnB_clean)
transaction_date
<S3: POSIXct>
receipt_number
<chr>
item_id
<chr>
item_group
<chr>
item_major_group
<chr>
quantity
<dbl>
2017-12-01 13:32:46	A0026694	I10100139	noodle_dish	food	1	
2017-12-01 13:32:46	A0026694	I10500037	drinks	beverages	1	
2017-12-01 13:33:39	A0026695	I10500044	drinks	beverages	1	
2017-12-01 13:33:39	A0026695	I10400009	side_dish	food	1	
2017-12-01 13:33:39	A0026695	I10500046	drinks	beverages	1	
2017-12-01 13:35:59	A0026696	I10300002	pastry	food	1	
6 rows | 1-6 of 11 columns
  
2.4 Data Aggregation
Data Aggregation was used to get the number of visitors per hour. This process was done by grouping visitors based on receipt number in each hour.

FnB_clean <- FnB_clean %>% 
  group_by(datetime) %>% 
  summarise(visitors = n_distinct(receipt_number)) %>% 
  ungroup()

FnB_clean
datetime
<S3: POSIXct>
visitors
<int>
2017-12-01 13:00:00	16
2017-12-01 14:00:00	38
2017-12-01 15:00:00	27
2017-12-01 16:00:00	29
2017-12-01 17:00:00	44
2017-12-01 18:00:00	50
2017-12-01 19:00:00	66
2017-12-01 20:00:00	70
2017-12-01 21:00:00	63
2017-12-01 22:00:00	63
...
1-10 of 1,244 rows
2.5 Data Padding
Forecasting future in time series required the data without missing intervals, missing values, and data need to be ordered by time. Data padding with pad() function from padr package was used to fill the missing intervals/values.

Checking the time interval.

range(FnB_clean$datetime)
#> [1] "2017-12-01 13:00:00 UTC" "2018-02-18 23:00:00 UTC"
FnB_clean <- FnB_clean %>% 
  pad(start_val = ymd_hms("2017-12-01 00:00:00"), end_val = ymd_hms("2018-02-18 23:00:00"))
FnB_clean
datetime
<S3: POSIXct>
visitors
<int>
2017-12-01 00:00:00	NA
2017-12-01 01:00:00	NA
2017-12-01 02:00:00	NA
2017-12-01 03:00:00	NA
2017-12-01 04:00:00	NA
2017-12-01 05:00:00	NA
2017-12-01 06:00:00	NA
2017-12-01 07:00:00	NA
2017-12-01 08:00:00	NA
2017-12-01 09:00:00	NA
...
1-10 of 1,920 rows
These missing values were replaced with 0.

FnB_clean <- FnB_clean %>% 
  mutate(visitors = replace_na(visitors, replace = 0))
FnB_clean
datetime
<S3: POSIXct>
visitors
<dbl>
2017-12-01 00:00:00	0
2017-12-01 01:00:00	0
2017-12-01 02:00:00	0
2017-12-01 03:00:00	0
2017-12-01 04:00:00	0
2017-12-01 05:00:00	0
2017-12-01 06:00:00	0
2017-12-01 07:00:00	0
2017-12-01 08:00:00	0
2017-12-01 09:00:00	0
...
1-10 of 1,920 rows
2.6 Data Subsetting
From the provided data, the stores were open from 10:00 AM to 22:00 PM, the data subsetting was used to collect the data for this time interval.

FnB_clean <-  FnB_clean %>% 
  filter(hour(datetime) %in% c(10:22))
FnB_clean
datetime
<S3: POSIXct>
visitors
<dbl>
2017-12-01 10:00:00	0
2017-12-01 11:00:00	0
2017-12-01 12:00:00	0
2017-12-01 13:00:00	16
2017-12-01 14:00:00	38
2017-12-01 15:00:00	27
2017-12-01 16:00:00	29
2017-12-01 17:00:00	44
2017-12-01 18:00:00	50
2017-12-01 19:00:00	66
...
1-10 of 1,040 rows
  
3. Seasonality Analysis

3.1 Seasonality, Trend, and Error
Object in time series consist of Trend, Seasonal, and Error/Residual. Decomposition is a step in time series to break the object into these three components:

Trend is a general data pattern, either increased or declined pattern. If the decomposition generates fluctuated trend from the object, means that there is still a pattern that could not not be captured properly.
Seasonal is the repeated pattern over a fixed period of time.
Error/Residual is the patterns that could not be captured by trends and seasonal.
The data should be in in time series class before they are decomposed. ts() function is used to convert from data frame to time series object. The time series object below is in hourly seasonality, frequency was set to 13 as the stores open from 10:00 AM to 22:00 PM.

FnB_ts <- ts(FnB_clean$visitors, frequency = 13)
FnB_ts

FnB_ts_dc <- FnB_ts %>% 
  decompose()

FnB_ts_dc %>% 
  autoplot()
The hourly seasonality showed that the trend still has a pattern. It might be because there is seasonality that has not been captured by trend and error, it indicates that the data has multi-seasonal cycles.Hence, the nest step is to process the data with multi-seasonal time series using weekly seasonality (frequency = 13*7)

FnB_msts <- msts(data = FnB_clean$visitors, seasonal.periods = c(13,13*7))
FnB_msts_dc <- FnB_msts %>% 
  mstl()

FnB_msts_dc %>% 
  autoplot()


The above decomposition showed a better trend and seasonality, therefore, this ts object will be used to build time series models later.

3.2 Stationarity Tests
Stationary tests were used to check if the variance and covariance change over time or no. Many statistical models require the series to be stationary (does not change over time) to make effective and precise predictions. Augmented Dickey Fuller (ADF) test and Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test were both used in this study.

adf.test(FnB_msts)
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  FnB_msts
#> Dickey-Fuller = -9.0673, Lag order = 10, p-value = 0.01
#> alternative hypothesis: stationary
kpss.test(FnB_msts)
#> 
#>  KPSS Test for Level Stationarity
#> 
#> data:  FnB_msts
#> KPSS Level = 0.04983, Truncation lag parameter = 7, p-value = 0.1
ADF test:

H0: Have a root unit(not stationary), p-value > 0.05.
H1: Don’t have a root unit(stationary), p-value < 0.05.
KPSS test:

H0: constant mean and variance(Stationary data), p-value > 0.05.
H1: mean and variance are not constant(data are not stationary), p-value < 0.05.
The ADF test above showed that p-value <0.05(alpha) and KPSS test p-value > 0.05, means the data is stationary and ready to be used for the models for an effective forecasting.

3.3 Seasonality Visualization
To visualize the better correlation between hourly and weekly seasonality, author conducted data aggregation of clean data and the decomposed of multi-seasonal time series object. After that, ggplot was used to make a better interpretation.

FnB_agg = FnB_clean %>% 
  mutate(Day = wday(datetime, label = T),
         Hour = hour(datetime))
FnB_agg

...
1-10 of 1,040 rows
FnB_df_agg <- as.data.frame(FnB_msts_dc) %>% 
  mutate(Date = FnB_agg$datetime,
         Day = FnB_agg$Day,
         Hour = FnB_agg$Hour) %>% 
  group_by(Day, Hour) %>% 
  summarise(Seasonal = mean(Seasonal13 + Seasonal91)) %>% 
  ungroup()
FnB_df_agg
Day
<ord>
Hour
<int>
Seasonal
<dbl>
Sun	10	-21.0657026
Sun	11	-13.6365997
Sun	12	-6.9956811
Sun	13	-2.0972657
Sun	14	-2.8374895
Sun	15	1.9298127
Sun	16	2.8300854
Sun	17	6.6523707
Sun	18	5.5196812
Sun	19	21.4123688
...
1-10 of 91 rows
P1 <- FnB_df_agg %>% 
ggplot(aes(x = Hour, y = Seasonal))+
  geom_col(aes(fill= Day))+
  labs(title = "Hourly and Weekly Seasonality", x =NULL, y= NULL, fill=NULL)+
  theme_minimal()+
  theme(legend.position = "top")
P1
The visualized-data above, showed that more visitors came to the store after 18:00 PM in any days. The peak of the sales happened during the weekend (Saturday and Sunday) after 18:00 PM. From this interpretation, author can suggest to the business owners, in case there is new product launch, they can focus in these time intervals, as more customers will be reached during this period.

4. Model Fitting and Evaluation

4.1 Cross Validation
The cross-validation for the time series analysis should be split sequentially.

FnB_train <- head(FnB_msts, -91)
FnB_test <- tail(FnB_msts, n = 91)
4.2 Model 1 - Triple Exponential Smoothing
The first model is Holt winters (Triple Exponential Smoothing), because based on the decomposed multi-seasonal time series object, the data have both trend and seasonal. Holt winters smoothed error, trend, and seasonal.

FnB_tes <- HoltWinters(FnB_msts)
FnB_tes

4.3 Model 2 - SARIMA
The second model that was used is SARIMA (Seasonal Auto-Regressive Integrated Moving Average). This model was used to compare if seasonal Arima can give better forecasting performance.

FnB_auto <- auto.arima(y = FnB_msts, seasonal = T)
FnB_auto
#> Series: FnB_msts 
#> ARIMA(1,0,2)(0,1,0)[91] 
#> 
#> Coefficients:
#>          ar1      ma1      ma2
#>       0.8483  -0.6518  -0.0830
#> s.e.  0.0867   0.0942   0.0447
#> 
#> sigma^2 estimated as 71.31:  log likelihood=-3369.82
#> AIC=6747.64   AICc=6747.68   BIC=6767.06
summary(FnB_auto)

4.4 Model 3 - STL with Multi Seasonal
The third model that was tried is STL (Seasonal and Trend decomposition using Loess) with multi seasonal periods. Time series may contain multiple seasonal cycles of different lengths. The objective of multiple seasonal process is to allow for the seasonal terms that represent a seasonal cycle to be updated more than once during the period of the cycle. In this model, seasonal components were estimated iteratively using STL and multiple seasonal periods were allowed. The trend component was computed for the last iteration of STL. Non-seasonal time series are decomposed into trend and remainder only.

FnB_stlm <- stlm(y = FnB_msts, method = "arima")
4.5 Model 4 - TBATS Model
The last model that was used is TBATS (Trigonometric Seasonal, Box-Cox Transformation, ARMA residuals, Trend and Seasonality). TBATS model allows the seasonality to change slowly over time, however, this model can be slow to estimate, especially with long time series.

FnB_tbats <- FnB_msts %>% 
  tbats(use.box.cox = FALSE,
        use.trend = TRUE,
        use.damped.trend = TRUE)

4.6 Forecasting
FnB_tes_forecast <- forecast(FnB_tes, h = 91) #Triple Exponential Smoothing
FnB_auto_forecast <- forecast(FnB_auto, h = 91) #SARIMA
FnB_stlm_forecast <- forecast(FnB_stlm, h = 91) #STLM 
FnB_tbats_forecast <- forecast(FnB_tbats, h = 91) #TBATS Model

4.7 Model Evaluation
Model evaluation was used to investigate the accuracy of each model, in this case by using MAE (Mean Absolute Error).

4.8 Actual and Forecast Data
Forecasting was used to predict future value by using the best mode from the evaluation results, which is STLM model.

FnB_stlm_forecast <- forecast(FnB_stlm, h = 91+91)
FnB_msts %>% 
  autoplot(series = "actual") +
  autolayer(FnB_stlm_forecast$fitted, series = "predict train") +
  autolayer(FnB_stlm_forecast$mean, series = "predict test")


5. Prediction Performance
5.1 Data Train Performance
accuracy(FnB_stlm_forecast) 

5.2 Preparation for Data Submission
data_test<- read.csv("Capstone_ML/data-test.csv")
head(data_test)
 
 
datetime
<chr>
visitor
<lgl>
1	2018-02-19T10:00:00Z	NA
2	2018-02-19T11:00:00Z	NA
3	2018-02-19T12:00:00Z	NA
4	2018-02-19T13:00:00Z	NA
5	2018-02-19T14:00:00Z	NA
6	2018-02-19T15:00:00Z	NA
6 rows
submission <- data_test %>% 
  mutate(visitor = round(FnB_final_forecast$mean),
         visitor = ifelse(visitor <0, 0, visitor))
submission

...
1-10 of 91 rows
write.csv(submission, "Submission.csv", row.names = F)
head(submission, 3)
 
 
datetime
<chr>
visitor
<dbl>
1	2018-02-19T10:00:00Z	4
2	2018-02-19T11:00:00Z	7
3	2018-02-19T12:00:00Z	14
3 rows

6. Assumption Check
Assumption check in time series was used to investigate if the model is able to process information from the data by checking that residuals. Good model does not have auto-correlated forecasting in time series and residuals are distributed normally. This means that there is no information left that can be used for forecasting and error is not accumulated only in several spots that could causing outliers.

6.1 Shapiro Test
For the Saphiro test - H0 : residuals are normally distributed, p-value > 0.05. - H1 : residuals are not normally distributed, p-value < 0.05.

shapiro.test(FnB_stlm$residuals)
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  FnB_stlm$residuals
#> W = 0.99115, p-value = 6.699e-06
Shapiro test (normality check) showed that p-value < 0.05 (reject H0 or accept H1) hence, the errors were not distributed normally. The residuals may not be appeared around its mean as seen in the histogram (below). However, if we inspect the distribution of residuals through a line plot, it is actually resembles the error plot from our time series object decomposition.

Those errors might emerge from various unpredictable events and it is actually inevitable. The solution to overcome this issue by analyzing what kinds of unpredictable events that might occur frequently. This can be done by time series analysis using seasonality adjustment.

hist(FnB_stlm$residuals, breaks = 20)


6.2 Ljung-Box Test
This test was use to check the presence of autocorrelation residual. - H0 : No autocorrelation in the forecast errors, p-value > 0.05. - H1 : There is an autocorrelation in the forecast errors, p-value < 0.05.

Box.test(x = FnB_stlm$residuals, type = "Ljung-Box")
#> 
#>  Box-Ljung test
#> 
#> data:  FnB_stlm$residuals
#> X-squared = 8.9703e-06, df = 1, p-value = 0.9976
Ljung Box test with p-value > 0.05 proved that there is no presence auto-correlated forecasting in time series.

7. Conclusion
The objective of this study was achieved by using time series analysis to investigate the number of customers that visited the stores and to forecast the number in the future so that business owners can prepare better business decisions in 2018.
The problem with multi-seasonal was solved using machine learning STLM model. This model worked efficiently to capture the change in the seasonal. The model was really accurate with the error (MAE) was only less than 4%.
The analysis and forecast from STLM model, showed that the peak of visitors who came to the stores happened during the weekend (Saturday and Sunday) after 18:00 PM. From this interpretation, author can suggest to the business owners, for example in case there is new product launch (introduction to new food and beverages), they can focus on the weekend evening/night, where the business owners can reach more customers.
