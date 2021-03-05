# Predicting-Number-of-Customers
Predicting Number of Customers at Food and Beverage Stores
Time Series and Forecasting
Theresa Tiurma
November 2, 2020
Food and Beverages Business Case
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
Time rounding was used to simplify the model and analysis, the idea was to get hourly based data by grouping transactions in different minutes and seconds but in the same hour.

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
#> Time Series:
#> Start = c(1, 1) 
#> End = c(80, 13) 
#> Frequency = 13 
#>    [1]  0  0  0 16 38 27 29 44 50 66 70 63 63 10 17 18 32 21 40 36 36 41 68 61
#>   [25] 62 54  7 13 20 35 23 30 31 44 55 66 47 49 54  5  9 10 13 12 15 19 27 20
#>   [49] 39 54 57 52  7 14 13  9 12 16 22 23 35 71 57 54 35  4 14 28 22 29 25 26
#>   [73] 30 35 47 48 58 33  2 10  9 14 16 21 20 34 40 60 57 53 41  0  0  0 13 19
#>   [97] 24 29 26 45 43 46 54 44 14 21 32 30 30 26 31 32 27 41 74 65 66  8 10 18
#>  [121] 16 22 21 25 37 41 50 62 56 46 14 13 13 18 14 18 16 17 26 27 51 48 43  9
#>  [145] 12 20 14 13 15 26 27 36 46 52 48 40 12 11 26 25 20 20 21 26 34 58 61 41
#>  [169] 40  8  6 18 21 14 23 21 23 30 47 42 62 37  0  0  0 11 22 17 26 21 44 57
#>  [193] 40 51 49 13 14 18 27 30 23 27 20 26 51 64 59 43  4 15 23 22 21 36 33 42
#>  [217] 39 48 56 58 40  8 11 17 18 11 22 23 28 37 54 61 56 42  7 15 17 13 17 15
#>  [241] 21 29 30 42 54 49 39  3 10 16 16 25 17 17 22 33 35 48 49 44 11 11 26 12
#>  [265] 18 17 24 26 26 52 32 43 32  0  0  0  4 24 28 17 21 27 39 60 38 52 13 16
#>  [289] 16 18 19 25 24 29 27 55 47 60 63  5 11 18 21 17 19 16 11 15 41 68 70 37
#>  [313]  5 10 20 32 21 21 22 28 33 31 64 51 41  3 16 12 18 26 18 27 28 33 53 40
#>  [337] 42 40  9  5  3 12 10 20 23 19 42 39 47 31 39  2 10 16 17 17 23 15 26 31
#>  [361] 34 56 58 34  0  0  0  6 22 16 18 24 32 57 61 52 46  4  8 30 26 21 21 29
#>  [385] 38 34 37 66 61 41  5 11 17 24 21 14 21 28 19 38 27 51 63  4 10 31 29 18
#>  [409] 23 33 39 37 64 56 58 48 11 18 18 21 24 25 27 31 49 57 54 38 42 12  7 16
#>  [433] 25 28 26 13 18 30 61 46 47 36  5  6 19 10 24 17  9 14 26 47 46 51 37  0
#>  [457]  0  0  8 21 26 17 25 33 46 65 42 44 11 18 28 26 35 18 38 36 54 53 59 67
#>  [481] 51 10 19 17 30 28 37 39 34 29 52 56 47 41  9 12  8 12 21 17 25 24 28 38
#>  [505] 51 44 35  9  5  9 17 21 20 18 20 22 45 43 46 44  8 12 12 16 31 22 21 19
#>  [529] 32 52 48 40 41  9  9 21 21 21 18 15 10 21 40 44 50 37  0  0  0 13 12 16
#>  [553] 14 30 33 53 63 41 37  9 11 22 29 26 24 27 32 44 53 63 61 41 13 11 28 19
#>  [577] 26 41 29 43 29 36 43 48 40  6  7 19 17  9 25 24 20 15 34 50 42 39  4 18
#>  [601] 21 12 23 24 23 27 28 37 47 41 51  6 14 20  9 18 17 28 28 21 44 49 47 37
#>  [625] 13  7 13 16 14 28 10 19 28 51 52 38 32  0  0  0  8 12 15 20 14 30 42 54
#>  [649] 60 48  5 15 29 29 20 22 26 27 37 57 66 58 58  7 19 28 42 31 33 41 45 37
#>  [673] 52 61 59 42  6 10 15 17 13 17 14 25 28 48 43 37 36  9 10 15 18 19 24 21
#>  [697] 35 27 45 30 46 34 12 12 19 10 19 16 24 27 20 31 43 52 35  5  7 18 22 17
#>  [721] 18 14 29 33 62 61 60 44  0  0  0  7 20 21 16 16 37 43 65 56 58  5 12 22
#>  [745] 25 35 29 33 39 34 54 80 61 50  8 16 21 30 32 32 33 36 41 60 57 50 47 14
#>  [769] 11 22 15 20 16 17 25 33 42 44 46 33  5  7 11 19 18 21 19 24 26 39 49 48
#>  [793] 42  7 19 20 16 23 20 14 27 31 43 64 46 40  9 10 24 11 14 19 20 19 34 50
#>  [817] 58 53 43  0  0  0  3 19 22 20 24 38 46 69 57 49 16 22 28 27 30 31 32 38
#>  [841] 38 47 76 61 48  6 19 23 29 38 41 48 36 47 59 83 65 44  5  5 15 10 17 22
#>  [865]  8 18 31 24 40 44 37  5 10 18 15  9 15 16 21 27 36 37 45 43  9 16 18 13
#>  [889] 14 16 30 28 31 39 45 36 45  9 20 18 15 19 23 28 18 23 43 57 61 47  0  0
#>  [913]  0  2 15 19 14 19 34 56 61 64 50  7 23 26 22 28 35 26 34 43 56 67 74 59
#>  [937]  6 21 29 26 29 33 25 45 47 60 70 57 53  6 13 20 21 14 22 25 17 24 42 52
#>  [961] 49 42  7  9 13 19 17 25 28 25 30 38 45 46 36  6  8 15 11  8 11 17 20 18
#>  [985] 33 49 62 49  6  4  6 13 14 14  7 15 19 30 35 40 42  0  0  0  3 21 24 34
#> [1009] 29 28 61 70 59 52  2 11 10 24 13 33 31 23 32 55 65 61 67 15 16 21 25 25
#> [1033] 27 34 26 31 56 67 49 41
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
datetime
<S3: POSIXct>
visitors
<dbl>
Day
<ord>
Hour
<int>
2017-12-01 10:00:00	0	Fri	10
2017-12-01 11:00:00	0	Fri	11
2017-12-01 12:00:00	0	Fri	12
2017-12-01 13:00:00	16	Fri	13
2017-12-01 14:00:00	38	Fri	14
2017-12-01 15:00:00	27	Fri	15
2017-12-01 16:00:00	29	Fri	16
2017-12-01 17:00:00	44	Fri	17
2017-12-01 18:00:00	50	Fri	18
2017-12-01 19:00:00	66	Fri	19
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
#> Holt-Winters exponential smoothing with trend and additive seasonal component.
#> 
#> Call:
#> HoltWinters(x = FnB_msts)
#> 
#> Smoothing parameters:
#>  alpha: 0.07480911
#>  beta : 0
#>  gamma: 0.3387189
#> 
#> Coefficients:
#>             [,1]
#> a    25.68702100
#> b    -0.02391659
#> s1  -23.78792906
#> s2  -20.83767537
#> s3  -12.54254972
#> s4  -13.59628737
#> s5  -14.70423959
#> s6   -9.75016754
#> s7  -11.27880304
#> s8   -8.86628436
#> s9   -2.01384446
#> s10   8.95108760
#> s11  18.46759057
#> s12  17.15343712
#> s13   9.99865275
#> s14 -22.27559996
#> s15 -18.44523778
#> s16 -13.75922814
#> s17 -11.18488035
#> s18 -11.85943476
#> s19  -6.98813966
#> s20  -6.05656894
#> s21  -3.12789229
#> s22   0.56997799
#> s23  12.44475920
#> s24  15.43078007
#> s25  18.17167369
#> s26  12.52506888
#> s27 -19.62829816
#> s28 -14.89010838
#> s29 -10.16794797
#> s30 -14.18062088
#> s31 -11.16162796
#> s32 -10.68082305
#> s33  -5.24635254
#> s34  -2.32089349
#> s35  -1.01529356
#> s36  12.61128294
#> s37  23.64707802
#> s38  23.12813771
#> s39  16.53567599
#> s40 -19.33135430
#> s41 -17.33340311
#> s42 -12.09869214
#> s43 -12.02247262
#> s44 -10.51692396
#> s45  -7.88360417
#> s46 -10.97563702
#> s47  -8.05717994
#> s48  -1.08060532
#> s49  16.53193052
#> s50  22.91546331
#> s51  24.47299817
#> s52  16.26494704
#> s53 -26.01381883
#> s54 -26.14282754
#> s55 -26.26312012
#> s56 -21.78844655
#> s57  -7.96052641
#> s58  -5.18453917
#> s59  -4.02873396
#> s60  -3.56616207
#> s61   5.89644174
#> s62  26.01911041
#> s63  37.36701711
#> s64  29.87995611
#> s65  22.22520093
#> s66 -21.14885234
#> s67 -11.73153377
#> s68  -6.83140480
#> s69  -2.49372961
#> s70  -3.62513318
#> s71   3.41271101
#> s72   2.44244753
#> s73   3.60669403
#> s74   9.71836824
#> s75  25.94520686
#> s76  40.85809172
#> s77  36.36104662
#> s78  29.82135768
#> s79 -18.45387126
#> s80 -10.88976865
#> s81  -4.71704870
#> s82  -1.24120959
#> s83   0.42541480
#> s84   3.45992874
#> s85   5.25561498
#> s86   6.70146376
#> s87   9.45300297
#> s88  26.88425181
#> s89  36.96282412
#> s90  24.90965066
#> s91  15.78965719
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
#> 
#> Training set error measures:
#>                      ME     RMSE      MAE MPE MAPE      MASE          ACF1
#> Training set -0.2467381 8.053834 5.791747 NaN  Inf 0.8870833 -0.0006087466
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

accuracy(FnB_tes_forecast) #Triple Exponential Smoothing
#>                     ME     RMSE      MAE MPE MAPE      MASE      ACF1
#> Training set 0.2403138 7.167253 5.337523 NaN  Inf 0.8175128 0.2370902
accuracy(FnB_auto_forecast) #SARIMA
#>                      ME     RMSE      MAE MPE MAPE      MASE          ACF1
#> Training set -0.2467381 8.053834 5.791747 NaN  Inf 0.8870833 -0.0006087466
accuracy(FnB_stlm_forecast) #STLM 
#>                       ME     RMSE      MAE MPE MAPE      MASE          ACF1
#> Training set -0.01176577 5.197046 3.949861 NaN  Inf 0.6049738 -9.273849e-05
accuracy(FnB_tbats_forecast)#TBATS Model
#>                      ME     RMSE      MAE MPE MAPE      MASE        ACF1
#> Training set -0.2214709 6.557409 5.062394 NaN  Inf 0.7753732 0.001431828
From the evaluation above, STLM is the best model to be used for this data, this model has the smallest error (MAE) than others. This model was able to capture the seasonal terms that represent a seasonal cycle that need to be updated more than once during the period of the cycle.

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
#>                       ME     RMSE      MAE MPE MAPE      MASE          ACF1
#> Training set -0.01176577 5.197046 3.949861 NaN  Inf 0.6049738 -9.273849e-05
FnB_final <- stlm(FnB_msts, method = "arima")
FnB_final_forecast <- forecast(FnB_final, h = 13*7)
FnB_final_forecast
#>          Point Forecast      Lo 80     Hi 80       Lo 95     Hi 95
#> 12.42857     3.82135391 -2.8517736 10.494481  -6.3843139 14.027022
#> 12.43956     7.17998882  0.2066373 14.153340  -3.4848320 17.844810
#> 12.45055    13.57749434  6.5365641 20.618425   2.8093208 24.345668
#> 12.46154    15.23096727  8.1396255 22.322309   4.3856959 26.076239
#> 12.47253    11.33698912  4.2079603 18.466018   0.4340803 22.239898
#> 12.48352    19.91811977 12.7608716 27.075368   8.9720534 30.864186
#> 12.49451    20.83507617 13.6566733 28.013479   9.8566563 31.813496
#> 12.50549    20.44921272 13.2549372 27.643488   9.4465178 31.451908
#> 12.51648    24.66446792 17.4582753 31.870661  13.6435473 35.685389
#> 12.52747    40.51490751 33.2997632 47.730052  29.4802965 51.549519
#> 12.53846    50.09745478 42.8755839 57.319326  39.0525564 61.142353
#> 12.54945    48.29392415 41.0669973 55.520851  37.2412934 59.346555
#> 12.56044    43.32390333 36.0931755 50.554631  32.2654595 54.382347
#> 12.57143     6.21275325 -1.0208325 13.446339  -4.8500615 17.275568
#> 12.58242    10.43569368  3.1999588 17.671429  -0.6304079 21.501795
#> 12.59341    11.92023283  4.6828816 19.157584   0.8516594 22.988806
#> 12.60440    16.03813652  8.7995697 23.276703   4.9677039 27.108569
#> 12.61538    15.15627973  7.9167986 22.395761   4.0844489 26.228111
#> 12.62637    21.87191674 14.6317479 29.112086  10.7990341 32.944799
#> 12.63736    23.61111756 16.3704315 30.851804  12.5374438 34.684791
#> 12.64835    23.95499320 16.7139180 31.196068  12.8807244 35.029262
#> 12.65934    26.53940196 19.2980341 33.780770  15.4646855 37.614118
#> 12.67033    43.10096112 35.8593731 50.342549  32.0259080 54.176014
#> 12.68132    44.99727495 37.7555213 52.239029  33.9219685 56.072581
#> 12.69231    47.55701546 40.3151372 54.798894  36.4815185 58.632512
#> 12.70330    46.29028491 39.0483130 53.532257  35.2146446 57.365925
#> 12.71429     8.22546430  0.9834219 15.467507  -2.8502838 19.301212
#> 12.72527    11.87032570  4.6282302 19.112421   0.7944965 22.946155
#> 12.73626    14.01267572  6.7705404 21.254811   2.9367855 25.088566
#> 12.74725    13.06084880  5.8186834 20.303014   1.9849127 24.136785
#> 12.75824    15.77542257  8.5332346 23.017611   4.6994519 26.851393
#> 12.76923    18.61201187 11.3698070 25.854217   7.5360153 29.688008
#> 12.78022    23.21028919 15.9680715 30.452507  12.1342730 34.286305
#> 12.79121    21.93960319 14.6973759 29.181830  10.8635723 33.015634
#> 12.80220    24.44421022 17.2019757 31.686445  13.3681683 35.520252
#> 12.81319    42.51739416 35.2751542 49.759634  31.4413440 53.593444
#> 12.82418    51.03035889 43.7881148 58.272603  39.9543024 62.106415
#> 12.83516    49.19834360 41.9560965 56.440591  38.1222824 60.274405
#> 12.84615    45.85476628 38.6125168 53.097016  34.7787016 56.930831
#> 12.85714     7.83234777  0.5900966 15.074599  -3.2437196 18.908415
#> 12.86813     8.89812549  1.6558730 16.140378  -2.1779439 19.974195
#> 12.87912    14.50365214  7.2613987 21.745906   3.4275813 25.579723
#> 12.89011    14.95410452  7.7118503 22.196359   3.8780325 26.030177
#> 12.90110    14.14159960  6.8993448 21.383854   3.0655267 25.217672
#> 12.91209    20.59031501 13.3480598 27.832570   9.5142415 31.666389
#> 12.92308    17.45711343 10.2148579 24.699369   6.3810394 28.533187
#> 12.93407    17.14504765  9.9027919 24.387303   6.0689733 28.221122
#> 12.94505    24.13541368 16.8931578 31.377670  13.0593390 35.211488
#> 12.95604    46.43224612 39.1899901 53.674502  35.3561713 57.508321
#> 12.96703    51.92920321 44.6869470 59.171459  40.8531282 63.005278
#> 12.97802    53.74148099 46.4992248 60.983737  42.6654059 64.817556
#> 12.98901    44.99877664 37.7565203 52.241033  33.9227014 56.074852
#> 13.00000     0.08144295 -7.1608134  7.323699 -10.9946323 11.157518
#> 13.01099    -0.33618006 -7.5784364  6.906076 -11.4122554 10.739895
#> 13.02198    -2.45699027 -9.6992467  4.785266 -13.5330656  8.619085
#> 13.03297     5.38543487 -1.8568215 12.627691  -5.6906405 16.461510
#> 13.04396    14.96788804  7.7256316 22.210144   3.8918126 26.043963
#> 13.05495    21.13998911 13.8977327 28.382246  10.0639137 32.216065
#> 13.06593    21.57420846 14.3319520 28.816465  10.4981330 32.650284
#> 13.07692    20.43195438 13.1896979 27.674211   9.3558790 31.508030
#> 13.08791    30.69684826 23.4545918 37.939105  19.6207728 41.772924
#> 13.09890    50.95707637 43.7148199 58.199333  39.8810009 62.033152
#> 13.10989    65.04718641 57.8049299 72.289443  53.9711110 76.123262
#> 13.12088    57.32946035 50.0872039 64.571717  46.2533849 68.405536
#> 13.13187    53.79681831 46.5545619 61.039075  42.7207429 64.872894
#> 13.14286     7.90092059  0.6586641 15.143177  -3.1751549 18.976996
#> 13.15385    15.36454183  8.1222854 22.606798   4.2884664 26.440617
#> 13.16484    21.02195133 13.7796949 28.264208   9.9458759 32.098027
#> 13.17582    25.16935014 17.9270937 32.411607  14.0932747 36.245426
#> 13.18681    23.07346093 15.8312045 30.315717  11.9973855 34.149536
#> 13.19780    28.54082009 21.2985636 35.783077  17.4647446 39.616896
#> 13.20879    31.85856523 24.6163088 39.100822  20.7824898 42.934641
#> 13.21978    30.95446017 23.7122037 38.196717  19.8783847 42.030536
#> 13.23077    36.01090843 28.7686520 43.253165  24.9348330 47.086984
#> 13.24176    53.63873295 46.3964765 60.880989  42.5626575 64.714808
#> 13.25275    69.37879480 62.1365383 76.621051  58.3027193 80.454870
#> 13.26374    65.78889067 58.5466342 73.031147  54.7128152 76.864966
#> 13.27473    58.44525873 51.2030023 65.687515  47.3691833 69.521334
#> 13.28571     9.01428604  1.7720296 16.256543  -2.0617894 20.090362
#> 13.29670    16.61736973  9.3751133 23.859626   5.5412943 27.693445
#> 13.30769    21.23819858 13.9959421 28.480455  10.1621231 32.314274
#> 13.31868    27.73589940 20.4936429 34.978156  16.6598239 38.811975
#> 13.32967    26.50030904 19.2580526 33.742566  15.4242336 37.576385
#> 13.34066    33.75856250 26.5163060 41.000819  22.6824870 44.834638
#> 13.35165    36.00920287 28.7669464 43.251459  24.9331274 47.085278
#> 13.36264    34.82102934 27.5787729 42.063286  23.7449539 45.897105
#> 13.37363    34.08572420 26.8434677 41.327981  23.0096487 45.161800
#> 13.38462    54.08237664 46.8401202 61.324633  43.0063012 65.158452
#> 13.39560    63.18584771 55.9435912 70.428104  52.1097722 74.261923
#> 13.40659    57.28626675 50.0440103 64.528523  46.2101913 68.362342
#> 13.41758    50.12205200 42.8797955 57.364308  39.0459765 61.198127
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
datetime
<chr>
visitor
<dbl>
2018-02-19T10:00:00Z	4
2018-02-19T11:00:00Z	7
2018-02-19T12:00:00Z	14
2018-02-19T13:00:00Z	15
2018-02-19T14:00:00Z	11
2018-02-19T15:00:00Z	20
2018-02-19T16:00:00Z	21
2018-02-19T17:00:00Z	20
2018-02-19T18:00:00Z	25
2018-02-19T19:00:00Z	41
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
