#import the libraries
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(seasonal)
library(xts)
library(mFilter)
library(class)
library(caret)
library(tibble)
library(lubridate)
library(extrafont)
font_import(path = "~/Library/Fonts")
set.seed(123)

# the scope of the data would be 1982-01-01 to 2021-01-01
start_date <- '1982-01-01'
start_year <- 1982
start_month <- 1
end_date <- '2021-01-01'
end_year <- 2021
end_month <- 1
# total months between 1982-01-01 to 2021-01-01
total_month <- interval(ymd(start_date), ymd(end_date)) / months(1)

#-----------------import data-----------------
# ind, non_agr, export, manu, overtime
ind <- read_excel('./data/工業生產指數.xlsx')[c(1, 4)] 
ind[1] <- paste(ind$月份, "-1", sep = "")
ind <- ind %>% 
  mutate(月份 = as.Date(月份, format = "%Y-%m-%d"))
colnames(ind) <- c('yyyymm', 'index')

#非農業
non_agr <- read_excel('./data/非農業部門就業人數.xlsx')[c(1, 4)]
non_agr[1] <- paste(non_agr$月份, "-1", sep = "")
non_agr <- non_agr %>% 
  mutate(月份 = as.Date(月份, format = "%Y-%m-%d"))
colnames(non_agr) <- c('yyyymm', 'index')
non_agr$index <- as.numeric(gsub(",", "", non_agr$index))

#實質海關出口
export <- read_excel('./data/實質海關出口值.xlsx')[c(1, 4)]
export[1] <- paste(export$月份, "-1", sep = "")
export <- export %>% 
  mutate(月份 = as.Date(月份, format = "%Y-%m-%d"))
colnames(export) <- c('yyyymm', 'index')
export$index <- as.numeric(gsub(",", "", export$index))

#製造業銷售量指數
manu <- read_excel('./data/製造業銷售量指數.xlsx')[c(1, 4)]
manu[1] <- paste(manu$月份, "-1", sep = "")
manu <- manu %>% 
  mutate(月份 = as.Date(月份, format = "%Y-%m-%d"))
colnames(manu) <- c('yyyymm', 'index')
manu$index <- as.numeric(gsub(",", "", manu$index))

#工業及服務業加班工時
overtime <- read_excel("./data/工業及服務業加班工時.xlsx", skip = 2)
colnames(overtime) <- c('yyyymm', 'index')
overtime$yyyymm <- seq(as.Date("1982-01-01"), by = "month", length.out = 490)

#--------------------Business Cycles-----------------
# expansion: 1, contraction: 0

# 5th cycle: 1975/2 to 1983/2,
# contraction: 1980/1-1983/2
cycle5_con_dates <- as.data.frame(seq(as.Date("1982-02-01"), by = "month",
                                      length.out = 13))
colnames(cycle5_con_dates) <- "yyyymm"
cycle <- rep(1, 13)
cycle5_con <- cbind(cycle5_con_dates, cycle)
cycle5 <- cycle5_con

# 6th cycle
# expansion
cycle6_exp_dates <- as.data.frame(seq(as.Date("1983-03-01"), by = "month",
                                      length.out = 15))
colnames(cycle6_exp_dates) <- "yyyymm"
cycle <- rep(1, 15)
cycle6_exp <- cbind(cycle6_exp_dates, cycle)

# contraction
cycle6_con_dates <- as.data.frame(seq(as.Date("1984-06-01"), by = "month",
                                      length.out = 15))
colnames(cycle6_con_dates) <- "yyyymm"
cycle <- rep(0, 15)
cycle6_con <- cbind(cycle6_con_dates, cycle)
cycle6 <- rbind(cycle6_exp, cycle6_con)

# 7th cycle
# expansion
cycle7_exp_dates <- as.data.frame(seq(as.Date("1985-09-01"), by = "month",
                                      length.out = 45))
colnames(cycle7_exp_dates) <- "yyyymm"
cycle <- rep(1, 45)
cycle7_exp <- cbind(cycle7_exp_dates, cycle)

#contraction
cycle7_con_dates <- as.data.frame(seq(as.Date("1989-06-01"), by = "month",
                                      length.out = 15))
colnames(cycle7_con_dates) <- "yyyymm"
cycle <- rep(0, 15)
cycle7_con <- cbind(cycle7_con_dates, cycle)
cycle7 <- rbind(cycle7_exp, cycle7_con)

# 8th cycle
# expansion
cycle8_exp_dates <- as.data.frame(seq(as.Date("1990-09-01"), by = "month",
                                      length.out = 54))
colnames(cycle8_exp_dates) <- "yyyymm"
cycle <- rep(1, 54)
cycle8_exp <- cbind(cycle8_exp_dates, cycle)

#contraction
cycle8_con_dates <- as.data.frame(seq(as.Date("1995-03-01"), by = "month",
                                      length.out = 13))
colnames(cycle8_con_dates) <- "yyyymm"
cycle <- rep(0, 13)
cycle8_con <- cbind(cycle8_con_dates, cycle)
cycle8 <- rbind(cycle8_exp, cycle8_con)

# 9th cycle
# expansion
cycle9_exp_dates <- as.data.frame(seq(as.Date("1996-04-01"), by = "month",
                                      length.out = 21))
colnames(cycle9_exp_dates) <- "yyyymm"
cycle <- rep(1, 21)
cycle9_exp <- cbind(cycle9_exp_dates, cycle)

#contraction
cycle9_con_dates <- as.data.frame(seq(as.Date("1998-01-01"), by = "month",
                                      length.out = 12))
colnames(cycle9_con_dates) <- "yyyymm"
cycle <- rep(0, 12)
cycle9_con <- cbind(cycle9_con_dates, cycle)
cycle9 <- rbind(cycle9_exp, cycle9_con)

# 10th cycle
# expansion
cycle10_exp_dates <- as.data.frame(seq(as.Date("1999-01-01"), by = "month",
                                       length.out = 21))
colnames(cycle10_exp_dates) <- "yyyymm"
cycle <- rep(1, 21)
cycle10_exp <- cbind(cycle10_exp_dates, cycle)

#contraction
cycle10_con_dates <- as.data.frame(seq(as.Date("2000-10-01"), by = "month",
                                       length.out = 12))
colnames(cycle10_con_dates) <- "yyyymm"
cycle <- rep(0, 12)
cycle10_con <- cbind(cycle10_con_dates, cycle)
cycle10 <- rbind(cycle10_exp, cycle10_con)

# 11th cycle
# expansion
cycle11_exp_dates <- as.data.frame(seq(as.Date("2001-10-01"), by = "month",
                                       length.out = 30))
colnames(cycle11_exp_dates) <- "yyyymm"
cycle <- rep(1, 30)
cycle11_exp <- cbind(cycle11_exp_dates, cycle)

#contraction
cycle11_con_dates <- as.data.frame(seq(as.Date("2004-04-01"), by = "month",
                                       length.out = 11))
colnames(cycle11_con_dates) <- "yyyymm"
cycle <- rep(0, 11)
cycle11_con <- cbind(cycle11_con_dates, cycle)
cycle11 <- rbind(cycle11_exp, cycle11_con)

# 12th cycle
# expansion
cycle12_exp_dates <- as.data.frame(seq(as.Date("2005-03-01"), by = "month",
                                       length.out = 37))
colnames(cycle12_exp_dates) <- "yyyymm"
cycle <- rep(1, 37)
cycle12_exp <- cbind(cycle12_exp_dates, cycle)

#contraction
cycle12_con_dates <- as.data.frame(seq(as.Date("2008-04-01"), by = "month",
                                       length.out = 11))
colnames(cycle12_con_dates) <- "yyyymm"
cycle <- rep(0, 11)
cycle12_con <- cbind(cycle12_con_dates, cycle)
cycle12 <- rbind(cycle12_exp, cycle12_con)

# 13th cycle
# expansion
cycle13_exp_dates <- as.data.frame(seq(as.Date("2009-03-01"), by = "month",
                                       length.out = 24))
colnames(cycle13_exp_dates) <- "yyyymm"
cycle <- rep(1, 24)
cycle13_exp <- cbind(cycle13_exp_dates, cycle)

#contraction
cycle13_con_dates <- as.data.frame(seq(as.Date("2011-03-01"), by = "month",
                                       length.out = 11))
colnames(cycle13_con_dates) <- "yyyymm"
cycle <- rep(0, 11)
cycle13_con <- cbind(cycle13_con_dates, cycle)
cycle13 <- rbind(cycle13_exp, cycle13_con)

# 14th cycle
# expansion
cycle14_exp_dates <- as.data.frame(seq(as.Date("2012-02-01"), by = "month",
                                       length.out = 33))
colnames(cycle14_exp_dates) <- "yyyymm"
cycle <- rep(1, 33)
cycle14_exp <- cbind(cycle14_exp_dates, cycle)

#contraction
cycle14_con_dates <- as.data.frame(seq(as.Date("2014-11-01"), by = "month",
                                       length.out = 16))
colnames(cycle14_con_dates) <- "yyyymm"
cycle <- rep(0, 16)
cycle14_con <- cbind(cycle14_con_dates, cycle)
cycle14 <- rbind(cycle14_exp, cycle14_con)

# 15th cycle
# expansion
cycle15_exp_dates <- as.data.frame(seq(as.Date("2016-03-01"), by = "month",
                                       length.out = 6))
colnames(cycle15_exp_dates) <- "yyyymm"
cycle <- rep(1, 6)
cycle15_exp <- cbind(cycle15_exp_dates, cycle)
cycle15 <- rbind(cycle15_exp)

# all cycles since 1982-01-01
all_cycle <- rbind(cycle5, cycle6, cycle7, cycle8, cycle9, cycle10, cycle11
                   , cycle12, cycle13, cycle14, cycle15)

# 還未公佈
# 53 because there are 53 months from 2016-09-01 to 2021-01-01
cycle15_after <- as.data.frame(seq(as.Date("2016-09-01"), by = "month",
                                   length.out = (total_month-nrow(all_cycle))))
colnames(cycle15_after) <- "yyyymm"
cycle <- rep(NA, (total_month-nrow(all_cycle)))
cycle_na <- cbind(cycle15_after, cycle)
business_cycle <- rbind(all_cycle, cycle_na)

# ---------------------- cycle dates for ggplot----------------------
#ggplot dates
cycle5_rect <- geom_rect(aes(xmin = as.Date("1982-01-01"), xmax = as.Date("1983-02-01"),
                             ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle6_rect <- geom_rect(aes(xmin = as.Date("1984-06-01"), xmax = as.Date("1985-08-01"),
                             ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle7_rect <- geom_rect(aes(xmin = as.Date("1989-06-01"), xmax = as.Date("1990-08-01"),
                             ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle8_rect <- geom_rect(aes(xmin = as.Date("1995-03-01"), xmax = as.Date("1996-03-01"),
                             ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle9_rect <- geom_rect(aes(xmin = as.Date("1998-01-01"), xmax = as.Date("1998-12-01"),
                             ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle10_rect <- geom_rect(aes(xmin = as.Date("2000-10-01"), xmax = as.Date("2001-09-01"),
                              ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle11_rect <- geom_rect(aes(xmin = as.Date("2004-04-01"), xmax = as.Date("2005-02-01"),
                              ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle12_rect <- geom_rect(aes(xmin = as.Date("2008-04-01"), xmax = as.Date("2009-02-01"),
                              ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle13_rect <- geom_rect(aes(xmin = as.Date("2011-03-01"), xmax = as.Date("2012-01-01"),
                              ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)
cycle14_rect <- geom_rect(aes(xmin = as.Date("2014-10-01"), xmax = as.Date("2016-02-01"),
                              ymin = -Inf, ymax = Inf), fill="grey50", alpha = .009)


# indicator_processing
indicator_processing <- function(df, start_date, end_date, indicator_type){
  indicator_processing_result <- list()
  df <- df %>% filter(yyyymm >= start_date & yyyymm <= end_date)
  df_index_plot <- ggplot(df, aes(x= yyyymm, y= index))+
    geom_line(color = "black", size = 0.8) +
    cycle5_rect + cycle6_rect + cycle7_rect + cycle8_rect + cycle9_rect +
    cycle10_rect + cycle11_rect + cycle12_rect + cycle13_rect + cycle14_rect +
    xlab("Dates") +
    ylab("Index") +
    ggtitle(indicator_type) +
    theme_bw()+
    theme(plot.title = element_text(size = 14, color = "black",
                                    face = "bold", hjust = 0.5)) +
    cycle5_rect + cycle6_rect + cycle7_rect + cycle8_rect + cycle9_rect +
    cycle10_rect + cycle11_rect + cycle12_rect + cycle13_rect + cycle14_rect
  indicator_processing_result$index_plot <- df_index_plot
  
  # convert it into time series
  df_ts <- ts(df$index, start=c(start_year, start_month), end=c(end_year, end_month), frequency=12) 
  
  # seasonal adjustment
  df_seas <- seas(df_ts)
  df_seaadj <- ts(as.data.frame(df_seas$data)[1],
                  start=c(start_year, start_month),
                  end=c(end_year, end_month),
                  frequency=12)
  indicator_processing_result$seaadj_result <- df_seaadj
  
  df_seaadj_hp1 <- hpfilter(df_seaadj, freq = 15426, type = c("lambda"))
  indicator_processing_result$seaadj_hp1 <- df_seaadj_hp1
  df_seaadj_hp2 <- hpfilter(df_seaadj_hp1$cycle, freq = 34, type = c("lambda"))
  df_seaadj_hp2 <- as.data.frame(df_seaadj_hp2$trend)
  colnames(df_seaadj_hp2) <- c("hp2")
  class(df_seaadj_hp2$hp2) <- "numeric"
  indicator_processing_result$seaadj_hp2 <- df_seaadj_hp2
  
  #graph the trends after two-layer hpfilter
  date_time <- seq(from = as.Date(start_date), to = as.Date(end_date), by = "month")
  df_seaadj_trend <- data.frame(date_time, df_seaadj_hp2$hp2)
  df_seaadj_plot <-
    ggplot(df_seaadj_trend, aes(x= date_time, y = df_seaadj_hp2.hp2)) +
    geom_line(color = "black", size = 0.8) +
    geom_hline(yintercept = 0) + 
    cycle5_rect + cycle6_rect + cycle7_rect + cycle8_rect + cycle9_rect +
    cycle10_rect + cycle11_rect + cycle12_rect + cycle13_rect + cycle14_rect +
    xlab("Dates") +
    ylab("Index") +
    ggtitle(indicator_type) +
    theme_bw()+
    theme(plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5)) +
    cycle5_rect + cycle6_rect + cycle7_rect + cycle8_rect + cycle9_rect +
    cycle10_rect + cycle11_rect + cycle12_rect + cycle13_rect + cycle14_rect
  indicator_processing_result$seaadj_plot <- df_seaadj_plot
  
  df_seaadj_hp2 <- df_seaadj_hp2 %>% mutate(hp2_lag = lag(hp2, default = 1))
  df_seaadj_hp2$change <- (df_seaadj_hp2$hp2 - df_seaadj_hp2$hp2_lag)
  class(df_seaadj_hp2$change) <- "numeric"
  attr(df_seaadj_hp2$change, "tsp") <- NULL
  
  # standardized
  df_seaadj_hp2 <- df_seaadj_hp2 %>% slice(2:nrow(df_seaadj_hp2))
  df_seaadj_hp2$change_std <- ((df_seaadj_hp2$change - mean(df_seaadj_hp2$change)) / sd(df_seaadj_hp2$change))
  
  # since we found the difference between data points, the starting data point would be
  dates <- as.data.frame(seq(as.Date("1982-02-01"), by = "month", length.out = total_month))
  colnames(dates) <- "dates"
  df_seaadj_hp2 <- cbind(dates, df_seaadj_hp2)
  df_seaadj_hp2 <- df_seaadj_hp2 %>% select(dates, change_std)
  indicator_processing_result$processed_index <- df_seaadj_hp2
  df_seaadj_index <- 
    ggplot(df_seaadj_hp2, aes(x= dates, y= change_std)) +
    geom_line(color = "black", size = 0.8) +
    geom_hline(yintercept = 0) + 
    cycle5_rect + cycle6_rect + cycle7_rect + cycle8_rect + cycle9_rect +
    cycle10_rect + cycle11_rect + cycle12_rect + cycle13_rect + cycle14_rect +
    xlab("Dates") +
    ylab("Index") +
    ggtitle(indicator_type) +
    theme_bw()+
    theme(plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5)) 
  indicator_processing_result$processed_index_plot <- df_seaadj_index
  return(indicator_processing_result)
}

ind_index <- indicator_processing(ind, start_date, end_date, "Industrial Production Index")
non_agr_index <- indicator_processing(non_agr, start_date, end_date, "Non-Farm Payroll")
export_index <- indicator_processing(export, start_date, end_date, "Export Index")
manu_index <- indicator_processing(manu, start_date, end_date, "Manufacturing Sales")
overtime_index <- indicator_processing(overtime, start_date, end_date, "Overtime Average")

# predict after 2016-02
# 全經濟指標
all_indicator <- cbind(ind_index$processed_index,
                       non_agr_index$processed_index$change_std,
                       export_index$processed_index$change_std,
                       manu_index$processed_index$change_std,
                       overtime_index$processed_index$change_std,
                       business_cycle$cycle)
colnames(all_indicator) <- c("date",
                             "ind_change_std",
                             "non_agr_change_std",
                             "export_change_std",
                             "manu_change_std",
                             "overtime_change_std", "cycle_indicator")
all_indicator$cycle_indicator <- as.factor(all_indicator$cycle_indicator)

# modeling
business_cycle_predict <- function(start_date, end_date){
  # specify the range:  IRB Predict
  train_indicator <- all_indicator %>% filter(date <= start_date)
  test_indicator <- all_indicator %>%
    filter(date > start_date)  %>%
    filter(date < end_date)
  
  test_iter <- tibble(date = as.Date(character()),
                      ind_change_std = numeric(),
                      non_agr_change_std = numeric(),
                      export_change_std = numeric(),
                      manu_change_std = numeric(),
                      overtime_change_std = numeric(),
                      cycle_indicator = factor(),
                      predict = factor())
  
  # iterate 100 times and if a month is determined as contraction (predict: 1) >70%
  # of the time then the month would be contraction
  iter_n <- 1000
  for (i in seq(1, iter_n)){
    # codebook 擴張期、收縮期編碼向量個數皆為收縮期個數
    N <- nrow(train_indicator[train_indicator$cycle_indicator == 0, ])
    set.seed(i)
    train_indicator_con <- train_indicator %>% filter(cycle_indicator == 0)
    train_indicator_exp <- train_indicator %>% filter(cycle_indicator == 1) %>% sample_n(.,N)
    train_indicator_df <- rbind(train_indicator_exp, train_indicator_con)
    codeBook = lvqinit(train_indicator_df[,c(2:6)], train_indicator_df$cycle_indicator, size = N*2)
    
    indicator_lvq <- olvq1(train_indicator[,c(2:6)], train_indicator$cycle_indicator,
                           codeBook, niter = 40 * N,
                           alpha = 0.3)
    predict = lvqtest(indicator_lvq, test_indicator[,c(2:6)])
    test_indicator_predict <- cbind(test_indicator, predict)
    test_iter <- rbind(test_iter, test_indicator_predict)
  }
  temp <- test_iter %>%
    group_by(date) %>%
    summarise(perc = sum(predict == 0, na.rm=T)/iter_n)
  predict <- left_join(test_indicator, temp, by = c("date" = "date"))
  predict$predict_indicator <- ifelse(predict$perc > 0.7, 0, 1)
  return(predict)
}
cycle15_predict <- business_cycle_predict("2016-08-01", "2021-01-01")

# previous cycles 檢驗
# 以1998-12-01後做測試
# 第10個循環：高峰
# 與國發會認定相差：0個月，與文獻相差+1個月
# 國發會認定：2000-09，LQV認定為2000-09
peak_10_predict <- business_cycle_predict("2000-06-01", "2001-09-01")
# 第10個循環：谷底
# 與國發會認定相差：+1個月，與文獻相差0個月
# 國發會認定：2001-09，LQV認定為2001-08
trough_10_predict <- business_cycle_predict("2001-06-01", "2004-03-01")
# 第11個循環：高峰
# 與國發會認定相差：-5個月，與文獻相差-2個月
# 國發會認定：2004-03，LQV認定為2004-08
peak_11_predict <- business_cycle_predict("2002-03-01", "2005-02-01")
# 第11個循環：谷底
# 與國發會認定相差：-2個月，與文獻相差-1個月
# 國發會認定：2005-02，LQV認定為2005-04
trough_11_predict <- business_cycle_predict("2004-09-01", "2008-03-01")
# 第12個循環：高峰
# 與國發會認定相差：-1個月，與文獻相差+2個月
# 國發會認定：2008-03，LQV認定為2008-02
peak_12_predict <- business_cycle_predict("2005-08-01", "2009-02-01")
# 第12個循環：谷底
# 與國發會認定相差：-1個月，與文獻相差0個月
# 國發會認定：2009-02，LQV認定為2009-03
trough_12_predict <- business_cycle_predict("2008-09-01", "2011-02-01")
# 第13個循環：高峰
# 與國發會認定相差：-1個月，與文獻相差-1個月
# 國發會認定：2011-02，LQV認定為2011-03
peak_13_predict <- business_cycle_predict("2009-09-01", "2012-01-01")
# 第13個循環：谷底
# 與國發會認定相差：+2個月，與文獻相差+2個月
# 國發會認定：2012-01，LQV認定為2011-11
trough_13_predict <- business_cycle_predict("2011-08-01", "2014-10-01")
# 第14個循環：高峰
# 與國發會認定相差：-3個月，與文獻相差0個月
# 國發會認定：2014-10，LQV認定為2015-01
peak_14_predict <- business_cycle_predict("2012-07-01", "2016-02-01")
# 第14個循環：谷底
# 與國發會認定相差：+2個月，與文獻相差0個月
# 國發會認定：2016-02，LQV認定為2015-12
trough_14_predict <- business_cycle_predict("2015-04-01", "2020-01-01")


# save RData
save.image(file = "business_cycle.RData")

ind_index$index_plot
ind_index$seaadj_plot
ind_index$processed_index_plot


