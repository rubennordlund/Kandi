# Library
install.packages("readxl")
install.packages("dplyr")
install.packages("lmtest")
install.packages("ggplot2")
install.packages(c("readxl", "sandwich", "lmtest"))
library(readxl)
library(dplyr)
library(lmtest)
library("ggplot2")
library(sandwich)

# OLS-REGRESSION MED HUBER-WHITE HETEROSKEDASTICITETES-ROBUSTA STANDARDFEL
file_path <- "/Users/ruben/Reg.xlsx"
data <- read_excel(file_path, range = "D1:Q60")
data$TV <- log(data$TV)
model <- lm (CAR1 ~ TV_log + Finadv + Gfc + Kont + Int + Lik, data = data)
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
coeftest(model, vcov = vcovHC(model, type = "HC1"))
model <- lm (LNCAR1 ~ Rykt + Gfc + Ln(TV), data = data)
model <- lm (LNCAR1 ~ Rykt + Gfc + Ln(TV) Finadv + Int, data = data)
model <- lm (LNCAR1 ~ Rykt + Gfc + Ln(TV) + Finadv + Kont + Int, data = data)
summary(model)
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
coeftest(model, vcov = vcovHC(model, type = "HC1"))
model <- lm (WCAR1 ~ Rykt + Gfc + Ln(TV), data = data)
model <- lm (WCAR1 ~ Rykt + Gfc + Ln(TV) + Finadv + Int, data = data)
model <- lm (WCAR1 ~ Rykt + Gfc + Ln(TV) + Finadv + Kont + Int, data = data)
summary(model)
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
coeftest(model, vcov = vcovHC(model, type = "HC1"))

# F-värdets kritiska värde
>df1 <- 5 
>df2 <- 52 
>critical_value <- qf(0.99, df1, df2)
>print(critical_value)


# DESKRIPTIV STATISTIK
>selected_vars <- data[, c("TV", "Lik", "Int", "Kont", "Finadv", "Gfc", "Rykt")]
>summary(selected_vars)
>describe(selected_vars)

# MODELLDIAGNOSTIK
library(car)
> vif_values <- vif(model)
> print(vif_values)
library(lmtest)
> bp_test <- bptest(model)
> print(bp_test)
>dw_test <- dwtest(model)
print(dw_test)
library(moments)
> skew_value <- skewness(residuals(model))
> print(skew_value)
> kurt_value <- kurtosis(residuals(model)
> print(kurt_value)
library(tseries)
> jb_test <- jarque.bera.test(residuals(model))
> print(jb_test)

# GRAFER
plot <- ggplot(data, aes(x = Event, y = CAAR1)) +
  geom_line() +                           
  geom_point() +                          
  labs(title = "CAAR med marknadsmodellen",  
       x = "Dagar före uppköp",             
       y = "Kumulativ genomsnittlig onormal avkastning") + 
  theme_minimal() +                       
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = scales::percent)
print(plot)

                         
