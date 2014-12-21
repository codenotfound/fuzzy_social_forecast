# Модель нечеткого прогнозирования распространения наркомании
(Fuzzy rule-based system based on space partition. 
Wang and Mendel’s technique (WM), 1992.)



```r
source("helper_functions.R")
filenames <- list("Численность безработных всего.xlsx", 
                  "Преступления связанные с незаконным оборотом наркотиков  зарегистрировано.xlsx", 
                  "Состоит на учете больных с диагнозом «наркомания», на 100 тыс. населения.xlsx")
factors <- lapply(filenames, load_xlsx)
factors <- lapply(factors, clean_xlsx_df)
filenames <- lapply(filenames, str_replace_all, "[^[:alnum:]]", ".")
names(factors) <- sub("\\.[[:alnum:]]+$", "", filenames)
zdf <- join_factors(factors)
```


```r
predictand = "Состоит.на.учете.больных.с.диагнозом..наркомания...на.100.тыс..населения"
model_data <- fuzzy_forecast(zdf, predictand)
```

```r
plot_model_data(model_data)
```

![plot of chunk cmp](figure/cmp.png) 

Сравнение модели WM (predicted) с эмпирическими данными (real) с горизонтом прогнозирования = 3

```r
## Error calculation
errors <- err_calc(model_data)
pander(errors$bench, style="rmarkdown", 
       caption="Значения модели и эмпирические значения")
```



|  predicted  |  real  |
|:-----------:|:------:|
|    178.3    | 221.8  |
|    183.9    | 220.2  |
|    178.1    | 187.4  |

Table: Значения модели и эмпирические значения

```r
pander(errors$err, style='rmarkdown', caption="Оценки ошибок прогноза")
```



|  MSE  |  RMSE  |  SMAPE  |
|:-----:|:------:|:-------:|
| 1099  | 33.14  |  3.733  |

Table: Оценки ошибок прогноза
