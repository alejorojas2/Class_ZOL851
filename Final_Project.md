# Final Project QMEE
Alejandro Rojas  


File import into R maintaining the first row as header for the columns.

```
##   State  Field Year Number_of_isolates Number_of_Species      Long
## 1  ARSO ARSO_1 2011                320                13 -91.41639
## 2  IASO IASO_1 2011                 99                15 -91.53795
## 3  IASO IASO_2 2011                 23                 9 -92.72658
## 4  IASO IASO_3 2011                 18                 8 -93.00555
## 5  IASO IASO_4 2011                 49                 9 -95.95764
## 6  IASO IASO_5 2011                 50                 9 -93.75066
##        Lat Sampling_date OTU  Chao chao_lci  chao_hci shannon shannon_lci
## 1 34.46175       7-15-11 107 981.2 482.5940 2141.7122  3.2835      3.0837
## 2 41.25190           N/A  37 163.0  78.5125  419.4388  2.8779      2.6162
## 3 41.74166           N/A  17  43.0  23.8160  116.1789  2.7284      2.4170
## 4 42.19056           N/A  13  28.0  16.5025   77.2401  2.4283      2.0548
## 5 41.64722           N/A  16  55.0  26.0886  166.7644  1.8868      1.5105
## 6 40.85603       5-31-11  12  19.0  13.3410   48.5405  1.7258      1.3960
##   shannon_hci Precipitation  Temp
## 1      3.4832        137.73 24.44
## 2      3.1395        114.00 18.87
## 3      3.0397        102.67 17.73
## 4      2.8018        101.22 17.21
## 5      2.2630        113.49 18.40
## 6      2.0555        121.10 18.20
```


```r
summary(oomy_data)
```

```
##      State        Field          Year      Number_of_isolates
##  IASO   : 9   ARSO_1 :  1   Min.   :2011   Min.   :  1.00    
##  MISO   : 9   ARSO2_1:  1   1st Qu.:2011   1st Qu.: 10.00    
##  KSSO   : 7   ARSO2_2:  1   Median :2011   Median : 22.00    
##  MISO2  : 7   ARSO2_3:  1   Mean   :2011   Mean   : 29.05    
##  ARSO2  : 6   ARSO2_4:  1   3rd Qu.:2012   3rd Qu.: 37.00    
##  ILSO   : 6   ARSO2_5:  1   Max.   :2012   Max.   :320.00    
##  (Other):69   (Other):107                                    
##  Number_of_Species      Long              Lat        Sampling_date
##  Min.   : 1.000    Min.   :-100.82   Min.   :33.80   N/A    :10   
##  1st Qu.: 5.000    1st Qu.: -96.63   1st Qu.:39.96   6-13-11: 8   
##  Median : 8.000    Median : -93.53   Median :41.74   6-20-11: 7   
##  Mean   : 7.841    Mean   : -92.26   Mean   :41.85   6-6-11 : 6   
##  3rd Qu.:10.000    3rd Qu.: -88.25   3rd Qu.:44.07   5-30-12: 4   
##  Max.   :18.000    Max.   : -84.01   Max.   :47.96   5-31-12: 4   
##                                                      (Other):74   
##       OTU              Chao           chao_lci          chao_hci      
##  Min.   :  1.00   Min.   :  1.00   Min.   :  0.000   Min.   :   0.00  
##  1st Qu.:  8.00   1st Qu.: 13.00   1st Qu.:  8.778   1st Qu.:  40.73  
##  Median : 13.00   Median : 29.00   Median : 17.477   Median :  78.16  
##  Mean   : 14.59   Mean   : 60.71   Mean   : 31.208   Mean   : 153.36  
##  3rd Qu.: 19.00   3rd Qu.: 60.00   3rd Qu.: 32.274   3rd Qu.: 163.18  
##  Max.   :107.00   Max.   :981.20   Max.   :482.594   Max.   :2141.71  
##                                                                       
##     shannon       shannon_lci     shannon_hci    Precipitation   
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   : 35.57  
##  1st Qu.:1.712   1st Qu.:1.248   1st Qu.:2.161   1st Qu.: 64.92  
##  Median :2.152   Median :1.767   Median :2.552   Median : 85.61  
##  Mean   :2.133   Mean   :1.741   Mean   :2.526   Mean   : 89.64  
##  3rd Qu.:2.633   3rd Qu.:2.283   3rd Qu.:2.993   3rd Qu.:115.72  
##  Max.   :3.583   Max.   :3.356   Max.   :3.811   Max.   :203.62  
##                                                                  
##       Temp      
##  Min.   :13.61  
##  1st Qu.:16.85  
##  Median :18.22  
##  Mean   :18.61  
##  3rd Qu.:20.37  
##  Max.   :24.97  
## 
```

```r
length(oomy_data$State)
```

```
## [1] 113
```

Transform year data into factor and data type is checked after transformation.

```r
oomy_data$Year <- factor(oomy_data$Year)
data.class(oomy_data$Year)
```

```
## [1] "factor"
```

Histogram representing the distribution of number of species found per year:
![](Final_Project_files/figure-html/unnamed-chunk-4-1.png) 

Boxplots for number of species in each state, compiling the 6 fields per state

```r
par(mfrow = c(1,1))
library(ggplot2)
ggplot(data = oomy_data, aes(y = Number_of_Species, x = State)) +
  geom_boxplot(aes(fill=Year)) + labs(y = "Number of species") + facet_grid(Year ~ .)
```

![](Final_Project_files/figure-html/unnamed-chunk-5-1.png) 
