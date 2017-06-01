

library(data.table)

library(ggplot2)
library(forecast)
library(caret)

# dates
library(lubridate)
library(xts)
library(rmeta)

library(TTR)


# --- check if still used

library(knitr)
library(dplyr)


# -------------------------------------------------------------------
#     DEFS
# -------------------------------------------------------------------

alpha <- 0.05
cap <- FALSE

articles_fname <- "article_master.txt"
sales_fname <- "sales.txt"
field_sep <- ";"

work_dir = dirname(parent.frame(2)$ofile)

# -------------------------------------------------------------------
#     DATA LOAD AND PREPROCESSING
# -------------------------------------------------------------------


setwd(work_dir)

# small file, dumb read
articles_df <- read.csv(articles_fname, sep = field_sep)
# article IDs should not be factors
articles_df$article <- as.character(articles_df$article)
str(articles_df)


if (!exists(deparse(substitute(sales_df))) || is.null(sales_df) || !is.data.frame(sales_df) || nrow(sales_df) <= 0) {
  print(paste("reading:",sales_fname))
  sales_df <- read.csv(sales_fname, sep = field_sep)
  # article IDs should not be factors
  sales_df$article <- as.character(sales_df$article)
  
} else {
  print(paste("data already loaded, NOT reading:",sales_fname))
}


# -------------- FIRST LOOK AT THE DATA -----------------------------
# quick peek at the data
str(sales_df)
summary(sales_df)
str(articles_df)
summary(articles_df)

# check if other countries present besides those requested
cat("countries present in data:",as.character(unique(sales_df$country)),"\n")

# check missing data
cat("sales: non complete cases present: ", any(!complete.cases(sales_df)),"\n")
cat("articles: non complete cases present: ", any(!complete.cases(articles_df)),"\n")

# -------------------- PREPROCESSING --------------------------------

# capping of outliers, in sales
if (cap) {
  qnt <- quantile(sales_df$sales, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(sales_df$sales, na.rm = T)
  outl <- which(sales_df$sales > (qnt[2] + H))
  if(any(outl)) sales_df[outl, ]$sales <- (qnt[2] + H-1)
}

# for now inner join, no check for eventual bad sales with article not in master 

distinct_art_sold <- unique(sales_df$article)
art_sales_df <- inner_join(sales_df,articles_df,by = "article")
if (length(distinct_art_sold) != length(unique(art_sales_df$article))) {
  msg <- paste("some sales do not correspond to articles in master, nr such sales"
               ,length(distinct_art_sold) - length(unique(art_sales_df$article))) 
  orphan_sales_articles <- setdiff(distinct_art_sold, unique(art_sales_df$article));
  
    cat("articles ID in sales data not found in master: "
        ,head(orphan_sales_articles)
        ,"...  just the first as a sample, check for others")
}

# -- ensure dates have date type ---
art_sales_df$retailweek <- as.POSIXct(strptime(as.character(art_sales_df$retailweek), "%Y-%m-%d"))
art_sales_df$retailweek <- art_sales_df$retailweek[order(art_sales_df$retailweek)]
# head(art_sales_df$retailweek);tail(art_sales_df$retailweek) # paranoid check

# --- check if dates are missing
weeks <- sort(unique(art_sales_df$retailweek))
# quick check
days_diff = round(difftime(weeks[length(weeks)], weeks[1], units = "days")) #
weeks_diff = as.numeric(days_diff/7)
if (length(weeks) != (weeks_diff+1)) {
  print("missing weeks")
} else {
  week_grp <- group_by(art_sales_df,retailweek)
  check_weeks <- summarise(week_grp, data_per_week_cnt = n())
  if (length(unique(check_weeks$data_per_week_cnt)) > 1) {
     warning(paste("some weeks have less data, data counts: "
                   ,unique(check_weeks$data_per_week_cnt)))
  } else {
    cat("quick&dirty check ok: no missing weeks, all weeks same # of data points:"
          ,unique(check_weeks$data_per_week_cnt))
  }
}

# -------------------- make var names more readable ------------------- 

colnames(art_sales_df)[which(colnames(art_sales_df) == "promo1")] <- "promo_media"
colnames(art_sales_df)[which(colnames(art_sales_df) == "promo2")] <- "promo_store"

# when program tested free memory here
#remove(sales_df); remove(articles_df)

# -------------- add variables --------------------------------------

# --- discount
art_sales_df$discount <- (1 - art_sales_df$ratio)
# wasteful, should remove it or remove $ratio
# let's keep both during initial development

# --- single global promo status 
art_sales_df$promo_status <- ifelse(art_sales_df$promo_media == 1
  ,ifelse(art_sales_df$promo_store == 1,"both","media")
  ,ifelse(art_sales_df$promo_store == 1,"store","none"))
art_sales_df$promo_status <- as.factor(art_sales_df$promo_status)

art_sales_df$promo_status <- relevel(art_sales_df$promo_status, ref="none")


# ------------------- EXPLORATION -----------------------------------

if (FALSE) {
  grp <- group_by(art_sales_df,retailweek)
  smrz <- summarize(grp, sales = sum(sales))
  qplot(y = sales, x= retailweek, data = smrz)
  qplot(y = log(sales), x= retailweek, data = smrz)
}
print("no seasonality seems present on plot of total sales per week")


# -------------------------------------------------------------------
#                 messages
# -------------------------------------------------------------------
print("- not analyzing per article, will probably use \"ratio\" as a pseudoprice for all articles")

# print("- not using time series")
# print("  for simplicity assuming sales do not depend on time, strong assumption that")
# print("  might be not unrealistic considering that")
# print("  stagionality analysis are not performed for lack of my time and real world background info")
# print("  the period is not extensive and that in such period the \"context\" should not change ")
# print("  (for economy of France this might be more uncertain")


# -------------------------------------------------------------------
#                 WHAT DRIVES SALES
# taken as: which country, product group, category, article sell the most
# -------------------------------------------------------------------

# --- simply check averages

art_sales_dt <-  data.table(art_sales_df)

sales_country <- art_sales_dt[ , list(sales = sum(sales)), by=list(country)]
sales_country <- sales_country[order(-rank(sales))]

sales_prodgroup <- art_sales_dt[ , list(sales = sum(sales)), by=list(productgroup)]
sales_prodgroup <- sales_prodgroup[order(-rank(sales))]

sales_prodgrpcat <- art_sales_dt[ , list(sales = sum(sales)), by=list(productgroup,category)]
sales_prodgrpcat <- sales_prodgrpcat[order(-rank(sales))]

sales_prodcat <- art_sales_dt[ , list(sales = sum(sales)), by=list(category)]
sales_prodcat <- sales_prodcat[order(-rank(sales))]

sales_article <- art_sales_dt[ , list(sales = sum(sales)), by=list(article)]
sales_article <- sales_article[order(-rank(sales))]

# head(sales_article)

# -------------------------------------------------------------------
#                 EFFECT OF DISCOUNTS
# -------------------------------------------------------------------

art_sales_nopromo_df <- art_sales_df[ art_sales_df$promo_status == "none"  ,  ]
discount_fit <- lm(sales ~ I(discount*100), data = art_sales_nopromo_df)
# visual check if assumptions old
# par(mfrow = c(2,2)) ;plot(discount_fit)

cat("simplest possible approach, consider only price, analyze data without promotions")
cat("effect of discount on sales supported by data: "
    ,summary(discount_fit)$coefficient[2,4] < alpha
    ,  "\n1% increases in discount modifies average sales of: "
    ,coef(discount_fit)[2],"items")

# for usage further down
discount_factor <- coef(discount_fit)[2]


# -------------------------------------------------------------------
#                 EFFECT OF PROMOTIONS
# -------------------------------------------------------------------

print("promotion effectiveness analysis, simplest possible approach")
print("all other factors ignored, an adjustment for discount is implemented")

# add var for sales with effect of discount removed
art_sales_df$sales_disc_adjust <- art_sales_df$sales-(art_sales_df$discount*discount_factor)
# paranoid check
rbind(head(art_sales_df$sales_disc_adjust),head(art_sales_df$sales),head(art_sales_df$discount))

promo_fit <- lm(sales_disc_adjust ~ promo_status, data = art_sales_df)
# par(mfrow = c(2,2)) ;plot(promo_fit)

print(paste("mean sales without promotions",coef(promo_fit)[1]))

print(paste("media promos effective? support by data: "
            ,(summary(promo_fit)$coefficients[3,4] < alpha )
            ,"p value: ", summary(promo_fit)$coefficients[3,4],
            "average sales increase: ", round(summary(promo_fit)$coefficients[3,1],2)))

print(paste("store+media promos effective? support by data: "
            ,(summary(promo_fit)$coefficients[2,4] < alpha )
            ,"p value: ", summary(promo_fit)$coefficients[2,4],
            "average sales increase: ", round(summary(promo_fit)$coefficients[2,1],2)))

print(paste("store promos effective? support by data: "
            ,(summary(promo_fit)$coefficients[4,4] < alpha )
            ,"p value: ", summary(promo_fit)$coefficients[4,4]))

# will be used to remove promo effects from sales to produce data for forecast
# based only on time series and sales (adjusted removing discount and promos effect)
lift_promo_store <- 1 # data do not support lift p > alpha
lift_promo_media <- (coef(promo_fit)[1]+summary(promo_fit)$coefficients[3,1])/coef(promo_fit)[1]
lift_promo_both <- (coef(promo_fit)[1]+summary(promo_fit)$coefficients[2,1])/coef(promo_fit)[1]


# --------------------------------------------------------------------
#                     PREDICT
# --------------------------------------------------------------------

# from sales with discount effect already removed, remove promo effect
adjust_by_promo <- function(sales, promo) { 
  if (promo == "none") return(sales);
  if (promo == "media") return(sales/lift_promo_media);
  if (promo == "both")  return(sales/lift_promo_both);
  if (promo == "store")  return(sales/lift_promo_store);
  stop(paste("should never get here, promo =",promo))
}

# remove effect of promos on sales (effect of discount removed previously)
art_sales_df$sales_adjust_fully <- mapply(adjust_by_promo,art_sales_df$sales_disc_adjust,art_sales_df$promo_status)

week_grp <- group_by(art_sales_df, retailweek)
week_sales <- summarise(week_grp, sales = sum(sales_adjust_fully))


# the quick and reasonably good tool I will use for lack of time is ets(),
# but if I do
# ... old code
# fit_sales <- ets(... )
# I get:
# I can't handle data with frequency greater than 24. Seasonality will be ignored. 

# --- quick and dirty non-general fix
# remove 4 weeks from each year, in a hard-coded way that only works with these data
# aggregate to time units of 4 weeks
# for lack of time will not impute  week 52 of 2014, though it would be important
# give the spikes of sales at end of year shown in the graph

# remove week of 2014, at row 1, then the 3rd week in each of the 4 13weeks block
# that make up a year
week_sales$week_nr <- c(52,1:52,1:52,1:18)
week_2remove_nrow <- c(1,seq(from = 4, to = 123, by = 13))
week_sales <- week_sales[-week_2remove_nrow, ]


ts_sales <- ts(week_sales$sales, start=c(2014,52),frequency = 52) 
# --- have a look
# par(mfrow = c(2,3))
# plot.ts(ts_sales)
# mess around a bit
# ts_components <- decompose(ts_sales);plot(ts_components)
# ts_month <- SMA(ts_sales,n=52/12);plot.ts(ts_month)
# ts_bimonth <- SMA(ts,n=52/12*2);plot.ts(ts_bimonth)
# ts_q <- SMA(ts,n=52/3);plot.ts(ts_q)
# ts_half <- SMA(ts,n=52/2);plot.ts(ts_half)


# trying to use weeks in 2015 and 2016
# ts_sales_w <- window(ts_sales,start=c(2015,1), end = c(2016,52), frequency = 52)
# the abo
# I can't handle data with frequency greater than 24. Seasonality will be ignored. 
fit_sales <- ets(ts_sales)



# -------------------------------------------------------------------
#  code for updates
# -------------------------------------------------------------------

# if(!require(installr)) {
#   install.packages("installr"); require(installr)
# } #load / install+load installr
# updateR() 
# source("http://bioconductor.org/biocLite.R")
# bioclite()




# -------------------------------------------------------------------
#  junk to remove
# -------------------------------------------------------------------






# if (!exists("ny") || is.null(ny) || length(ny) <= 0) {
#   print("subsetting NY state")
#   ny <- filter(data, Provider.State == 'NY')
# }
# 
# 
# colsToCheck <- c(which(names(ny) == "Average.Covered.Charges"), 
#                  which(names(ny) == "Average.Total.Payments"))
# cc <-  complete.cases( ny[  , c(10,11)])
# nyCompl <- ny[cc, ]
# 
# 
# with(nyCompl,plot(Average.Covered.Charges, Average.Total.Payments))
# linea <- lm( Average.Total.Payments ~ Average.Covered.Charges, data = ny)
# abline(linea)
# plot1<- "plot1.pdf"
# unlink(plot1, force = TRUE)
# dev.copy2pdf(file=plot1)
# # dev.off()
# 
# 
# stati <- as.character(unique(data$Provider.State))
# medCond <- as.character(unique(data$DRG.Definition))
# nrCol <- length(stati)/2
# if ((nrCol - floor(nrCol)) != 0 ) {
#   nrCol <- floor(nrCol)+1
# }
# par( mfrow = c(2,nrCol), oma = c(2,2,4,1))
# for (i in seq_along(stati)) {
#   print(stati[i])
#   dataSub <- data[data$Provider.State == stati[i] , ]
#   # head(dataSub)
#   with(dataSub,plot(Average.Covered.Charges, Average.Total.Payments, 
#                     col = DRG.Definition,
#                     main = stati[i],
#                     xlab = "Avg. Cov. Charges", ylab = "Avg Paym"))
#   linea <- lm( Average.Total.Payments ~ Average.Covered.Charges, data = dataSub)
#   abline(linea)
# }
# mtext("Covered charges and total payments by state\n(and med. condition, by color)", outer = TRUE, cex = 1.5)
# plot2 <- "plot2.pdf"
# unlink(plot2, force = TRUE)
# dev.copy2pdf(file=plot2)
# 

