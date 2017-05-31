

library(ggplot2)
library(caret)
library(data.table)
# --- check if still used

library(knitr)
library(dplyr)


# -------------------------------------------------------------------
#     DEFS
# -------------------------------------------------------------------

alpha <- 0.05

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
art_sales_df$retailweek <- strptime(as.character(art_sales_df$retailweek), "%Y-%m-%d")
art_sales_df$retailweek <- art_sales_df$retailweek[order(art_sales_df$retailweek)]
# head(art_sales_df$retailweek);tail(art_sales_df$retailweek) # paranoid check

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

# currently not used, relevel just  just in case
art_sales_df$promo_status <- relevel(art_sales_df$promo_status, ref="none")

# --- redundant, at end of development evaluate removing them
art_sales_df$promo_media_only <- (art_sales_df$promo_status == "media")*1
art_sales_df$promo_store_only <- (art_sales_df$promo_status == "store")*1
art_sales_df$promo_both_only  <- (art_sales_df$promo_status == "both")*1
art_sales_df$promo_any_only   <- (art_sales_df$promo_status != "none")*1


# ------------------- EXPLORATION -----------------------------------

# description does not specify is media promotion are tied to a price discount
# informal check of correlation, for all promotype as we are at it
plot(art_sales_df$promo_status,art_sales_df$discount)
disc_promo_any_ttest   <- t.test(art_sales_df$discount ~ as.factor(art_sales_df$promo_any_only))
disc_promo_media_ttest <- t.test(art_sales_df$discount ~ as.factor(art_sales_df$promo_media_only))
disc_promo_store_ttest <- t.test(art_sales_df$discount ~ as.factor(art_sales_df$promo_store_only))
disc_promo_both_ttest  <- t.test(art_sales_df$discount ~ as.factor(art_sales_df$promo_both_only))


# -------------------------------------------------------------------
#                 messages
# -------------------------------------------------------------------
print("- not analyzing per article, will probably use \"ratio\" as a pseudoprice for all articles")

print("- not using time series")
print("  for simplicity assuming sales do not depend on time, strong assumption that")
print("  might be not unrealistic considering that")
print("  stagionality analysis are not performed for lack of my time and real world background info")
print("  the period is not extensive and that in such period the \"context\" should not change ")
print("  (for economy of France this might be more uncertain")


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
head(sales_article)


# -------------------------------------------------------------------
#                 EFFECT OF PROMOTIONS
# -------------------------------------------------------------------

# --- simplest approach possible

print("promotion effectiveness analysis, simplest possible approach, all other factors")
print("(ex. discount) ignored")

promo_fit <- lm(sales ~ promo_status, data = art_sales_df)

print(paste("mean sales without promotions",coef(promo_fit)[1,1]))

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




# --------------------------------------------------------------------
#                     PREDICT
# --------------------------------------------------------------------

pairs(art_sales_df[c(3,4)], pch = 18)

# fit <- lm(sales ~ promo_media_only + promo_store_only + promo_both_only + ratio, data = art_sales_df)

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
