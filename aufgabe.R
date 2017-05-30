

library(ggplot2)
library(caret)
library(data.table)
# --- check if still used

library(knitr)
library(dplyr)


# -------------------------------------------------------------------
#     DEFS
# -------------------------------------------------------------------

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
str(sales_df)





# -------------------- join ----------------------------------------- 
# for now inner join, no check for eventual bad sales with article not in master 

distinct_art_sold <- unique(sales_df$article)
art_sales_df <- inner_join(sales_df,articles_df,by = "article")
if (length(distinct_art_sold) != length(unique(art_sales_df$article))) {
  msg <- paste("some sales do not correspond to articles in master, nr such sales"
               ,length(distinct_art_sold) - length(unique(art_sales_df$article))) 
  orphan_sales_articles <- setdiff(distinct_art_sold, unique(art_sales_df$article));
  
    cat("first articles ID in sales data not found in master:\n"
        ,head(orphan_sales_articles)
        ,"\nNB just the first ones as a sample, check for others")
}

# -- adjust dates ---
art_sales_df$retailweek <- strptime(as.character(art_sales_df$retailweek), "%Y-%m-%d")
art_sales_df$retailweek <- art_sales_df$retailweek[order(art_sales_df$retailweek)]
# head(art_sales_df$retailweek);tail(art_sales_df$retailweek) # paranoid check

# -------------------- change to more human names ------------------- 
colnames(art_sales_df)[which(colnames(art_sales_df) == "promo1")] <- "promo_media"
colnames(art_sales_df)[which(colnames(art_sales_df) == "promo2")] <- "promo_store"



# when program tested free memory here
#remove(sales_df); remove(articles_df)

# -------------- add variables --------------------------------------

art_sales_df$promo_status <- ifelse(art_sales_df$promo_media == 1
  ,ifelse(art_sales_df$promo_store == 1,"both","media")
  ,ifelse(art_sales_df$promo_store == 1,"store","none"))
art_sales_df$promo_status <- as.factor(art_sales_df$promo_status)
art_sales_df$promo_status <- relevel(art_sales_df$promo_status, ref="none")

art_sales_df$discount <- 999999999
art_sales_df$profit <- 999999999


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


# --- t test for each promo type against no promo

# media only, exclude if also store promo
art_sales_df$promo_media_only <- (art_sales_df$promo_status == "media")*1
promo_media_only_ttest <- t.test(sales ~ promo_media_only, data = art_sales_df)

# store only, exclude if also media promo
art_sales_df$promo_store_only <- (art_sales_df$promo_status == "store")*1
promo_store_only_ttest <- t.test(sales ~ promo_store_only, data = art_sales_df)

# overlapping promos, both media and store
art_sales_df$promo_both <- (art_sales_df$promo_status == "both")*1
promo_both_ttest <- t.test(sales ~ promo_both, data = art_sales_df)

# --- simply average by group
sales_promo <- art_sales_dt[ , list(sales_mean = mean(sales)), by=list(promo_status)]
sales_promo <- sales_promo[order(-rank(sales_mean))]


# --------------------------------------------------------------------
#                     PREDICT
# --------------------------------------------------------------------

fit <- lm(sales ~ promo_media + promo_store, data = art_sales_df)


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
