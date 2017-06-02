

library(data.table)

library(ggplot2)
library(forecast)
library(caret)

# dates
library(lubridate)
library(xts)
# library(rmeta)

# library(TTR)


# --- check if still used

library(knitr)
library(dplyr)


# -------------------------------------------------------------------
#     DEFS & GLOBALS
# -------------------------------------------------------------------

alpha <- 0.05
cap <- FALSE

articles_fname <- "article_master.txt"
sales_fname <- "sales.txt"
field_sep <- ";"
nr_top_items <- 5 # nr of best performing items reported


france <- "France";germany <-"Germany";austria <-"Austria"; mcountry="mcountry"

# --- create and init results container, awkward but saves subsetting ---
results_row <- list(
   country = character()
  ,sales_tot = -1
  ,top_prd_grp_names = character(), top_prd_grp_sales = numeric()
  ,top_prd_grpcat_names = character(), top_prd_grpcat_sales = numeric()
  ,top_prd_art_names = character(), top_prd_art_sales = numeric()
  ,sales_avg_m = -1
  ,sales_avg_w = -1

  ,mediap_eff = FALSE,mediap_lift = -1,media_p = -1
  ,storep_eff = FALSE,storep_lift = -1,store_p = -1
  ,discount_eff = FALSE,discount_lift = -1, discount_p = -1
  
  )
results <- list(
  Germany = results_row
  ,France = results_row
  ,Austria = results_row
  ,mcountry = results_row)

results[[germany]][["country"]] <- germany
results[[france]][["country"]] <- france
results[[austria]][["country"]] <- austria
results[[mcountry]][["country"]] <- mcountry





# -------------------------------------------------------------------
#     DATA LOAD AND PREPROCESSING
# -------------------------------------------------------------------
load_preprocess_alldata <- function() {

  articles_df <- read.csv(articles_fname, sep = field_sep)
  
  articles_df$article <- as.character(articles_df$article) # should not be factors
  
  if (!exists(deparse(substitute(sales_df))) || is.null(sales_df) || !is.data.frame(sales_df) || nrow(sales_df) <= 0) {
    print(paste("reading:",sales_fname))
    sales_df <- read.csv(sales_fname, sep = field_sep)
    # article IDs should not be factors
    sales_df$article <- as.character(sales_df$article)
  } else {
    print(paste("data already loaded, NOT reading:",sales_fname))
  }
  
  # -------------- quick peek at the data -----------------------------
  # str(sales_df); summary(sales_df)
  # str(articles_df); summary(articles_df)
  
  # check if other countries present besides those requested
  cat("countries present in data:",as.character(unique(sales_df$country)),"\n")
  # check missing data
  cat("sales: non complete cases present: ", any(!complete.cases(sales_df)),"\n")
  cat("articles: non complete cases present: ", any(!complete.cases(articles_df)),"\n")
  
  # -------------------- PREPROCESSING --------------------------------
  
  # eventual capping of sales outliers
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
    cat("articles ID in sales data not found in master: ",orphan_sales_articles)
  }
  
  # -- ensure dates have date type ---
  art_sales_df$retailweek <- as.POSIXct(strptime(as.character(art_sales_df$retailweek), "%Y-%m-%d"))
  art_sales_df$retailweek <- art_sales_df$retailweek[order(art_sales_df$retailweek)]
  # head(art_sales_df$retailweek);tail(art_sales_df$retailweek) # paranoid check
  
  # --- check if dates are missing
  weeks <- sort(unique(art_sales_df$retailweek)) # try to rewrite with min and max
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
    }
  }
  
  # -------------------- make var names more readable ------------------- 
  colnames(art_sales_df)[which(colnames(art_sales_df) == "promo1")] <- "promo_media"
  colnames(art_sales_df)[which(colnames(art_sales_df) == "promo2")] <- "promo_store"
  
  # when program tested free memory here
  #remove(sales_df); remove(articles_df)
  
  # -------------- add variables --------------------------------------
  art_sales_df$discount <- (1 - art_sales_df$ratio)*100 # in percentage

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


  # calculate some global values
  art_sales_dt <-  data.table(art_sales_df)
  sales_country <- art_sales_dt[ , list(sales = sum(sales), sales_avg = mean(sales)) ,by=list(country)]
  
  apply(sales_country,1,function(cur_row) { 
    country <- cur_row[1]
    results[[country]]$sales_tot <<- as.numeric(cur_row[2]);
    })
  
  sales_tot_allcountries <-sum(art_sales_dt$sales)
  if (sales_tot_allcountries == (results[[germany]]$sales_tot + results[[france]]$sales_tot + results[[austria]]$sales_tot)) {
    results[[mcountry]]$sales_tot <<- sales_tot_allcountries;
  } else {
    stop("calculation problem, tot sales")
  }

  return(art_sales_df)
}



analyze <- function(country_name,art_sales_df) {

  art_sales_dt <- data.table(art_sales_df)
  if (country_name != mcountry) {
    art_sales_dt <- art_sales_dt[ country == country_name, ,] # redundant, but ...
    # print(paste())
  } 
  cat("\n",paste("analyzing data for: ",country_name,"nr rows:",nrow(art_sales_dt)))
  
  if (country_name != mcountry &
    nrow(art_sales_df[art_sales_df$country !=  country_name, ]) > 0) {
    stop(paste("internal error: found  data for country other than:",country_name))
  }
  
  # -------------------------------------------------------------------
  #                 WHAT DRIVES SALES
  # taken as: which country, product group, category, article sell the most
  # -------------------------------------------------------------------
  
  set_top_items <- function(country, fieldnameroot, values) {
    namefield <- paste(fieldnameroot,"_names",sep="");
    salesfield <- paste(fieldnameroot,"_sales",sep="");
    results[[country]][[namefield]]   <<- as.character(values[ ,1][[1]])
    results[[country]][[salesfield]] <<- as.vector(values[ ,2])
  }
  
  # --- simply check averages
  sales_prodgroup <- art_sales_dt[ ,list(sales = sum(sales)), by=list(productgroup)]
  sales_prodgroup <- head(sales_prodgroup[order(-rank(sales))],nr_top_items)
  set_top_items(country_name,"top_prd_grp",sales_prodgroup)
  
  sales_prodgrpcat <- art_sales_dt[ ,list(sales = sum(sales)), by=list(productgroup,category)]
  sales_prodgrpcat <- head(sales_prodgrpcat[order(-rank(sales))],nr_top_items)
  set_top_items(country_name,"top_prd_grpcat",sales_prodgrpcat)
  
  sales_article <- art_sales_dt[, list(sales = sum(sales)), by=list(article)]
  sales_article <- head(sales_article[order(-rank(sales))],nr_top_items)
  set_top_items(country_name,"top_prd_art",sales_article)
  
  # head(sales_article)
  
  # -------------------------------------------------------------------
  #                 EFFECT OF PROMOS AND DISCOUNTS
  # -------------------------------------------------------------------

  set_discpromos <- function(country, fieldnameroot, coeff_row) {
    field_effect <- paste(fieldnameroot,"_eff",sep="");
    field_lift <- paste(fieldnameroot,"_lift",sep="");
    field_p <- paste(fieldnameroot,"_p",sep="");
    results[[country]][[field_effect]]   <<-(coeff_row[4] < alpha)
    results[[country]][[field_lift]]     <<- coeff_row[1]
    results[[country]][[field_p]]        <<- coeff_row[4]
  }
    
  fit_promo_disc <- lm(sales ~ discount * promo_media * promo_store, data = art_sales_df)
  
  set_discpromos(country_name,"discount",summary(fit_promo_disc)$coeff[2, ])
  set_discpromos(country_name,"media",summary(fit_promo_disc)$coeff[3, ])
  set_discpromos(country_name,"store",summary(fit_promo_disc)$coeff[4, ])
  

  # print("")
  # print(paste("DISCOUNT     effectiveness supported by data: "
  #             ,(summary(fit_promo_disc)$coefficients[2,4] < alpha )
  #             ,"p value:", summary(fit_promo_disc)$coefficients[2,4]
  #     , "1% discount > delta sales:"
  #     ,summary(fit_promo_disc)$coefficient[2,1],"items"))
  # 
  # print(paste("MEDIA promos effectiveness supported by data: "
  #             ,(summary(fit_promo_disc)$coefficients[3,4] < alpha )
  #             ,"p value: ", summary(fit_promo_disc)$coefficients[3,4],
  #             "average sales increase: ", round(summary(fit_promo_disc)$coefficients[3,1],2)))
  # 
  # print(paste("store promos effectiveness supported by data: "
  #             ,(summary(fit_promo_disc)$coefficients[4,4] < alpha )
  #             ,"p value: ", summary(fit_promo_disc)$coefficients[4,4],
  #             "average sales increase: ", round(summary(fit_promo_disc)$coefficients[4,1],2)))
  # 
  # print(paste("store promos effectiveness supported by data: "
  #             ,(summary(fit_promo_disc)$coefficients[4,4] < alpha )
  #             ,"p value: ", summary(fit_promo_disc)$coefficients[4,4]))

    

  # --------------------------------------------------------------------
  #                     PREDICT
  # --------------------------------------------------------------------
  
  # --------------------- STLF, by week --------------------------------
  

  # --- check if dates are missing
  data_weeks_avail = length(unique(art_sales_df$retailweek))
  days_diff = round(difftime(max(art_sales_df$retailweek),min(art_sales_df$retailweek), units = "days"))
  data_weeks_expected <- round(days_diff/7) + 1
  if (data_weeks_avail != data_weeks_expected) {
    warning(cat("expected data weeks:",data_weeks_expected,"found",data_weeks_avail))
    # return(0)
  } else {
    week_grp <- group_by(art_sales_df,retailweek)
    check_weeks <- summarise(week_grp, data_per_week_cnt = n())
    if (length(unique(check_weeks$data_per_week_cnt)) > 1) {
      warning(paste("some weeks have less data, data counts: "
                    ,unique(check_weeks$data_per_week_cnt)))
    }
  }
  
  
  
  week_grp <- group_by(art_sales_df, retailweek)
  week_sales <- summarise(week_grp, sales = sum(sales))
  sales_avg_week <- mean(week_sales$sales)
  
  print(paste("predicing for",country_name))
  ts_sales_w <- ts(week_sales$sales, start=c(2014,52),frequency = 52) 
  stlf_w <- stlf(ts_sales_w, h=5);
  cat("\n",country_name,"weekly forecasts (stlf)")
  print(stlf_w$mean[c(1:5)])
  # plot(stlf_w);Sys.sleep(10)
  
  return(0)
  
  # --------------------- ETS -----------------------------------------
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
  week_sales$week_nr <- c(52,1:52,1:52,1:18) # just to debug
  week_2remove_nrow <- c(1,seq(from = 4, to = 123, by = 13))
  week_sales <- week_sales[-week_2remove_nrow, ] # now 48 weeks per year
  
  # --- aggregate per year and month 
  week_sales$year <- year(week_sales$retailweek)
  week_sales$month <- month(week_sales$retailweek)
  
  sales_month_grp <- group_by(week_sales,year,month)
  sales_month <- summarize(sales_month_grp, sales = sum(sales))
  sales_avg_month <- mean(sales_month$sales)
  
  ts_sales_m <- ts(sales_month$sales, start=c(2015,1),frequency = 12) 
  plot.ts(ts_sales_m)
  # --- have a look
  # par(mfrow = c(2,3))
  # plot.ts(ts_sales_m)
  # mess around a bit
  # ts_components <- decompose(ts_sales_m);plot(ts_components)
  
  fit_sales <- ets(ts_sales_m)
  fcast <- forecast(fit_sales, h = 3)
  print("monthly forecasts (ets)")
  print(fcast$mean[1])
  par(mfrow=c(1,1));plot(fcast)
  
}

print_results <- function() {
  
  for (cntry in results) {
    cat(paste("\n--- country:",cntry[["country"]] , " ---\n"))
    cntry_str = ""
    cntry_str <- paste(cntry_str,"top groups:")
    tmp <- paste(cntry[["top_prd_grp_names"]], collapse = ' ')
    cntry_str <- paste(cntry_str,tmp)

    tmp <- paste(cntry[["top_prd_grp_sales"]])
    cntry_str <- paste(cntry_str,tmp)
    
    print(cntry_str)
  }
}


#work_dir = dirname(parent.frame(2)$ofile)
#setwd(getSrcDirectory()[1])

art_sales_df_all <- load_preprocess_alldata()

analyze(mcountry,art_sales_df_all)
analyze(germany,art_sales_df_all[art_sales_df_all$country == germany , ])
# analyze(france, art_sales_df_all[art_sales_df_all$country == france  , ])
analyze(austria,art_sales_df_all[art_sales_df_all$country == austria , ])

print_results()



