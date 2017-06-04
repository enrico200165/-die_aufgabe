


library(data.table)

library(ggplot2)
library(forecast)
library(car)
library(MASS)

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

# reduce risk of typos
france <- "France";
germany <-"Germany";
austria <-"Austria"; 
mcountry="mcountry"; # analyze all data, all the countries


# --- create and init results container, awkward but saves subsetting ---
results_row <- list(
   country = character()
  ,sales_tot = -1
  ,top_prd_grp_names = character(), top_prd_grp_sales = numeric(), top_prd_grp_pct = numeric()
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




# ------------------------------------------------------------------
#' Data Load and Preprocessing
#' @return data frame with sales data joined to articles master data
#' @author Enrico
#' @export
#' @details
#' Reads the data files, joins the sales and articles data, 
#' performs some simple preprocessing
# ------------------------------------------------------------------
load_preprocess_alldata <- function() {

  # read articles data
  articles_df <- read.csv(articles_fname, sep = field_sep)
  articles_df$article <- as.character(articles_df$article) # should not be factors
  
  # read sales data. The if/"lazy read" worked when this code was not in a function,
  # it does NOT perform lazy load now inside a function (quickly) 
  # for the time being keep it just in case
  if (!exists(deparse(substitute(sales_df))) || is.null(sales_df) 
      || !is.data.frame(sales_df) || nrow(sales_df) <= 0) {
    print(paste("reading:",sales_fname))
    sales_df <- read.csv(sales_fname, sep = field_sep)
    # article IDs should not be factors
    sales_df$article <- as.character(sales_df$article)
  } else {
    print(paste("data already loaded, NOT reading:",sales_fname))
  }
  
  
  # -------------- quick peek at the data -----------------------------
  # code below no longer used, keep it just in case
  # str(sales_df); summary(sales_df)
  # str(articles_df); summary(articles_df)
  
  # check if other countries present besides the 3 requested
  # cat("countries present in data:",as.character(unique(sales_df$country)),"\n")
  # check NAs
  # cat("sales: non complete cases present: ", any(!complete.cases(sales_df)),"\n")
  # cat("articles: non complete cases present: ", any(!complete.cases(articles_df)),"\n")
  
  
  # -------------------- PREPROCESSING --------------------------------
  
  # eventual capping of sales outliers
  if (cap) {
    qnt <- quantile(sales_df$sales, probs=c(.25, .75), na.rm = T)
    H <- 1.5 * IQR(sales_df$sales, na.rm = T)
    outl <- which(sales_df$sales > (qnt[2] + H))
    if(any(outl)) sales_df[outl, ]$sales <- (qnt[2] + H-1)
  }
  
  # quick check duplicates for in master
  if (length(articles_df$article) != length(unique(articles_df$article))) {
    warning(paste("duplicate articles in",articles_fname))
  }
  
  # check for eventual  sales without articles in master 

  # join sales with article data to work more easily
  distinct_art_sold <- unique(sales_df$article)
  if (length(distinct_art_sold) != length(unique(sales_df$article))) {
    msg <- paste("some sales do not correspond to articles in master, nr such sales"
                 ,length(distinct_art_sold) - length(unique(art_sales_df$article))) 
    orphan_sales_articles <- setdiff(distinct_art_sold, unique(art_sales_df$article));
    warming("articles ID in sales data not found in master: ",orphan_sales_articles)
  }
  art_sales_df <- inner_join(sales_df,articles_df,by = "article")
  
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
  
  cat("\n-----------------------------------------------------------------------------")
  cat("\nanalyzing data for: ",toupper(country_name),", nr rows:",nrow(art_sales_dt))
  cat("\n-----------------------------------------------------------------------------\n")
      
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
    pctfield <- paste(fieldnameroot,"_pct",sep="");
    results[[country]][[namefield]]   <<- as.character(values[ ,1][[1]])
    results[[country]][[salesfield]] <<- as.vector(values[ ,2])
    results[[country]][[pctfield]] <<- round(as.vector(values[ ,2])/sales_country_tot*100,2)
  }

  sales_country_tot <- sum(art_sales_dt$sales)
  cat(country_name,"tot sales",sales_country_tot)
  # --- summarize
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
  # fit_promo_disc <- lm(sales ~ discount + promo_media + promo_store, data = art_sales_df)
  # print(vif(fit_promo_disc))
  # https://stats.stackexchange.com/questions/141060/multicollinearity-using-vif-and-condition-indeces
  
  # Check residuals
  # qqPlot(fit_promo_disc, main="t")
  # distribution of studentized residuals
  # sresid <- studres(fit_promo_disc) 
  # hist(sresid, freq=FALSE,main="Student Residuals")
  # xfit<-seq(min(sresid),max(sresid),length=40); yfit<-dnorm(xfit) 
  # lines(xfit, yfit)
  
  set_discpromos(country_name,"discount",summary(fit_promo_disc)$coeff[2, ])
  set_discpromos(country_name,"media",summary(fit_promo_disc)$coeff[3, ])
  set_discpromos(country_name,"store",summary(fit_promo_disc)$coeff[4, ])
  

  cat("DISCOUNT","\neffectiveness supported by data: ",(summary(fit_promo_disc)$coefficients[2,4] < alpha )
      ,"\n1% discount > delta sales:",summary(fit_promo_disc)$coefficient[2,1],"items"
      ,"\np value:", summary(fit_promo_disc)$coefficients[2,4])

  cat("\nMEDIA promos","\neffectiveness supported by data: "
    ,(summary(fit_promo_disc)$coefficients[3,4] < alpha )
    ,"\naverage sales increase:", round(summary(fit_promo_disc)$coefficients[3,1],2)
    ,"\np value: ", summary(fit_promo_disc)$coefficients[3,4])

  cat("\nSTORE promos"
      ,"\neffectiveness supported by data: ",(summary(fit_promo_disc)$coefficients[4,4] < alpha )
      ,"\naverage sales increase: ", round(summary(fit_promo_disc)$coefficients[4,1],2)
      ,"\np value: ", summary(fit_promo_disc)$coefficients[4,4]
  )

  # --------------------------------------------------------------------
  #                     PREDICT
  # --------------------------------------------------------------------
  
  # --------------------- STLF, by week --------------------------------

  cat("\n --- predicting for",country_name)
  
  # --- check if dates are missing
  data_weeks_avail = length(unique(art_sales_df$retailweek))
  days_diff = round(difftime(max(art_sales_df$retailweek),min(art_sales_df$retailweek), units = "days"))
  data_weeks_expected <- round(days_diff/7) + 1
  if (data_weeks_avail != data_weeks_expected) {
    warning(paste("\n",country_name," expected data weeks:",data_weeks_expected,"found",data_weeks_avail))
  } else {
    week_grp <- group_by(art_sales_df,retailweek)
    check_weeks <- summarise(week_grp, data_per_week_cnt = n())
    if (length(unique(check_weeks$data_per_week_cnt)) > 1) {
      warning(paste("some weeks have less data, data counts: "
                    ,unique(check_weeks$data_per_week_cnt)))
    }
  }

  week_sales <- summarise(group_by(art_sales_df, retailweek), sales = sum(sales))
  sales_avg_week <- mean(week_sales$sales)
  
  predict_weeks_nr <- 5
  if (country_name != france) {
    ts_sales_w <- ts(week_sales$sales, start=c(2014,52),frequency = 52) 
    stlf_w <- stlf(ts_sales_w, h=predict_weeks_nr);
    cat("\n",country_name," weekly forecasts (stlf), next",predict_weeks_nr,"weeks:",stlf_w$mean[c(1:5)]
        ,"\n(the weekly mean is: ",sales_avg_week,")")
    if (data_weeks_avail != data_weeks_expected) {
      cat("\nFORECAST above is UNRELIABLE due to ",data_weeks_expected-data_weeks_avail,"missing week")
    }
    # plot(stlf_w);Sys.sleep(10)
  } else {
    warning(country_name," not 2 periods available, not performing stlf prediction")
  }

  # --------------------- ETS prediction, using 4weeks ----------------

  # old adjustment used in old approach of 4w months+48weeks year, kept just in case
  # week_2remove_nrow <- c(1,seq(from = 4, to = 123, by = 13))
  # week_sales <- week_sales[-week_2remove_nrow, ] # now 48 weeks per year
  
  # --- aggregate per year and month, add these variables 
  week_sales$year <- year(week_sales$retailweek); 
  week_sales$month <- month(week_sales$retailweek)
  # --- calculate averages
  sales_month <- summarize(group_by(week_sales,year,month), sales = sum(sales))
  sales_avg_month <- mean(sales_month$sales)
  
  ts_sales_m <- ts(sales_month$sales, start=c(2015,1),frequency = 12) 
  # --- have a look
  # par(mfrow = c(2,3)); plot.ts(ts_sales_m)
  # ts_components <- decompose(ts_sales_m);plot(ts_components)
  
  fit_sales <- ets(ts_sales_m)
  fcast <- forecast(fit_sales, h = 1)

  cat("\n",country_name," next month forecasts (est)",fcast$mean[1]
      ,"\n(the month mean is: ",sales_avg_month)
  if (data_weeks_avail != data_weeks_expected) {
    cat("\nFORECAST above is UNRELIABLE due to ",data_weeks_expected-data_weeks_avail,"weeks data missing")
  }
  
  print(fcast$mean[1])
  par(mfrow=c(1,1));plot(fcast)
  

  #--------------------------------------------------------------------
  #               PRICE OPTIMIZATION
  #--------------------------------------------------------------------

  # consider sales without promotions
  art_sales_nopromo_dt <- art_sales_dt[promo_status == "none" , ]
  cat("\n",nrow(art_sales_nopromo_dt),"out of",nrow(art_sales_dt),"ie"
      ,nrow(art_sales_nopromo_dt)/nrow(art_sales_dt)*100,"%\n")
  
  # find articles most sold
  art_sales_nopromo_sum_dt <- art_sales_dt[ ,list(sales = sum(sales)), by=list(article)]
  print(art_sales_nopromo_sum_dt[1:5]$sales)
  art_sales_order_dt <- art_sales_nopromo_sum_dt[order(-rank(sales)),,]

  for (art in art_sales_nopromo_sum_dt[1:5,,]$article) {

    # subset all data for each top article into dedicated data table art_dt
    art_dt <- art_sales_nopromo_dt[article == art]
    # calculate total profit for the article from the data
    nrows_art <- nrow(art_dt) 

    # --- build profit equation to optimize
    # get sales linear eq coefficients
    b <- lm(sales ~ current_price, data = art_dt)$coefficients
    # duplicate other parameters from equation into more user friendly variables
    art_reg_price <- art_dt[1]$regular_price;
    art_current_price_avg <- mean(art_dt$current_price) # just for info
    sales_cost <- art_dt[1]$cost
    # finally assemble the profit equation
    art_profit <- function(price) {
      b[2]*price^2+price*(b[1]-b[2]*sales_cost) - sales_cost*b[1]
    }
    # optimize profit over price
    opt <- optimize(art_profit,lower = art_reg_price*0, upper = art_reg_price*2,maximum = TRUE)

    profit_from_data <- sum(art_dt$sales*art_dt$current_price -art_dt$cost)
    # profit with optimized values
    sales_opt <- b[1]+b[2]*opt$maximum
    profit_opt <- sales_opt*(opt$maximum - sales_cost)
    # calculating profit from past data we summed nrows_art data points
    profit_opt <- profit_opt * nrows_art
    
    print(paste("art:",art,"current price avg",art_current_price_avg,"optim price",opt$maximum
                ,"intercept",b[1],"slope",b[2],
      "profits[current",profit_from_data,"(theoretical) optim on past data"
                ,profit_opt,"] theoric profit improvement:", round((profit_opt/profit_from_data-1)*100,2),"%"))
  }
  
  cat("") # just for breakpoint
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

    tmp <- paste(cntry[["top_prd_grp_pct"]])
    cntry_str <- paste(cntry_str,tmp)
    
        
    print(cntry_str)
  }
}


#work_dir = dirname(parent.frame(2)$ofile)
#setwd(getSrcDirectory()[1])

art_sales_df_all <- load_preprocess_alldata()

analyze(mcountry,art_sales_df_all)
analyze(germany,art_sales_df_all[art_sales_df_all$country == germany , ])
analyze(france, art_sales_df_all[art_sales_df_all$country == france  , ])
analyze(austria,art_sales_df_all[art_sales_df_all$country == austria , ])

print_results()



