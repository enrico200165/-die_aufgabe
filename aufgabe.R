#'
#' ANALYSIS OF SALES DATA
#' 
#' @author Enrico
#' 
#' @details
#' Analysis of sales data
#' PROTOTYPE code to demonstrate feasibility
#' 
#' The code has been structured to be able to perform analysis
#' both by single country and over all the countries together, 
#' by isolating the analysis in a functions that takes the data 
#' set as a parameter.
#' 
#' If this code is evolved to be run regularly to produce reports the 
#' best approach would be to produce and .rmd document with code 
#' embedded ( http://rmarkdown.rstudio.com/ ) and use slidify 
#' (http://slidify.org/samples/intro/#1  ) if slides are needed
#' In view of such an evolution an experimentation has been started 
#' already in this version putting the results of the analysis in 
#' data frames to allow easy production of plots in ggplot2,
#' NB the dataframes currently built (with an advanced "trick" to 
#' need to have their metadata cleaned, to be done in next iterations)


# suppressWarnings(suppressMessages(library(data.table)))

# --- structural ---
library(dtplyr)
library(dplyr)
library(data.table)

# -- stats etc ---
library(forecast)
library(car)
library(MASS)

# -- helpers ---
library(lubridate)
# library(xts)

# -- output&reprots ---
library(grid)
library(gridExtra)
library(ggplot2)
# library(knitr)


options(warn=1)

# -------------------------------------------------------------------
#     DEFS & GLOBALS
# -------------------------------------------------------------------

alpha <- 0.05
cap <- FALSE # whether to cap outliers or not

test_run   <- FALSE # whether to run eventual test/debug functions
show_plots <- FALSE # mostly not to show some plots that take time

# data files
articles_fname <- "article_master.txt"
sales_fname    <- "sales.txt"
field_sep <- ";"


nr_top_items <- 5 # nr of best performing items reported


# -- Regions/Countries
france   <- "France";
germany  <-"Germany";
austria  <-"Austria"; 
mcountry <-"mcountry"; # pseudo-country, analyze all data/countries together

# --- Discount types
discount <- "discount"
media    <- "media"
store    <- "store"


# ------------------------------------------------------------------
# --- Data Frames to contain results and helper functions -----------
# ------------------------------------------------------------------
# data frames meant, in next releases, to 
# - easily produce graphs and plots with ggplot2
# - be displayed in .rmd/slidify documents


# general service function for data frames
# ------------------------------------------------------------------
#' Append new row to any data-frame and
#' if passed (by col. name or col. number) set some values in it
#' @param df dataframe
#' 
#' @param fnames optional
#' vector of columun/var names. 
#' positionally related to fvalues
#' @param fvalues optional, necessary for fnames
#' list of values to set, position i contains value for comun with name fnames[i]
#' 
#' @param col_idxes optional, 
#' vector of idexes of clomuns to fill with values from col_values
#' @col_values optional but necessary for col_values
#' list of values to set, ith value to column with index contained 
#' in col_idxes[i]
#' 
#' @return data frame with new row appended
#' 
#' @details
#' rather dirty but better than nothing and works ok for small datframes
#' manually filled with analysis results
# ------------------------------------------------------------------
add_df_row <- function(df,fnames,fvalues,col_idxes,col_values) {
  
  # create dummy row matrix
  newrow <- data.frame(matrix(c(rep.int(NA,length(df))),nrow=1,ncol=length(df)))
  colnames(newrow) <- colnames(df)
  
  # set vectors of values passed by column names (names in fnames, values in fvalues)
  if (!missing(fnames)) {
    for (i in 1:length(fnames)) {
      newrow[[fnames[i]]] <- fvalues[i]
    }
  }
  
  # set vectors of values passed by column index (idx in col_idxes, values in col_values)
  if (!missing(col_idxes)) {
    for (i in 1:length(col_idxes)) {
      idx <- col_idxes[i]
      newrow[[idx]] <- col_values[[i]]
    }
  }
  df <- rbind(df,newrow,row.names = NULL)
  df
}



# --- Effect of discounts and promotions
# --- results of analysis
promo_effect_df <- data.frame(
  country = character()
  , promo_name = character()
  , deltax_descr = character()
  , stat_signif = logical()
  , pvalue  = double()
  , ydelta = double()
)
# helper for data frame res_promo_add
# easily add a new row of data 
res_promo_add <- function( country, promo_name, deltax_descr
                       , summary_fit_row) {

    stat_signif <- summary_fit_row[4] < alpha
    pvalue <-      summary_fit_row[4]
    ydelta <- round(summary_fit_row[1],2)
    fnames <- c("country", "promo_name","stat_signif","pvalue","ydelta","deltax_descr")
    fvalues <- list(country, promo_name, stat_signif,  pvalue,  ydelta, deltax_descr)
    promo_effect_df <<- add_df_row(promo_effect_df,fnames,fvalues)
}



# --- Top selling (article) groups ---
# --- results of analysis
res_topgroups_df <- data.frame(
  country = character()
  , name = character()
  , sales  = double()
  , pct_of_tot_sales = double()
)
# helper for data frame top_groups_df
# easily add a new row of data 
res_top_group_add <- function( country,values,totcountrysales) {
  
  v <- as.data.frame(values) # data table was not comfortable with 
  # subsetting&levels together
  levelschar <- as.character(levels(v[,1]))
  levelschar[which(levelschar == "HARDWARE ACCESSORIES")] <- "HW ACCS."

  fnames <- c("country", "name" , "sales", "pct_of_tot_sales")
  for (i in 1:nrow(values)) {
    levels_val <- which(levels(v[,1]) == v[i,1])
    fvalues <-list(country, levelschar[levels_val], values[i,2]
                   ,round(values[i,2]/totcountrysales * 100,2))
    res_topgroups_df <<- add_df_row(res_topgroups_df, fnames, fvalues)
  }
}



# --- Price Optimization  ---
# --- results of analysis
res_priceopt_df <- data.frame(
  country = character()
  , article = character()
  , price_recomm  = double()
  , price_optim  = double()
  , profit_cur = double()
  , profit_optim = double()
  , profit_delta_pct = double()
)
# helper for data frame top_groups_df
# easily add a new row of data 
res_priceopt_add <- function(country, article, price_recomm
                             ,price_optim,profit_cur,profit_optim) {
  
  fnames   <- c("country","article","price_recomm","price_optim"
                ,"profit_cur","profit_optim","profit_delta_pct")
  fvalues <-list(country,  article,  price_recomm
                 ,round(price_optim,2),round(profit_cur,0) 
                 , round(profit_optim,0)
                 ,round(profit_optim/profit_cur*100-100,2))
  
  res_priceopt_df <<- add_df_row(res_priceopt_df, fnames, fvalues)
}



# --- Article Groups Predictions ---
# --- results of analysis
res_artgrouppredict_df <- data.frame(
   country = character()
  ,article = character()
  ,sales_predict  = double()
  ,sales_past_average = double()
  ,nr_week_data_expected = integer()
  ,nr_week_data_available = integer()
)


# helper for data frame top_groups_df
# easily add a new row of data 
res_artgrouppredict_add <- function( country,article,sales_predict,sales_past_average
  ,nr_week_data_expected,nr_week_data_available) {
  
  fnames <- c("country", "article" , "sales_predict", "sales_past_average"
              ,"nr_week_data_expected","nr_week_data_available")

  fvalues <-list(country, article, sales_predict, sales_past_average
    ,as.integer(nr_week_data_expected),as.integer(nr_week_data_available))

    res_artgrouppredict_df <<- add_df_row(res_artgrouppredict_df, fnames, fvalues)
}




# ------------------------------------------------------------------
#' Data Load and Preprocessing
#' Operates globally (all data read, all countries)
#' 
#' @return data frame with sales data joined to articles master data,
#' with some variables renamed for readability, some variables added 
#' to work more easily
#' @author Enrico
#' @details
#' Reads the data files, joins the sales and articles data, 
#' performs some simple preprocessing
# ------------------------------------------------------------------
load_preprocess_alldata <- function() {

  # read articles data
  articles_df <- read.csv(articles_fname, sep = field_sep)
  articles_df$article <- as.character(articles_df$article) 
  # should not be factors, TODO address data whe reading file
  
  # read sales data.
  # The if/"lazy read" worked when this code was not in a function,
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
  
  # paranoid check for duplicates for in master
  if (length(articles_df$article) != length(unique(articles_df$article))) {
    warning(paste("duplicate articles in",articles_fname))
  }
  
  # check for eventual  sales without articles in master 
  distinct_art_sold <- unique(sales_df$article)
  if (length(distinct_art_sold) != length(unique(sales_df$article))) {
    msg <- paste("some sales do not correspond to articles in master, nr such sales"
                 ,length(distinct_art_sold) - length(unique(art_sales_df$article))) 
    orphan_sales_articles <- setdiff(distinct_art_sold, unique(art_sales_df$article));
    warming("articles ID in sales data not found in master: ",orphan_sales_articles)
  }
  
  # join sales with article data to work more easily
  art_sales_df <- inner_join(sales_df,articles_df,by = "article")
  
  # -- ensure dates have date type ---
  art_sales_df$retailweek <- as.POSIXct(
    strptime(as.character(art_sales_df$retailweek), "%Y-%m-%d"))
  art_sales_df$retailweek <- art_sales_df$retailweek[order(art_sales_df$retailweek)]
  # head(art_sales_df$retailweek);tail(art_sales_df$retailweek) # paranoid check
  

  # --------------- make var names more readable -------------------- 
  colnames(art_sales_df)[which(colnames(art_sales_df) == "promo1")] <- "promo_media"
  colnames(art_sales_df)[which(colnames(art_sales_df) == "promo2")] <- "promo_store"
  
  # ----- add variables to work more easily (resources allow it) ----
  
  # simple/readable discount as percentage
  art_sales_df$discount <- (1 - art_sales_df$ratio)*100 # in percentage

  # "index" with global promo status
  art_sales_df$promo_status <- ifelse(art_sales_df$promo_media == 1
                                      ,ifelse(art_sales_df$promo_store == 1,"both","media")
                                      ,ifelse(art_sales_df$promo_store == 1,"store","none"))
  art_sales_df$promo_status <- as.factor(art_sales_df$promo_status)
  art_sales_df$promo_status <- relevel(art_sales_df$promo_status, ref="none")
  
  
  # ------------------- QUICK EXPLORATION -----------------------------------
  if (FALSE) {
    grp <- group_by(art_sales_df,retailweek)
    smrz <- summarize(grp, sales = sum(sales))
    qplot(y = sales, x= retailweek, data = smrz)
    qplot(y = log(sales), x= retailweek, data = smrz)
  }


  # --- calculate some global values. NB might not actually need/use them ----
  art_sales_dt <-  data.table(art_sales_df)
  sales_country <- art_sales_dt[ , list(sales = sum(sales)
      , sales_avg = mean(sales)) ,by=list(country)]
  
  sales_tot_allcountries <-sum(art_sales_dt$sales)

  return(art_sales_df)
}




# ------------------------------------------------------------------
#' Analyzes data performing all functions requested
#' @param country_name
#' Country/region data in art_sales_df parameter
#' note that also the global data_set can be passed as a pseudo-country
#' (mcountry)
#' @return currently no real return value for lack of time, this may change
#' @author Enrico
#' @details
#' Currently dirty, operates by side-effects, if there were/will be time 
#' the results of the analysis should go in data-frames to feed directly
#' ggplot graphics
#' CAVEAT: in some places a normal data frame, in other a data.table, 
#' this just to write more easily, no real/semantic difference and 
#' no special reason just untidiness due to hurry
#' TODO tidy up and use always and only one  data structure
# ------------------------------------------------------------------
analyze <- function(country_name, art_sales_df) {

  art_sales_dt <- data.table(art_sales_df)
  if (country_name != mcountry) {
    # for safety, currently redundant: subsetting done by calling code
    art_sales_dt <- art_sales_dt[ country == country_name, ,] 
  } 
  
  cat("\n#############################################################################")
  cat("\nanalyzing:",toupper(country_name),", nr data rows:",nrow(art_sales_dt))
  cat("\n#############################################################################\n")
      
  if (country_name != mcountry &
    nrow(art_sales_df[art_sales_df$country !=  country_name, ]) > 0) {
    stop(paste("internal error: found  data for country other than:",country_name))
  }
  
  # -------------------------------------------------------------------
  #                 WHAT DRIVES SALES
  # taken as: which country, product group, category, article sell the most
  # -------------------------------------------------------------------
  
  sales_country_tot <- sum(art_sales_dt$sales)
  cat(country_name,"tot sales",sales_country_tot)
  # --- summarize
  sales_prodgroup <- art_sales_dt[ ,list(sales = sum(sales)), by=list(productgroup)]
  sales_prodgroup <- head(sales_prodgroup[order(-rank(sales))],nr_top_items)
  res_top_group_add(country_name,sales_prodgroup,sales_country_tot)


  sales_prodgrpcat <- art_sales_dt[ ,list(sales = sum(sales)), by=list(productgroup,category)]
  sales_prodgrpcat <- head(sales_prodgrpcat[order(-rank(sales))],nr_top_items)
  # produced but not used
  
  sales_article <- art_sales_dt[, list(sales = sum(sales)), by=list(article)]
  sales_article <- head(sales_article[order(-rank(sales))],nr_top_items)
  # produced but not used
  
  # head(sales_article)
  
  # -------------------------------------------------------------------
  #                 EFFECT OF PROMOS AND DISCOUNTS
  # -------------------------------------------------------------------


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
  
  # store the results (tentative)
  res_promo_add(country_name, discount, "disc.+1%", summary(fit_promo_disc)$coeff[2, ])
  res_promo_add(country_name, media,    "promo ON"    , summary(fit_promo_disc)$coeff[3, ])
  res_promo_add(country_name, store,    "promo ON"    , summary(fit_promo_disc)$coeff[4, ])

  # -----------------------------------------------------------------
  #                     PREDICT
  # -----------------------------------------------------------------
  
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

  # calculate total sales by week, useful also for non-weekly predictions
  week_sales <- summarise(group_by(art_sales_df, retailweek), sales = sum(sales))
  
  # abandoned in favour of ETS
  # sales_avg_week <- mean(week_sales$sales)
  # 
  # predict_weeks_nr <- 5
  # if (country_name != france) {
  #   ts_sales_w <- ts(week_sales$sales, start=c(2014,52),frequency = 52) 
  #   stlf_w <- stlf(ts_sales_w, h=predict_weeks_nr);
  #   cat("\n",country_name," weekly forecasts (stlf), next",predict_weeks_nr,"weeks:",stlf_w$mean[c(1:5)]
  #       ,"\n(the weekly mean is: ",sales_avg_week,")")
  #   if (data_weeks_avail != data_weeks_expected) {
  #     cat("\nFORECAST above is UNRELIABLE due to ",data_weeks_expected-data_weeks_avail,"missing week")
  #   }
  #   # plot(stlf_w);Sys.sleep(10)
  # } else {
  #   warning(country_name," not 2 periods available, not performing stlf prediction")
  # }

  
  # --------------------- (ets) prediction --------------------------

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

  res_artgrouppredict_add(country_name,"all-groups",fcast$mean[1],sales_avg_month
                          ,data_weeks_expected,data_weeks_avail) 
  
  cat("\n",country_name," next month (ets) forecasts",fcast$mean[1]
      ," (past months mean: ",sales_avg_month,")")
  if (data_weeks_avail != data_weeks_expected) {
    cat("\nFORECAST above is UNRELIABLE due to ",data_weeks_expected-data_weeks_avail,"weeks of data missing\n")
  }
  m <- paste(country_name,"#Items Sales Month Forecast",round(fcast$mean[1],0),"\n(past mean:"
             ,round(sales_avg_month,0),")")
  #print(fcast$mean[1])
  par(mfrow=c(1,1));plot(fcast,main = m)
  invisible(readline(prompt="Press [enter] to continue"))
  # qplot(fcast)
  # invisible(readline(prompt="Press [enter] to continue"))
  
  
  #--------------------------------------------------------------------
  #               PRICE OPTIMIZATION
  #--------------------------------------------------------------------

  # consider ONLY sales without promotions
  art_sales_nopromo_dt <- art_sales_dt[promo_status == "none" , ]
  cat("\n",nrow(art_sales_nopromo_dt),"out of",nrow(art_sales_dt),"ie"
      ,nrow(art_sales_nopromo_dt)/nrow(art_sales_dt)*100,"%\n")
  
  # find articles that sold more items, very simplistic choice, other choices
  # (largest revenue based on recommended price ,a mix of revenue and delta between
  # recommended and actul prices) would have been more useful and easy to implement
  # unfortunately ran too short of time to implement them
  art_sales_nopromo_sum_dt <- art_sales_nopromo_dt[ ,list(sales = sum(sales)), by=list(article)]
  art_sales_order_dt <- art_sales_nopromo_sum_dt[order(-rank(sales)),,]
  # print(head(art_sales_order_dt$sales,10))

  nr_prices_optim <- 0
  for (art in art_sales_order_dt$article) {
    # subset all data for each top article into dedicated data table art_dt
    art_dt <- art_sales_nopromo_dt[article == art]
    nrows_art <- nrow(art_dt) 

    # --- build profit equation to optimize
    # get sales linear eq coefficients
    fit <- lm(sales ~ current_price, data = art_dt)
    b <- fit$coefficients; 
    if (summary(fit)$coefficients[1,4] >= alpha | (summary(fit)$coefficients[2,4] >= alpha)) {
      # print(paste(art,"non meaningful coefficients",summary(fit)$coefficients[1,4] ,summary(fit)$coefficients[2,4] ))
    } else {
      # duplicate other parameters from equation into more user friendly variables
      art_reg_price <- art_dt[1]$regular_price;
      art_current_price_avg <- mean(art_dt$current_price) # just for info
      sales_cost <- art_dt[1]$cost
      # finally assemble the profit equation
      art_profit <- function(price) {
        profit <- b[2]*price^2+price*(b[1]-b[2]*sales_cost) - sales_cost*b[1]
        profit
      }
      xyz <- art_profit(0); # debug
      # optimize profit over price
      opt <- optimize(art_profit,lower = art_reg_price*0.5, upper = art_reg_price*10,maximum = TRUE)
      
      profit_from_data <- sum(art_dt$sales*art_dt$current_price -art_dt$cost)
      # profit with optimized valuespromo_effect_df
      sales_opt <- b[1]+b[2]*opt$maximum
      profit_opt <- sales_opt*(opt$maximum - sales_cost)
      # calculating profit from past data we summed nrows_art data points
      profit_opt <- profit_opt * nrows_art
      
      
      if (profit_opt > profit_from_data) {
        # print(paste("art:",art,"current price avg",art_current_price_avg,"optim price",opt$maximum
        #             ,"intercept",b[1],"slope",b[2],
        #             "profits[current",profit_from_data,"(theoretical) optim on past data"
        #             ,profit_opt,"] theoric profit improvement:", round((profit_opt/profit_from_data)*100-100,2),"%"))
        res_priceopt_add(country_name, art, art_reg_price,opt$maximum,profit_from_data,profit_opt)
        
        art_dt <- NULL # paranoid clean up because of strange behaviours of R
        nr_prices_optim <- nr_prices_optim + 1
      } else {
        # profit from real/provided data is greater than that of optimized price
        # we have just one single optimized price per article and "project" it 
        # prices in the data can differ across sales, "large" prices can happen to be
        # with large sales
        # we only consider interesting and report cases where optimized price increases revenue
      }
      
      
      if (nr_prices_optim >= 5)
        break;
    }
  }
}



quickprint_results <- function(country_name) {
  
  print("");print("");
  print("######################################################################")
  print(paste("            ",toupper(country_name)))
  print("######################################################################")
  
  print("");print("------ PROMO & DISCOUNT EFFECTIVENESS ------")
  print(promo_effect_df[promo_effect_df$country == country_name, ])
  # grid.table(promo_effect_df) abandoned, no time to complete 
  # automatic plots, .rmd/slidify
  
  print("");print("------ ARTICLE GROUPS MOST SOLD ------")
  print(res_topgroups_df[res_topgroups_df$country == country_name, ])

  print("");print("------ PRICE OPTIMIZATION ON TOP ARTICLES ------")
  print(res_priceopt_df[res_priceopt_df$country == country_name, ])
  
  print("");print("------ SALES PREDICTION ------")
  print(res_artgrouppredict_df[res_artgrouppredict_df$country == country_name, ])
  print("");
}


#work_dir = dirname(parent.frame(2)$ofile)
#setwd(getSrcDirectory()[1])

art_sales_df_all <- load_preprocess_alldata()

analyze(mcountry,art_sales_df_all)
analyze(germany,art_sales_df_all[art_sales_df_all$country == germany , ])
analyze(france, art_sales_df_all[art_sales_df_all$country == france  , ])
analyze(austria,art_sales_df_all[art_sales_df_all$country == austria , ])

quickprint_results(germany)
quickprint_results(austria)
quickprint_results(france)



