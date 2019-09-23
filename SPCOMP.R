# Libraries
library(stringr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(psych)
library(openxlsx)

wd ="D:/Capstone Project/Data/"

start_date = "2003-06-01"
end_date = "2018-05-31"

start_date_esg = "2002-12-01"
end_date_esg = "2017-12-31"

# Files with data
file_names = c("SPCOMP_filter.csv",
               "SPCOMP_stock_returns.csv",
               "SPCOMP_benchmark_returns.csv",
               "SPCOMP_market_cap.csv",
               "SPCOMP_price_to_book_ratio.csv"
              )

xts_names = c("filter",
              "stock_returns",
              "benchmark_returns",
              "size_score",
              "ptb_score"
              )

# Read files with data
for(i in 1:length(file_names)){
  df = read.csv(paste0(wd, file_names[i]))
  
  rownames(df) = df$Date
  df$Date = NULL
  
  # Remove the first symbol from column names
  colnames(df) = substring(colnames(df), 2)
  
  # Create xts object
  xts_object = xts(x = df, order.by = as.Date(rownames(df)))
  
  # Analyzed Period
  xts_object = window(x = xts_object, start = start_date, end = end_date)
  assign(xts_names[i], xts_object)
}


# Convert percent to number
stock_returns = stock_returns / 100
benchmark_returns = benchmark_returns / 100

# Normalize size and ptb scores
size_score = t(apply(size_score, 1, function(x)(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))))
size_score = xts(size_score, order.by = as.Date(rownames(size_score)))
ptb_score= t(apply(ptb_score, 1, function(x)(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))))
ptb_score = xts(ptb_score, order.by = as.Date(rownames(ptb_score)))

# Convert size and ptb scores from monthly to weekly data
score_year_month = paste0(year(index(size_score)), month(index(size_score)))
size_score_weekly = xts(matrix(NA, nrow = nrow(stock_returns), ncol = ncol(stock_returns)), order.by = index(stock_returns))
ptb_score_weekly = xts(matrix(NA, nrow = nrow(stock_returns), ncol = ncol(stock_returns)), order.by = index(stock_returns))
colnames(size_score_weekly) = colnames(size_score)
colnames(ptb_score_weekly) = colnames(ptb_score)
score_weekly_year_month = paste0(year(index(stock_returns)), month(index(stock_returns)))

for(i in 1:length(score_weekly_year_month)){
  year_month = score_weekly_year_month[i]
  index = match(year_month,score_year_month)
  size_score_weekly[i, ] = size_score[index, ]
  ptb_score_weekly[i, ] = ptb_score[index, ]
}

size_score = size_score_weekly
ptb_score = ptb_score_weekly

# Calculate mom score
mom_score = xts(matrix(NA, nrow = nrow(stock_returns), ncol = ncol(stock_returns)), order.by = index(stock_returns))
colnames(mom_score) = colnames(stock_returns)

for(i in 7:nrow(mom_score)){
    mom_score[i,] = cumprod(1+stock_returns[(i-6):(i-1),])[6,] 
}

mom_score[mom_score==1]=NA

# Normalize mom_score
mom_score= t(apply(mom_score, 1, function(x)(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))))
mom_score = xts(mom_score, order.by = as.Date(rownames(mom_score)))

# Universe info 
all_stocks_num = ncol(stock_returns)
all_stocks_codes = colnames(stock_returns)
weeks_num_in_period = nrow(stock_returns)
period = index(stock_returns)

# Read monthly ESG scores
esg_scores_df = read.csv(file = paste0(wd,"SPCOMP_scores_monthly.csv"),header = F, sep =";", dec = ",")
header_df = read.csv(file = paste0(wd,"SPCOMP_header.csv"),header = F, sep =";", dec = ",")

# Convert to character
header_df[] = apply(header_df, 2, as.character)          


# Create xts object
esg_scores = xts(x = esg_scores_df[,-1], order.by = as.Date(esg_scores_df[,1], format = "%d.%m.%Y"))

# Analyzed period
esg_scores_monthly = window(x = esg_scores, start = start_date_esg, end = end_date_esg)
monthly_period = index(esg_scores_monthly)

# Make values numeric
esg_scores_monthly_df = as.data.frame(apply(esg_scores_monthly, 2, as.numeric))
esg_scores_monthly = xts(x = esg_scores_monthly_df, order.by = monthly_period)

# Split esg scores into 5 groups: ESGCS, ESG, E , S , G
esg_scores_patterns = c("ESG.Combined.Score",
                         "ESG.Score",
                         "Environmental",
                         "Social",
                         "Corporate.Governance")

esg_scores = c("esgcs",
                "esg",
                "e",
                "s",
                "g")

esg_scores_num = length(esg_scores)

#  Create 5 xts objects for 5 diff scores                        
for(i in 1:esg_scores_num){
  # Apply pattern
  stock_ind  = grep(pattern = esg_scores_patterns[i], x = as.character(header_df[1,]))
  # Take only required score data
  xts_object = esg_scores_monthly[, stock_ind]
  colnames(xts_object) = as.character(header_df[2,])[stock_ind]
  # Assign name to new xts object
  assign(x = esg_scores[i], value = xts_object)
}

# Create equal esg
equal_esg = (1/3)*(e + s + g)

# Add equal esg
esg_scores = c(esg_scores,
               "equal_esg")
esg_scores_num = length(esg_scores)


################################# Correlation #####################################

# Download Sustainanalytics ESG score
esg_msciusa_sa = read.csv(paste0(wd, "MSCIUSASTD_esg.csv"))
rownames(esg_msciusa_sa) = esg_msciusa_sa$Date
esg_msciusa_sa$Date = NULL
colnames(esg_msciusa_sa) = substring(colnames(esg_msciusa_sa), 2)

# Replace zeros with NA and delete rows with all NA
esg_msciusa_sa[esg_msciusa_sa == 0] = NA
esg_msciusa_sa = esg_msciusa_sa[apply(esg_msciusa_sa,1,function(x)any(!is.na(x))),]

# Convert weekly to monthly data keeping only the last observation for every month
esg_msciusa_sa$year_month = paste0(year(rownames(esg_msciusa_sa)), month(rownames(esg_msciusa_sa)))
esg_msciusa_sa = esg_msciusa_sa[!duplicated(esg_msciusa_sa[, "year_month"], fromLast=T),]

# Create xts object
esg_msciusa_sa$year_month = NULL
esg_msciusa_sa = xts(x = esg_msciusa_sa, order.by = as.Date(rownames(esg_msciusa_sa)))

# Analyzed period for correlation
start_date_esg_msciusa_sa = "2009-12-01"
esg_msciusa_sa = window(x = esg_msciusa_sa, start = start_date_esg_msciusa_sa, end = end_date_esg)

#  Create 6 xts objects for 6 diff scores
esg_scores_monthly = paste0(esg_scores, "_monthly")

for(i in 1:esg_scores_num){
  # Monthly esg scores
  xts_object = get(esg_scores[i])
  xts_object_monthly = window(x = xts_object, start = start_date_esg_msciusa_sa, end = end_date_esg)
  
  # Take only decembers as we need annual data for portfolios construction
  xts_object = window(x = xts_object, start = start_date_esg, end = end_date_esg)
  xts_object = xts_object[month(index(xts_object))==12]

  # Assign name to new xts object
  assign(x = esg_scores_monthly[i], value = xts_object_monthly)
  assign(x = esg_scores[i], value = xts_object)
}

dec_period = index(e)

# Read Firm Info for MSCI USA
msciusa_firm_info = readRDS(paste0(wd, "MSCIUSASTD_firm_info.rds"))

# Read Firm Info for SPCOMP
spcomp_firm_info = readRDS(paste0(wd, "SPCOMP_firm_info.rds"))

# Merge Firm Info dataframes for SPCOMP and MSCI USA
all_firm_info = merge(x = spcomp_firm_info, y = msciusa_firm_info, by.x = "ISIN", by.y = "MSISIN", suffixes = c(".SP", ".MSCI"), all.x = TRUE)
all_firm_info$matched = !is.na(all_firm_info$NAME.MSCI)

# Keep only rows that matched by ISIN in both SPCOMP and MSCI USA
all_firm_info_reduced = all_firm_info[all_firm_info$matched == TRUE,]

# Replace Sustainanalytics code with ISIN in colnames
colnames(esg_msciusa_sa) = unlist(msciusa_firm_info[, "MSISIN"])

# Create data frame with correlations
correlation = data.frame(matrix(NA, nrow = nrow(esg_msciusa_sa), ncol = esg_scores_num))
correlation = xts(correlation, order.by = index(esg_msciusa_sa))
colnames(correlation) = esg_scores

for(i in 1:esg_scores_num){
  # Monthly esg scores
  xts_object = get(esg_scores_monthly[i])
  
  ind_in_all_firm_info_reduced = which(colnames(xts_object) %in% all_firm_info_reduced$DSCD.SP)
  xts_object = xts_object[,ind_in_all_firm_info_reduced]
  
  # Replace Datastream code with MSISIN in colnames
  ind_in_spcomp_firm_info = match(colnames(xts_object), rownames(spcomp_firm_info))
  colnames(xts_object) = unlist(spcomp_firm_info[ind_in_spcomp_firm_info, "ISIN"])
  
  # Keep corresponding columns only
  esg_msciusa_sa_current = esg_msciusa_sa[,colnames(xts_object)]
  
  # Compute correlation
  correlation[, i] = diag(cor(t(xts_object), t(esg_msciusa_sa_current), use = "pairwise.complete.obs", method = "spearman"))
  
  # Assign name to new xts object
  assign(x = esg_scores_monthly[i], value = xts_object)
}


# Plot correlation
Sys.setlocale("LC_ALL", "English")
plot(correlation, legend.loc = "left", main = "Correlation between Datastream and Sustainanalytics ESG scores (SPCOMP)")

############################# Portfolios #######################################

# Portfolios info
portfolio_names = c("Portfolio 1",
                    "Portfolio 2",
                    "Portfolio 3",
                    "Portfolio 4",
                    "Portfolio 5")

portfolios_num = length(portfolio_names)

extended_portfolio_names = c(portfolio_names,
                             "All best",
                             "Benchmark")

extended_portfolios_num = length(extended_portfolio_names)

# Create xts objects to save results

# Create xts object to save portfolio returns
df1 = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = extended_portfolios_num))
colnames(df1) = extended_portfolio_names
xts_object1 = xts(x = df1, order.by = period)
rm(df1)
# Create xts object containing filter(NA,1,2,3,4,5) to show in which portfolio is stock in current week
df2 = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = all_stocks_num))
colnames(df2) = all_stocks_codes
xts_object2 = xts(x = df2, order.by = period)
rm(df2)

for(esg_score in esg_scores){
  
  # Create xts object to save portfolio returns
  xts_object1_name = paste0(esg_score, "_portfolio_returns")
  assign(x = xts_object1_name, value = xts_object1)
  
  # Create xts object to save in which portfolio is particular stock in particular week
  xts_object2_name = paste0(esg_score, "_filter")
  assign(x = xts_object2_name, value = xts_object2)
}

# Create xts objects to save average scores
avg_size_score = c("_avg_esg_score",
               "_avg_size_score",
               "_avg_ptb_score",
               "_avg_mom_score")

df = data.frame(matrix(NA, nrow = weeks_num_in_period, ncol = portfolios_num))
colnames(df) = portfolio_names
xts_object = xts(x = df, order.by = period)
rm(df)

for(esg_score in esg_scores){
  for(avg_score in avg_size_score){
    xts_object_name = paste0(esg_score, avg_score)
    assign(x = xts_object_name, value = xts_object)
  }
}

# Create xts objects to save results for "all best" stocks
# Number of "all best" stocks in current week
all_best_stocks_num = xts(x = rep(NA, weeks_num_in_period), order.by = period)
colnames(all_best_stocks_num) = "Number of stocks"

all_best_portfolio_returns = xts(x = rep(NA, weeks_num_in_period), order.by = period)
colnames(all_best_portfolio_returns) = "Return"

# Create xts object containing filter (NA, 1) to show if stock in "all best" in current week
all_best_stock_filter_df = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = all_stocks_num))
colnames(all_best_stock_filter_df) = all_stocks_codes
all_best_stock_filter = xts(x = all_best_stock_filter_df, order.by = period)

# Create xts object to show number of stocks in index with existing esg score in current week
stocks_num_with_esg_score_df = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = esg_scores_num ))
colnames(stocks_num_with_esg_score_df) = esg_scores
stocks_num_with_esg_score = xts(x = stocks_num_with_esg_score_df, order.by = period)

# Create statistics df
# For the whole period (not for a week)
statistics_df_names = c("annual_return_df",
                        "annual_sd_df",
                        "avg_esg_score_df",
                        "avg_size_score_df",
                        "avg_ptb_score_df",
                        "avg_mom_score_df",
                        "beta_df",
                        "bull_beta_df",
                        "bear_beta_df")

df = data.frame(matrix(NA, nrow = portfolios_num, ncol = esg_scores_num))
rownames(df) = portfolio_names
colnames(df) = esg_scores

for(statistics_df_name in statistics_df_names)
{
  assign(x = statistics_df_name, value = df)
}
rm(df)

# Scores are changing once a  (in Dec)
week_num = 0
# get years
years = year(dec_period)[-1]

for(i in 1:length(years)){
  
  year = years[i]
  previous_year = year - 1
  next_year = year + 1
  
  # Data of current year
  first_date_curr_year = as.Date(paste0(year, "-06-01"))
  last_date_curr_year = as.Date(paste0(next_year, "-05-31"))
  
  # Create xts objects (filter, stock returns,size/ptb/mom scores) for current investment year
  filter_curr_year = window(x = filter, start = first_date_curr_year, end = last_date_curr_year)
  stock_returns_curr_year = window(x = stock_returns, start = first_date_curr_year, end = last_date_curr_year)
  size_score_curr_year = window(x = size_score, start = first_date_curr_year, end = last_date_curr_year)
  ptb_score_curr_year = window(x = ptb_score, start = first_date_curr_year, end = last_date_curr_year)
  mom_score_curr_year = window(x = mom_score, start = first_date_curr_year, end = last_date_curr_year)
  
  # Loop through every week of current investment year
  for (j in 1:nrow(stock_returns_curr_year)){
    
    # Create vectors to save stock codes for stocks that are in index and have esg sore (current week)
    vector = c()
    for(esg_score in esg_scores){
      vector_name = paste0("stock_codes_with_", esg_score)
      assign(x = vector_name, value = vector)
    }
    
    # To observe how loop is running
    week_num = week_num + 1
    cat("Row" , week_num, "out of", weeks_num_in_period, "\n")
    
    # Data for current week
    filter_curr_week = as.vector(filter_curr_year[j,])
    names(filter_curr_week) = colnames(filter_curr_year)
    stock_returns_curr_week = as.vector(stock_returns_curr_year[j,])
    names(stock_returns_curr_week) = colnames(stock_returns_curr_year)
    size_score_curr_week = as.vector(size_score_curr_year[j,])
    names(size_score_curr_week) = colnames(size_score_curr_year)
    ptb_score_curr_week= as.vector(ptb_score_curr_year[j,])
    names(ptb_score_curr_week) = colnames(ptb_score_curr_year)
    mom_score_curr_week = as.vector(mom_score_curr_year[j,])
    names(mom_score_curr_week) = colnames(mom_score_curr_year)
    
    stock_codes_in_index = names(filter_curr_year)[which(as.vector(filter_curr_week)== 1)]
    
    # esgcs

    # Loop through all stocks
    for(stock_code in stock_codes_in_index){
      # Returns stock code or numeric(0)
      stock_code_with_any_score = grep(pattern = stock_code, x = colnames(esgcs), value = T)    
      # In case a stock has a score or NA
      if(length(stock_code_with_any_score) > 0){
        # Returns esg score for a stock (or NA)
        any_score = esgcs[i,stock_code_with_any_score]
        # If this esg scores exists (not NA)
        if(is.na(any_score) == F){
          # Add this stock code to vector of stock codes that are in index and have esg score (current week)
          stock_codes_with_esgcs = c(stock_codes_with_esgcs, stock_code)
        }#endif not NA
      }#endif not esg exists
    }#endfor all stocks
    
    stocks_num_with_esg_score[week_num, "esgcs"] = length(stock_codes_with_esgcs)
    
    
    # esg
    
    # Loop through all stocks
    for(stock_code in stock_codes_in_index){
      # Returns stock code or numeric(0)
      stock_code_with_any_score = grep(pattern = stock_code, x = colnames(esg), value = T)    
      # In case a stock has a score or NA
      if(length(stock_code_with_any_score) > 0){
        # Returns esg score for a stock (or NA)
        any_score = esg[i,stock_code_with_any_score]
        # If this esg scores exists (not NA)
        if(is.na(any_score) == F){
          # Add this stock code to vector of stock codes that are in index and have esg score (current week)
          stock_codes_with_esg = c(stock_codes_with_esg, stock_code)
        }#endif not NA
      }#endif not esg exists
    }#endfor all stocks
    
    stocks_num_with_esg_score[week_num, "esg"] = length(stock_codes_with_esg)
    
    
    # e
    
    # Loop through all stocks
    for(stock_code in stock_codes_in_index){
      # Returns stock code or numeric(0)
      stock_code_with_any_score = grep(pattern = stock_code, x = colnames(e), value = T)    
      # In case a stock has a score or NA
      if(length(stock_code_with_any_score) > 0){
        # Returns esg score for a stock (or NA)
        any_score = e[i,stock_code_with_any_score]
        # If this esg scores exists (not NA)
        if(is.na(any_score) == F){
          # Add this stock code to vector of stock codes that are in index and have esg score (current week)
          stock_codes_with_e = c(stock_codes_with_e, stock_code)
        }#endif not NA
      }#endif not esg exists
    }#endfor all stocks
    
    stocks_num_with_esg_score[week_num, "e"] = length(stock_codes_with_e)
    
    
    # s
    
    # Loop through all stocks
    for(stock_code in stock_codes_in_index){
      # Returns stock code or numeric(0)
      stock_code_with_any_score = grep(pattern = stock_code, x = colnames(s), value = T)    
      # In case a stock has a score or NA
      if(length(stock_code_with_any_score) > 0){
        # Returns esg score for a stock (or NA)
        any_score = s[i,stock_code_with_any_score]
        # If this esg scores exists (not NA)
        if(is.na(any_score) == F){
          # Add this stock code to vector of stock codes that are in index and have esg score (current week)
          stock_codes_with_s = c(stock_codes_with_s, stock_code)
        }#endif not NA
      }#endif not esg exists
    }#endfor all stocks
    
    stocks_num_with_esg_score[week_num, "s"] = length(stock_codes_with_s)
    
    
    
    # g
    
    # Loop through all stocks
    for(stock_code in stock_codes_in_index){
      # Returns stock code or numeric(0)
      stock_code_with_any_score = grep(pattern = stock_code, x = colnames(g), value = T)    
      # In case a stock has a score or NA
      if(length(stock_code_with_any_score) > 0){
        # Returns esg score for a stock (or NA)
        any_score = g[i,stock_code_with_any_score]
        # If this esg scores exists (not NA)
        if(is.na(any_score) == F){
          # Add this stock code to vector of stock codes that are in index and have esg score (current week)
          stock_codes_with_g = c(stock_codes_with_g, stock_code)
        }#endif not NA
      }#endif not esg exists
    }#endfor all stocks
    
    stocks_num_with_esg_score[week_num, "g"] = length(stock_codes_with_g)
    
    
    # equal_esg
    
    # Loop through all stocks
    for(stock_code in stock_codes_in_index){
      # Returns stock code or numeric(0)
      stock_code_with_any_score = grep(pattern = stock_code, x = colnames(equal_esg), value = T)    
      # In case a stock has a score or NA
      if(length(stock_code_with_any_score) > 0){
        # Returns esg score for a stock (or NA)
        any_score = equal_esg[i,stock_code_with_any_score]
        # If this esg scores exists (not NA)
        if(is.na(any_score) == F){
          # Add this stock code to vector of stock codes that are in index and have esg score (current week)
          stock_codes_with_equal_esg = c(stock_codes_with_equal_esg, stock_code)
        }#endif not NA
      }#endif not esg exists
    }#endfor all stocks
    
    stocks_num_with_esg_score[week_num, "equal_esg"] = length(stock_codes_with_equal_esg)
    
    # esgcs
    
    # Calculate number of stocks in one portfolio
    stocks_num_in_esgcs_portfolio = length(stock_codes_with_esgcs) %/% 5
    # esg score we analyze current week
    esgcs_curr_week = as.vector(esgcs[i,stock_codes_with_esgcs])
    names(esgcs_curr_week) = colnames(esgcs[i,stock_codes_with_esgcs])
                    
    
      for(l in 1:portfolios_num){
      
      # Find constituents of portfolio 
      largest_score_portfolio_l = sort(esgcs_curr_week, decreasing = F)[stocks_num_in_esgcs_portfolio]
      esgcs_portfolio_l_ind = which(esgcs_curr_week <= largest_score_portfolio_l)
      # Could be several scores with the same value on the "border"
      esgcs_portfolio_l_ind = esgcs_portfolio_l_ind[1:stocks_num_in_esgcs_portfolio]
      esgcs_portfolio_l= esgcs_curr_week[esgcs_portfolio_l_ind]
      
      esgcs_portfolio_l_stock_codes  = names(esgcs_portfolio_l)
     
      # Calculate portfolio return current week
      esgcs_portfolio_returns[week_num,l] = mean(stock_returns_curr_week[esgcs_portfolio_l_stock_codes])
      
      # Calculate average esg score in portfolio current week
      esgcs_avg_esg_score[week_num,l] = mean(esgcs_portfolio_l, na.rm = T)
      
      # Calculate average size score in portfolio current week
      esgcs_avg_size_score[week_num,l] = mean(size_score_curr_week[esgcs_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average ptb score in portfolio current week
      esgcs_avg_ptb_score[week_num,l] = mean(ptb_score_curr_week[esgcs_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average momentum score in portfolio current week
      esgcs_avg_mom_score[week_num,l] = mean(mom_score_curr_week[esgcs_portfolio_l_stock_codes], na.rm = T)
      
      # Fill filter
      esgcs_filter[week_num, esgcs_portfolio_l_stock_codes] = l
      
      # Replace esg scores in portfolio with very big number
      esgcs_curr_week[esgcs_portfolio_l_stock_codes] = 1000000
    }  
      
      
    # esg
    
    # Calculate number of stocks in one portfolio
    stocks_num_in_esg_portfolio = length(stock_codes_with_esg) %/% 5
    # esg score we analyze current week
    esg_curr_week = as.vector(esg[i,stock_codes_with_esg])
    names(esg_curr_week) = colnames(esg[i,stock_codes_with_esg])
    
    for(l in 1:portfolios_num){
      
      # Find constituents of portfolio 
      largest_score_portfolio_l = sort(esg_curr_week, decreasing = F)[stocks_num_in_esg_portfolio]
      esg_portfolio_l_ind = which(esg_curr_week <= largest_score_portfolio_l)
      # Could be several scores with the same value on the "border"
      esg_portfolio_l_ind = esg_portfolio_l_ind[1:stocks_num_in_esg_portfolio]
      esg_portfolio_l= esg_curr_week[esg_portfolio_l_ind]
      
      esg_portfolio_l_stock_codes  = names(esg_portfolio_l)
      
      # Calculate portfolio return current week
      esg_portfolio_returns[week_num,l] = mean(stock_returns_curr_week[esg_portfolio_l_stock_codes])
      
      # Calculate average esg score in portfolio current week
      esg_avg_esg_score[week_num,l] = mean(esg_portfolio_l, na.rm = T)
      
      # Calculate average size score in portfolio current week
      esg_avg_size_score[week_num,l] = mean(size_score_curr_week[esg_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average ptb score in portfolio current week
      esg_avg_ptb_score[week_num,l] = mean(ptb_score_curr_week[esg_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average momentum score in portfolio current week
      esg_avg_mom_score[week_num,l] = mean(mom_score_curr_week[esg_portfolio_l_stock_codes], na.rm = T)
      
      # Fill filter
      esg_filter[week_num, esg_portfolio_l_stock_codes] = l
      
      # Replace esg scores in portfolio with very big number
      esg_curr_week[esg_portfolio_l_stock_codes] = 1000000
    }
    
    
    # e
    
    # Calculate number of stocks in one portfolio
    stocks_num_in_e_portfolio = length(stock_codes_with_e) %/% 5
    # esg score we analyze current week
    e_curr_week = as.vector(e[i,stock_codes_with_e])
    names(e_curr_week) = colnames(e[i,stock_codes_with_e])
    
    for(l in 1:portfolios_num){
      
      # Find constituents of portfolio 
      largest_score_portfolio_l = sort(e_curr_week, decreasing = F)[stocks_num_in_e_portfolio]
      e_portfolio_l_ind = which(e_curr_week <= largest_score_portfolio_l)
      # Could be several scores with the same value on the "border"
      e_portfolio_l_ind = e_portfolio_l_ind[1:stocks_num_in_e_portfolio]
      e_portfolio_l= e_curr_week[e_portfolio_l_ind]
      
      e_portfolio_l_stock_codes  = names(e_portfolio_l)
      
      # Calculate portfolio return current week
      e_portfolio_returns[week_num,l] = mean(stock_returns_curr_week[e_portfolio_l_stock_codes])
      
      # Calculate average esg score in portfolio current week
      e_avg_esg_score[week_num,l] = mean(e_portfolio_l, na.rm = T)
      
      # Calculate average size score in portfolio current week
      e_avg_size_score[week_num,l] = mean(size_score_curr_week[e_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average ptb score in portfolio current week
      e_avg_ptb_score[week_num,l] = mean(ptb_score_curr_week[e_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average momentum score in portfolio current week
      e_avg_mom_score[week_num,l] = mean(mom_score_curr_week[e_portfolio_l_stock_codes], na.rm = T)
      
      # Fill filter
      e_filter[week_num, e_portfolio_l_stock_codes] = l
      
      # Replace esg scores in portfolio with very big number
      e_curr_week[e_portfolio_l_stock_codes] = 1000000
      
      if(l== 5){
        best_e_stocks = e_portfolio_l_stock_codes
      }
    }
    
    # s
    
    # Calculate number of stocks in one portfolio
    stocks_num_in_s_portfolio = length(stock_codes_with_s) %/% 5
    # esg score we analyze current week
    s_curr_week = as.vector(s[i,stock_codes_with_s])
    names(s_curr_week) = colnames(s[i,stock_codes_with_s])
    
    for(l in 1:portfolios_num){
      
      # Find constituents of portfolio 
      largest_score_portfolio_l = sort(s_curr_week, decreasing = F)[stocks_num_in_s_portfolio]
      s_portfolio_l_ind = which(s_curr_week <= largest_score_portfolio_l)
      # Could be several scores with the same value on the "border"
      s_portfolio_l_ind = s_portfolio_l_ind[1:stocks_num_in_s_portfolio]
      s_portfolio_l= s_curr_week[s_portfolio_l_ind]
      
      s_portfolio_l_stock_codes  = names(s_portfolio_l)
      
      # Calculate portfolio return current week
      s_portfolio_returns[week_num,l] = mean(stock_returns_curr_week[s_portfolio_l_stock_codes])
      
      # Calculate average esg score in portfolio current week
      s_avg_esg_score[week_num,l] = mean(s_portfolio_l, na.rm = T)
      
      # Calculate average size score in portfolio current week
      s_avg_size_score[week_num,l] = mean(size_score_curr_week[s_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average ptb score in portfolio current week
      s_avg_ptb_score[week_num,l] = mean(ptb_score_curr_week[s_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average momentum score in portfolio current week
      s_avg_mom_score[week_num,l] = mean(mom_score_curr_week[s_portfolio_l_stock_codes], na.rm = T)
      
      # Fill filter
      s_filter[week_num, s_portfolio_l_stock_codes] = l
      
      # Replace esg scores in portfolio with very big number
      s_curr_week[s_portfolio_l_stock_codes] = 1000000
      
      if(l== 5){
        best_s_stocks = s_portfolio_l_stock_codes
      }
    }
    
    
    # g
    
    # Calculate number of stocks in one portfolio
    stocks_num_in_g_portfolio = length(stock_codes_with_g) %/% 5
    # esg score we analyze current week
    g_curr_week = as.vector(g[i,stock_codes_with_g])
    names(g_curr_week) = colnames(g[i,stock_codes_with_g])
    
    for(l in 1:portfolios_num){
      
      # Find constituents of portfolio 
      largest_score_portfolio_l = sort(g_curr_week, decreasing = F)[stocks_num_in_g_portfolio]
      g_portfolio_l_ind = which(g_curr_week <= largest_score_portfolio_l)
      # Could be several scores with the same value on the "border"
      g_portfolio_l_ind = g_portfolio_l_ind[1:stocks_num_in_g_portfolio]
      g_portfolio_l= g_curr_week[g_portfolio_l_ind]
      
      g_portfolio_l_stock_codes  = names(g_portfolio_l)
      
      # Calculate portfolio return current week
      g_portfolio_returns[week_num,l] = mean(stock_returns_curr_week[g_portfolio_l_stock_codes])
      
      # Calculate average esg score in portfolio current week
      g_avg_esg_score[week_num,l] = mean(g_portfolio_l, na.rm = T)
      
      # Calculate average size score in portfolio current week
      g_avg_size_score[week_num,l] = mean(size_score_curr_week[g_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average ptb score in portfolio current week
      g_avg_ptb_score[week_num,l] = mean(ptb_score_curr_week[g_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average momentum score in portfolio current week
      g_avg_mom_score[week_num,l] = mean(mom_score_curr_week[g_portfolio_l_stock_codes], na.rm = T)
      
      # Fill filter
      g_filter[week_num, g_portfolio_l_stock_codes] = l
      
      # Replace esg scores in portfolio with very big number
      g_curr_week[g_portfolio_l_stock_codes] = 1000000
      
      if(l== 5){
        best_g_stocks = g_portfolio_l_stock_codes
      }
    }
    
    
    # equal_esg
    
    # Calculate number of stocks in one portfolio
    stocks_num_in_equal_esg_portfolio = length(stock_codes_with_equal_esg) %/% 5
    # esg score we analyze current week
    equal_esg_curr_week = as.vector(equal_esg[i,stock_codes_with_equal_esg])
    names(equal_esg_curr_week) = colnames(equal_esg[i,stock_codes_with_equal_esg])
    
    for(l in 1:portfolios_num){
      
      # Find constituents of portfolio 
      largest_score_portfolio_l = sort(equal_esg_curr_week, decreasing = F)[stocks_num_in_equal_esg_portfolio]
      equal_esg_portfolio_l_ind = which(equal_esg_curr_week <= largest_score_portfolio_l)
      # Could be several scores with the same value on the "border"
      equal_esg_portfolio_l_ind = equal_esg_portfolio_l_ind[1:stocks_num_in_equal_esg_portfolio]
      equal_esg_portfolio_l= equal_esg_curr_week[equal_esg_portfolio_l_ind]
      
      equal_esg_portfolio_l_stock_codes  = names(equal_esg_portfolio_l)
      
      # Calculate portfolio return current week
      equal_esg_portfolio_returns[week_num,l] = mean(stock_returns_curr_week[equal_esg_portfolio_l_stock_codes])
      
      # Calculate average esg score in portfolio current week
      equal_esg_avg_esg_score[week_num,l] = mean(equal_esg_portfolio_l, na.rm = T)
      
      # Calculate average size score in portfolio current week
      equal_esg_avg_size_score[week_num,l] = mean(size_score_curr_week[equal_esg_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average ptb score in portfolio current week
      equal_esg_avg_ptb_score[week_num,l] = mean(ptb_score_curr_week[equal_esg_portfolio_l_stock_codes], na.rm = T)
      
      # Calculate average momentum score in portfolio current week
      equal_esg_avg_mom_score[week_num,l] = mean(mom_score_curr_week[equal_esg_portfolio_l_stock_codes], na.rm = T)
      
      # Fill filter
      equal_esg_filter[week_num, equal_esg_portfolio_l_stock_codes] = l
      
      # Replace esg scores in portfolio with very big number
      equal_esg_curr_week[equal_esg_portfolio_l_stock_codes] = 1000000
      
    }

    # Find number of all best stocks
    all_best_stocks = Reduce(intersect, list(best_e_stocks, best_s_stocks, best_g_stocks))
    all_best_stocks_num[week_num,"Number of stocks"] = length(all_best_stocks)
    # Fill filter
    all_best_stock_filter[week_num, all_best_stocks] = 1
    
    # Find returns of all best stocks
    all_best_portfolio_returns[week_num,"Return"] =  mean(stock_returns_curr_week[all_best_stocks])
    

  }#end of curr. week
}#end of curr. year


  # esgcs
  esgcs_portfolio_returns$`All best` = all_best_portfolio_returns
  esgcs_portfolio_returns$Benchmark = benchmark_returns
  
  # esg
  esg_portfolio_returns$`All best` = all_best_portfolio_returns
  esg_portfolio_returns$Benchmark = benchmark_returns
  
  # e
  e_portfolio_returns$`All best` = all_best_portfolio_returns
  e_portfolio_returns$Benchmark = benchmark_returns
  
  # s
  s_portfolio_returns$`All best` = all_best_portfolio_returns
  s_portfolio_returns$Benchmark = benchmark_returns
  
  # g
  g_portfolio_returns$`All best` = all_best_portfolio_returns
  g_portfolio_returns$Benchmark = benchmark_returns
  
  # equal_esg
  equal_esg_portfolio_returns$`All best` = all_best_portfolio_returns
  equal_esg_portfolio_returns$Benchmark = benchmark_returns

# Statistics

# Benchmark
benchmark_annual_return = round(mean(benchmark_returns) * 5200, 2)
benchmark_annual_sd = round(sd(benchmark_returns) * sqrt(52) * 100, 2)

# All best
all_best_portfolio_annual_return = round(mean(all_best_portfolio_returns) * 5200, 2)
all_best_portfolio_annual_sd = round(sd(all_best_portfolio_returns) * sqrt(52) * 100, 2)

# Replace NaN with NA
esgcs_avg_mom_score[is.nan(esgcs_avg_mom_score)]=NA
e_avg_mom_score[is.nan(e_avg_mom_score)]=NA
s_avg_mom_score[is.nan(s_avg_mom_score)]=NA
g_avg_mom_score[is.nan(g_avg_mom_score)]=NA
esg_avg_mom_score[is.nan(esg_avg_mom_score)]=NA
equal_esg_avg_mom_score[is.nan(equal_esg_avg_mom_score)]=NA

# esgcs
annual_return_df$esgcs = round(colMeans(esgcs_portfolio_returns[,1:5]) * 5200, 2)
annual_sd_df$esgcs = round(unlist(lapply(esgcs_portfolio_returns[,1:5], sd)) * sqrt(52) * 100, 2)
avg_esg_score_df$esgcs = round(colMeans(esgcs_avg_esg_score), 2)
avg_size_score_df$esgcs = round(colMeans(esgcs_avg_size_score), 4)
avg_ptb_score_df$esgcs = round(colMeans(esgcs_avg_ptb_score), 4)
avg_mom_score_df$esgcs = round(colMeans(esgcs_avg_mom_score, na.rm = TRUE), 4)
beta_df$esgcs = round(t(CAPM.beta(esgcs_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$esgcs = round(t(CAPM.beta.bull(esgcs_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$esgcs = round(t(CAPM.beta.bear(esgcs_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Environmental
annual_return_df$e = round(colMeans(e_portfolio_returns[,1:5]) * 5200, 2)
annual_sd_df$e = round(unlist(lapply(e_portfolio_returns[,1:5], sd)) * sqrt(52) * 100, 2)
avg_esg_score_df$e = round(colMeans(e_avg_esg_score), 2)
avg_size_score_df$e = round(colMeans(e_avg_size_score), 4)
avg_ptb_score_df$e = round(colMeans(e_avg_ptb_score), 4)
avg_mom_score_df$e = round(colMeans(e_avg_mom_score, na.rm = TRUE), 4)
beta_df$e = round(t(CAPM.beta(e_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$e= round(t(CAPM.beta.bull(e_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$e = round(t(CAPM.beta.bear(e_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Social
annual_return_df$s = round(colMeans(s_portfolio_returns[,1:5]) * 5200, 2)
annual_sd_df$s = round(unlist(lapply(s_portfolio_returns[,1:5], sd)) * sqrt(52) * 100, 2)
avg_esg_score_df$s = round(colMeans(s_avg_esg_score), 2)
avg_size_score_df$s= round(colMeans(s_avg_size_score), 4)
avg_ptb_score_df$s = round(colMeans(s_avg_ptb_score), 4)
avg_mom_score_df$s = round(colMeans(s_avg_mom_score, na.rm = TRUE), 4)
beta_df$s = round(t(CAPM.beta(s_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$s = round(t(CAPM.beta.bull(s_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$s = round(t(CAPM.beta.bear(s_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Corporate Governance
annual_return_df$g = round(colMeans(g_portfolio_returns[,1:5]) * 5200, 2)
annual_sd_df$g = round(unlist(lapply(g_portfolio_returns[,1:5], sd)) * sqrt(52) * 100, 2)
avg_esg_score_df$g = round(colMeans(g_avg_esg_score), 2)
avg_size_score_df$g= round(colMeans(g_avg_size_score), 4)
avg_ptb_score_df$g = round(colMeans(g_avg_ptb_score), 4)
avg_mom_score_df$g = round(colMeans(g_avg_mom_score, na.rm = TRUE), 4)
beta_df$g = round(t(CAPM.beta(g_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$g = round(t(CAPM.beta.bull(g_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$g = round(t(CAPM.beta.bear(g_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# ESG
annual_return_df$esg = round(colMeans(esg_portfolio_returns[,1:5]) * 5200, 2)
annual_sd_df$esg = round(unlist(lapply(esg_portfolio_returns[,1:5], sd)) * sqrt(52) * 100, 2)
avg_esg_score_df$esg = round(colMeans(esg_avg_esg_score), 2)
avg_size_score_df$esg = round(colMeans(esg_avg_size_score), 4)
avg_ptb_score_df$esg = round(colMeans(esg_avg_ptb_score), 4)
avg_mom_score_df$esg = round(colMeans(esg_avg_mom_score, na.rm = TRUE), 4)
beta_df$esg = round(t(CAPM.beta(esg_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$esg = round(t(CAPM.beta.bull(esg_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$esg = round(t(CAPM.beta.bear(esg_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Equal ESG
annual_return_df$equal_esg = round(colMeans(equal_esg_portfolio_returns[,1:5]) * 5200, 2)
annual_sd_df$equal_esg = round(unlist(lapply(equal_esg_portfolio_returns[,1:5], sd)) * sqrt(52) * 100, 2)
avg_esg_score_df$equal_esg = round(colMeans(equal_esg_avg_esg_score), 2)
avg_size_score_df$equal_esg = round(colMeans(equal_esg_avg_size_score), 4)
avg_ptb_score_df$equal_esg = round(colMeans(equal_esg_avg_ptb_score), 4)
avg_mom_score_df$equal_esg = round(colMeans(equal_esg_avg_mom_score, na.rm = TRUE), 4)
beta_df$equal_esg = round(t(CAPM.beta(equal_esg_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$equal_esg = round(t(CAPM.beta.bull(equal_esg_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$equal_esg = round(t(CAPM.beta.bear(equal_esg_portfolio_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Calculate cumulative returns
cum_esgcs_portfolio_returns = cumprod(1+esgcs_portfolio_returns) 
cum_e_portfolio_returns = cumprod(1+e_portfolio_returns) 
cum_s_portfolio_returns = cumprod(1+s_portfolio_returns) 
cum_g_portfolio_returns = cumprod(1+g_portfolio_returns)
cum_esg_portfolio_returns = cumprod(1+esg_portfolio_returns)
cum_equal_esg_portfolio_returns = cumprod(1+equal_esg_portfolio_returns)

# Sharpe Ratio
sharpe_ratio_df = data.frame(matrix(NA, nrow = 5, ncol = 6))
rownames(sharpe_ratio_df) = rownames(annual_return_df)
colnames(sharpe_ratio_df) = colnames(annual_return_df)
rf_df = data.frame(matrix(1, nrow = 5, ncol = 6))
sharpe_ratio_df = round((annual_return_df - rf_df)/annual_sd_df, 2)

# Benchmark and all best portfolio statistics
benchmark_and_all_best_stat = data.frame(matrix(NA, nrow = 3, ncol = 2))
colnames(benchmark_and_all_best_stat) = c("benchmark", "all_best_portfolio")
rownames(benchmark_and_all_best_stat) = c("Average Annual Return", "Annual Standard Deviation", "Sharpe Ratio")

benchmark_and_all_best_stat$benchmark[1] = benchmark_annual_return
benchmark_and_all_best_stat$benchmark[2] = benchmark_annual_sd
benchmark_and_all_best_stat$benchmark[3] = (benchmark_annual_return - 1) / benchmark_annual_sd

benchmark_and_all_best_stat$all_best_portfolio[1] = all_best_portfolio_annual_return
benchmark_and_all_best_stat$all_best_portfolio[2] = all_best_portfolio_annual_sd
benchmark_and_all_best_stat$all_best_portfolio[3] = (all_best_portfolio_annual_return - 1) / all_best_portfolio_annual_sd

benchmark_and_all_best_stat = round(benchmark_and_all_best_stat, 2)

# Plot
plot(cum_esgcs_portfolio_returns, legend.loc = "left", main = "SP500 ESGCS Portfolios")
plot(cum_e_portfolio_returns, legend.loc = "left", main = "SP500 Environmental Portfolios")
plot(cum_s_portfolio_returns, legend.loc = "left", main = "SP500 Social Portfolios")
plot(cum_g_portfolio_returns, legend.loc = "left", main = "SP500 Corporate Governance Portfolios")
plot(cum_esg_portfolio_returns, legend.loc = "left", main = "SP500 ESG Portfolios")
plot(cum_equal_esg_portfolio_returns, legend.loc = "left", main = "SP500 Equal ESG Portfolios")

plot(esgcs_avg_esg_score, legend.loc = "left", main = "SP500 ESGCS Average Score")
plot(e_avg_esg_score, legend.loc = "left", main = "SP500 Environmental Average Score")
plot(s_avg_esg_score, legend.loc = "left", main = "SP500 Social Average Score")
plot(g_avg_esg_score, legend.loc = "left", main = "SP500 Corporate Governance Average Score")
plot(esg_avg_esg_score, legend.loc = "left", main = "SP500 ESG Average Score")
plot(equal_esg_avg_esg_score, legend.loc = "left", main = "SP500 Equal ESG Average Score")

plot(esgcs_avg_size_score, legend.loc = "left", main = "SP500 ESGCS Average Size Score")
plot(e_avg_size_score, legend.loc = "left", main = "SP500 Environmental Average Size Score")
plot(s_avg_size_score, legend.loc = "left", main = "SP500 Social Average Size Score")
plot(g_avg_size_score, legend.loc = "left", main = "SP500 Corporate Governance Average Size Score")
plot(esg_avg_size_score, legend.loc = "left", main = "SP500 ESG Average Size Score")
plot(equal_esg_avg_size_score, legend.loc = "left", main = "SP500 Equal ESG Average Size Score")

plot(esgcs_avg_ptb_score, legend.loc = "left", main = "SP500 ESGCS Average ptb Score")
plot(e_avg_ptb_score, legend.loc = "left", main = "SP500 Environmental Average ptb Score")
plot(s_avg_ptb_score, legend.loc = "left", main = "SP500 Social Average ptb Score")
plot(g_avg_ptb_score, legend.loc = "left", main = "SP500 Corporate Governance Average ptb Score")
plot(esg_avg_ptb_score, legend.loc = "left", main = "SP500 ESG Average ptb Score")
plot(equal_esg_avg_ptb_score, legend.loc = "left", main = "SP500 Equal ESG Average ptb Score")

plot(esgcs_avg_mom_score, legend.loc = "left", main = "SP500 ESGCS Average Momentum Score")
plot(e_avg_mom_score, legend.loc = "left", main = "SP500 Environmental Average Momentum Score")
plot(s_avg_mom_score, legend.loc = "left", main = "SP500 Social Average Momentum Score")
plot(g_avg_mom_score, legend.loc = "left", main = "SP500 Corporate Governance Average Momentum Score")
plot(esg_avg_mom_score, legend.loc = "left", main = "SP500 ESG Average Momentum Score")
plot(equal_esg_avg_mom_score, legend.loc = "left", main = "SP500 Equal ESG Average Momentum Score")

plot(all_best_stocks_num, main = "SP500 Number of all best companies")

plot(stocks_num_with_esg_score, legend.loc = "left", main = "SP500 Number of companies with esg score")


# Create dataframes from xts objects
stocks_num_with_esg_score_df = as.data.frame(stocks_num_with_esg_score)
correlation_df = as.data.frame(correlation)
all_best_stocks_num_df = as.data.frame(all_best_stocks_num)

cum_esgcs_portfolio_returns_df = as.data.frame(cum_esgcs_portfolio_returns)
cum_e_portfolio_returns_df = as.data.frame(cum_e_portfolio_returns)
cum_s_portfolio_returns_df = as.data.frame(cum_s_portfolio_returns)
cum_g_portfolio_returns_df = as.data.frame(cum_g_portfolio_returns)
cum_esg_portfolio_returns_df = as.data.frame(cum_esg_portfolio_returns)
cum_equal_esg_portfolio_returns_df = as.data.frame(cum_equal_esg_portfolio_returns)

esgcs_avg_esg_score_df = as.data.frame(esgcs_avg_esg_score)
e_avg_esg_score_df = as.data.frame(e_avg_esg_score)
s_avg_esg_score_df = as.data.frame(s_avg_esg_score)
g_avg_esg_score_df = as.data.frame(g_avg_esg_score)
esg_avg_esg_score_df = as.data.frame(esg_avg_esg_score)
equal_esg_avg_esg_score_df = as.data.frame(equal_esg_avg_esg_score)

esgcs_avg_size_score_df = as.data.frame(esgcs_avg_size_score)
e_avg_size_score_df = as.data.frame(e_avg_size_score)
s_avg_size_score_df = as.data.frame(s_avg_size_score)
g_avg_size_score_df = as.data.frame(g_avg_size_score)
esg_avg_size_score_df = as.data.frame(esg_avg_size_score)
equal_esg_avg_size_score_df = as.data.frame(equal_esg_avg_size_score)

esgcs_avg_ptb_score_df = as.data.frame(esgcs_avg_ptb_score)
e_avg_ptb_score_df = as.data.frame(e_avg_ptb_score)
s_avg_ptb_score_df = as.data.frame(s_avg_ptb_score)
g_avg_ptb_score_df = as.data.frame(g_avg_ptb_score)
esg_avg_ptb_score_df = as.data.frame(esg_avg_ptb_score)
equal_esg_avg_ptb_score_df = as.data.frame(equal_esg_avg_ptb_score)

esgcs_avg_mom_score_df = as.data.frame(esgcs_avg_mom_score)
e_avg_mom_score_df = as.data.frame(e_avg_mom_score)
s_avg_mom_score_df = as.data.frame(s_avg_mom_score)
g_avg_mom_score_df = as.data.frame(g_avg_mom_score)
esg_avg_mom_score_df = as.data.frame(esg_avg_mom_score)
equal_esg_avg_mom_score_df = as.data.frame(equal_esg_avg_mom_score)

save.image(file=paste0(wd, "SPCOMP.RData"))

