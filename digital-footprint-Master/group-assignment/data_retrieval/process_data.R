# [1] Load packages ----

library("httr2")
library("tidyverse")
library(dplyr)
library(ggplot2)
library(corrplot)
library(syuzhet)
library(tm)
library(textdata)


# [2] Load data ----

load(here::here("collect_data_API.RData"))


# [3] Unpack elements from list ----
# apply httr2::resp_body_json to each element to extract list

all_content <- purrr::map(all_response_objects, 
                          httr2::resp_body_json)

# Use separate object
all_data <- purrr::map(all_content, "data")
all_data <- purrr::map(all_data, "lend")
all_data <- purrr::map(all_data, "loan")



# [4] Load functions ----
# Use functions to extract elements that are in nested lists

source(here::here("aux_functions.R"))



# [5] Rearrange data in a tibble and rearrange ----

df_loans <-
  all_data %>%
  {
    tibble(
      loan_id                        = map_int(., "id"),
      name                           = map_chr(., "name"),
      description                    = map_chr(., "description"),
      use                            = map_chr(., "use", .default = NA),
      gender                         = map_chr(., "gender", .default = NA),
      disbursalDate                  = map_chr(., "disbursalDate", .default = NA),
      raisedDate                     = map_chr(., "raisedDate", .default = NA),
      plannedExpirationDate          = map_chr(., "plannedExpirationDate"),
      lenderRepaymentTerm            = map_int(., "lenderRepaymentTerm", .default = NA),
      loanAmount                     = map_chr(., "loanAmount"),
      fundraisingDate                = map_chr(., "fundraisingDate"),
      status                         = map_chr(., "status"),
      fundedAmount                   = map_chr(., f_get_fundedAmount),
      city                           = map_chr(., f_get_city),
      state                          = map_chr(., f_get_state),
      postalCode                     = map_chr(., f_get_postalCode),
      latitude                       = map_dbl(., f_get_latitude),
      longitude                      = map_dbl(., f_get_longitude),
      country                        = map_chr(., f_get_country),
      isoCode                        = map_chr(., f_get_isoCode),
      region                         = map_chr(., f_get_region),
      ppp                            = map_chr(., f_get_ppp),
      numLoansFundraisingCountry     = map_int(., f_get_numLoansFundraisingCountry),
      fundsLentInCountry             = map_int(., f_get_fundsLentInCountry),
      thumbnailImageIdVideo          = map_chr(., f_get_thumbnailImageIdVideo),
      youtubeIdVideo                 = map_chr(., f_get_youtubeIdVideo),
      repaymentInterval              = map_chr(., "repaymentInterval"),
      delinquent                     = map_lgl(., "delinquent"),
      hasCurrencyExchangeLossLenders = map_lgl(., "hasCurrencyExchangeLossLenders"),
      reservedAmount                 = map_chr(., f_get_reservedAmount),
      isExpiringSoon                 = map_lgl(., f_get_isExpiringSoon),
      researchScore                  = map_dbl(., "researchScore", .default = NA),
      imageUrl                       = map_chr (., f_get_url),
      activity                       = map_chr(., f_get_activity),
      sector                         = map_chr(., f_get_sector)
    )
  }

#Extract features that will be used/needed to process
df_loans <-
  df_loans %>%
  mutate(loan_id = as.numeric(loan_id),
         loanAmount = as.numeric(loanAmount),
         fundedAmount = as.numeric(fundedAmount),
         lenderRepaymentTerm = as.numeric(lenderRepaymentTerm),
         loanAmount = as.numeric(loanAmount),
         fundedAmount = as.numeric(fundedAmount),
         ppp = as.numeric(gsub("[\\$,]", "", ppp)),
         numLoansFundraisingCountry = as.numeric(numLoansFundraisingCountry),
         fundsLentInCountry = as.numeric(fundsLentInCountry),
         reservedAmount = as.numeric(reservedAmount),
         researchScore = as.numeric(researchScore)
         )


# Convert features into correct type
df_loans$gender <- as.factor(df_loans$gender)
df_loans$country <- as.factor(df_loans$country)
df_loans$sector <- as.factor(df_loans$sector)

# Feature engineering: Calculate new features
df_loans$fundraisingDate <- as.Date(df_loans$fundraisingDate)
df_loans$fundedAmount <- df_loans$fundedAmount/df_loans$loanAmount
df_loans <- df_loans |>
  mutate(daysSinceFundraising = as.numeric(difftime(as.Date("2024-05-15"), fundraisingDate, units = "days")))
text_corpus <- Corpus(VectorSource(df_loans$description))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))
text_corpus <- tm_map(text_corpus, stripWhitespace)
clean_text <- sapply(text_corpus, as.character)
sentiment_score <- get_nrc_sentiment(clean_text)
df_loans <- df_loans |>
  mutate(sentiment = sentiment_score$positive)


#Engineered dataframe with features used for clustering 
df_loans <- df_loans |>
  select(loan_id, sentiment, gender, lenderRepaymentTerm, loanAmount, daysSinceFundraising, 
         fundedAmount, country, researchScore, sector)


#Summary statistics of the dataframe
str(df_loans)
summary(df_loans)


#Create folder for plots, if there isnt one already
if (!file.exists("plots/")) {
  dir.create("plots/")
}


##Data visualization
#Histograms (for numerical features)
create_histogram <- function(column_name) {
  percentile_99.9 <- quantile(df_loans[[column_name]], probs = 0.999, na.rm = TRUE)
  
  plot <- ggplot(df_loans, aes(x = !!rlang::sym(column_name))) +
    geom_histogram(fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", column_name), x = column_name, y = "Frequency") +
    theme_minimal() +
    xlim(0, percentile_99.9)
  
  png(file.path(paste0("plots/", column_name, "_histogram.png")))
  print(plot)
  dev.off()
}

numeric_columns <- Filter(is.numeric, df_loans)
numeric_columns <- numeric_columns[names(numeric_columns) != "loan_id"]
for (column_name in names(numeric_columns)) {
  create_histogram(column_name)
}


# Bar plots (for categorical features)
create_barplots <- function(column_name) {
  plot <- ggplot(df_loans, aes(x = !!rlang::sym(column_name))) +
    geom_bar() +
    labs(title = paste("Bar plot of", column_name), x = column_name, y = "Frequency") +
    theme_minimal() +
    coord_flip()
  
  png(file.path(paste0("plots/", column_name, "_barPlots.png")))
  print(plot)
  dev.off()
}

categorical_columns <- setdiff(names(df_loans), names(numeric_columns))
for (column_name in categorical_columns) {
  create_barplots(column_name)  
}


#Correlation plot
df_loans_corr <- df_loans |>
  select(where(is.numeric)) |>
  na.omit() |>
  select(-loan_id)
corr_matrix <- cor(df_loans_corr, use = "complete.obs")
if (!is.null(corr_matrix)) {
  # Clear plot window and adjust text size
  png("plots/correlation_plot.png", width = 800, height = 800)
  corrplot(corr_matrix, method = "circle", tl.cex = 2)
  dev.off()
} else {
  print("Correlation matrix computation failed.")
}


# [6] Save object ----

save(df_loans, 
     file = here::here("process_data.RData"))