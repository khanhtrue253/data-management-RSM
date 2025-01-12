# [1] Get vector with unique IDs from collect_data_uniqueID.R file ----

load(here::here("collect_data_uniqueID.RData"))



# [2] Get URL ----

get_url <- "https://api.kivaws.org/graphql"



# [3] Set parameters for loop ----
# total no. loops equal to no. IDs

totalCount <- length(vec_loan_IDs) 
all_response_objects <- vector(mode = "list", length = totalCount)
request_number <- 1



# [4] Perform API requests based on unique ID's ----

while(request_number <= totalCount) {
  # use tic toc functions to see how much time it takes per request
  tictoc::tic(glue::glue("duration for request number {request_number}"))
  
  loan_id <- vec_loan_IDs[[request_number]]
  
  response <- httr2::req_perform(
    req = httr2::request(base_url = get_url) |>
      httr2::req_url_query(
        query = glue::glue(
          "{{
lend {{
    loan(id: {loan_id}) {{
      id
      description
      disbursalDate
      fundraisingDate
      raisedDate
      plannedExpirationDate
      status
      gender
      lenderRepaymentTerm
      use
      geocode {{
          city
          state
          postalCode
          latitude
          longitude
          country {{
             name
             isoCode
             region
             ppp
             numLoansFundraising
             fundsLentInCountry
              }}
      }}
      name
      video {{
          thumbnailImageId
          youtubeId
      }}
      loanAmount
      repaymentInterval
      delinquent
      hasCurrencyExchangeLossLenders
        loanFundraisingInfo {{
        fundedAmount
        reservedAmount
        isExpiringSoon
        }}
      researchScore
        image {{
          url(presetSize: small)
        }}
      activity {{
        name
        }}
      sector {{
          name
        }}
      }}
    }}
     }}"
          
        )
      ),
    verbosity = 0
  )
  
  # Print message to see if request is successfull
  
  if ((response |>
       httr2::resp_status_desc()) == "OK") {
    message("Request number ", request_number, " has status OK")
  } else {
    stop("Request number ",
         request_number,
         "status not OK, please check")
  }
  
  if (!is.null((response |>
                httr2::resp_body_json())[["errors"]])) {
    stop("Request number ",
         request_number,
         " has an error, please check")
  }
  
  # add the current response to all_response_objects
  all_response_objects[[request_number]] <- response
  names(all_response_objects)[request_number] <- paste0("request_", request_number)
  
  # increment the request number
  request_number <- request_number + 1
  
  # stop stopwatch
  tictoc::toc()
}



# [5] Save object ----

save(all_response_objects, 
     file = here::here("collect_data_API.RData"))