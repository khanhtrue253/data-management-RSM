# [1] Get URL ----

get_url <- "https://api.kivaws.org/graphql"



# [2] Create filter ----
# Check how many total observations there are with filter settings

response_allresults <-
  httr2::req_perform(
    req = httr2::request(base_url = get_url) |>
      httr2::req_url_query(
        query = '{
                            lend {
                                loans (sortBy: newest, limit : 1100) {
                                            totalCount
                                            values {
                                                id
                                            }
                                       }
                                }
                        }'
      ),
    verbosity = 1
  )


# Save output into list 
content_allresults <- response_allresults |>
  httr2::resp_body_json()



# [3] Use a loop and offset ----
# based on the previous query, I know the total number of results with 
# filter set

totalCount <- content_allresults[["data"]][["lend"]][["loans"]][["totalCount"]]

#> I can get max 100 IDs per request, so I need this many requests:
n_per_request <- 100
N_requests <- totalCount %/% n_per_request + 
  as.integer((totalCount %% n_per_request) > 0)

# Create object to store unique IDs
all_response_objects <- vector(mode = "list", length = N_requests)
request_number <- 1    



tictoc::tic("while loop takes this much:")

while (request_number <= N_requests) {
  # use tic toc functions to see how much time it takes per request
  tictoc::tic(glue::glue("duration for request number {request_number}"))
  
  current_offset <- (request_number - 1) * n_per_request
  
  response <- httr2::req_perform(
    req = httr2::request(base_url = get_url) |>
      httr2::req_url_query(
        query = glue::glue(
          "{{
										lend {{
											loans (offset: {current_offset},
													limit: {n_per_request},
                                    				sortBy: newest) {{
                                    					totalCount
                                            	values {{
                                            				id
                                            			}}
                                    				}}
                                		}}
								   }}"
        )
      ),
    verbosity = 0
  )
  
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
         "has an error, please check")
  }
  
  # add the current response to all_response_objects
  all_response_objects[[request_number]] <- response
  names(all_response_objects)[request_number] <- paste0("request_", request_number)
  
  # increment the request number
  request_number <- request_number + 1
  
  tictoc::toc()
}
tictoc::toc()



#> check that you have as many unique loan IDs as you were expecting
all_content <- purrr::map(all_response_objects, httr2::resp_body_json)
all_content <- purrr::map(all_content, "data")
all_content <- purrr::map(all_content, "lend")
all_content <- purrr::map(all_content, "loans")
all_content <- purrr::map(all_content, "values")
all_content <- purrr::flatten(all_content)

vec_loan_IDs <- purrr::map_int(all_content, "id")



# [4] Save object ----

save(vec_loan_IDs, file = here::here("collect_data_uniqueID.RData"))