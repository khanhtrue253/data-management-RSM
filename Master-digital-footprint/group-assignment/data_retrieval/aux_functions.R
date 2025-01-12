f_get_fundedAmount <- function(input_list) {
    if(is.null(input_list[["loanFundraisingInfo"]][["fundedAmount"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["loanFundraisingInfo"]][["fundedAmount"]])	
    }
}

f_get_city <- function(input_list) {
    if(is.null(input_list[["geocode"]][["city"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["city"]])	
    }
}


f_get_state <- function(input_list) {
    if(is.null(input_list[["geocode"]][["state"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["state"]])	
    }
}

f_get_postalCode <- function(input_list) {
    if(is.null(input_list[["geocode"]][["postalCode"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["postalCode"]])	
    }
}

f_get_latitude <- function(input_list) {
    if(is.null(input_list[["geocode"]][["latitude"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["latitude"]])	
    }
}


f_get_longitude <- function(input_list) {
    if(is.null(input_list[["geocode"]][["longitude"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["longitude"]])	
    }
}


f_get_country <- function(input_list) {
    if(is.null(input_list[["geocode"]][["country"]][["name"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["country"]][["name"]])	
    }
}



f_get_isoCode <- function(input_list) {
    if(is.null(input_list[["geocode"]][["country"]][["isoCode"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["country"]][["isoCode"]])	
    }
}


f_get_region <- function(input_list) {
    if(is.null(input_list[["geocode"]][["country"]][["region "]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["country"]][["region "]])	
    }
}


f_get_ppp <- function(input_list) {
    if(is.null(input_list[["geocode"]][["country"]][["ppp"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["country"]][["ppp"]])	
    }
}


f_get_numLoansFundraisingCountry <- function(input_list) {
    if(is.null(input_list[["geocode"]][["country"]][["numLoansFundraising"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["country"]][["numLoansFundraising"]])	
    }
}


f_get_fundsLentInCountry <- function(input_list) {
    if(is.null(input_list[["geocode"]][["country"]][["fundsLentInCountry"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["country"]][["fundsLentInCountry"]])	
    }
}



f_get_country <- function(input_list) {
    if(is.null(input_list[["geocode"]][["country"]][["name"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["geocode"]][["country"]][["name"]])	
    }
}


f_get_thumbnailImageIdVideo <- function(input_list) {
    if(is.null(input_list[["video"]][["thumbnailImageId"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["video"]][["thumbnailImageId"]])	
    }
}

f_get_youtubeIdVideo <- function(input_list) {
    if(is.null(input_list[["video"]][["youtubeId"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["video"]][["youtubeId"]])	
    }
}


f_get_reservedAmount <- function(input_list) {
    if(is.null(input_list[["loanFundraisingInfo"]][["reservedAmount"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["loanFundraisingInfo"]][["reservedAmount"]])	
    }
}

f_get_isExpiringSoon <- function(input_list) {
    if(is.null(input_list[["loanFundraisingInfo"]][["isExpiringSoon"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["loanFundraisingInfo"]][["isExpiringSoon"]])	
    }
}



f_get_url <- function(input_list) {
    if(is.null(input_list[["image"]][["url"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["image"]][["url"]])	
    }
}

f_get_activity <- function(input_list) {
    if(is.null(input_list[["activity"]][["name"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["activity"]][["name"]])	
    }
}

f_get_sector <- function(input_list) {
    if(is.null(input_list[["sector"]][["name"]])) {
        # you can change the label 
        return(NA)
    } else {
        return(input_list[["sector"]][["name"]])	
    }
}

