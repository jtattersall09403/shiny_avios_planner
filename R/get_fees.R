# Get pricing information from Amadeus flights API
# Example URL to GET:
# https://test.api.amadeus.com/v2/shopping/flight-offers?originLocationCode=HEL&destinationLocationCode=BKK&departureDate=2023-01-01&adults=1&travelClass=BUSINESS&includedAirlineCodes=AY&nonStop=true&currencyCode=GBP&max=250
# Example return
# {
#   "meta": {
#     "count": 2,
#     "links": {
#       "self": "https://test.api.amadeus.com/v2/shopping/flight-offers?originLocationCode=HEL&destinationLocationCode=BKK&departureDate=2023-01-01&adults=1&travelClass=BUSINESS&includedAirlineCodes=AY&nonStop=true&currencyCode=GBP&max=250"
#     }
#   },
#   "data": [
#     {
#       "type": "flight-offer",
#       "id": "1",
#       "source": "GDS",
#       "instantTicketingRequired": false,
#       "nonHomogeneous": false,
#       "oneWay": false,
#       "lastTicketingDate": "2022-11-07",
#       "numberOfBookableSeats": 9,
#       "itineraries": [
#         {
#           "duration": "PT11H40M",
#           "segments": [
#             {
#               "departure": {
#                 "iataCode": "HEL",
#                 "at": "2023-01-01T00:45:00"
#               },
#               "arrival": {
#                 "iataCode": "BKK",
#                 "at": "2023-01-01T17:25:00"
#               },
#               "carrierCode": "AY",
#               "number": "143",
#               "aircraft": {
#                 "code": "359"
#               },
#               "operating": {
#                 "carrierCode": "AY"
#               },
#               "duration": "PT11H40M",
#               "id": "1",
#               "numberOfStops": 0,
#               "blacklistedInEU": false
#             }
#           ]
#         }
#       ],
#       "price": {
#         "currency": "GBP",
#         "total": "1747.40", # <----- This is the bit you want! Total price minus base  = taxes and fees.
#         "base": "1722.00",
#         "fees": [
#           {
#             "amount": "0.00",
#             "type": "SUPPLIER"
#           },
#           {
#             "amount": "0.00",
#             "type": "TICKETING"
#           }
#         ],
#         "grandTotal": "1747.40"
#       },
#       "pricingOptions": {
#         "fareType": [
#           "PUBLISHED"
#         ],
#         "includedCheckedBagsOnly": true
#       },
#       "validatingAirlineCodes": [
#         "AY"
#       ],
#       "travelerPricings": [
#         {
#           "travelerId": "1",
#           "fareOption": "STANDARD",
#           "travelerType": "ADULT",
#           "price": {
#             "currency": "GBP",
#             "total": "1747.40",
#             "base": "1722.00"
#           },
#           "fareDetailsBySegment": [
#             {
#               "segmentId": "1",
#               "cabin": "BUSINESS",
#               "fareBasis": "RNN0S9CZ",
#               "brandedFare": "BCLASSIC",
#               "class": "R",
#               "includedCheckedBags": {
#                 "quantity": 2
#               }
#             }
#           ]
#         }
#       ]
#     },
#     {
#       "type": "flight-offer",
#       "id": "2",
#       "source": "GDS",
#       "instantTicketingRequired": false,
#       "nonHomogeneous": false,
#       "oneWay": false,
#       "lastTicketingDate": "2022-11-07",
#       "numberOfBookableSeats": 3,
#       "itineraries": [
#         {
#           "duration": "PT11H40M",
#           "segments": [
#             {
#               "departure": {
#                 "iataCode": "HEL",
#                 "at": "2023-01-01T14:45:00"
#               },
#               "arrival": {
#                 "iataCode": "BKK",
#                 "at": "2023-01-02T07:25:00"
#               },
#               "carrierCode": "AY",
#               "number": "141",
#               "aircraft": {
#                 "code": "359"
#               },
#               "operating": {
#                 "carrierCode": "AY"
#               },
#               "duration": "PT11H40M",
#               "id": "2",
#               "numberOfStops": 0,
#               "blacklistedInEU": false
#             }
#           ]
#         }
#       ],
#       "price": {
#         "currency": "GBP",
#         "total": "1747.40",
#         "base": "1722.00",
#         "fees": [
#           {
#             "amount": "0.00",
#             "type": "SUPPLIER"
#           },
#           {
#             "amount": "0.00",
#             "type": "TICKETING"
#           }
#         ],
#         "grandTotal": "1747.40"
#       },
#       "pricingOptions": {
#         "fareType": [
#           "PUBLISHED"
#         ],
#         "includedCheckedBagsOnly": true
#       },
#       "validatingAirlineCodes": [
#         "AY"
#       ],
#       "travelerPricings": [
#         {
#           "travelerId": "1",
#           "fareOption": "STANDARD",
#           "travelerType": "ADULT",
#           "price": {
#             "currency": "GBP",
#             "total": "1747.40",
#             "base": "1722.00"
#           },
#           "fareDetailsBySegment": [
#             {
#               "segmentId": "2",
#               "cabin": "BUSINESS",
#               "fareBasis": "RNN0S9CZ",
#               "brandedFare": "BCLASSIC",
#               "class": "R",
#               "includedCheckedBags": {
#                 "quantity": 2
#               }
#             }
#           ]
#         }
#       ]
#     }
#   ],
#   "dictionaries": {
#     "locations": {
#       "BKK": {
#         "cityCode": "BKK",
#         "countryCode": "TH"
#       },
#       "HEL": {
#         "cityCode": "HEL",
#         "countryCode": "FI"
#       }
#     },
#     "aircraft": {
#       "359": "AIRBUS A350-900"
#     },
#     "currencies": {
#       "GBP": "POUND STERLING"
#     },
#     "carriers": {
#       "AY": "FINNAIR"
#     }
#   }
# }

library(httr)
library(rjson)

get_token <- function() {
  amadeus_api_key_prod <- Sys.getenv("AMADEUS_API_KEY") 
  amadeus_api_secret_prod <- Sys.getenv("AMADEUS_SECRET") 
  
  # Get Token
  response <- POST("https://test.api.amadeus.com/v1/security/oauth2/token",
                   add_headers("Content-Type" = "application/x-www-form-urlencoded"),
                   body = list(
                     "grant_type"     = "client_credentials",
                     "client_id"      = amadeus_api_key_prod,
                     "client_secret"  = amadeus_api_secret_prod),
                   encode = "form")
  
  rsp_content <- content(response, as = "parsed", type = "application/json")
  access_token <- paste0("Bearer ", rsp_content$access_token)
  
  # Return
  access_token
}

get_fees <- function(access_token, origin, dest, dep_date, max_num_flights, airline_code, travel_class) {

  
  # origin <- "HEL"
  # dest <- "DEL"
  # dep_date <- "2022-11-12"
  # return_date <- ""
  # max_num_flights <- 1
  # airline_code <- "AY"
  # travel_class <- "BUSINESS"
  url <- paste0("https://test.api.amadeus.com/v2/shopping/flight-offers?originLocationCode=", 
                origin, 
                "&destinationLocationCode=",
                dest,
                "&departureDate=",
                dep_date,
                "&max=",
                max_num_flights,
                "&travelClass=",
                travel_class,
                "&includedAirlineCodes=",
                airline_code,
                "&adults=1&nonStop=true&currencyCode=GBP")
  
  # Get flight info
  response <- GET(url,
                  add_headers("Content-Type" = "application/x-www-form-urlencoded",
                              "Authorization" = access_token),
                  encode = "form")
  
  response
  rsp_content <- content(response, as = "parsed", type = "application/json")
  
  if (length(rsp_content$data) > 0) {
    price_data <- rsp_content$data[[1]]$price
    charges <- as.numeric(price_data$total) - as.numeric(price_data$base)
  } else {
    charges <- NA
  }
  
  # Return
  charges

}
