# Shared lookup tables used across summary, plotting and mapping functions.
# Defined once here rather than inline in each function.

#' Vehicle type grouping lookup table
#'
#' A data frame mapping detailed STATS19 vehicle types to simplified short
#' names and driver-type labels. Used by [summarise_vehicle_types()] and
#' [summarise_vehicles_per_collision()].
#'
#' @format A data frame with 23 rows and 3 columns:
#'   \describe{
#'     \item{vehicle_type}{Original STATS19 vehicle type label}
#'     \item{short_name}{Simplified category (e.g. \code{"Car"},
#'       \code{"Motorcycle"})}
#'     \item{driver_type}{Driver/rider label (e.g. \code{"Car driver"},
#'       \code{"Motorcyclist"})}
#'   }
#' @examples
#' head(vehicle_type_lookup)
#' @export
vehicle_type_lookup <- data.frame(
  vehicle_type = c(
    "Car", "Motorcycle 125cc and under", "Taxi/Private hire car",
    "Motorcycle over 500cc", "Motorcycle over 125cc and up to 500cc",
    "Goods 7.5 tonnes mgw and over", "Goods over 3.5t. and under 7.5t",
    "Bus or coach (17 or more pass seats)",
    "Van / Goods 3.5 tonnes mgw or under", "Motorcycle 50cc and under",
    "Other vehicle", "Pedal cycle", "Motorcycle - unknown cc",
    "Electric motorcycle", "e-scooter", "Minibus (8 - 16 passenger seats)",
    "Mobility scooter", "Unknown vehicle type (self rep only)",
    "Agricultural vehicle", "Goods vehicle - unknown weight",
    "Data missing or out of range", "Ridden horse", "Tram"
  ),
  short_name = c(
    "Car", "Motorcycle", "Taxi", "Motorcycle", "Motorcycle",
    "Goods vehicle", "Goods vehicle", "Bus", "Goods vehicle", "Motorcycle",
    "Other vehicle", "Pedal cycle", "Motorcycle", "Motorcycle", "e-scooter",
    "Bus", "Mobility scooter", "Other vehicle", "Agricultural vehicle",
    "Goods vehicle", "Data missing or out of range", "Ridden horse", "Tram"
  ),
  driver_type = c(
    "Car driver", "Motorcyclist", "Taxi driver", "Motorcyclist",
    "Motorcyclist", "Goods vehicle driver", "Goods vehicle driver",
    "Bus driver", "Goods vehicle driver", "Motorcyclist", "Other vehicle",
    "Cyclist", "Motorcyclist", "Motorcyclist", "E-scooter driver",
    "Bus driver", "Mobility scooter rider", "Other vehicle",
    "Agricultural vehicle driver", "Goods vehicle driver", "Data missing",
    "Horse rider", "Tram driver"
  )
)

#' Casualty type grouping lookup table
#'
#' A data frame mapping detailed STATS19 casualty types to simplified short
#' names and an "in or on" mode label. Used by [summarise_casualty_types()]
#' and shown in the report appendix via [tabulate_casualty_groupings()].
#'
#' @format A data frame with 22 rows and 3 columns:
#'   \describe{
#'     \item{casualty_type}{Original STATS19 casualty type label}
#'     \item{short_name}{Simplified category (e.g. \code{"Motorcyclist"})}
#'     \item{in_or_on}{Mode label (e.g. \code{"Foot"}, \code{"Bicycle"})}
#'   }
#' @examples
#' head(casualty_type_lookup)
#' @export
casualty_type_lookup <- data.frame(
  casualty_type = c(
    "Car occupant", "Motorcycle 125cc and under rider or passenger",
    "Cyclist", "Pedestrian", "Motorcycle over 500cc rider or passenger",
    "Motorcycle over 125cc and up to 500cc rider or  passenger",
    "Motorcycle 50cc and under rider or passenger",
    "Bus or coach occupant (17 or more pass seats)",
    "Taxi/Private hire car occupant",
    "Van / Goods vehicle (3.5 tonnes mgw or under) occupant",
    "Other vehicle occupant", "Data missing or out of range",
    "Motorcycle - unknown cc rider or passenger",
    "Goods vehicle (7.5 tonnes mgw and over) occupant",
    "Electric motorcycle rider or passenger",
    "Minibus (8 - 16 passenger seats) occupant",
    "Mobility scooter rider", "Horse rider",
    "Goods vehicle (over 3.5t. and under 7.5t.) occupant",
    "Goods vehicle (unknown weight) occupant",
    "Agricultural vehicle occupant", "E-scooter rider"
  ),
  short_name = c(
    "Car occupant", "Motorcyclist", "Cyclist", "Pedestrian", "Motorcyclist",
    "Motorcyclist", "Motorcyclist", "Bus occupant", "Taxi occupant",
    "Goods vehicle occupant", "Other vehicle", "Data missing",
    "Motorcyclist", "Goods vehicle occupant", "Motorcyclist",
    "Bus occupant", "Mobility scooter rider", "Horse rider",
    "Goods vehicle occupant", "Goods vehicle occupant",
    "Agricultural vehicle occupant", "E-scooter rider"
  ),
  in_or_on = c(
    "Car", "Motorcyclist", "Bicycle", "Foot", "Motorcyclist", "Motorcyclist",
    "Motorcyclist", "Bus", "Taxi", "Goods vehicle", "Other vehicle",
    "Data missing", "Motorcyclist", "Goods vehicle", "Motorcyclist",
    "Bus", "Mobility scooter", "Horse", "Goods vehicle", "Goods vehicle",
    "Agricultural vehicle", "E-scooter"
  )
)

#' DfT standard age band breaks and labels
#'
#' Age band definitions matching those used in DfT road casualty
#' publications (which differ from the bands included in the raw STATS19
#' casualty data).
#'
#' @format `dft_age_breaks` is a numeric vector of 11 cut points;
#'   `dft_age_labels` is a character vector of 10 band labels.
#' @examples
#' cut(c(8, 23, 67), breaks = dft_age_breaks, labels = dft_age_labels)
#' @export
dft_age_breaks <- c(0, 11, 15, 19, 24, 29, 39, 49, 59, 69, 100)

#' @rdname dft_age_breaks
#' @export
dft_age_labels <- c(
  "0-11", "12-15", "16-19", "20-24", "25-29",
  "30-39", "40-49", "50-59", "60-69", "70+"
)

#' Available basemap tile providers
#'
#' Character vector of basemap tile names suitable for
#' \code{tmap::tm_basemap()}.
#' @examples
#' basemap_options
#' @export
basemap_options <- c(
  "CartoDB.DarkMatter", "Stadia.AlidadeSmoothDark", "CartoDB.Positron"
)
