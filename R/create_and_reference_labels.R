

#' Extract marker type from marker name.
#'
#' @param marker_name a \code{character} string with the new marker name.
#' @return \code{character} string containing the marker type.
#' @export
extract_marker_type <- function(marker_name){
  marker_split <- strsplit(marker_name, "\\.")[[1]]
  if(length(marker_split) == 2){
    marker_type <- strsplit(marker_name, "\\.")[[1]][1]
  } else {
    stop("marker name incorrect format. Use '<type>.<marker_name>'.")
  }
  return(marker_type)
}

#' Count the number of markers with a specific marker type
#'
#' @param marker_obj \code{marker_obj}.
#' @param type \code{character} string with marker type to count.
#' @return \code{integer} with the number of markers of type \code{type}.
#' @export
get_type_count <- function(marker_obj, type){
  all_marker_type <- lapply(marker_obj, function(x) x$type)
  type_count <- sum(grepl(type, all_marker_type))
  return(type_count)
}

#' Functions to create and reference table and figure numbers.
#'
#' Takes a named list of markers and their number.
#' Adds \code{marker_name} to \code{marker_obj} with proper number.
#' The \code{marker_name} is parsed to determine the marker type, i.e. figure
#' or table based on the marker prefix.
#'
#' Each marker prefix is given a seperate counter.
#'
#' @param marker_obj Named \code{list} with caption marker and numbers.
#' @param marker_name \code{character} string with marker name. marker name
#'    should have the format <type>.<marker> where <type> is the figure or
#'    table type. For example \code{"fig.car_plot"} or
#'    \code{"tab.p_values"}.
#' @return Named \code{list} with new marker and number
#' @seealso \code{\link{extract_marker_type}}
#' @export
label <- function(marker_obj, marker_name){
  marker_type <- extract_marker_type(marker_name)
  if (marker_name %in% names(marker_obj)){
    warning("marker, '", marker_name, "' already present!")
  } else {
    previous_markers <- get_type_count(marker_obj, marker_type)
    new_marker_label <- previous_markers + 1
    new_marker <- list(
      marker = marker_name,
      type = marker_type,
      label = new_marker_label
    )
    marker_obj[[marker_name]] <- new_marker
  }
  return(marker_obj)
}

#' Reference a predefined marker and print the assigned number.
#'
#' @param marker_obj Named \code{list} containing markers and their numbers.
#' @param marker \code{character} string of marker to reference.
#' @return \code{character} containing marker number or "??" for undefined
#'  reference.
#'  @export
ref <- function(marker_obj, marker){
  marker_label <- marker_obj[[marker]]$label
  if (is.null(marker_label)){
    marker_label <- paste0("**??", marker, "??**")
    warning("No marker called ", marker)
  }

  return(marker_label)
}



