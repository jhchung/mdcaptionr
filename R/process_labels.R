#' Create a table of all markers used in the document.
#'
#' Create a table that lists all markers by type and number. The columns include
#' \code{c("Marker", "Type", "Label")}
#'
#' @param marker_obj A \code{list} containing all markers.
#' @return \code{data.frame} with marker name, marker type, and label number.
#' @export
summarize_markers <- function(marker_obj){
  marker_table <- plyr::ldply(marker_obj) %>%
    dplyr::rename(Marker = .id, Label = V1) %>%
    dplyr::mutate(Type = laply(Marker, extract_marker_type))  %>%
    dplyr::arrange(Type, Label) %>%
    dplyr::select(Marker, Type, Label)
  return(marker_table)
}

#' Check if a label prefix is a supplemental figure
#'
#' Check if Label ends with \code{"S"} or contains \code{"Supplemental"}. Case
#' insensitive.
#'
#' @param prefix \code{character} string with caption label prefix.
#' @return \code{boolean}.
#' @export
check_if_prefix_is_supplemental <- function(prefix){
  contains_supplemental <- grepl("sup", prefix, ignore.case = TRUE)
  is_supplemental <- any(ends_in_s, contains_supplemental)
  return(is_supplemental)
}

#' Check if prefix ends in S.
#' @param prefix \code{character} string with caption prefix.
#' @return \code{boolean}
prefix_ends_in_s <- function(prefix){
  prefix_check <- grepl(" s", substr(prefix, nchar(prefix) - 1, nchar(prefix)),
                        ignore.case = TRUE)
  return(prefix_check)
}

#' For printing labels in a caption or other text environment.
#'
#' @param marker_obj \code{list} containing the markers and their numbers.
#' @param marker \code{character}. Name of the marker to print.
#' @param prefix \code{character} string with label prefix.
#' @return \code{character} string with complete caption label
#' @export
#' @seealso \code{\link{check_if_prefix_is_supplemental}}
print_label <- function(marker_obj, marker, prefix){
  ends_with_s <- prefix_ends_in_s(prefix)
  if(ends_with_s){
    output_label <- paste0(prefix, ref(marker_obj, marker), ":")
  } else {
    output_label <- paste0(prefix, " ", ref(marker_obj, marker), ":")
  }
  return(output_label)
}

#' Print caption with caption label.
#'
#' @param marker_obj \code{marker_obj} containing markers and their labels.
#' @param marker \code{character} string with marker name.
#' @param prefix \code{character} string with caption label prefix.
#' @param caption \code{character} string with caption text.
#' @return \code{character} string with full caption.
#' @export
print_caption <- function(marker_obj, marker, prefix, caption){
  output_caption <- paste(print_label(marker_obj, marker, prefix), caption)
  return(output_caption)
}
