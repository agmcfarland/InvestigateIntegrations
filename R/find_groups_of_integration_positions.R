#' Find Groups of Integration Positions
#'
#' This function identifies groups of positions where consecutive values in a sorted vector
#' are within a specified overlap range. It can also filter groups based on a minimum group size.
#'
#' @param positions_to_check A numeric vector of positions to be grouped. Must contain more than one value.
#' @param overlap_search_range A numeric value indicating the maximum allowable distance between
#' consecutive positions for them to be considered part of the same group. Default is 10,000.
#' @param min_group_size An integer specifying the minimum size of a group to include in the results.
#' Groups smaller than this size will be excluded. Default is 1 (no filtering).
#'
#' @return A list of numeric vectors, where each vector represents a group of positions.
#' If `min_group_size` is greater than 1, only groups meeting this size requirement will be included.
#'
#' @export
find_groups_of_integration_positions <- function(positions_to_check, overlap_search_range = 10000, min_group_size = 1) {

  positions_to_check_sorted <- sort(positions_to_check)

  total_positions <- length(positions_to_check_sorted)

  testthat::expect_gt(total_positions, 1, label = 'positions_to_check must be be a vector with more than one item.')

  grouped_positions <- list()

  sorted_index_id_ <- 1

  group_id_ <- 1

  local_group <- c(positions_to_check_sorted[1])

  for (position_ in positions_to_check_sorted) {

    if (position_ + overlap_search_range >= positions_to_check_sorted[sorted_index_id_ + 1]) {

      local_group <- c(local_group, positions_to_check_sorted[sorted_index_id_ + 1])

    } else {

      grouped_positions[[group_id_]] <- local_group

      group_id_ <- group_id_ + 1

      local_group <- c(positions_to_check_sorted[sorted_index_id_ + 1])

    }

    sorted_index_id_ <- sorted_index_id_ + 1

    if (sorted_index_id_ == total_positions) {

      grouped_positions[[group_id_]] <- local_group

      break
    }

  }

  if (min_group_size > 1) {

    grouped_positions_filtered <- list()

    group_id_ <- 1

    for (i_ in grouped_positions) {

      if (length(i_) > 1) {

        grouped_positions_filtered[[group_id_]] <- i_
        }
    }
    return(grouped_positions_filtered)
  }

  return(grouped_positions)

  }

