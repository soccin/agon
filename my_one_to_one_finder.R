# =============================================================================
# One-to-One Spatial Relationship Finder
# =============================================================================
# Written from scratch to find bidirectional one-to-one relationships
# between two spatial objects using their intersection mappings
# =============================================================================

#' Find one-to-one spatial relationships between two sets of spatial objects
#'
#' Takes the output of bidirectional spatial intersections (e.g., from st_intersects)
#' and identifies pairs where each element in set A corresponds to exactly one
#' element in set B, and vice versa.
#'
#' @param mapping List containing two sgbp objects:
#'   - First element: A-to-B intersections (e.g., points_to_rectangles)
#'   - Second element: B-to-A intersections (e.g., rectangles_to_points)
#' @return Data frame with columns A_index and B_index for one-to-one matches
#'
#' @examples
#' # Typical usage with sf objects
#' points_to_rects <- st_intersects(points_sf, rectangles_sf)
#' rects_to_points <- st_intersects(rectangles_sf, points_sf)
#' mapping <- list(points_to_rects, rects_to_points)
#' one_to_one_matches <- find_one_to_one_relationships(mapping)
#'
find_one_to_one_relationships <- function(mapping) {
  
  # Extract the two sgbp objects
  A_to_B <- mapping[[1]]  # e.g., points to rectangles
  B_to_A <- mapping[[2]]  # e.g., rectangles to points
  
  # Step 1: Find elements in A that intersect exactly one element in B
  A_single_matches <- tibble(
    A_index = seq_along(A_to_B),
    n_B_matches = lengths(A_to_B),
    B_indices = A_to_B
  ) %>%
    filter(n_B_matches == 1) %>%
    unnest(B_indices) %>%
    rename(B_index = B_indices)
  
  # Step 2: Find elements in B that intersect exactly one element in A  
  B_single_matches <- tibble(
    B_index = seq_along(B_to_A),
    n_A_matches = lengths(B_to_A),
    A_indices = B_to_A
  ) %>%
    filter(n_A_matches == 1) %>%
    unnest(A_indices) %>%
    rename(A_index = A_indices)
  
  # Step 3: Find the intersection - pairs that satisfy both conditions
  one_to_one_pairs <- A_single_matches %>%
    inner_join(B_single_matches, by = c("A_index", "B_index")) %>%
    select(A_index, B_index)
  
  # Step 4: Verification (optional but good practice)
  # Every pair should be bidirectionally consistent by construction,
  # but let's verify anyway
  verification_passed <- all(
    map2_lgl(one_to_one_pairs$A_index, one_to_one_pairs$B_index, 
             function(a_idx, b_idx) {
               # Check A -> B direction
               a_intersects_b <- b_idx %in% A_to_B[[a_idx]]
               # Check B -> A direction  
               b_intersects_a <- a_idx %in% B_to_A[[b_idx]]
               return(a_intersects_b && b_intersects_a)
             })
  )
  
  if (!verification_passed) {
    stop("FATAL ERROR: Bidirectional consistency check failed")
  }
  
  return(one_to_one_pairs)
}

# =============================================================================
# DEMONSTRATION AND TEST FUNCTIONS
# =============================================================================

#' Create mock sgbp objects for testing
create_mock_intersections <- function() {
  # Scenario: 5 points, 4 rectangles
  # Point 1 -> Rectangles [1, 2] (multiple matches)
  # Point 2 -> Rectangle [3] (single match)
  # Point 3 -> Rectangle [4] (single match)  
  # Point 4 -> No rectangles [] (no matches)
  # Point 5 -> Rectangle [1] (single match)
  
  # Rectangle 1 -> Points [1, 5] (multiple matches)
  # Rectangle 2 -> Point [1] (single match)
  # Rectangle 3 -> Point [2] (single match)
  # Rectangle 4 -> Point [3] (single match)
  
  points_to_rects <- list(
    c(1L, 2L),  # Point 1 -> Rects 1,2
    3L,         # Point 2 -> Rect 3
    4L,         # Point 3 -> Rect 4
    integer(0), # Point 4 -> No rects
    1L          # Point 5 -> Rect 1
  )
  class(points_to_rects) <- "sgbp"
  
  rects_to_points <- list(
    c(1L, 5L),  # Rect 1 -> Points 1,5
    1L,         # Rect 2 -> Point 1
    2L,         # Rect 3 -> Point 2
    3L          # Rect 4 -> Point 3
  )
  class(rects_to_points) <- "sgbp"
  
  return(list(points_to_rects, rects_to_points))
}

#' Test the one-to-one finder with mock data
test_one_to_one_finder <- function() {
  cat("Testing one-to-one relationship finder...\n")
  
  # Create test data
  mock_mapping <- create_mock_intersections()
  
  # Find one-to-one relationships
  results <- find_one_to_one_relationships(mock_mapping)
  
  # Expected results:
  # Point 2 <-> Rectangle 3 (both have single matches to each other)
  # Point 3 <-> Rectangle 4 (both have single matches to each other)
  # 
  # NOT included:
  # Point 1: intersects multiple rectangles
  # Point 5: intersects Rectangle 1, but Rectangle 1 intersects multiple points
  # Rectangle 2: intersects Point 1, but Point 1 intersects multiple rectangles
  
  cat("One-to-one matches found:\n")
  print(results)
  
  expected_matches <- 2
  if (nrow(results) == expected_matches) {
    cat("✓ Correct number of matches found\n")
    
    # Check specific matches
    if (2 %in% results$A_index && 3 %in% results$B_index) {
      cat("✓ Point 2 <-> Rectangle 3 match found\n")
    }
    if (3 %in% results$A_index && 4 %in% results$B_index) {
      cat("✓ Point 3 <-> Rectangle 4 match found\n")
    }
    
    cat("🎉 Test passed!\n")
  } else {
    cat("❌ Test failed: Expected", expected_matches, "matches, got", nrow(results), "\n")
  }
}

# =============================================================================
# ALGORITHM EXPLANATION
# =============================================================================

#' Algorithm Logic Explanation:
#' 
#' 1. FORWARD FILTER: From A_to_B mapping, find all A elements that 
#'    intersect exactly one B element
#'    
#' 2. REVERSE FILTER: From B_to_A mapping, find all B elements that
#'    intersect exactly one A element
#'    
#' 3. INTERSECTION: Find pairs (A_i, B_j) that appear in both filtered sets
#'    This ensures BOTH conditions are met simultaneously:
#'    - A_i intersects only B_j
#'    - B_j intersects only A_i
#'    
#' 4. VERIFICATION: Double-check that each pair is truly bidirectional
#'    (This should always pass by construction, but good to verify)
#'
#' Time Complexity: O(n + m + k) where n=|A|, m=|B|, k=number of intersections
#' Space Complexity: O(k) for intermediate tibbles

# Run test if sourced interactively
# if (interactive()) {
#   library(tidyverse)
#   test_one_to_one_finder()
# }