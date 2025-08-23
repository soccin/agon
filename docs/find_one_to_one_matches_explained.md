# Understanding the `find_one_to_one_matches` Function

## Overview

The `find_one_to_one_matches` function identifies perfect 1:1 correspondences between Cortana cell points and Halo rectangles. This is crucial for spatial analysis as it finds cases where a cell point is located in exactly one rectangle, and that rectangle contains exactly one cell point.

## The Goal

Find pairs where:
- **Cortana point** is in exactly **one** Halo rectangle 
- **That Halo rectangle** contains exactly **one** Cortana point
- This creates a perfect 1:1 correspondence

## Function Implementation

```r
find_one_to_one_matches <- function(mapping) {
  xx=mapping$cortana_to_halo %>%
    filter(Nch==1) %>%
    unnest(C2H) %>%
    left_join(mapping$halo_to_cortana,by=c(C2H="Halo")) %>%
    filter(Nhc==1) %>%
    unnest(H2C) %>%
    rename(Halo=C2H)
  n_missMatch <- xx %>% filter(Cortana!=H2C) %>% nrow
  if(n_missMatch>0) {
    rlang::abort("FATAL ERROR::miss matched h2c<->c2h")
  }
  xx %>% select(Cortana,Halo)
}
```

## Step-by-Step Walkthrough

### Input Data Structure

The `mapping` parameter contains bidirectional intersection mappings:

```r
mapping$cortana_to_halo:
  Cortana | Nch | C2H
  1       | 2   | [1,3]     # Point 1 is in rectangles 1 and 3
  2       | 1   | [2]       # Point 2 is in rectangle 2 only
  3       | 0   | []        # Point 3 is in no rectangles

mapping$halo_to_cortana:
  Halo | Nhc | H2C  
  1    | 1   | [1]         # Rectangle 1 contains point 1 only
  2    | 1   | [2]         # Rectangle 2 contains point 2 only  
  3    | 2   | [1,4]       # Rectangle 3 contains points 1 and 4
```

Where:
- `Nch` = Number of rectangles containing each Cortana point
- `Nhc` = Number of Cortana points in each Halo rectangle
- `C2H` = List of rectangles containing each point
- `H2C` = List of points in each rectangle

### Step 1: Find Cortana Points in Exactly One Rectangle

```r
mapping$cortana_to_halo %>%
  filter(Nch==1) %>%
  unnest(C2H)
```

**Purpose**: Identify points that have unambiguous rectangle assignment

**Result**:
```
Cortana | Nch | C2H
2       | 1   | 2     # Only point 2 qualifies (single rectangle)
```

**Filtered Out**: Point 1 (in 2 rectangles), Point 3 (in 0 rectangles)

### Step 2: Join with Rectangle Data

```r
left_join(mapping$halo_to_cortana, by=c(C2H="Halo"))
```

**Purpose**: Add information about how many points each rectangle contains

**Result**:
```
Cortana | Nch | C2H | Nhc | H2C
2       | 1   | 2   | 1   | [2]   # Rectangle 2 info joined in
```

### Step 3: Keep Only Rectangles with Exactly One Point

```r
filter(Nhc==1)
```

**Purpose**: Ensure the rectangle also has unambiguous point assignment

**Result**: Same as above (rectangle 2 has `Nhc==1`)

**Would Filter Out**: Any rectangle containing multiple points

### Step 4: Expand Rectangle-to-Points Mapping

```r
unnest(H2C)
```

**Purpose**: Convert list column to individual rows for consistency checking

**Result**:
```
Cortana | Nch | C2H | Nhc | H2C
2       | 1   | 2   | 1   | 2     # Rectangle 2 contains point 2
```

### Step 5: Verify Bidirectional Consistency

```r
n_missMatch <- xx %>% filter(Cortana!=H2C) %>% nrow
if(n_missMatch>0) {
  rlang::abort("FATAL ERROR::miss matched h2c<->c2h")
}
```

**Purpose**: Verify that the forward mapping (point→rectangle) and reverse mapping (rectangle→point) are consistent

**Check**: Does `Cortana` (original point ID) equal `H2C` (point ID from rectangle's perspective)?

In our example: `Cortana=2` and `H2C=2` ✅

**Error Case**: If somehow the bidirectional mappings were inconsistent, this would catch data corruption

### Step 6: Return Clean Results

```r
xx %>% select(Cortana, Halo)
```

**Final Output**:
```
Cortana | Halo
2       | 2      # Point 2 ↔ Rectangle 2 (perfect 1:1 match)
```

## Why This Logic is Mathematically Sound

The algorithm implements a **bidirectional filter** that ensures:

1. **Forward Constraint**: Point is in exactly 1 rectangle (`Nch==1`)
2. **Reverse Constraint**: That rectangle contains exactly 1 point (`Nhc==1`)  
3. **Consistency Verification**: The relationship is symmetric (`Cortana==H2C`)

This creates the intersection of two sets:
- **Set A**: Points with single rectangle membership
- **Set B**: Rectangles with single point membership  
- **Verification**: Ensures A∩B relationships are bidirectionally consistent

## Edge Cases Handled

| Scenario | Handling | Result |
|----------|----------|--------|
| Point in multiple rectangles | Filtered out by `Nch==1` | Excluded |
| Rectangle with multiple points | Filtered out by `Nhc==1` | Excluded |  
| Point in no rectangles | Filtered out by `Nch==1` | Excluded |
| Rectangle with no points | Filtered out by `Nhc==1` | Excluded |
| Data corruption/inconsistency | Caught by consistency check | Error thrown |

## Visual Example

```
Scenario 1: Perfect 1:1 Match
┌─────────┐    ┌─────────────┐
│ Point A │────│ Rectangle 1 │
└─────────┘    └─────────────┘
Point A → Rectangle 1 (only)
Rectangle 1 → Point A (only)
✅ One-to-one match found

Scenario 2: Point in Multiple Rectangles  
┌─────────┐    ┌─────────────┐
│ Point B │────│ Rectangle 2 │
└─────────┘    └─────────────┘
     │         ┌─────────────┐
     └─────────│ Rectangle 3 │
               └─────────────┘
Point B → Rectangles 2,3
❌ Filtered out at Nch==1

Scenario 3: Rectangle with Multiple Points
┌─────────┐    ┌─────────────┐
│ Point C │────│ Rectangle 4 │
└─────────┘    └─────────────┘
┌─────────┐           │
│ Point D │───────────┘
└─────────┘
Rectangle 4 → Points C,D  
❌ Filtered out at Nhc==1
```

## Algorithm Complexity

- **Time Complexity**: O(n + m) where n = number of points, m = number of rectangles
- **Space Complexity**: O(k) where k = number of intersection relationships
- **Scalability**: Efficient for large datasets due to vectorized operations

## Usage in Analysis Pipeline

```r
# Complete workflow
intersections <- find_intersections(rect_sf, points_sf)
mapping <- create_intersection_mapping(intersections)
one_to_one <- find_one_to_one_matches(mapping)

cat("Perfect matches found:", nrow(one_to_one), "\n")
```

The function is essential for identifying high-confidence spatial relationships between cell detection systems (Cortana) and region definitions (Halo).