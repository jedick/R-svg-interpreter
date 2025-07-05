# R SVG Interpreter for R Logo - Improved Version
# This script reads Rlogo.svg and plots it using base R graphics

library(xml2)

# Function to parse SVG path commands and handle complex paths with holes
parse_path <- function(path_d) {
  # Tokenize: split into command letters and numbers
  tokens <- unlist(regmatches(path_d, gregexpr("[MCLZ]|-?\\d*\\.?\\d+(?:e[-+]?\\d+)?", path_d, perl=TRUE)))
  i <- 1
  n <- length(tokens)
  paths <- list()
  current_path <- list()
  current_x <- 0
  current_y <- 0
  while (i <= n) {
    cmd <- tokens[i]
    i <- i + 1
    if (cmd == "M") {
      if (length(current_path) > 0) {
        paths[[length(paths) + 1]] <- do.call(rbind, current_path)
      }
      current_path <- list()
      current_x <- as.numeric(tokens[i])
      current_y <- as.numeric(tokens[i+1])
      current_path[[length(current_path) + 1]] <- c(current_x, current_y)
      i <- i + 2
    } else if (cmd == "C") {
      while (i + 5 <= n && !tokens[i] %in% c("M", "C", "L", "Z")) {
        x1 <- as.numeric(tokens[i])
        y1 <- as.numeric(tokens[i+1])
        x2 <- as.numeric(tokens[i+2])
        y2 <- as.numeric(tokens[i+3])
        x <- as.numeric(tokens[i+4])
        y <- as.numeric(tokens[i+5])
        t_vals <- seq(0, 1, length.out = 15)
        for (t in t_vals) {
          x_point <- (1-t)^3 * current_x + 3*(1-t)^2*t * x1 + 3*(1-t)*t^2 * x2 + t^3 * x
          y_point <- (1-t)^3 * current_y + 3*(1-t)^2*t * y1 + 3*(1-t)*t^2 * y2 + t^3 * y
          current_path[[length(current_path) + 1]] <- c(x_point, y_point)
        }
        current_x <- x
        current_y <- y
        i <- i + 6
      }
    } else if (cmd == "L") {
      while (i + 1 <= n && !tokens[i] %in% c("M", "C", "L", "Z")) {
        current_x <- as.numeric(tokens[i])
        current_y <- as.numeric(tokens[i+1])
        current_path[[length(current_path) + 1]] <- c(current_x, current_y)
        i <- i + 2
      }
    } else if (cmd == "Z") {
      if (length(current_path) > 0) {
        current_path[[length(current_path) + 1]] <- current_path[[1]]
      }
    }
  }
  if (length(current_path) > 0) {
    paths[[length(paths) + 1]] <- do.call(rbind, current_path)
  }
  return(paths)
}

# Function to create gradient color
create_gradient_color <- function(start_color, end_color, n_points = 100) {
  # Parse RGB colors
  start_rgb <- as.numeric(strsplit(gsub("rgb\\(|\\)", "", start_color), ",")[[1]])
  end_rgb <- as.numeric(strsplit(gsub("rgb\\(|\\)", "", end_color), ",")[[1]])
  
  # Create gradient
  colors <- sapply(1:n_points, function(i) {
    t <- (i - 1) / (n_points - 1)
    r <- round(start_rgb[1] * (1-t) + end_rgb[1] * t)
    g <- round(start_rgb[2] * (1-t) + end_rgb[2] * t)
    b <- round(start_rgb[3] * (1-t) + end_rgb[3] * t)
    rgb(r, g, b, maxColorValue = 255)
  })
  
  return(colors)
}

# Function to check if a point is inside a polygon (for evenodd rule)
point_in_polygon <- function(point, polygon) {
  x <- point[1]
  y <- point[2]
  n <- nrow(polygon)
  inside <- FALSE
  
  j <- n
  for (i in 1:n) {
    if (((polygon[i,2] > y) != (polygon[j,2] > y)) &&
        (x < (polygon[j,1] - polygon[i,1]) * (y - polygon[i,2]) / 
         (polygon[j,2] - polygon[i,2]) + polygon[i,1])) {
      inside <- !inside
    }
    j <- i
  }
  return(inside)
}

# Function to draw polygon with evenodd fill rule
draw_evenodd_polygon <- function(paths, gradient_colors) {
  if (length(paths) == 0) return()
  
  # Use the first color from gradient for simplicity
  fill_color <- gradient_colors[1]
  
  # For evenodd rule, we need to determine which areas to fill
  # This is a simplified approach - we'll draw the outer path and subtract inner paths
  if (length(paths) >= 2) {
    # Draw the outer path (first path)
    if (nrow(paths[[1]]) >= 3) {
      polygon(paths[[1]][,1], paths[[1]][,2], col = fill_color, border = NA)
    }
    
    # Draw inner paths (holes) in background color to create the hole effect
    for (i in 2:length(paths)) {
      if (nrow(paths[[i]]) >= 3) {
        polygon(paths[[i]][,1], paths[[i]][,2], col = "white", border = NA)
      }
    }
  } else {
    # Single path, just draw it
    if (nrow(paths[[1]]) >= 3) {
      polygon(paths[[1]][,1], paths[[1]][,2], col = fill_color, border = NA)
    }
  }
}

# Main function to read and plot SVG
plot_r_logo <- function() {
  # Read the SVG file
  svg_content <- readLines("Rlogo.svg")
  svg_text <- paste(svg_content, collapse = "\n")
  
  # Parse XML using xml2
  doc <- read_xml(svg_text)
  
  # Find paths using xml2's XPath support
  paths <- xml_find_all(doc, "//*[local-name()='path']")
  cat("Found", length(paths), "paths\n")
  
  # Extract gradients
  gradients <- list()
  gradient_nodes <- xml_find_all(doc, "//*[local-name()='linearGradient']")
  cat("Found", length(gradient_nodes), "gradient nodes\n")
  
  for (grad in gradient_nodes) {
    grad_id <- xml_attr(grad, "id")
    cat("Processing gradient:", grad_id, "\n")
    
    # Find stops within this gradient
    stops <- xml_find_all(grad, ".//*[local-name()='stop']")
    cat("Found", length(stops), "stops\n")
    
    if (length(stops) >= 2) {
      start_color <- xml_attr(stops[[1]], "stop-color")
      end_color <- xml_attr(stops[[2]], "stop-color")
      cat("Colors:", start_color, "to", end_color, "\n")
      gradients[[grad_id]] <- list(start = start_color, end = end_color)
    }
  }
  
  cat("Found", length(gradients), "gradients\n")
  
  # Set up the plot
  par(mar = c(0, 0, 0, 0))
  plot(0, 0, type = "n", xlim = c(0, 724), ylim = c(561, 0), 
       asp = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Process each path
  for (i in 1:length(paths)) {
    path <- paths[[i]]
    path_d <- xml_attr(path, "d")
    fill_url <- xml_attr(path, "fill")
    fill_rule <- xml_attr(path, "fill-rule")
    
    cat("Processing path", i, "with fill-rule:", fill_rule, "\n")
    cat("Fill URL:", fill_url, "\n")
    
    # Parse the path (returns list of subpaths)
    path_list <- parse_path(path_d)
    
    cat("Path", i, "has", length(path_list), "subpaths\n")
    for (j in 1:length(path_list)) {
      if (nrow(path_list[[j]]) > 0) {
        cat("  Subpath", j, "has", nrow(path_list[[j]]), "points, first few:", 
            head(path_list[[j]], 3), "\n")
      }
    }
    
    if (length(path_list) > 0) {
      # Get gradient colors with error handling
      grad_colors <- "gray"  # default
      
      if (!is.null(fill_url) && grepl("gradientFill-1", fill_url)) {
        if ("gradientFill-1" %in% names(gradients)) {
          grad_colors <- create_gradient_color(
            gradients[["gradientFill-1"]]$start,
            gradients[["gradientFill-1"]]$end
          )
        } else {
          cat("Warning: gradientFill-1 not found, using default color\n")
        }
      } else if (!is.null(fill_url) && grepl("gradientFill-2", fill_url)) {
        if ("gradientFill-2" %in% names(gradients)) {
          grad_colors <- create_gradient_color(
            gradients[["gradientFill-2"]]$start,
            gradients[["gradientFill-2"]]$end
          )
        } else {
          cat("Warning: gradientFill-2 not found, using default color\n")
        }
      }
      
      # Draw the polygon with appropriate fill rule
      if (fill_rule == "evenodd") {
        draw_evenodd_polygon(path_list, grad_colors)
      } else {
        # Default fill rule
        for (path_coords in path_list) {
          if (nrow(path_coords) >= 3) {
            polygon(path_coords[,1], path_coords[,2], col = grad_colors[1], border = NA)
          }
        }
      }
    }
  }
  
  # Add title
  title("R Logo - SVG Interpreter", line = -2)
}

# Run the interpreter
cat("Interpreting Rlogo.svg and plotting with base R graphics...\n")
plot_r_logo()
cat("Done! The R logo should now be displayed.\n") 