# R SVG Interpreter for R Logo
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
      # Move to command
      if (length(current_path) > 0) {
        paths[[length(paths) + 1]] <- do.call(rbind, current_path)
      }
      current_path <- list()
      current_x <- as.numeric(tokens[i])
      current_y <- as.numeric(tokens[i+1])
      current_path[[length(current_path) + 1]] <- c(current_x, current_y)
      i <- i + 2
    } else if (cmd == "C") {
      # Cubic Bezier curve
      while (i + 5 <= n && !tokens[i] %in% c("M", "C", "L", "Z")) {
        x1 <- as.numeric(tokens[i])
        y1 <- as.numeric(tokens[i+1])
        x2 <- as.numeric(tokens[i+2])
        y2 <- as.numeric(tokens[i+3])
        x <- as.numeric(tokens[i+4])
        y <- as.numeric(tokens[i+5])
        # Generate points along the cubic Bezier curve
        t_vals <- seq(0, 1, length.out = 15)
        for (t in t_vals) {
          # Cubic Bezier formula: B(t) = (1-t)^3*P0 + 3(1-t)^2*t*P1 + 3(1-t)*t^2*P2 + t^3*P3
          x_point <- (1-t)^3 * current_x + 3*(1-t)^2*t * x1 + 3*(1-t)*t^2 * x2 + t^3 * x
          y_point <- (1-t)^3 * current_y + 3*(1-t)^2*t * y1 + 3*(1-t)*t^2 * y2 + t^3 * y
          current_path[[length(current_path) + 1]] <- c(x_point, y_point)
        }
        current_x <- x
        current_y <- y
        i <- i + 6
      }
    } else if (cmd == "L") {
      # Line to command
      while (i + 1 <= n && !tokens[i] %in% c("M", "C", "L", "Z")) {
        current_x <- as.numeric(tokens[i])
        current_y <- as.numeric(tokens[i+1])
        current_path[[length(current_path) + 1]] <- c(current_x, current_y)
        i <- i + 2
      }
    } else if (cmd == "Z") {
      # Close path (connect to first point)
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

# Rasterize a polygon with a diagonal gradient fill
rasterize_polygon_gradient <- function(paths, gradient_colors, resolution = 100) {
  # Only works for a single outer path and optional holes (evenodd)
  if (length(paths) == 0) return()
  outer <- paths[[1]]
  holes <- if (length(paths) > 1) paths[2:length(paths)] else list()
  
  # Bounding box
  min_x <- floor(min(outer[,1])); max_x <- ceiling(max(outer[,1]))
  min_y <- floor(min(outer[,2])); max_y <- ceiling(max(outer[,2]))
  
  # Calculate grid step based on resolution and image dimensions
  # Higher resolution = smaller step = better quality but slower
  image_width <- max_x - min_x
  image_height <- max_y - min_y
  step <- max(1, min(image_width, image_height) / resolution)
  
  xs <- seq(min_x, max_x, by=step)
  ys <- seq(min_y, max_y, by=step)
  
  # For each grid point, check if inside (evenodd)
  for (x in xs) {
    for (y in ys) {
      pt <- c(x, y)
      inside <- point_in_polygon(pt, outer)
      if (length(holes) > 0) {
        for (h in holes) {
          if (point_in_polygon(pt, h)) inside <- !inside
        }
      }
      if (inside) {
        # Diagonal gradient position (0 to 1)
        # Normalize position from top-left (0) to bottom-right (1)
        diag_pos <- ((x - min_x) / (max_x - min_x) + (y - min_y) / (max_y - min_y)) / 2
        diag_pos <- max(0, min(1, diag_pos))  # Clamp to [0,1]
        
        # Map to gradient color index
        color_idx <- 1 + round(diag_pos * (length(gradient_colors)-1))
        color_idx <- max(1, min(length(gradient_colors), color_idx))
        
        rect(x, y, x+step, y+step, col=gradient_colors[color_idx], border=NA)
      }
    }
  }
}

# Function to draw polygon with simple fill (no gradient)
draw_simple_polygon <- function(paths, gradient_colors, gradient_color = "first") {
  if (length(paths) == 0) return()
  
  # Select color from gradient based on gradient_color argument
  if (gradient_color == "first") {
    color_idx <- 1
  } else if (gradient_color == "middle") {
    color_idx <- round(length(gradient_colors) / 2)
  } else if (gradient_color == "last") {
    color_idx <- length(gradient_colors)
  } else {
    # Default to first if invalid option
    color_idx <- 1
  }
  
  # Ensure color_idx is within bounds
  color_idx <- max(1, min(length(gradient_colors), color_idx))
  fill_color <- gradient_colors[color_idx]
  
  # For evenodd rule, we need to handle the paths in the correct order
  # The first path is the outer boundary, subsequent paths are holes
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
plot_r_logo <- function(method = "polygon", resolution = 100, gradient_color = "first") {
  #' Plot the R logo from Rlogo.svg using base R graphics.
  #'
  #' @param method Drawing method: "polygon" (default, fast, no gradient) or "raster" (slower, with gradient)
  #' @param resolution Resolution for raster method (higher = better quality, slower; default 100)
  #' @param gradient_color For polygon method: "first" (default), "middle", or "last" gradient color
  #'
  #' Usage:
  #'   plot_r_logo()  # Fast polygon method (no gradient)
  #'   plot_r_logo(gradient_color = "last")  # Polygon method with last gradient color
  #'   plot_r_logo(method = "raster")  # Raster method with default resolution
  #'   plot_r_logo(method = "raster", resolution = 300)  # High-quality raster
  #'   plot_r_logo(method = "raster", resolution = 50)   # Fast raster, lower quality
  
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
          cat("Using gradientFill-1 with", length(grad_colors), "colors\n")
        } else {
          cat("Warning: gradientFill-1 not found, using default color\n")
        }
      } else if (!is.null(fill_url) && grepl("gradientFill-2", fill_url)) {
        if ("gradientFill-2" %in% names(gradients)) {
          grad_colors <- create_gradient_color(
            gradients[["gradientFill-2"]]$start,
            gradients[["gradientFill-2"]]$end
          )
          cat("Using gradientFill-2 with", length(grad_colors), "colors\n")
        } else {
          cat("Warning: gradientFill-2 not found, using default color\n")
        }
      }
      
      # Draw the polygon with appropriate fill rule
      if (method == "raster") {
        # Use rasterization for gradient effects
        rasterize_polygon_gradient(path_list, grad_colors, resolution=resolution)
      } else {
        # Use polygon-based drawing (faster, no gradient)
        draw_simple_polygon(path_list, grad_colors, gradient_color=gradient_color)
      }
    }
  }
  
  # Add title
  title(paste0("R Logo - SVG Interpreter (", method, " method)"), line = -2)
} 
