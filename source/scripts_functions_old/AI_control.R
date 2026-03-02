# Step Program Validator
# This script checks a classification step program for:
# 1. Circular references (loops)
# 2. Broken paths (references to non-existent steps)
# 3. Unused nodes (steps that are never reached)

validate_step_program <- function(step_list) {
  # Extract all step numbers as a reference set
  all_steps <- sapply(step_list, function(x) {
    if(x$type == "STEP") return(x$step_number)
    return(NULL)
  })
  all_steps <- unlist(all_steps[!sapply(all_steps, is.null)])
  # Remove "STEP" text if present and strip whitespace
  all_steps <- gsub("STEP", "", all_steps)
  all_steps <- trimws(all_steps)
  
  cat("Found", length(all_steps), "steps in the program.\n")
  
  # Create a map of step numbers to indices in the list
  step_map <- sapply(1:length(step_list), function(i) {
    if(step_list[[i]]$type == "STEP") {
      step_num <- gsub("STEP", "", step_list[[i]]$step_number)
      return(trimws(step_num))
    }
    return(NULL)
  })
  step_map <- step_map[!sapply(step_map, is.null)]
  step_indices <- setNames(which(sapply(step_list, function(x) x$type == "STEP")), step_map)
  
  # Track referenced steps and build dependency graph
  referenced_steps <- character(0)
  dependency_graph <- list()
  
  # Process each STEP
  for(i in 1:length(step_list)) {
    if(step_list[[i]]$type != "STEP") next
    
    current_step <- trimws(gsub("STEP", "", step_list[[i]]$step_number))
    dependency_graph[[current_step]] <- character(0)
    
    # Check next_step references from answers
    if(!is.null(step_list[[i]]$answers)) {
      for(a in 1:length(step_list[[i]]$answers)) {
        answer <- step_list[[i]]$answers[[a]]
        
        if(!is.null(answer$next_step) && !is.na(answer$next_step) && answer$next_step != "NA") {
          # Add to referenced steps
          if(answer$next_step != "SLEUTEL") {
            referenced_steps <- c(referenced_steps, answer$next_step)
            # Add to dependency graph
            dependency_graph[[current_step]] <- c(dependency_graph[[current_step]], answer$next_step)
          }
        }
        
        # Check for other_key references
        if(!is.null(answer$other_key) && !is.na(answer$other_key) && answer$other_key != "NA") {
          referenced_steps <- c(referenced_steps, answer$other_key)
        }
      }
    }
  }
  
  # Check for broken paths
  broken_paths <- setdiff(referenced_steps, c(all_steps, "SLEUTEL", "BOSSEN_STRUWELEN"))
  if(length(broken_paths) > 0) {
    cat("\nBROKEN PATHS DETECTED:\n")
    cat("The following step references don't exist in the program:\n")
    print(unique(broken_paths))
  } else {
    cat("\nNo broken paths detected.\n")
  }
  
  # Check for unused nodes
  unused_nodes <- setdiff(all_steps, referenced_steps)
  # Remove "0" as it's typically the root node
  unused_nodes <- setdiff(unused_nodes, "0")
  
  if(length(unused_nodes) > 0) {
    cat("\nUNUSED NODES DETECTED:\n")
    cat("The following steps are never referenced by any other step:\n")
    print(unused_nodes)
  } else {
    cat("\nNo unused nodes detected (excluding the root node).\n")
  }
  
  # Check for circular references using DFS
  detect_cycles <- function(graph) {
    visited <- rep(FALSE, length(graph))
    names(visited) <- names(graph)
    stack <- rep(FALSE, length(graph))
    names(stack) <- names(graph)
    cycles <- list()
    
    dfs <- function(node, path = c()) {
      if(!visited[node]) {
        visited[node] <- TRUE
        stack[node] <- TRUE
        path <- c(path, node)
        
        for(neighbor in graph[[node]]) {
          if(neighbor %in% names(graph)) {
            if(!visited[neighbor]) {
              result <- dfs(neighbor, path)
              if(!is.null(result)) return(result)
            } else if(stack[neighbor]) {
              # Found a cycle
              cycle_start <- which(path == neighbor)
              return(c(path[cycle_start:length(path)], neighbor))
            }
          }
        }
        
        stack[node] <- FALSE
        return(NULL)
      }
      return(NULL)
    }
    
    for(node in names(graph)) {
      if(!visited[node]) {
        cycle <- dfs(node)
        if(!is.null(cycle)) cycles <- append(cycles, list(cycle))
      }
    }
    
    return(cycles)
  }
  
  cycles <- detect_cycles(dependency_graph)
  
  if(length(cycles) > 0) {
    cat("\nCIRCULAR REFERENCES DETECTED:\n")
    cat("The following cycles were found in the step program:\n")
    for(i in 1:length(cycles)) {
      cat("Cycle", i, ":", paste(cycles[[i]], collapse = " -> "), "\n")
    }
  } else {
    cat("\nNo circular references detected.\n")
  }
  
  # Return results
  return(list(
    broken_paths = unique(broken_paths),
    unused_nodes = unused_nodes,
    cycles = cycles
  ))
}

# Example usage:
# result <- validate_step_program(my_step_list)
# If your list is called 'step_list', run:
#result <- validate_step_program(step_list)


# Now let's create a visualization function to generate a graph of the step program
visualize_step_program <- function(step_list, output_file = "step_program_graph.pdf") {
  if(!requireNamespace("igraph", quietly = TRUE)) {
    install.packages("igraph")
    library(igraph)
  }
  library(igraph)
  
  # Extract all edges (source -> target)
  edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  
  for(i in 1:length(step_list)) {
    if(step_list[[i]]$type != "STEP") next
    
    current_step <- trimws(gsub("STEP", "", step_list[[i]]$step_number))
    
    # Check next_step references from answers
    if(!is.null(step_list[[i]]$answers)) {
      for(a in 1:length(step_list[[i]]$answers)) {
        answer <- step_list[[i]]$answers[[a]]
        
        if(!is.null(answer$next_step) && !is.na(answer$next_step) && 
           answer$next_step != "NA" && answer$next_step != "SLEUTEL") {
          edges <- rbind(edges, data.frame(
            from = current_step,
            to = answer$next_step,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Create graph
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  # Set layout and plot
  layout <- layout_with_fr(g)
  pdf(output_file, width = 10, height = 10)
  plot(g, layout = layout, 
       vertex.size = 8,
       vertex.label.cex = 0.8,
       edge.arrow.size = 0.5,
       main = "Step Program Flow Graph")
  dev.off()
  
  cat("Graph visualization saved to", output_file, "\n")
  return(g)
}