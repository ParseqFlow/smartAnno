#' @name smartAnno-package
#' @title Single-cell RNA-seq Cell Type Annotation
#' @description Functions for intelligent cell type annotation using AI models
#' @importFrom dplyr filter arrange group_by slice_head ungroup distinct left_join select mutate summarise desc case_when n %>%
#' @importFrom future plan multisession
#' @importFrom furrr future_imap
#' @importFrom purrr map_dfr map
#' @importFrom stringr str_match str_detect str_replace str_extract str_remove_all str_trim str_squish
#' @importFrom tibble tibble deframe
#' @importFrom utils head write.csv str
#' @importFrom Seurat FindAllMarkers
NULL

# Declare global variables to avoid R CMD check NOTEs
utils::globalVariables(c(
  "p_val_adj", "cluster", "avg_log2FC", "gene", "type_pattern", 
  "cell_type", "subtype", "confidence", "top_genes", "status", 
  "tokens", "attempts", "timestamp", "model", "high_confidence", 
  "total_clusters"
))

#' Single-cell Cell Type Annotation Function
#'
#' This function performs intelligent cell type annotation for single-cell RNA-seq data
#' using AI models. It can work with Seurat objects or pre-computed marker genes.
#'
#' @param seurat_object Seurat object (can be NULL if markers parameter is provided)
#' @param background Sample background information description
#' @param gene_number Number of top genes to select per cluster, default 10
#' @param specific_genes User-specified gene list, optional
#' @param p_threshold P-value threshold, default 0.05
#' @param markers Pre-computed marker gene results (optional, skips FindAllMarkers if provided)
#' @param selected_clusters Select specific clusters for annotation, default NULL (annotate all clusters)
#' @param api_key API key for the AI service
#' @param base_url Base URL for the API
#' @param model_name Name of the AI model to use
#' @param api_format API format, default "openai"
#' @param workers Number of parallel worker processes, default 4
#' @param max_retries Maximum number of retry attempts, default 3
#' @param temperature API temperature parameter, default 0.3
#' @param max_tokens Maximum number of tokens for API, default 4000
#' @param time_out Request timeout in seconds, default 200
#' @param retry_delay Retry delay in seconds, default 1
#' @param log_file Optional log file path to record API requests and responses, default NULL
#' @return A data frame containing annotation results with columns: cluster, cell_type, subtype, confidence, top_genes
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with Seurat object
#' result <- annotate_cell_types(
#'   seurat_object = pbmc,
#'   background = "Human PBMC sample",
#'   api_key = "your_api_key",
#'   base_url = "your_api_url",
#'   model_name = "gpt-4"
#' )
#' 
#' # Usage with specific clusters
#' result <- annotate_cell_types(
#'   seurat_object = pbmc,
#'   background = "Human PBMC sample",
#'   selected_clusters = c("0", "1", "3"),
#'   api_key = "your_api_key",
#'   base_url = "your_api_url",
#'   model_name = "gpt-4"
#' )
#' }
annotate_cell_types <- function(seurat_object = NULL,
                               background,
                               gene_number = 10,
                               specific_genes = NULL,
                               p_threshold = 0.05,
                               markers = NULL,
                               selected_clusters = NULL,
                               api_key,
                               base_url,
                               model_name,
                               api_format = "openai",
                               workers = 4,
                               max_retries = 3,
                               temperature = 0.3,
                               max_tokens = 4000,
                               time_out = 200,
                               retry_delay = 1,
                               log_file = NULL) {

  cat("=== Starting single-cell annotation analysis ===\n")
  cat("Sample background:", background, "\n")
  cat("Gene number:", gene_number, "\n")
  cat("P-value threshold:", p_threshold, "\n")

  # Step 1: Get marker genes (use existing results or recalculate)
    if (is.null(markers)) {
      cat("\nStep 1: Finding differentially expressed genes...\n")

    if (is.null(seurat_object)) {
      stop("If markers parameter is not provided, seurat_object parameter must be provided")
    }

    markers <- FindAllMarkers(
      object = seurat_object,
      test.use = "wilcox",
      only.pos = TRUE,
      logfc.threshold = 0.25,
      min.pct = 0.1
    )

    cat("Found", nrow(markers), "differentially expressed genes\n")
  } else {
    cat("\nStep 1: Using provided marker gene results\n")
      cat("Existing", nrow(markers), "differentially expressed genes\n")
  }

  # Step 2: Filter significant genes
    cat("\nStep 2: Filtering genes with p-value <", p_threshold, "...\n")

  significant_markers <- markers %>%
    filter(p_val_adj < p_threshold) %>%
    arrange(cluster, desc(avg_log2FC))

  cat("Remaining", nrow(significant_markers), "significant genes after filtering\n")

  # Step 3: Select top genes for each cluster
    cat("\nStep 3: Selecting top", gene_number, "genes for each cluster...\n")

  top_markers <- significant_markers %>%
    group_by(cluster) %>%
    slice_head(n = gene_number) %>%
    ungroup()

  # Step 4: Handle user-specified genes
    if (!is.null(specific_genes)) {
      cat("\nStep 4: Adding user-specified genes:", paste(specific_genes, collapse = ", "), "\n")

    # Check if specified genes are in marker list
    specific_markers <- significant_markers %>%
      filter(gene %in% specific_genes)

    if (nrow(specific_markers) > 0) {
      # Merge top genes and specified genes, remove duplicates
      top_markers <- rbind(top_markers, specific_markers) %>%
        distinct(cluster, gene, .keep_all = TRUE)

      cat("Successfully added", nrow(specific_markers), "specified genes\n")
    } else {
      cat("Warning: Specified genes not found in significant markers\n")
    }
  }

  # Step 5: Prepare parallel AI annotation
    cat("\nStep 5: Starting AI cell type annotation...\n")

  # Prepare gene list
  gene_list <- top_markers %>%
    group_by(cluster) %>%
    arrange(desc(avg_log2FC)) %>%
    summarise(genes = list(as.character(gene)), .groups = "drop") %>%
    tibble::deframe()

  # Filter specified clusters
  if (!is.null(selected_clusters)) {
    selected_clusters <- as.character(selected_clusters)
    valid_clusters <- intersect(selected_clusters, names(gene_list))

    if (length(valid_clusters) == 0) {
      stop("No valid clusters available for processing!")
    }

    if (length(valid_clusters) < length(selected_clusters)) {
      missing <- setdiff(selected_clusters, names(gene_list))
      warning(paste("The following clusters do not exist:", paste(missing, collapse = ", ")))
    }

    gene_list <- gene_list[valid_clusters]
  }

  cat("Clusters to be annotated:", paste(names(gene_list), collapse = ", "), "\n")
    cat("Using", workers, "parallel processes\n")

  # Set up parallel computing
  plan(multisession, workers = workers, .cleanup = FALSE)

  # Define annotation function for single cluster
  annotate_single_cluster <- function(cluster_genes, cluster_id) {
    result <- list(
      cluster = cluster_id,
      status = "error",
      message = "Initialization failed",
      content = NA,
      tokens = 0,
      attempts = 0
    )

    for (attempt in 1:max_retries) {
      result$attempts <- attempt

      tryCatch({
        start_time <- Sys.time()
        cat(sprintf("[%s] Cluster %s starting request (attempt %d/%d)\n",
                    Sys.time(), cluster_id, attempt, max_retries))

        # Build prompt
        prompt <- paste(
          "You are a senior biomedical professor specializing in single-cell data analysis.",
          "The user is conducting single-cell annotation analysis and will provide:",
          "1. Sample background:", background,
          "2. The top", length(cluster_genes), "highly expressed genes in cluster", cluster_id, ":", paste(cluster_genes, collapse = ", "),
          "\n\nIMPORTANT OUTPUT FORMAT REQUIREMENTS:",
          "- You MUST start your response with EXACTLY this format: >CellType (subtype)<",
          "- Replace 'CellType' with the main cell type (e.g., Cardiomyocyte, Fibroblast, T cell)",
          "- Replace 'subtype' with the specific subtype (e.g., activated, stressed, naive)",
          "- If subtype is unclear, use 'unspecified': >CellType (unspecified)<",
          "- If cell type is uncertain, use: >Uncertain (unknown)<",
          "- Do NOT include any text before the >< format",
          "- Examples: >Cardiomyocyte (stressed)<, >T cell (activated)<, >Fibroblast (unspecified)<",
          "\n\nBased on the marker genes provided, determine the most likely cell type and subtype.",
          "Start your response immediately with the required format, then provide brief justification."
        )

        # Call AI model
        ai_result <- call_ai_model(
          model_name = model_name,
          prompt = prompt,
          api_key = api_key,
          base_url = base_url,
          api_format = api_format,
          max_tokens = max_tokens,
          temperature = temperature,
          log_file = log_file
        )

        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cat(sprintf("[%s] Cluster %s received response, time elapsed %.1fs\n",
                    Sys.time(), cluster_id, elapsed))

        if (ai_result$success) {
          result <- list(
            cluster = cluster_id,
            status = "success",
            message = "OK",
            content = ai_result$content,
            tokens = 0,  # Add token information here if API returns it
            attempts = attempt
          )
          break
        } else {
          stop(ai_result$error)
        }

      }, error = function(e) {
        result$message <<- if (grepl("Timeout", e$message)) {
          sprintf("Attempt %d timed out", attempt)
        } else {
          sprintf("Attempt %d failed: %s", attempt, e$message)
        }

        # If 4xx error, do not retry
        if (grepl("4[0-9]{2}", result$message)) {
          result$message <<- paste(result$message, "(no retry)")
          attempt <<- max_retries
        }

        # Wait before retry
        if (attempt < max_retries) Sys.sleep(retry_delay)
      })
    }

    return(result)
  }

  # Prepare message list for parallel processing
    cat("\nStarting parallel processing...\n")

  # Use future_imap for parallel processing
  results <- future_imap(
    gene_list,
    ~ {
      Sys.sleep(0.3)  # Avoid too frequent API requests
      annotate_single_cluster(.x, .y)
    },
    .progress = TRUE
  )

  # Process results
  annotation_results <- map_dfr(results, ~ {
    tibble(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      cluster = .x$cluster,
      status = .x$status,
      message = .x$message,
      content = ifelse(is.na(.x$content), "", .x$content),
      tokens = .x$tokens,
      attempts = .x$attempts
    )
  })

  # Parse AI response, extract cell type and subtype
  annotation_results <- annotation_results %>%
    mutate(
      # Extract core content - fixed regex to handle >CellType format without requiring <
      type_pattern = ">\\s*([^\n]+)",
      extracted = str_match(content, type_pattern)[,2],

      # Clean results
      celltype = case_when(
        status != "success" ~ "failed",
        is.na(extracted) ~ "unknown",
        # Handle trailing punctuation
        str_detect(extracted, "[:;,.]+$") ~ str_replace(extracted, "[:;,.]+$", ""),
        TRUE ~ extracted
      ) %>% str_squish(),

      # Separate type and subtype
      cell_type = case_when(
        celltype == "failed" ~ "failed",
        celltype == "unknown" ~ "unknown",
        str_detect(celltype, "\\(") ~ str_extract(celltype, "^[^(]+") %>% str_trim(),
        TRUE ~ celltype
      ),

      subtype = case_when(
        celltype == "failed" ~ "failed",
        celltype == "unknown" ~ "unknown",
        str_detect(celltype, "\\(") ~ str_extract(celltype, "\\(([^)]+)\\)") %>% str_remove_all("[()]"),
        TRUE ~ "unknown"
      ),

      confidence = case_when(
        status == "success" & !is.na(extracted) ~ "high",
        status == "success" & is.na(extracted) ~ "unknown",
        TRUE ~ "failed"
      )
    )

  # Add gene information
  gene_info <- map_dfr(names(gene_list), ~ {
    tibble(
      cluster = .x,
      top_genes = paste(gene_list[[.x]], collapse = "; ")
    )
  })

  annotation_results <- annotation_results %>%
    left_join(gene_info, by = "cluster") %>%
    select(cluster, cell_type, subtype, confidence, top_genes, content, status, message, tokens, attempts, timestamp)

  # Check results
  if (all(annotation_results$status != "success")) {
    cat("\033[31mPlease check if the api_key or URL is filled in correctly!\033[0m\n")
  }

  # Show success statistics
  success_count <- sum(annotation_results$status == "success")
  total_count <- nrow(annotation_results)
  cat(sprintf("\nAnnotation completed: %d/%d clusters successfully annotated\n", success_count, total_count))

  cat("\n=== Annotation completed ===\n")

  # Return data frame format directly, consistent with multi_model_annotate
  return(annotation_results)
}

#' Display Annotation Results Summary
#'
#' This function displays a summary of cell type annotation results including
#' statistics on confidence levels and success rates.
#'
#' @param annotation_results Annotation results data frame
#' @export
#' @examples
#' \dontrun{
#' show_annotation_summary(results)
#' }
show_annotation_summary <- function(annotation_results) {
  cat("\n=== Cell Type Annotation Results Summary ===\n")

  results_df <- annotation_results

  for (i in 1:nrow(results_df)) {
    row <- results_df[i, ]
    cat("\nCluster", row$cluster, ":\n")
    cat("  Cell type:", row$cell_type, "(", row$subtype, ")\n")
    cat("  Confidence:", row$confidence, "\n")
    cat("  Key genes:", substr(row$top_genes, 1, 100), "...\n")
  }

  # Statistics
  cat("\n=== Statistics ===\n")
  cat("Total clusters:", nrow(results_df), "\n")
  cat("Successful annotations:", sum(results_df$confidence == "high"), "\n")
  cat("Uncertain annotations:", sum(results_df$confidence == "unknown"), "\n")
  cat("API failures:", sum(results_df$confidence == "failed"), "\n")
}

#' Export Annotation Results
#'
#' This function exports cell type annotation results to a CSV file.
#'
#' @param annotation_results Annotation results data frame
#' @param output_file Output file path, default "cell_type_annotation_results.csv"
#' @export
#' @examples
#' \dontrun{
#' export_annotation_results(results, "my_results.csv")
#' }
export_annotation_results <- function(annotation_results, output_file = "cell_type_annotation_results.csv") {
  write.csv(annotation_results, output_file, row.names = FALSE)
  cat("Annotation results saved to:", output_file, "\n")
}

# Usage examples
if (FALSE) {
  # Example usage

  # Set API parameters
  api_key <- "your_api_key_here"
  base_url <- "https://api.openai.com"  # or other API endpoint
  model_name <- "gpt-4"  # or other model

  # Execute annotation (all clusters)
  results <- annotate_cell_types(
    seurat_object = scRNA_merge_undoublet_harmony,
    background = "Human peripheral blood mononuclear cells (PBMC) sample",
    gene_number = 10,
    specific_genes = c("CD3D", "CD8A", "CD4"),  # optional specific genes
    p_threshold = 0.05,
    api_key = api_key,
    base_url = base_url,
    model_name = model_name
  )
  
  # Or annotate only specific clusters
  results_selected <- annotate_cell_types(
    seurat_object = scRNA_merge_undoublet_harmony,
    background = "Human peripheral blood mononuclear cells (PBMC) sample",
    selected_clusters = c("0", "1", "3"),  # only annotate clusters 0, 1, 3
    gene_number = 10,
    p_threshold = 0.05,
    api_key = api_key,
    base_url = base_url,
    model_name = model_name
  )

  # Show results
  show_annotation_summary(results)

  # Export results
  export_annotation_results(results)
}

#' Multi-model Cell Type Annotation Function
#'
#' This function performs cell type annotation using multiple AI models and compares results.
#' It automatically detects API format based on model names and processes annotations in parallel.
#' Additionally, it can generate a detailed log file containing all API requests and responses.
#'
#' @param model_name Character vector containing multiple model names, e.g. c("gpt-4o", "claude-3-5-haiku-20241022")
#' @param markers Marker gene data frame
#' @param background Sample background description
#' @param api_key API key for the AI service
#' @param base_url Base URL for the API
#' @param selected_clusters Select specific clusters for annotation, default NULL (annotate all clusters)
#' @param gene_number Number of genes to select per cluster, default 20
#' @param p_threshold P-value threshold, default 0.01
#' @param workers Number of parallel worker processes, default 3
#' @param max_retries Maximum number of retry attempts, default 5
#' @param temperature Temperature parameter, default 0.1
#' @param max_tokens Maximum number of tokens, default 1000
#' @param time_out Timeout in seconds, default 60
#' @param retry_delay Retry delay in seconds, default 1
#' @param output_prefix Output file prefix, default "multi_model"
#' @param log_file Path to log file for recording all API requests and responses, default NULL (no logging)
#' @return A data frame containing annotation results from all models with columns: model, cluster, cell_type, subtype, confidence, top_genes
#' @export
#' @examples
#' \dontrun{
#' # Multi-model annotation
#' results <- multi_model_annotate(
#'   model_name = c("gpt-4", "claude-3-sonnet"),
#'   markers = marker_genes,
#'   background = "Human PBMC sample",
#'   api_key = "your_api_key",
#'   base_url = "your_api_url"
#' )
#' }
multi_model_annotate <- function(model_name,
                                markers,
                                background,
                                api_key,
                                base_url,
                                selected_clusters = NULL,
                                gene_number = 20,
                                p_threshold = 0.01,
                                workers = 3,
                                max_retries = 5,
                                temperature = 0.1,
                                max_tokens = 1000,
                                time_out = 60,
                                retry_delay = 1,
                                output_prefix = "multi_model",
                                log_file = NULL) {
  
  # Check input parameters
  if (length(model_name) < 1) {
    stop("At least 1 model must be provided")
  }

  cat(sprintf("\n=== Starting multi-model annotation (%d models) ===\n", length(model_name)))
  cat("Model list:", paste(model_name, collapse = ", "), "\n")
  
  # Initialize log file if specified
  if (!is.null(log_file)) {
    # Create log file with header
    log_header <- paste0(
      "=== smartAnno Multi-Model Annotation Log ===\n",
      "Start Time: ", Sys.time(), "\n",
      "Models: ", paste(model_name, collapse = ", "), "\n",
      "Background: ", background, "\n",
      "Parameters: gene_number=", gene_number, ", p_threshold=", p_threshold, 
      ", temperature=", temperature, ", max_tokens=", max_tokens, "\n",
      "==========================================\n\n"
    )
    writeLines(log_header, log_file)
    cat("Log file initialized:", log_file, "\n")
  }
  
  # Store annotation results from all models
  all_annotations <- list()
  
  # Process each model in loop
  for (i in seq_along(model_name)) {
    current_model <- model_name[i]
    cat(sprintf("\n--- Processing model %d/%d: %s ---\n", i, length(model_name), current_model))
    
    # Auto-detect API format using the updated function
    api_format <- get_api_format(current_model)
    
    cat("Detected API format:", api_format, "\n")
    
    # Execute annotation
    tryCatch({
      result <- annotate_cell_types(
        markers = markers,
        background = background,
        selected_clusters = selected_clusters,
        gene_number = gene_number,
        p_threshold = p_threshold,
        api_key = api_key,
        base_url = base_url,
        model_name = current_model,
        api_format = api_format,
        workers = workers,
        max_retries = max_retries,
        temperature = temperature,
        max_tokens = max_tokens,
        time_out = time_out,
        retry_delay = retry_delay,
        log_file = log_file
      )
      
      # Extract annotation results and add model identifier
      model_annotation <- result %>%
        select(cluster, cell_type, subtype, confidence, top_genes) %>%
        mutate(model = current_model) %>%
        select(model, cluster, cell_type, subtype, confidence, top_genes)
      
      all_annotations[[current_model]] <- model_annotation
      
      cat(sprintf("[SUCCESS] Model %s annotation completed\n", current_model))
      
    }, error = function(e) {
      cat(sprintf("[ERROR] Model %s annotation failed: %s\n", current_model, e$message))
      
      # Create failed annotation records for all clusters
      unique_clusters <- unique(markers$cluster)
      failed_annotation <- data.frame(
        model = current_model,
        cluster = unique_clusters,
        cell_type = "failed",
        subtype = "failed",
        confidence = "failed",
        top_genes = "API call failed",
        stringsAsFactors = FALSE
      )
      
      all_annotations[[current_model]] <- failed_annotation
    })
  }
  
  # Check if there are successful results
  if (length(all_annotations) == 0) {
    stop("All models failed to annotate")
  }
  
  # Check if all models failed (all have confidence == "failed")
  all_failed <- all(sapply(all_annotations, function(x) all(x$confidence == "failed")))
  if (all_failed) {
    cat("[WARNING] All models failed, but returning failure records for analysis\n")
  }
  
  # Count successful models
  successful_models <- sum(sapply(all_annotations, function(x) !all(x$confidence == "failed")))
  cat(sprintf("\n=== Merging results (%d successful models) ===\n", successful_models))
  
  # Merge annotation results from all models
  final_results <- do.call(rbind, all_annotations)
  
  # Reorder
  final_results <- final_results %>%
    arrange(model, as.numeric(cluster))
  
  # Show results summary
   cat("\n=== Annotation Results Summary ===\n")
   cat("Total records:", nrow(final_results), "\n")
   cat("Successful models:", paste(names(all_annotations), collapse = ", "), "\n")
  
  # Detailed statistics by model
  success_stats <- final_results %>%
    group_by(model) %>%
    summarise(
      total_clusters = n(),
      high_confidence = sum(confidence == "high"),
      unknown_confidence = sum(confidence == "unknown"),
      api_failed = sum(confidence == "failed"),
      success_rate = round(high_confidence / total_clusters * 100, 1),
      .groups = "drop"
    )
  
  cat("\nDetailed statistics by model:\n")
  print(success_stats)
  
  # Do not export file (per user request)
   # output_file <- paste0(output_prefix, "_all_models_annotation_results.csv")
   # write.csv(final_results, output_file, row.names = FALSE)
   # cat(sprintf("\nAll results saved to: %s\n", output_file))
  
  cat("\n=== Multi-model annotation completed ===\n")
  
  # Return data frame
  return(final_results)
}

#' Reorganize Multi-model Annotation Results
#'
#' This function reorganizes multi-model annotation results from long format to wide format,
#' where each row represents a cluster and columns contain results from different models.
#'
#' @param results_file Path to the multi-model results CSV file
#' @param output_file Output file path (optional, if NULL will auto-generate)
#' @param include_reasoning Whether to include reasoning/thinking process columns, default TRUE
#' @return A data frame in wide format with cluster-wise comparison across models
#' @export
#' @examples
#' \dontrun{
#' # Reorganize results from multi-model annotation
#' wide_results <- reorganize_multi_model_results(
#'   results_file = "multi_model_results.csv",
#'   include_reasoning = TRUE
#' )
#' }
reorganize_multi_model_results <- function(results_file, output_file = NULL, include_reasoning = TRUE) {
  
  # Check if required packages are available
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required but not installed.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }
  
  # Load required libraries
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  
  # Check if file exists
  if (!file.exists(results_file)) {
    stop(paste("Results file not found:", results_file))
  }
  
  # Read original results file
  cat("Reading results file:", results_file, "\n")
  original_results <- readr::read_csv(results_file, show_col_types = FALSE)
  
  # Display data structure
  cat("Original data dimensions:", dim(original_results), "\n")
  cat("Models included:", paste(unique(original_results$model), collapse = ", "), "\n")
  cat("Clusters included:", paste(sort(unique(original_results$cluster)), collapse = ", "), "\n")
  
  # Process content field to separate reasoning and final answer if needed
  processed_results <- original_results %>%
    mutate(
      # Check if content contains reasoning process separation
      has_reasoning = str_detect(content, "Reasoning process:|Final answer:"),
      
      # Extract reasoning process
      reasoning_content = case_when(
        has_reasoning ~ str_extract(content, "Reasoning process:\\s*([\\s\\S]*?)(?=\\n\\nFinal answer:|$)"),
        TRUE ~ "无单独推理过程"
      ),
      
      # Extract final answer
      final_answer = case_when(
        has_reasoning ~ str_extract(content, "Final answer:\\s*([\\s\\S]*)$"),
        TRUE ~ content
      ),
      
      # Clean extracted content
      reasoning_content = str_remove(reasoning_content, "^Reasoning process:\\s*"),
      final_answer = str_remove(final_answer, "^Final answer:\\s*"),
      
      # Create combined cell type information
      cell_info = paste0(cell_type, " (", subtype, ")"),
      
      # Simplify model names
      model_short = case_when(
        grepl("claude", model, ignore.case = TRUE) ~ "Claude",
        grepl("deepseek", model, ignore.case = TRUE) ~ "DeepSeek", 
        grepl("gpt", model, ignore.case = TRUE) ~ "GPT",
        TRUE ~ model
      )
    )
  
  # Select columns for reshaping
  if (include_reasoning) {
    reshape_data <- processed_results %>%
      select(cluster, cell_info, final_answer, reasoning_content, model_short)
    
    # Convert to wide format with reasoning
    reshaped_results <- reshape_data %>%
      pivot_wider(
        id_cols = cluster,
        names_from = model_short,
        values_from = c(cell_info, final_answer, reasoning_content),
        names_sep = "_"
      )
    
    # Get available models for column ordering
    available_models <- unique(processed_results$model_short)
    
    # Create column order dynamically
    base_cols <- "cluster"
    cell_type_cols <- paste0("cell_info_", available_models)
    answer_cols <- paste0("final_answer_", available_models)
    reasoning_cols <- paste0("reasoning_content_", available_models)
    
    # Select and reorder columns
    reshaped_results <- reshaped_results %>%
      select(all_of(c(base_cols, cell_type_cols, answer_cols, reasoning_cols)))
    
    # Create new column names
    new_names <- c(
      "Cluster",
      paste0(available_models, "_细胞类型"),
      paste0(available_models, "_回复结果"),
      paste0(available_models, "_思考过程")
    )
    
  } else {
    reshape_data <- processed_results %>%
      select(cluster, cell_info, final_answer, model_short)
    
    # Convert to wide format without reasoning
    reshaped_results <- reshape_data %>%
      pivot_wider(
        id_cols = cluster,
        names_from = model_short,
        values_from = c(cell_info, final_answer),
        names_sep = "_"
      )
    
    # Get available models for column ordering
    available_models <- unique(processed_results$model_short)
    
    # Create column order dynamically
    base_cols <- "cluster"
    cell_type_cols <- paste0("cell_info_", available_models)
    answer_cols <- paste0("final_answer_", available_models)
    
    # Select and reorder columns
    reshaped_results <- reshaped_results %>%
      select(all_of(c(base_cols, cell_type_cols, answer_cols)))
    
    # Create new column names
    new_names <- c(
      "Cluster",
      paste0(available_models, "_细胞类型"),
      paste0(available_models, "_回复结果")
    )
  }
  
  # Rename columns
  names(reshaped_results) <- new_names
  
  # Sort by cluster
  reshaped_results <- reshaped_results %>%
    arrange(Cluster)
  
  # Generate output file name if not provided
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    if (include_reasoning) {
      output_file <- paste0("reorganized_results_with_reasoning_", timestamp, ".csv")
    } else {
      output_file <- paste0("reorganized_results_", timestamp, ".csv")
    }
  }
  
  # Save reorganized results
  readr::write_csv(reshaped_results, output_file)
  
  cat("\nReorganized results saved to:", output_file, "\n")
  cat("New data dimensions:", dim(reshaped_results), "\n")
  
  # Display column names
  cat("\nColumn names:\n")
  cat(paste(names(reshaped_results), collapse = ", "), "\n")
  
  # Check reasoning separation statistics
  if (include_reasoning) {
    reasoning_check <- processed_results %>%
      group_by(model_short) %>%
      summarise(
        total_records = n(),
        has_reasoning_separation = sum(has_reasoning),
        .groups = "drop"
      )
    
    cat("\nReasoning process separation check:\n")
    print(reasoning_check)
  }
  
  cat("\n=== Results reorganization completed ===\n")
  
  # Return the reorganized data frame
  return(reshaped_results)
}
