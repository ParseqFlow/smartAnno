#' AI single-cell annotation
#' @description This function utilizes an AI big language model to automatically annotate cell clusters in single-cell sequencing data.
#' @param background Sample background description information
#' @param markers The results of FindAllMarkers must include the cluster, p-val, avg.log2FC, and gene columns.
#' @param selected_clusters Select the cluster (character vector) for analysis
#' @param api_key API key, read by default from the environment variable API_KEY
#' @param api_url API request address, read by default from the environment variable API-URL
#' @param model Model name used
#' @param gene_number The first n genes were selected for annotation
#' @param workers Number of parallel working processes
#' @param max_retries Maximum retry times, default 3 times
#' @param temperature API temperature parameter, default 0.3
#' @param max_tokens The maximum number of tokens for the API is 8190 by default
#' @param time_out Request timeout (seconds)
#' @param retry_delay Wait time for retry (seconds)
#' @export
#' @importFrom httr POST add_headers content status_code timeout
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom dplyr filter group_by summarise top_n
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate select case_when across
#' @importFrom tibble deframe tibble
#' @importFrom future plan multisession
#' @importFrom furrr future_imap
#' @importFrom purrr map_dfr imap
#' @importFrom stringr str_trunc str_match str_squish str_detect str_replace
#' @examples
#' \dontrun{
#' markers=read.csv("data/all_DEG.csv",row.names = 1)
#' Sys.setenv(API_URL = "xxxx")
#' Sys.setenv(API_KEY = "sk-xxxxxxxxx")
#' ann <- anno(
#'   markers,
#'   selected_clusters = c("0","1"),
#'   background = "Human peripheral blood single-cell data",
#'   workers = 6
#' )
#' }



anno <- function(markers,
                 selected_clusters = NULL,
                 api_key = Sys.getenv("API_KEY"),
                 api_url = Sys.getenv("API_URL"),
                 model = "deepseek-r1-250120",
                 gene_number = 100,
                 workers = 6,
                 background = NULL,
                 max_retries = 3,
                 time_out = 200,
                 retry_delay = 1,
                 temperature = 0.3,
                 max_tokens = 8190) {




  prepare_genes <- function(markers) {
    markers %>%
      dplyr::filter(p_val < 0.05) %>%
      dplyr::group_by(cluster) %>%
      dplyr::top_n(n = gene_number, wt = avg_log2FC) %>%
      dplyr::summarise(genes = list(as.character(gene))) %>%
      tibble::deframe()
  }


  gene_list <- prepare_genes(markers)


  chat_completion <- function(messages, cluster_id) {



    result <- list(
      cluster = cluster_id,
      status = "error",
      message = "Initialization failed",
      content = NA,
      think_content = NA,
      tokens = 0,
      attempts = 0
    )

    for (attempt in 1:max_retries) {
      result$attempts <- attempt

      tryCatch({
        start_time <- Sys.time()
        cat(sprintf("[%s] Cluster %s starts the request\n",
                    Sys.time(), cluster_id))

        response <- POST(
          api_url,
          add_headers(
            "Authorization" = paste("Bearer", api_key),
            "Content-Type" = "application/json"
          ),
          body = toJSON(list(
            model = model,
            messages = messages,
            temperature = temperature,
            max_tokens = max_tokens
          ), auto_unbox = TRUE),
          encode = "json",
          timeout(time_out)
        )

        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cat(sprintf("[%s] Cluster %s receives a response, which takes %.1fs\n",
                    Sys.time(), cluster_id, elapsed))


        if (status_code(response) != 200) {
          err_msg <- if (!is.null(content(response)$error)) {
            content(response)$error$message
          } else {
            paste("HTTP Error:", status_code(response))
          }
          stop(err_msg)
        }


        response_content <- content(response, "parsed")


        thinking <- tryCatch({
          response_content$choices[[1]]$message$reasoning_content
        }, error = function(e) NA)


        result <- list(
          cluster = cluster_id,
          status = "success",
          message = "OK",
          content = response_content$choices[[1]]$message$content,
          think_content = ifelse(is.null(thinking), NA, thinking),
          tokens = response_content$usage$total_tokens,
          attempts = attempt
        )

        break

      }, error = function(e) {

        result$message <<- if (grepl("Timeout", e$message)) {
          sprintf("Timeout on attempt %d", attempt)
        } else {
          sprintf("Attempt %d failed: %s", attempt, e$message)
        }


        if (grepl("4[0-9]{2}", result$message)) {
          result$message <<- paste(result$message, "(No retry)")
          attempt <<- max_retries
        }


        if (attempt < max_retries) Sys.sleep(retry_delay)
      })
    }

    return(result)
  }

  process_results <- function(results) {
    map_dfr(results, ~ {
      tibble(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        cluster = .x$cluster,
        status = .x$status,
        message = .x$message,
        content = ifelse(is.na(.x$content), "", .x$content),
        think_content = ifelse(is.na(.x$think_content), "", .x$think_content),
        tokens = .x$tokens,
        attempts = .x$attempts
      )
    }) %>%
      mutate(
        across(c(content, think_content), ~ ifelse(nchar(.) > 1000, substr(., 1, 1000) %>% paste0("..."), .))
      )
  }


  process_clusters <- function(gene_list,selected_clusters = NULL,workers) {


    if (!is.null(selected_clusters)) {
      selected_clusters <- as.character(selected_clusters)
      valid_clusters <- intersect(selected_clusters, names(gene_list))

      if (length(valid_clusters) == 0) {
        stop("No valid cluster is available for processing！")
      }

      if (length(valid_clusters) < length(selected_clusters)) {
        missing <- setdiff(selected_clusters, names(gene_list))
        warning(paste("The following cluster does not exist:", paste(missing, collapse = ", ")))
      }

      gene_list <- gene_list[valid_clusters]
    }

    log_file <- sprintf("api_log_%s.txt", format(Sys.time(), "%Y%m%d%H%M%S"))
    sink(log_file, split = TRUE)

    plan(multisession, workers = workers, .cleanup = FALSE)

    messages_list <- imap(gene_list, ~ list(
      list(
        role = "system",
        content = paste(
          "You are a senior biomedical professor specializing in single-cell data analysis.",
          "The user is conducting single-cell annotation analysis and will provide:",
          "1. Sample background:：", background,
          "2. The top ", gene_number, " highly expressed genes in each cluster",
          "Please infer cell type and subtype based on gene expression characteristics",
          "Please provide the type and subtype in English at the beginning, The format is:>Type (subtype)<",
          "supplement：answer the rest in the same language as the user's question. Just provide one type and subtype, if there are multiple, please write 'uncertain'"
        )),
      list(
        role = "user",
        content = background),
      list(
        role = "user",
        content = paste("Cluster", .y, "：", paste(.x, collapse = ", ")))
    ))


    results <- future_imap(
      messages_list,
      ~ {
        Sys.sleep(0.3)
        chat_completion(.x, .y)
      },
      .progress = TRUE
    )
    on.exit(sink())



    results_df <- process_results(results)

    if (all(results_df$status != "success")) {
      cat("\033[31m请检查api和key是否填写正确！\nPlease check if the api_key or URL is filled in correctly!\033[0m\n")
    }

    return(results_df)

  }
  subanno <- function(annotations) {

    pattern <- ">\\s*(.*?)\\s*<"

    annotations %>%
      mutate(
        # Extract core content
        extracted = str_match(content, pattern)[,2],

        # Clean results
        celltype = case_when(
          is.na(extracted) ~ "Uncertain",
          # Handle trailing punctuation
          str_detect(extracted, "[:;,.]+$") ~ str_replace(extracted, "[:;,.]+$", ""),
          TRUE ~ extracted
        ) %>% str_squish()
      ) %>%
      select(cluster, celltype)
  }

  annotations = process_clusters(gene_list, selected_clusters, workers)
  subannotations = subanno(annotations)
  annlist = list(smartAnno = annotations,
                 ann=subannotations)


  return(annlist)


}
