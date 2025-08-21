#' @title API Functions for smartAnno Package
#' @description Functions for calling different AI model APIs with proper request format classification
#' @importFrom httr POST add_headers content status_code
#' @importFrom jsonlite fromJSON toJSON
#' @name api-functions
NULL

#' Determine API format based on model name or user input
#' 
#' @param model_name Character string of the model name
#' @param api_format Character string to manually specify API format ("openai", "claude", "gemini", or "responses"). 
#'                  If NULL, will attempt auto-detection based on model name.
#' @return Character string indicating API format ("openai", "claude", "gemini", or "responses")
#' @export
get_api_format <- function(model_name, api_format = NULL) {
  
  # If user manually specifies API format, use it
  if (!is.null(api_format)) {
    if (api_format %in% c("openai", "claude", "gemini", "responses")) {
      return(api_format)
    } else {
      stop("api_format must be one of: 'openai', 'claude', 'gemini', or 'responses'")
    }
  }
  
  # Auto-detection based on model name
  if (grepl("claude", model_name, ignore.case = TRUE)) {
    return("claude")
  } else if (grepl("gemini", model_name, ignore.case = TRUE)) {
    return("gemini")
  } else if (grepl("gpt-4\\.[1-9]", model_name, ignore.case = TRUE) || grepl("gpt-5", model_name, ignore.case = TRUE)) {
    # GPT-4.1+ and GPT-5 use responses format
    return("responses")
  } else {
    # Default to OpenAI format for other models
    return("openai")
  }
}

#' Call AI model API with automatic format detection or manual specification
#' 
#' @param model_name Character string of the model name
#' @param prompt Character string of the prompt
#' @param api_key Character string of the API key
#' @param base_url Character string of the base URL (default: "https://newapi.nbchat.site")
#' @param max_tokens Integer for maximum tokens (default: 500)
#' @param temperature Numeric for temperature (default: 0.7)
#' @param api_format Character string to manually specify API format ("openai", "claude", "gemini", or "responses"). 
#'                  If NULL, will attempt auto-detection based on model name.
#' @param reasoning_effort Character string for reasoning effort ("minimal", "low", "medium", "high"). Default: "medium"
#' @param verbosity Character string for verbosity ("low", "medium", "high"). Default: "medium"
#' @param log_file Character string path to log file for recording API requests and responses (optional)
#' @return List containing success status and response content
#' @export
call_ai_model <- function(model_name, prompt, api_key, 
                         base_url = "https://newapi.nbchat.site",
                         max_tokens = 2000, temperature = 0.7,
                         api_format = NULL, reasoning_effort = "low", verbosity = "medium",
                         log_file = NULL) {
  
  # Determine API format
  detected_format <- get_api_format(model_name, api_format)
  
  # Log request information if log_file is specified
  if (!is.null(log_file)) {
    log_entry <- paste0(
      "[REQUEST] ", Sys.time(), "\n",
      "Model: ", model_name, "\n",
      "API Format: ", detected_format, "\n",
      "Temperature: ", temperature, "\n",
      "Max Tokens: ", max_tokens, "\n",
      "Prompt: ", substr(prompt, 1, 200), "...\n",
      "---\n"
    )
    cat(log_entry, file = log_file, append = TRUE)
  }
  
  # Route to appropriate API function based on format
  if (detected_format == "claude") {
    result <- call_claude_api(model_name, prompt, api_key, base_url, max_tokens, log_file)
  } else if (detected_format == "gemini") {
    result <- call_gemini_api(model_name, prompt, api_key, base_url, max_tokens, temperature, log_file)
  } else if (detected_format == "responses") {
    result <- call_responses_api(model_name, prompt, api_key, base_url, max_tokens, temperature, reasoning_effort, verbosity, log_file)
  } else {
    result <- call_openai_api(model_name, prompt, api_key, base_url, max_tokens, temperature, log_file)
  }
  
  # Log response information if log_file is specified
  if (!is.null(log_file)) {
    response_content <- if (result$success) {
      substr(result$content, 1, 500)
    } else {
      paste("ERROR:", result$error)
    }
    
    log_entry <- paste0(
      "[RESPONSE] ", Sys.time(), "\n",
      "Success: ", result$success, "\n",
      "Content: ", response_content, "...\n",
      "========================================\n\n"
    )
    cat(log_entry, file = log_file, append = TRUE)
  }
  
  return(result)
}

#' Call OpenAI format API
#' 
#' @param model_name Character string of the model name
#' @param prompt Character string of the prompt
#' @param api_key Character string of the API key
#' @param base_url Character string of the base URL
#' @param max_tokens Integer for maximum tokens
#' @param temperature Numeric for temperature
#' @param log_file Character string path to log file (optional)
#' @return List containing success status and response content
call_openai_api <- function(model_name, prompt, api_key, base_url, max_tokens, temperature, log_file = NULL) {
  
  url <- paste0(base_url, "/v1/chat/completions")
  
  headers <- add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )
  
  body <- list(
    model = model_name,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    max_tokens = max_tokens,
    temperature = temperature
  )
  
  tryCatch({
    response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
    
    if (status_code(response) == 200) {
      response_text <- content(response, "text", encoding = "UTF-8")
      result <- fromJSON(response_text)
      
      # Extract content from OpenAI format response
      content_text <- extract_openai_content(result)
      
      # Check if content extraction failed
      if (content_text == "Unable to extract response content") {
      cat(sprintf("[Content extraction failed] Response structure: %s\n", paste(names(result), collapse = ", ")))
      return(list(
        success = FALSE,
        content = "Unable to extract response content",
        error = "Unable to extract valid content from API response"
        ))
      }
      
      return(list(
        success = TRUE,
        content = content_text,
        raw_response = result
      ))
    } else {
      error_msg <- content(response, "text", encoding = "UTF-8")
      cat(sprintf("[API Error] HTTP %d: %s\n", status_code(response), error_msg))
      return(list(
        success = FALSE,
        error = paste("HTTP", status_code(response), ":", error_msg)
      ))
    }
  }, error = function(e) {
    cat(sprintf("[Request Error] %s\n", e$message))
    return(list(
      success = FALSE,
      error = paste("Request error:", e$message)
    ))
  })
}

#' Call Claude format API
#' 
#' @param model_name Character string of the model name
#' @param prompt Character string of the prompt
#' @param api_key Character string of the API key
#' @param base_url Character string of the base URL
#' @param max_tokens Integer for maximum tokens
#' @param log_file Character string path to log file (optional)
#' @return List containing success status and response content
call_claude_api <- function(model_name, prompt, api_key, base_url, max_tokens, log_file = NULL) {
  
  url <- paste0(base_url, "/v1/messages")
  
  headers <- add_headers(
    "Content-Type" = "application/json",
    "anthropic-version" = "2023-06-01",
    "x-api-key" = api_key
  )
  
  body <- list(
    model = model_name,
    max_tokens = max_tokens,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    )
  )
  
  tryCatch({
    response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
    
    if (status_code(response) == 200) {
      response_text <- content(response, "text", encoding = "UTF-8")
      result <- fromJSON(response_text)
      
      # Extract content from Claude format response
      content_text <- extract_claude_content(result)
      
      # Check if content extraction failed
      if (content_text == "Unable to extract response content") {
      cat(sprintf("[Content extraction failed] Response structure: %s\n", paste(names(result), collapse = ", ")))
      return(list(
        success = FALSE,
        content = "Unable to extract response content",
        error = "Unable to extract valid content from API response"
        ))
      }
      
      return(list(
        success = TRUE,
        content = content_text,
        raw_response = result
      ))
    } else {
      error_msg <- content(response, "text", encoding = "UTF-8")
      cat(sprintf("[API Error] HTTP %d: %s\n", status_code(response), error_msg))
      return(list(
        success = FALSE,
        error = paste("HTTP", status_code(response), ":", error_msg)
      ))
    }
  }, error = function(e) {
    cat(sprintf("[Request Error] %s\n", e$message))
    return(list(
      success = FALSE,
      error = paste("Request error:", e$message)
    ))
  })
}

#' Extract content from OpenAI format response
#' 
#' @param result Parsed JSON response from OpenAI API
#' @return Character string of extracted content
extract_openai_content <- function(result) {
  
  # Debug output for troubleshooting grok models
  if (exists("debug_grok", envir = .GlobalEnv) && get("debug_grok", envir = .GlobalEnv)) {
    cat("\n=== Debugging grok response structure ===\n")
    cat("Response type:", class(result), "\n")
    cat("Response fields:", paste(names(result), collapse = ", "), "\n")
    if ("choices" %in% names(result)) {
      cat("choices structure:\n")
      print(str(result$choices))
    }
    cat("=== Debug end ===\n\n")
  }
  
  # Try standard OpenAI format first
  if ("choices" %in% names(result) && length(result$choices) > 0) {
    choices <- result$choices
    
    # Handle both list and data.frame formats
    if (is.list(choices) && !is.data.frame(choices)) {
      first_choice <- choices[[1]]
    } else {
      first_choice <- choices[1, ]
    }
    
    # Standard message format
    if ("message" %in% names(first_choice)) {
      message_obj <- first_choice$message
      
      # For DeepSeek reasoning models, check reasoning_content first
      if ("reasoning_content" %in% names(message_obj) && !is.null(message_obj$reasoning_content) && nchar(message_obj$reasoning_content) > 0) {
        reasoning <- message_obj$reasoning_content
        content <- if ("content" %in% names(message_obj)) message_obj$content else ""
        combined_content <- paste("Reasoning process:", reasoning, "\n\nFinal answer:", content)
        return(convert_gpt5_format(combined_content))
      }
      
      # Regular content extraction
      if ("content" %in% names(message_obj) && !is.null(message_obj$content) && nchar(as.character(message_obj$content)) > 0) {
        return(as.character(message_obj$content))
      }
      
      # Handle empty content but valid message structure
      if ("content" %in% names(message_obj) && (is.null(message_obj$content) || nchar(as.character(message_obj$content)) == 0)) {
        # Check finish_reason to understand why content is empty
        finish_reason <- if ("finish_reason" %in% names(first_choice)) first_choice$finish_reason else "unknown"
        
        # Check if there are other fields in message
        other_fields <- setdiff(names(message_obj), c("role", "content"))
        if (length(other_fields) > 0) {
          for (field in other_fields) {
            if (!is.null(message_obj[[field]]) && is.character(message_obj[[field]]) && nchar(message_obj[[field]]) > 0) {
              return(as.character(message_obj[[field]]))
            }
          }
        }
        
        # If no content found, return informative message based on finish_reason
        if (finish_reason == "length") {
          return("Response was truncated due to length limit. No content was generated.")
        } else if (finish_reason == "content_filter") {
          return("Response was filtered due to content policy.")
        } else {
          return(paste("No content generated. Finish reason:", finish_reason))
        }
      }
    }
    
    # Alternative formats for different models
    if ("text" %in% names(first_choice) && !is.null(first_choice$text) && nchar(as.character(first_choice$text)) > 0) {
      return(as.character(first_choice$text))
    }
    
    if ("content" %in% names(first_choice) && !is.null(first_choice$content) && nchar(as.character(first_choice$content)) > 0) {
      return(as.character(first_choice$content))
    }
    
    # For some models, the response might be directly in the choice
    if (is.character(first_choice) && nchar(first_choice) > 0) {
      return(first_choice)
    }
    
    # Try to extract from any field in first_choice that contains text
    for (field_name in names(first_choice)) {
      field_value <- first_choice[[field_name]]
      if (is.character(field_value) && length(field_value) == 1 && nchar(field_value) > 5) {
        return(field_value)
      }
      # Handle nested structures
      if (is.list(field_value) && "content" %in% names(field_value)) {
        if (!is.null(field_value$content) && nchar(as.character(field_value$content)) > 0) {
          return(as.character(field_value$content))
        }
      }
    }
  }
  
  # Try alternative top-level response structures
  if ("response" %in% names(result) && !is.null(result$response) && nchar(as.character(result$response)) > 0) {
    return(as.character(result$response))
  }
  
  if ("text" %in% names(result) && !is.null(result$text) && nchar(as.character(result$text)) > 0) {
    return(as.character(result$text))
  }
  
  if ("content" %in% names(result) && !is.null(result$content) && nchar(as.character(result$content)) > 0) {
    return(as.character(result$content))
  }
  
  # If all else fails, try to extract any text-like content
  for (field_name in names(result)) {
    field_value <- result[[field_name]]
    if (is.character(field_value) && length(field_value) == 1 && nchar(field_value) > 10) {
      return(field_value)
    }
  }
  
  return("Unable to extract response content")
}

#' Call Responses format API (for GPT-4.1+, GPT-5, etc.)
 #' 
 #' @param model_name Character string of the model name
 #' @param prompt Character string of the prompt
 #' @param api_key Character string of the API key
 #' @param base_url Character string of the base URL
 #' @param max_tokens Integer for maximum tokens
 #' @param temperature Numeric for temperature (not used in responses API)
 #' @param reasoning_effort Character string for reasoning effort ("minimal", "low", "medium", "high")
 #' @param verbosity Character string for verbosity ("low", "medium", "high")
 #' @param log_file Character string path to log file (optional)
 #' @return List containing success status and response content
 call_responses_api <- function(model_name, prompt, api_key, base_url, max_tokens, temperature, reasoning_effort = "medium", verbosity = "medium", log_file = NULL) {
   
   url <- paste0(base_url, "/v1/responses")
   
   headers <- add_headers(
     "Content-Type" = "application/json",
     "Authorization" = paste("Bearer", api_key)
   )
   
   # Validate reasoning effort parameter
   valid_efforts <- c("minimal", "low", "medium", "high")
   if (!reasoning_effort %in% valid_efforts) {
     reasoning_effort <- "high"
     warning("Invalid reasoning_effort. Using 'medium' as default.")
   }
   
   # Validate verbosity parameter
   valid_verbosity <- c("low", "medium", "high")
   if (!verbosity %in% valid_verbosity) {
     verbosity <- "medium"
     warning("Invalid verbosity. Using 'medium' as default.")
   }
   
   body <- list(
     model = model_name,
     input = prompt,
     reasoning = list(
       effort = reasoning_effort
     ),
     text = list(
       verbosity = verbosity
     )
   )
   
   # Add max_output_tokens if max_tokens is specified
   # For responses API, we need higher token limits for complete responses
   if (!is.null(max_tokens) && max_tokens > 0) {
     # Ensure minimum token count to avoid incomplete responses
     body$max_output_tokens <- max(max_tokens, 3000)
   } else {
     # Default to 5000 tokens if not specified
     body$max_output_tokens <- 5000
   }
  
  tryCatch({
    response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
    
    if (status_code(response) == 200) {
      response_text <- content(response, "text", encoding = "UTF-8")
      result <- fromJSON(response_text)
      
      # Extract content from responses format response
      content_text <- extract_responses_content(result)
      
      # Check if content extraction failed
      if (content_text == "Unable to extract response content") {
        cat(sprintf("[Content extraction failed] Response structure: %s\n", paste(names(result), collapse = ", ")))
        return(list(
          success = FALSE,
          content = "Unable to extract response content",
          error = "Unable to extract valid content from API response",
          raw_response = result
        ))
      }
      
      return(list(
        success = TRUE,
        content = content_text,
        raw_response = result
      ))
    } else {
      error_msg <- content(response, "text", encoding = "UTF-8")
      cat(sprintf("[API Error] HTTP %d: %s\n", status_code(response), error_msg))
      return(list(
        success = FALSE,
        error = paste("HTTP", status_code(response), ":", error_msg)
      ))
    }
  }, error = function(e) {
    cat(sprintf("[Request Error] %s\n", e$message))
    return(list(
      success = FALSE,
      error = paste("Request error:", e$message)
    ))
  })
}

#' Call Google Gemini format API (compatible with nbchat.site)
#' 
#' @param model_name Character string of the model name
#' @param prompt Character string of the prompt
#' @param api_key Character string of the API key
#' @param base_url Character string of the base URL
#' @param max_tokens Integer for maximum tokens
#' @param temperature Numeric for temperature
#' @param log_file Character string path to log file (optional)
#' @return List containing success status and response content
call_gemini_api <- function(model_name, prompt, api_key, base_url, max_tokens, temperature, log_file = NULL) {
  
  # Try OpenAI format first (for nbchat.site compatibility)
  url <- paste0(base_url, "/v1/chat/completions")
  
  headers <- add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )
  
  # OpenAI format for better compatibility
  body <- list(
    model = model_name,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    max_tokens = max_tokens,
    temperature = temperature
  )
  
  tryCatch({
    response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
    
    if (status_code(response) == 200) {
      response_text <- content(response, "text", encoding = "UTF-8")
      result <- fromJSON(response_text)
      
      # Check if response is in OpenAI format (choices structure)
      if ("choices" %in% names(result) && length(result$choices) > 0) {
        # Handle OpenAI format response
        choices <- result$choices
        if (is.data.frame(choices)) {
          first_choice <- choices[1, ]
        } else {
          first_choice <- choices[[1]]
        }
        
        # Debug: Print choice structure
        # Try message.content first
        if ("message" %in% names(first_choice) && !is.null(first_choice$message)) {
          message_obj <- first_choice$message
          if ("content" %in% names(message_obj) && !is.null(message_obj$content) && nchar(as.character(message_obj$content)) > 0) {
            return(list(
              success = TRUE,
              content = as.character(message_obj$content),
              raw_response = result
            ))
          }
        }
        
        # Try direct text field in choice
        if ("text" %in% names(first_choice) && !is.null(first_choice$text) && nchar(as.character(first_choice$text)) > 0) {
          return(list(
            success = TRUE,
            content = as.character(first_choice$text),
            raw_response = result
          ))
        }
        
        # Try direct content field in choice
        if ("content" %in% names(first_choice) && !is.null(first_choice$content) && nchar(as.character(first_choice$content)) > 0) {
          return(list(
            success = TRUE,
            content = as.character(first_choice$content),
            raw_response = result
          ))
        }
      }
      
      # Fallback: Try Gemini format if OpenAI format fails
      content_text <- extract_gemini_content(result)
      
      # Check if content extraction failed
      if (content_text == "Unable to extract response content") {
        cat(sprintf("[Content extraction failed] Response structure: %s\n", paste(names(result), collapse = ", ")))
        return(list(
          success = FALSE,
          content = "Unable to extract response content",
          error = "Unable to extract valid content from API response",
          raw_response = result
        ))
      }
      
      return(list(
        success = TRUE,
        content = content_text,
        raw_response = result
      ))
    } else {
      error_msg <- content(response, "text", encoding = "UTF-8")
      cat(sprintf("[API Error] HTTP %d: %s\n", status_code(response), error_msg))
      return(list(
        success = FALSE,
        error = paste("HTTP", status_code(response), ":", error_msg)
      ))
    }
  }, error = function(e) {
    cat(sprintf("[Request Error] %s\n", e$message))
    return(list(
      success = FALSE,
      error = paste("Request error:", e$message)
    ))
  })
}

#' Extract content from responses format response
 #' 
 #' @param result Parsed JSON response from responses API
 #' @return Character string of extracted content
 extract_responses_content <- function(result) {
   if (is.null(result) || !is.list(result)) {
     return("Unable to extract response content")
   }
   
   # Check if response is incomplete
   if ("status" %in% names(result) && result$status == "incomplete") {
     cat("[WARNING] GPT-5 response is incomplete\n")
     if ("incomplete_details" %in% names(result) && "reason" %in% names(result$incomplete_details)) {
       cat(sprintf("[WARNING] Incomplete reason: %s\n", result$incomplete_details$reason))
     }
   }
 
   # Prefer top-level aggregated text if available (GPT-5 Responses API)
   if ("output_text" %in% names(result) && !is.null(result$output_text)) {
     if (is.character(result$output_text) && length(result$output_text) > 0 && nchar(result$output_text[1]) > 0) {
       cat("[DEBUG] Using top-level output_text field\n")
       raw_content <- paste(result$output_text, collapse = "\n")
       return(convert_gpt5_format(raw_content))
     }
   }
 
   # Check for choices array (standard OpenAI format)
   if ("choices" %in% names(result) && length(result$choices) > 0) {
     choices <- result$choices
     
     # Handle data.frame format
     if (is.data.frame(choices)) {
       if ("message" %in% names(choices) && nrow(choices) > 0) {
         # Get the first message
         message_list <- choices$message[[1]]
         if (is.list(message_list) && "content" %in% names(message_list)) {
           raw_content <- message_list$content
           if (!is.null(raw_content) && is.character(raw_content) && nchar(raw_content) > 0) {
             return(convert_gpt5_format(raw_content))
           }
         }
       }
     }
     
     # Handle list format
     if (is.list(choices)) {
       first_choice <- choices[[1]]
       if (is.list(first_choice) && "message" %in% names(first_choice)) {
         message <- first_choice$message
         if (is.list(message) && "content" %in% names(message)) {
           raw_content <- message$content
           if (!is.null(raw_content) && is.character(raw_content) && nchar(raw_content) > 0) {
             return(convert_gpt5_format(raw_content))
           }
         }
       }
     }
   }
     
     # GPT-5 responses API format: check output field first
       if ("output" %in% names(result) && !is.null(result$output)) {
         cat("[DEBUG] Found output field\n")
         output <- result$output
         cat(sprintf("[DEBUG] Output type: %s\n", class(output)))
         
         # Handle data.frame output format
         if (is.data.frame(output)) {
           cat(sprintf("[DEBUG] Output is data.frame with %d rows and columns: %s\n", nrow(output), paste(names(output), collapse = ", ")))
           # Filter for message type rows, but if none exist, use reasoning rows
            if ("type" %in% names(output)) {
              cat(sprintf("[DEBUG] Type column values: %s\n", paste(output$type, collapse = ", ")))
              message_rows <- output[output$type == "message" | is.na(output$type), , drop = FALSE]
              cat(sprintf("[DEBUG] Found %d message rows\n", nrow(message_rows)))
              if (nrow(message_rows) > 0) {
                output <- message_rows
              } else {
                # If no message rows, check reasoning rows for content
                reasoning_rows <- output[output$type == "reasoning", , drop = FALSE]
                cat(sprintf("[DEBUG] Found %d reasoning rows, using them for content extraction\n", nrow(reasoning_rows)))
                if (nrow(reasoning_rows) > 0) {
                  output <- reasoning_rows
                }
              }
            }
            
            # Check for content in message rows
             if ("content" %in% names(output)) {
               for (i in 1:nrow(output)) {
                 content_field <- output$content[[i]]
                 if (!is.null(content_field) && length(content_field) > 0) {
                   cat(sprintf("[DEBUG] Row %d content type: %s, length: %d\n", i, class(content_field), length(content_field)))
                   
                   # Handle data.frame content (GPT-5 format)
                   if (is.data.frame(content_field)) {
                     cat(sprintf("[DEBUG] Content is data.frame with %d rows and columns: %s\n", nrow(content_field), paste(names(content_field), collapse = ", ")))
                     
                     # Look for text column in the data.frame
                     if ("text" %in% names(content_field) && nrow(content_field) > 0) {
                       for (row_idx in 1:nrow(content_field)) {
                         text_content <- content_field$text[row_idx]
                         if (!is.null(text_content) && is.character(text_content) && nchar(text_content) > 0) {
                           cat(sprintf("[DEBUG] Found text in data.frame row %d: %s\n", row_idx, substr(text_content, 1, 100)))
                           return(convert_gpt5_format(text_content))
                         }
                       }
                     }
                   }
                   # Handle list content
                   else if (is.list(content_field)) {
                     for (j in seq_along(content_field)) {
                       content_item <- content_field[[j]]
                       cat(sprintf("[DEBUG] Content item %d type: %s\n", j, class(content_item)))
                       
                       if (is.list(content_item)) {
                         cat(sprintf("[DEBUG] Content item %d fields: %s\n", j, paste(names(content_item), collapse = ", ")))
                         
                         # Look for text field in content item (GPT-5 format)
                         if ("text" %in% names(content_item) && !is.null(content_item$text) && is.character(content_item$text) && nchar(content_item$text) > 0) {
                           cat(sprintf("[DEBUG] Found text in content item %d: %s\n", j, substr(content_item$text, 1, 100)))
                           return(convert_gpt5_format(content_item$text))
                         }
                         
                         # Look for content field in content item
                         if ("content" %in% names(content_item) && !is.null(content_item$content) && is.character(content_item$content) && nchar(content_item$content) > 0) {
                           return(convert_gpt5_format(content_item$content))
                         }
                       } else if (is.character(content_item) && nchar(content_item) > 0) {
                         return(convert_gpt5_format(content_item))
                       }
                     }
                   } else if (is.character(content_field) && nchar(content_field) > 0) {
                     return(convert_gpt5_format(content_field))
                   }
                 }
               }
             }
          
          # Check if there's a content column (already handled above)
          if ("content" %in% names(output) && nrow(output) > 0) {
            content_col <- output$content
            if (is.list(content_col) && length(content_col) > 0) {
              # Extract text from list content
              text_parts <- c()
              for (i in seq_along(content_col)) {
                item <- content_col[[i]]
                if (is.list(item) && "text" %in% names(item) && !is.null(item$text) && is.character(item$text)) {
                  text_parts <- c(text_parts, item$text)
                } else if (is.character(item) && nchar(item) > 0) {
                  text_parts <- c(text_parts, item)
                }
              }
              if (length(text_parts) > 0) {
                raw_content <- paste(text_parts, collapse = "\n")
                return(convert_gpt5_format(raw_content))
              }
            } else if (is.character(content_col) && length(content_col) > 0 && nchar(content_col[1]) > 0) {
              raw_content <- paste(content_col, collapse = "\n")
              return(convert_gpt5_format(raw_content))
            }
          }
          
          # Check if there's a summary column with nested content (skip if already processed content)
          if ("summary" %in% names(output) && !exists("content_processed", inherits = FALSE)) {
            cat("[DEBUG] Found summary column\n")
            summary_col <- output$summary
            cat(sprintf("[DEBUG] Summary column type: %s, length: %d\n", class(summary_col), length(summary_col)))
            if (is.character(summary_col) && length(summary_col) > 0) {
              cat(sprintf("[DEBUG] Summary column content: %s\n", substr(summary_col[1], 1, 100)))
              # Handle character vector summary
              summary_values <- summary_col[!is.na(summary_col) & nchar(summary_col) > 0]
              if (length(summary_values) > 0) {
                # Check if this looks like actual content (not just an ID)
                if (!grepl("^rs_[a-f0-9]+$", summary_values[1])) {
                  raw_content <- paste(summary_values, collapse = "\n")
                  return(convert_gpt5_format(raw_content))
                } else {
                  cat("[DEBUG] Skipping summary as it appears to be an ID\n")
                }
              }
            } else if (is.list(summary_col) && length(summary_col) > 0) {
               cat(sprintf("[DEBUG] Processing summary list with %d items\n", length(summary_col)))
               for (i in seq_along(summary_col)) {
                 summary_item <- summary_col[[i]]
                 cat(sprintf("[DEBUG] Summary item %d type: %s\n", i, class(summary_item)))
                 if (is.list(summary_item)) {
                    cat(sprintf("[DEBUG] Summary item %d fields: %s\n", i, paste(names(summary_item), collapse = ", ")))
                    cat(sprintf("[DEBUG] Summary item %d length: %d\n", i, length(summary_item)))
                    
                    # If no named fields, check unnamed elements
                    if (length(names(summary_item)) == 0 || all(names(summary_item) == "")) {
                      cat(sprintf("[DEBUG] Summary item %d has no named fields, checking unnamed elements\n", i))
                      for (j in seq_along(summary_item)) {
                        element <- summary_item[[j]]
                        cat(sprintf("[DEBUG] Summary item %d element %d type: %s\n", i, j, class(element)))
                        if (is.character(element) && length(element) > 0 && nchar(element[1]) > 10) {
                          cat(sprintf("[DEBUG] Summary item %d element %d content: %s\n", i, j, substr(element[1], 1, 100)))
                          if (!grepl("^rs_[a-f0-9]+$", element[1])) {
                            return(convert_gpt5_format(element[1]))
                          }
                        }
                      }
                    }
                    
                    # Look for content or text fields in summary
                    if ("content" %in% names(summary_item) && !is.null(summary_item$content) && is.character(summary_item$content) && nchar(summary_item$content) > 0) {
                      cat(sprintf("[DEBUG] Found content in summary item %d: %s\n", i, substr(summary_item$content, 1, 100)))
                      if (!grepl("^rs_[a-f0-9]+$", summary_item$content)) {
                        return(convert_gpt5_format(summary_item$content))
                      }
                    }
                    if ("text" %in% names(summary_item) && !is.null(summary_item$text) && is.character(summary_item$text) && nchar(summary_item$text) > 0) {
                      cat(sprintf("[DEBUG] Found text in summary item %d: %s\n", i, substr(summary_item$text, 1, 100)))
                      if (!grepl("^rs_[a-f0-9]+$", summary_item$text)) {
                        return(convert_gpt5_format(summary_item$text))
                      }
                    }
                  } else if (is.character(summary_item) && nchar(summary_item) > 0) {
                   cat(sprintf("[DEBUG] Found character summary item %d: %s\n", i, substr(summary_item, 1, 100)))
                   if (!grepl("^rs_[a-f0-9]+$", summary_item)) {
                     return(convert_gpt5_format(summary_item))
                   }
                 }
               }
             }
          }
        }
        
        # Handle character vector output
        if (is.character(output) && length(output) > 0 && nchar(output[1]) > 0) {
          raw_content <- paste(output, collapse = "\n")
          return(convert_gpt5_format(raw_content))
        } 
        
        # Handle list output
        if (is.list(output)) {
          # Check for text content in output object
          if ("content" %in% names(output) && !is.null(output$content) && is.character(output$content) && nchar(output$content) > 0) {
            return(convert_gpt5_format(output$content))
          }
          if ("text" %in% names(output) && !is.null(output$text) && is.character(output$text) && nchar(output$text) > 0) {
            return(convert_gpt5_format(output$text))
          }
        }
      }
     
     # Check top-level text field
     if ("text" %in% names(result) && !is.null(result$text)) {
       text_field <- result$text
       if (is.character(text_field) && length(text_field) > 0 && nchar(text_field[1]) > 0) {
         raw_content <- paste(text_field, collapse = "\n")
         return(convert_gpt5_format(raw_content))
       } else if (is.list(text_field)) {
         if ("content" %in% names(text_field) && !is.null(text_field$content) && is.character(text_field$content) && nchar(text_field$content) > 0) {
           return(convert_gpt5_format(text_field$content))
         }
         # Check for format field with nested content
         if ("format" %in% names(text_field) && is.list(text_field$format)) {
           format_field <- text_field$format
           if (length(format_field) > 0) {
             for (i in seq_along(format_field)) {
               format_item <- format_field[[i]]
               if (is.list(format_item) && "content" %in% names(format_item) && !is.null(format_item$content) && is.character(format_item$content) && nchar(format_item$content) > 0) {
                 return(convert_gpt5_format(format_item$content))
               }
               if (is.list(format_item) && "text" %in% names(format_item) && !is.null(format_item$text) && is.character(format_item$text) && nchar(format_item$text) > 0) {
                 return(convert_gpt5_format(format_item$text))
               }
             }
           }
         }
       }
     }
     
     # Check reasoning field for any text content
     if ("reasoning" %in% names(result) && !is.null(result$reasoning)) {
       reasoning <- result$reasoning
       if (is.character(reasoning) && length(reasoning) > 0 && nchar(reasoning[1]) > 0) {
         raw_content <- paste(reasoning, collapse = "\n")
         return(convert_gpt5_format(raw_content))
       } else if (is.list(reasoning)) {
         if ("content" %in% names(reasoning) && !is.null(reasoning$content) && is.character(reasoning$content) && nchar(reasoning$content) > 0) {
           return(convert_gpt5_format(reasoning$content))
         }
         # Check for summary field in reasoning
         if ("summary" %in% names(reasoning) && is.list(reasoning$summary)) {
           summary_field <- reasoning$summary
           if (length(summary_field) > 0) {
             for (i in seq_along(summary_field)) {
               summary_item <- summary_field[[i]]
               if (is.list(summary_item) && "content" %in% names(summary_item) && !is.null(summary_item$content) && is.character(summary_item$content) && nchar(summary_item$content) > 0) {
                 return(convert_gpt5_format(summary_item$content))
               }
               if (is.list(summary_item) && "text" %in% names(summary_item) && !is.null(summary_item$text) && is.character(summary_item$text) && nchar(summary_item$text) > 0) {
                 return(convert_gpt5_format(summary_item$text))
               }
             }
           }
         }
       }
     }
     
   # Check reasoning field specifically for GPT-5 (only if no content found in output)
      if ("reasoning" %in% names(result) && !is.null(result$reasoning) && !exists("content_processed", inherits = FALSE)) {
        cat("[DEBUG] Found reasoning field\n")
        reasoning_content <- result$reasoning
        cat(sprintf("[DEBUG] Reasoning content type: %s, length: %d\n", class(reasoning_content), length(reasoning_content)))
        
        if (is.list(reasoning_content) && length(reasoning_content) > 0) {
          cat(sprintf("[DEBUG] Processing reasoning list with %d items\n", length(reasoning_content)))
          for (i in seq_along(reasoning_content)) {
            reasoning_item <- reasoning_content[[i]]
            cat(sprintf("[DEBUG] Reasoning item %d type: %s\n", i, class(reasoning_item)))
            if (is.character(reasoning_item) && length(reasoning_item) > 0) {
              cat(sprintf("[DEBUG] Reasoning item %d content: %s\n", i, substr(reasoning_item[1], 1, 100)))
              if (nchar(reasoning_item[1]) > 10 && !grepl("^rs_[a-f0-9]+$", reasoning_item[1])) {
                return(convert_gpt5_format(reasoning_item[1]))
              }
            } else if (is.list(reasoning_item)) {
              cat(sprintf("[DEBUG] Reasoning item %d is a list with fields: %s\n", i, paste(names(reasoning_item), collapse = ", ")))
              # Look for content in nested list
              if ("content" %in% names(reasoning_item) && !is.null(reasoning_item$content) && is.character(reasoning_item$content)) {
                cat(sprintf("[DEBUG] Found content in reasoning item %d: %s\n", i, substr(reasoning_item$content, 1, 100)))
                if (nchar(reasoning_item$content) > 10 && !grepl("^rs_[a-f0-9]+$", reasoning_item$content)) {
                  return(convert_gpt5_format(reasoning_item$content))
                }
              }
              if ("text" %in% names(reasoning_item) && !is.null(reasoning_item$text) && is.character(reasoning_item$text)) {
                cat(sprintf("[DEBUG] Found text in reasoning item %d: %s\n", i, substr(reasoning_item$text, 1, 100)))
                if (nchar(reasoning_item$text) > 10 && !grepl("^rs_[a-f0-9]+$", reasoning_item$text)) {
                  return(convert_gpt5_format(reasoning_item$text))
                }
              }
            }
          }
        } else if (is.character(reasoning_content) && length(reasoning_content) > 0) {
          cat(sprintf("[DEBUG] Reasoning content: %s\n", substr(reasoning_content[1], 1, 100)))
          if (nchar(reasoning_content[1]) > 10 && !grepl("^rs_[a-f0-9]+$", reasoning_content[1])) {
            return(convert_gpt5_format(reasoning_content[1]))
          }
        }
      }
    
    # Try to extract from any top-level field that might contain response content
     for (field_name in names(result)) {
       if (field_name %in% c("id", "object", "created", "created_at", "status", "model", "usage", "error", "incomplete_details", "system_fingerprint", "background", "content_filters", "instructions", "max_output_tokens", "max_tool_calls", "parallel_tool_calls", "previous_response_id", "prompt_cache_key", "safety_identifier", "service_tier", "store", "temperature", "tool_choice", "tools", "top_p", "truncation", "user", "metadata")) {
          next  # Skip metadata fields
        }
      
       field_value <- result[[field_name]]
       if (is.character(field_value) && length(field_value) > 0 && nchar(field_value[1]) > 10) {
         # Check if this looks like actual content (not just an ID)
         if (!grepl("^rs_[a-f0-9]+$", field_value[1])) {
           raw_content <- paste(field_value, collapse = "\n")
           return(convert_gpt5_format(raw_content))
         }
       }
      
      if (is.list(field_value) && length(field_value) > 0) {
        # Look for content in nested structures
        for (subfield in names(field_value)) {
          subvalue <- field_value[[subfield]]
          if (is.character(subvalue) && length(subvalue) > 0 && nchar(subvalue[1]) > 10) {
            raw_content <- paste(subvalue, collapse = "\n")
            return(convert_gpt5_format(raw_content))
          }
        }
      }
    }
   
   return("Unable to extract response content")
 }

#' Extract content from Gemini format response
#' 
#' @param result Parsed JSON response from Gemini API
#' @return Character string of extracted content
extract_gemini_content <- function(result) {
  if (is.null(result) || !is.list(result)) {
    return("Unable to extract response content")
  }
  
  # Check for candidates array (standard Gemini format)
  # Based on Google Gemini API docs: data['candidates'][0]['content']['parts'][0]['text']
  if ("candidates" %in% names(result) && length(result$candidates) > 0) {
    candidates <- result$candidates
    
    # Handle data.frame format
    if (is.data.frame(candidates)) {
      if (nrow(candidates) > 0) {
        first_candidate <- candidates[1, ]
        
        # Check for content field in candidate
        if ("content" %in% names(first_candidate) && !is.null(first_candidate$content)) {
          content_obj <- first_candidate$content
          
          # Handle nested content structure
          if (is.list(content_obj) && "parts" %in% names(content_obj)) {
            parts <- content_obj$parts
            if (is.list(parts) && length(parts) > 0) {
              first_part <- parts[[1]]
              if (is.list(first_part) && "text" %in% names(first_part) && !is.null(first_part$text)) {
                return(convert_gpt5_format(first_part$text))
              }
            }
          }
        }
      }
    } else if (is.list(candidates)) {
      # Handle list format
      first_candidate <- candidates[[1]]
      
      # Check for content.parts[0].text structure
      if (is.list(first_candidate) && "content" %in% names(first_candidate)) {
        content_obj <- first_candidate$content
        if (is.list(content_obj) && "parts" %in% names(content_obj)) {
          parts <- content_obj$parts
          if (is.list(parts) && length(parts) > 0) {
            first_part <- parts[[1]]
            if (is.list(first_part) && "text" %in% names(first_part) && !is.null(first_part$text)) {
              return(convert_gpt5_format(first_part$text))
            }
          }
        }
      }
    }
    
    # Fallback: Handle data.frame format with direct content access
    if (is.data.frame(candidates)) {
      if ("content" %in% names(candidates) && nrow(candidates) > 0) {
        content_list <- candidates$content[[1]]
        if (is.list(content_list) && "parts" %in% names(content_list)) {
          parts <- content_list$parts
          if (is.list(parts) && length(parts) > 0) {
            first_part <- parts[[1]]
            if (is.list(first_part) && "text" %in% names(first_part)) {
              return(as.character(first_part$text))
            }
          }
        }
      }
    }
    
    # Handle list format - improved extraction
    if (is.list(candidates)) {
      first_candidate <- candidates[[1]]
      if (is.list(first_candidate)) {
        # Try direct content access
        if ("content" %in% names(first_candidate)) {
          content <- first_candidate$content
          if (is.list(content) && "parts" %in% names(content)) {
            parts <- content$parts
            if (is.list(parts) && length(parts) > 0) {
              first_part <- parts[[1]]
              if (is.list(first_part) && "text" %in% names(first_part)) {
                return(as.character(first_part$text))
              }
            }
          }
        }
        
        # Try alternative structure: candidates[[1]]$parts[[1]]$text
        if ("parts" %in% names(first_candidate)) {
          parts <- first_candidate$parts
          if (is.list(parts) && length(parts) > 0) {
            first_part <- parts[[1]]
            if (is.list(first_part) && "text" %in% names(first_part)) {
              return(as.character(first_part$text))
            }
          }
        }
        
        # Try direct text field in candidate
        if ("text" %in% names(first_candidate)) {
          return(as.character(first_candidate$text))
        }
      }
    }
  }
  
  # Check for direct text field (alternative format)
  if ("text" %in% names(result) && !is.null(result$text) && nchar(as.character(result$text)) > 0) {
    return(as.character(result$text))
  }
  
  # Check for content field (alternative format)
  if ("content" %in% names(result) && !is.null(result$content) && nchar(as.character(result$content)) > 0) {
    return(as.character(result$content))
  }
  
  # Try to extract from any text-like content
  for (field_name in names(result)) {
    field_value <- result[[field_name]]
    if (is.character(field_value) && length(field_value) == 1 && nchar(field_value) > 10) {
      return(field_value)
    }
  }
  
  return("Unable to extract response content")
}

#' Convert GPT-5 format to multi_model_annotate expected format
 #' 
 #' @param content Character string containing GPT-5 response
 #' @return Character string in converted format
 convert_gpt5_format <- function(content) {
   if (is.null(content) || !is.character(content) || nchar(content) == 0) {
     return(content)
   }
   
   # Normalize line endings
   content <- gsub("\r\n?", "\n", content)
   
   # Check if content contains "Final answer:" pattern (for DeepSeek reasoning models)
   if (grepl("Final answer:", content, ignore.case = TRUE)) {
     # Extract content after "Final answer:"
     final_answer_pattern <- ".*Final answer:\\s*(.*)$"
     if (grepl(final_answer_pattern, content, ignore.case = TRUE)) {
       content <- gsub(final_answer_pattern, "\\1", content, ignore.case = TRUE)
       content <- gsub("^\\s+|\\s+$", "", content)  # Trim whitespace
     }
   }
   
   # Check if content is already in >Cell Type< format
   if (grepl("^\\s*>", content) && grepl("<\\s*$", content)) {
     # Content is already in correct format, return as is
     return(content)
   }
   
   # Convert "Population X: [Cell Type]" to ">Cell Type<"
   # Split by lines and process each line
   lines <- strsplit(content, "\n")[[1]]
   converted_lines <- c()
   
   for (line in lines) {
     # Match pattern "Population X: [Cell Type]"
     if (grepl("^Population\\s+\\d+:\\s*(.+)$", line, ignore.case = TRUE)) {
       # Extract the cell type part
       cell_type <- gsub("^Population\\s+\\d+:\\s*(.+)$", "\\1", line, ignore.case = TRUE)
       # Convert to >Cell Type< format
       converted_line <- paste0(">", cell_type, "<")
       converted_lines <- c(converted_lines, converted_line)
     } else {
       # Keep other lines as is
       converted_lines <- c(converted_lines, line)
     }
   }
   
   return(paste(converted_lines, collapse = "\n"))
 }

#' Extract content from Claude format response
#' 
#' @param result Parsed JSON response from Claude API
#' @return Character string of extracted content
extract_claude_content <- function(result) {
  
  if ("content" %in% names(result) && length(result$content) > 0) {
    content_items <- result$content
    
    # Handle direct character string (new format)
    if (is.character(content_items)) {
      # Remove <think> tags and their content for thinking models
      cleaned_content <- gsub("<think>.*?</think>", "", content_items, ignore.case = TRUE)
      # Clean up extra whitespace
      cleaned_content <- gsub("^\\s+|\\s+$", "", cleaned_content)
      cleaned_content <- gsub("\\s+", " ", cleaned_content)
      return(cleaned_content)
    }
    
    # Handle both list and data.frame formats (old format)
    if (is.list(content_items) && !is.data.frame(content_items)) {
      first_content <- content_items[[1]]
    } else if (is.data.frame(content_items)) {
      # For data.frame, extract the first row safely
      if (nrow(content_items) > 0) {
        first_content <- content_items[1, , drop = FALSE]
        # Convert to list for consistent access
        first_content <- as.list(first_content)
      } else {
        return("Unable to extract response content")
      }
    } else {
      first_content <- content_items
    }
    
    if ("text" %in% names(first_content) && !is.null(first_content$text)) {
      # Remove <think> tags and their content for thinking models
      cleaned_content <- gsub("<think>.*?</think>", "", first_content$text, ignore.case = TRUE)
      # Clean up extra whitespace
      cleaned_content <- gsub("^\\s+|\\s+$", "", cleaned_content)
      cleaned_content <- gsub("\\s+", " ", cleaned_content)
      return(cleaned_content)
    }
  }
  
  return("Unable to extract response content")
}

#' Create annotation prompt for cell type annotation
#' 
#' @param markers Data frame containing marker genes
#' @param tissue_context Character string describing tissue context
#' @return Character string containing the formatted prompt
create_annotation_prompt <- function(markers, tissue_context = NULL) {
  
  # Group markers by cluster
  clusters <- split(markers, markers$cluster)
  
  prompt_parts <- c(
    "Please annotate cell types for each cell population based on the following marker genes:\n\n"
  )
  
  if (!is.null(tissue_context)) {
    prompt_parts <- c(prompt_parts, paste("Tissue background:", tissue_context, "\n\n"))
  }
  
  for (cluster_id in names(clusters)) {
    cluster_markers <- clusters[[cluster_id]]
    top_genes <- head(cluster_markers$gene, 10)  # Top 10 genes
    
    prompt_parts <- c(
      prompt_parts,
      paste("Population", cluster_id, "marker genes:", paste(top_genes, collapse = ", "), "\n")
    )
  }
  
  prompt_parts <- c(
    prompt_parts,
    "\nPlease provide the most likely cell type annotation for each population in the following format:\n",
    "Population 0: [Cell Type]\n",
    "Population 1: [Cell Type]\n",
    "...\n",
    "Please annotate based on marker gene expression patterns and biological knowledge."
  )
  
  return(paste(prompt_parts, collapse = ""))
}