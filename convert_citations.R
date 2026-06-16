# convert_citations.R - Fixed version
# Script to process citations in Hugo markdown files using Pandoc

# Function to ensure Pandoc is available
ensure_pandoc <- function() {
  # Try system pandoc
  pandoc_check <- tryCatch({
    system("pandoc --version", intern = TRUE, ignore.stderr = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  
  if (pandoc_check) {
    cat("✅ Pandoc found in system PATH\n")
    return("pandoc")
  }
  
  # Try RStudio's Pandoc
  rstudio_pandoc <- Sys.getenv("RSTUDIO_PANDOC")
  if (rstudio_pandoc != "") {
    pandoc_exe <- file.path(rstudio_pandoc, "pandoc")
    if (file.exists(pandoc_exe)) {
      cat("✅ Pandoc found in RStudio:", pandoc_exe, "\n")
      return(pandoc_exe)
    }
  }
  
  # Try rmarkdown's Pandoc
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    pandoc_path <- tryCatch({
      rmarkdown::pandoc_exec()
    }, error = function(e) NULL)
    
    if (!is.null(pandoc_path) && file.exists(pandoc_path)) {
      cat("✅ Pandoc found via rmarkdown:", pandoc_path, "\n")
      return(pandoc_path)
    }
  }
  
  # If we get here, pandoc is not found
  cat("\n❌ Pandoc not found!\n")
  cat("Please install Pandoc manually from: https://pandoc.org/installing.html\n")
  cat("Or run: install.packages('rmarkdown')\n")
  return(NULL)
}

# Function to convert with pandoc
convert_with_pandoc <- function(input_file, output_file = NULL) {
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop("❌ Input file not found: ", input_file)
  }
  
  # Generate output filename if not provided
  if (is.null(output_file)) {
    output_file <- gsub("\\.md$", "-converted.md", input_file)
  }
  
  # Check for required files
  if (!file.exists("references.bib")) {
    cat("⚠️ references.bib not found. Creating placeholder...\n")
    writeLines('@example{test2024, title={Test}, author={Test}, year={2024}}', "references.bib")
  }
  
  if (!file.exists("apa.csl")) {
    cat("📥 Downloading apa.csl...\n")
    tryCatch({
      download.file(
        "https://raw.githubusercontent.com/citation-style-language/styles/master/apa.csl",
        "apa.csl",
        quiet = TRUE
      )
      cat("✅ Downloaded apa.csl\n")
    }, error = function(e) {
      cat("⚠️ Could not download apa.csl. Using a basic CSL.\n")
      # Create a basic CSL
      writeLines('<?xml version="1.0" encoding="utf-8"?><style xmlns="http://purl.org/net/xbiblio/csl" version="1.0"><info><title>APA</title></info></style>', "apa.csl")
    })
  }
  
  # Ensure pandoc is available
  pandoc <- ensure_pandoc()
  if (is.null(pandoc)) {
    stop("❌ Pandoc not available. Cannot process citations.")
  }
  
  # Build the pandoc command
  cmd <- paste(
    shQuote(pandoc),
    shQuote(input_file),
    "--bibliography references.bib",
    "--csl apa.csl",
    "--citeproc",
    "-t markdown",
    "-o", shQuote(output_file)
  )
  
  cat("\n🔧 Running command:\n", cmd, "\n\n")
  
  # Execute the command
  result <- tryCatch({
    system(cmd, intern = TRUE, ignore.stderr = FALSE)
  }, error = function(e) {
    cat("❌ Error running pandoc:", e$message, "\n")
    return(NULL)
  }, warning = function(w) {
    cat("⚠️ Warning:", w$message, "\n")
    return(NULL)
  })
  
  # Check if conversion worked
  if (file.exists(output_file)) {
    message("\n✅ SUCCESS! Citations processed in: ", output_file)
    
    # Show a preview with citations
    content <- readLines(output_file, warn = FALSE)
    
    # Check for APA-style citations
    apa_citations <- grep("\\([A-Za-zÀ-ÿ]+,\\s*[0-9]{4}\\)|et al\\.", content, value = TRUE)
    raw_citations <- grep("\\[@[a-zA-Z0-9_]+\\]", content, value = TRUE)
    
    cat("\n📄 Preview of converted citations:\n")
    cat("----------------------------------------\n")
    if (length(apa_citations) > 0) {
      cat("✅ Found APA-style citations:\n")
      cat(head(apa_citations, 5), sep = "\n")
    } else if (length(raw_citations) > 0) {
      cat("⚠️ Raw citations still present. Pandoc might not be processing correctly.\n")
      cat("Found:", head(raw_citations, 5), sep = "\n")
    } else {
      cat("No citations found in the document.\n")
    }
    cat("\n----------------------------------------\n")
    
    return(output_file)
  } else {
    cat("\n❌ Conversion failed.\n")
    cat("💡 Troubleshooting tips:\n")
    cat("1. Check if Pandoc is installed: pandoc --version\n")
    cat("2. Verify files exist: references.bib, apa.csl\n")
    cat("3. Check file path contains no special characters\n")
    return(NULL)
  }
}

# Fix filenames with spaces
fix_filename <- function(file_path) {
  new_path <- gsub(" ", "-", file_path)
  if (file_path != new_path) {
    result <- file.rename(file_path, new_path)
    if (result) {
      cat("📝 Renamed:", basename(file_path), "→", basename(new_path), "\n")
      return(new_path)
    } else {
      cat("⚠️ Could not rename file. Using original path.\n")
      return(file_path)
    }
  }
  return(file_path)
}

# Main execution
cat("\n📚 CITATION PROCESSOR FOR HUGO\n")
cat("===============================\n")
cat("Working directory:", getwd(), "\n\n")

# Create content/post if it doesn't exist
if (!dir.exists("content/post")) {
  cat("⚠️ content/post directory not found. Creating it...\n")
  dir.create("content/post", recursive = TRUE)
}

# Find all markdown files in content/post
post_files <- list.files("content/post", pattern = "\\.md$", full.names = TRUE)

if (length(post_files) == 0) {
  cat("❌ No .md files found in content/post/\n")
  cat("💡 Make sure your post files are in: content/post/\n")
  quit(status = 1)
}

cat("📄 Found", length(post_files), "post(s):\n")
for (i in seq_along(post_files)) {
  cat("   ", i, ". ", basename(post_files[i]), "\n", sep = "")
}

# Let user select which file to process
cat("\n🔢 Enter the number of the file to process (or 'all' for all files): ")
choice <- readline(prompt = "> ")

# Process user input without coercion warnings
files_to_process <- NULL

if (tolower(choice) == "all") {
  files_to_process <- post_files
  cat("Processing all files...\n")
} else if (grepl("^[0-9]+$", choice)) {
  # This is a number - check it's valid
  file_num <- as.numeric(choice)
  if (!is.na(file_num) && file_num >= 1 && file_num <= length(post_files)) {
    files_to_process <- post_files[file_num]
    cat("Processing file:", basename(post_files[file_num]), "\n")
  } else {
    cat("❌ Invalid selection. Please enter a number between 1 and", length(post_files), "\n")
    cat("Processing the first file instead.\n")
    files_to_process <- post_files[1]
  }
} else {
  cat("❌ Invalid input. Processing the first file.\n")
  files_to_process <- post_files[1]
}

# Process each selected file
converted_files <- character()
for (file in files_to_process) {
  cat("\n" , rep("=", 50), "\n", sep = "")
  cat("📝 Processing:", basename(file), "\n")
  
  # Fix filename if it has spaces
  fixed_file <- fix_filename(file)
  
  # Process with Pandoc
  output_file <- gsub("\\.md$", "-converted.md", fixed_file)
  result <- tryCatch({
    convert_with_pandoc(fixed_file, output_file)
  }, error = function(e) {
    cat("❌ Error processing file:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(result) && file.exists(result)) {
    converted_files <- c(converted_files, result)
    
    # Ask to replace original
    cat("\n❓ Replace original file with processed version?\n")
    response <- tolower(readline(prompt = "Replace? (y/n): "))
    
    if (response == "y") {
      success <- file.copy(result, fixed_file, overwrite = TRUE)
      if (success) {
        file.remove(result)
        cat("✅ Updated:", basename(fixed_file), "\n")
      } else {
        cat("⚠️ Could not update original file. Check file permissions.\n")
      }
    } else {
      cat("ℹ️ Processed version saved as:", basename(result), "\n")
    }
  } else {
    cat("⚠️ No output generated for:", basename(file), "\n")
  }
}

# Summary
cat("\n" , rep("=", 50), "\n", sep = "")
cat("📊 SUMMARY\n")
cat("----------------------------------------\n")
cat("✅ Processed:", length(files_to_process), "file(s)\n")
cat("✅ Converted:", length(converted_files), "file(s)\n")
cat("\n📋 Next steps:\n")
cat("1. Preview your post with: blogdown::serve_site()\n")
cat("2. If citations look good, commit and push\n")
cat("3. Check: http://localhost:4321\n")
cat("\n" , rep("=", 50), "\n", sep = "")