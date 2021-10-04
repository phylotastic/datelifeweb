input_taxa <- c("Rhea americana, Pterocnemia pennata, Struthio camelus")
input_partial <- TRUE
input_usetnrs <- TRUE
input_approximatematch <- FALSE
input_highertaxon <- TRUE

rv <- list(input_taxa = input_taxa,
           input_partial =input_partial,
           input_usetnrs = input_usetnrs,
           input_approximatematch = input_approximatematch,
           input_highertaxon = input_highertaxon
           )

      get_filtered_results <- 
         datelife::get_datelife_result(input = rv$input_taxa,  #input$taxa,
         partial = rv$input_partial, use_tnrs = rv$input_usetnrs,
         approximate_match = rv$input_approximatematch,
         get_spp_from_taxon = rv$input_highertaxon)
      