

     ### top 8
      
      ## X ~ M -- derive top 8
      # Initialize vectors to store R-squared values
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_X ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      # Get indices of the top 10 individual R^2 values
      top_indices_8 <- order(adj_rsquared_individual, decreasing = TRUE)[1:8]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_8 <- adj_rsquared_individual[top_indices_8]
      
      ## X ~ M -- indiv
      rsquared_individual_x = numeric(length(top_indices_8))
      adj_rsquared_individual_x = numeric(length(top_indices_8))
      # Loop through each M variable
      for (i in 1:length(top_indices_8)) {
        # Define the formula for the linear model
        factor_index <- top_indices_8[i]
        formula = as.formula(paste("Average_X ~ ", paste0("Average_M", factor_index)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_x[i] = summary(model)$r.squared
        adj_rsquared_individual_x[i] = summary(model)$adj.r.squared
        mem_output_df_top_8_indiv <- rbind(mem_output_df_top_8_indiv, data.frame(
                                      tbl = tbl,
                                      mem = mem,
                                      ROI = ROI,
                                      num_factors = factor_index,
                                      indiv_rsq = rsquared_individual_x[i],
                                      indiv_adj_rsq = adj_rsquared_individual_x[i]))
      }
      ## X ~ M -- cumulative
      # Initialize vectors to store cumulative R-squared and adjusted R-squared values
      cumulative_rsquared_x <- numeric(length(top_indices_8))
      cumulative_adj_rsquared_x <- numeric(length(top_indices_8))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_8)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_8[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_x <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_x <- summary(cumulative_model)$adj.r.squared
        
        mem_output_df_top_8 <- rbind(mem_output_df_top_8, data.frame(tbl = tbl,
                                                                     mem = mem,
                                                                     ROI = ROI,
                                                                     num_factors = i,
                                                                     cumulative_rsq = cumulative_rsquared_x,
                                                                     cumulative_adj_rsq = cumulative_adj_rsquared_x))
      }
      
      ## Y ~ M -- derive top
      # Initialize vectors to store R-squared values
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_Y ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      # Get indices of the top 10 individual R^2 values
      top_indices_8 <- order(adj_rsquared_individual, decreasing = TRUE)[1:8]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_8 <- adj_rsquared_individual[top_indices_8]
      
      
      ## Y ~ M -- indiv
      top_indices_8 <- order(adj_rsquared_individual, decreasing = TRUE)[1:8]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_8 <- adj_rsquared_individual[top_indices_8]
      
      # Initialize vectors to store R-squared and adjusted R-squared values for the top factors
      rsquared_indivdual <- numeric(length(top_indices_8))
      adj_rsquared_individual <- numeric(length(top_indices_8))
      # Loop over the top_indices instead of 1:how_many_fac
      for (i in seq_along(top_indices_8)) {
        # Get the factor index from top_indices
        factor_index <- top_indices_8[i]
        # Define the formula for the linear model using the top factor
        formula <- as.formula(paste("Average_Y ~ ", paste0("Average_M", factor_index)))
        # Fit the linear model
        model <- lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared and adjusted R-squared values
        rsquared_individual[i] <- summary(model)$r.squared
        adj_rsquared_individual[i] <- summary(model)$adj.r.squared
        output_df_top_8_indiv <- rbind(output_df_top_8_indiv, data.frame(
                                      tbl = tbl,
                                      mem = mem,
                                      ROI = ROI,
                                      num_factors = factor_index,
                                      indiv_rsq = rsquared_individual[i],
                                      indiv_adj_rsq = adj_rsquared_individual[i]))
      }
      
      ## Y ~ M -- cumulative
      cumulative_rsquared <- numeric(length(top_indices_8))
      cumulative_adj_rsquared <- numeric(length(top_indices_8))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_8)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_8[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        output_df_top_8 <- rbind(output_df_top_8, data.frame(tbl = tbl,
                                                             mem = mem,
                                                             ROI = ROI,
                                                             num_factors = i,
                                                             cumulative_rsq = cumulative_rsquared ,
                                                             cumulative_adj_rsq = cumulative_adj_rsquared))
      }
      
      ## Y ~ X*M -- derive top
      # Initialize vectors to store R-squared values
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      # Get indices of the top 10 individual R^2 values
      top_indices_8 <- order(adj_rsquared_individual, decreasing = TRUE)[1:8]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_8 <- adj_rsquared_individual[top_indices_8]
      
      
      ## Y ~ X*M -- indiv
      rsquared_individual_xm = numeric(length(top_indices_8))
      adj_rsquared_individual_xm = numeric(length(top_indices_8))
      # Loop through each M variable
      for (i in 1:length(top_indices_8)) {
        factor_index <- top_indices_8[i]
        # Define the formula for the linear model using the top factor
        formula <- as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", factor_index)))
        # Define the formula for the linear model
        #formula = as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_xm[i] = summary(model)$r.squared
        adj_rsquared_individual_xm[i] = summary(model)$adj.r.squared
        interaction_output_df_top_8_indiv <- rbind(interaction_output_df_top_8_indiv, data.frame(
                                              tbl = tbl,
                                              mem = mem,
                                              ROI = ROI,
                                              num_factors = factor_index,
                                              indiv_rsq = rsquared_individual_xm[i],
                                              indiv_adj_rsq = adj_rsquared_individual_xm[i]))
      }
      
      ## Y ~ X*M -- cumulative
      cumulative_rsquared_xm <- numeric(length(top_indices_8))
      cumulative_adj_rsquared_xm <- numeric(length(top_indices_8))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_8)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_8[1:i]
        # Create the model formula by including the selected top factors cumulatively
        #formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        #cumulative_formula <- as.formula(paste("Average_Y ~ Average_X + ", formula_terms))
        formula_terms <- paste(paste0("Average_X*Average_M", selected_factors), collapse=" + ")
        # Combine the predictor terms with the dependent variable to form the complete formula
        cumulative_formula <- paste("Average_Y ~", formula_terms)
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_xm[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_xm[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_xm <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_xm <- summary(cumulative_model)$adj.r.squared
        interaction_output_df_top_8 <- rbind(interaction_output_df_top_8, data.frame(tbl = tbl,
                                                                                     mem = mem,
                                                                                     ROI = ROI,
                                                                                     num_factors = i,
                                                                                     cumulative_rsq = cumulative_rsquared_xm,
                                                                                     cumulative_adj_rsq = cumulative_adj_rsquared_xm))
      }
      
      
      
      ### top 50
      
      ## X ~ M -- derive top 50
      # Initialize vectors to store R-squared values
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_X ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      # Get indices of the top 10 individual R^2 values
      top_indices_50 <- order(adj_rsquared_individual, decreasing = TRUE)[1:50]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_50 <- adj_rsquared_individual[top_indices_50]
      
      
      ## X ~ M -- indiv
      rsquared_individual_x = numeric(length(top_indices_50))
      adj_rsquared_individual_x = numeric(length(top_indices_50))
      # Loop through each M variable
      for (i in 1:length(top_indices_50)) {
        factor_index <- top_indices_50[i]
        # Define the formula for the linear model
        formula = as.formula(paste("Average_X ~ ", paste0("Average_M", factor_index)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_x[i] = summary(model)$r.squared
        adj_rsquared_individual_x[i] = summary(model)$adj.r.squared
        
        mem_output_df_top_50_indiv <- rbind(mem_output_df_top_50_indiv, data.frame(
                                      tbl = tbl,
                                      mem = mem,
                                      ROI = ROI,
                                      num_factors = factor_index,
                                      indiv_rsq =rsquared_individual_x[i],
                                      indiv_adj_rsq =  adj_rsquared_individual_x[i]))
      }
      
      
      ## X ~ M -- cumulative
      cumulative_rsquared_x <- numeric(length(top_indices_50))
      cumulative_adj_rsquared_x <- numeric(length(top_indices_50))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_50)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_50[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_X ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_x[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_x[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_x <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_x <- summary(cumulative_model)$adj.r.squared
        
        mem_output_df_top_50 <- rbind(mem_output_df_top_50, data.frame(tbl = tbl,
                                                                       mem = mem,
                                                                       ROI = ROI,
                                                                       num_factors = i,
                                                                       cumulative_rsq = cumulative_rsquared_x,
                                                                       cumulative_adj_rsq = cumulative_adj_rsquared_x))
      }
      
      
      
      ## Y ~ M -- derive top
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_Y ~ ", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      # 
      # # Get indices of the top 10 R^2 values
      top_indices_50 <- order(adj_rsquared_individual, decreasing = TRUE)[1:50]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_50 <- adj_rsquared_individual[top_indices_50]
      
      
      ## Y ~ M -- indiv
      rsquared_indiv <- numeric(length(top_indices_50))
      adj_rsquared_indiv <- numeric(length(top_indices_50))
      # Loop over the top_indices instead of 1:how_many_fac
      for (i in seq_along(top_indices_50)) {
        # Get the factor index from top_indices
        factor_index <- top_indices_50[i]
        # Define the formula for the linear model using the top factor
        formula <- as.formula(paste("Average_Y ~ ", paste0("Average_M", factor_index)))
        # Fit the linear model
        model <- lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared and adjusted R-squared values
        rsquared_indiv[i] <- summary(model)$r.squared
        adj_rsquared_indiv[i] <- summary(model)$adj.r.squared
        output_df_top_50_indiv <- rbind(output_df_top_50_indiv, data.frame(
                                  tbl = tbl,
                                  mem = mem,
                                  ROI = ROI,
                                  num_factors = factor_index,
                                  indiv_rsq = rsquared_indiv[i],
                                  indiv_adj_rsq =  adj_rsquared_indiv[i]))
      }
      
      ## Y ~ M -- cumulative
      cumulative_rsquared <- numeric(length(top_indices_50))
      cumulative_adj_rsquared <- numeric(length(top_indices_50))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_50)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_50[1:i]
        # Create the model formula by including the selected top factors cumulatively
        formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        cumulative_formula <- as.formula(paste("Average_Y ~ ", formula_terms))
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared <- summary(cumulative_model)$adj.r.squared
        
        output_df_top_50 <- rbind(output_df_top_50, data.frame(tbl = tbl,
                                                               mem = mem,
                                                               ROI = ROI,
                                                               num_factors = i,
                                                               cumulative_rsq = cumulative_rsquared ,
                                                               cumulative_adj_rsq = cumulative_adj_rsquared))
      }
      
      ## Y ~ X*M -- derive top 50
      rsquared_individual = numeric(how_many_fac)
      adj_rsquared_individual = numeric(how_many_fac)
      # Loop through each M variable
      for (i in 1:how_many_fac) {
        # Define the formula for the linear model
        formula = as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", i)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual[i] = summary(model)$r.squared
        adj_rsquared_individual[i] = summary(model)$adj.r.squared
      }
      # 
      # # Get indices of the top 10 R^2 values
      top_indices_50 <- order(adj_rsquared_individual, decreasing = TRUE)[1:50]
      # Extract the top 10 R^2 values using the indices
      top_rsq_values_50 <- adj_rsquared_individual[top_indices_50]
      
      
      
      ## Y ~ X*M -- indiv
      rsquared_individual_xm = numeric(length(top_indices_50))
      adj_rsquared_individual_xm = numeric(length(top_indices_50))
      # Loop through each M variable
      for (i in 1:length(top_indices_50)) {
        factor_index <- top_indices_50[i]
        # Define the formula for the linear model
        #formula = as.formula(paste("Average_Y ~ Average_X + ", paste0("Average_M", i)))
        formula = as.formula(paste("Average_Y ~ Average_X*", paste0("Average_M", factor_index)))
        # Fit the linear model
        model = lm(formula, data = average_values_by_Item)
        # Extract and store the R-squared value
        rsquared_individual_xm[i] = summary(model)$r.squared
        adj_rsquared_individual_xm[i] = summary(model)$adj.r.squared
        interaction_output_df_top_50_indiv <- rbind(interaction_output_df_top_50_indiv, data.frame(
                                                tbl = tbl,
                                                mem = mem,
                                                ROI = ROI,
                                                num_factors = factor_index,
                                                indiv_rsq = rsquared_individual_xm[i],
                                                indiv_adj_rsq =  adj_rsquared_individual_xm[i]))
        
      }
      
      
      ## Y ~ X*M -- cumulative
      cumulative_rsquared_xm <- numeric(length(top_indices_50))
      cumulative_adj_rsquared_xm <- numeric(length(top_indices_50))
      # Loop to calculate cumulative R-squared values using top factors
      for (i in 1:length(top_indices_50)) {
        # Select the top factors up to the i-th
        selected_factors <- top_indices_50[1:i]
        # Create the model formula by including the selected top factors cumulatively
        #formula_terms <- paste(paste0("Average_M", selected_factors), collapse=" + ")
        #cumulative_formula <- as.formula(paste("Average_Y ~ Average_X + ", formula_terms))
        formula_terms <- paste(paste0("Average_X*Average_M", selected_factors), collapse=" + ")
        # Combine the predictor terms with the dependent variable to form the complete formula
        cumulative_formula <- paste("Average_Y ~", formula_terms)
        # Fit the linear model with the cumulative set of factors
        cumulative_model <- lm(cumulative_formula, data = average_values_by_Item)
        # Extract and store the cumulative R-squared and adjusted R-squared values
        # cumulative_rsquared_xm[i] <- summary(cumulative_model)$r.squared
        # cumulative_adj_rsquared_xm[i] <- summary(cumulative_model)$adj.r.squared
        cumulative_rsquared_xm <- summary(cumulative_model)$r.squared
        cumulative_adj_rsquared_xm <- summary(cumulative_model)$adj.r.squared
        
        interaction_output_df_top_50 <- rbind(interaction_output_df_top_50, data.frame(tbl = tbl,
                                                                                       mem = mem,
                                                                                       ROI = ROI,
                                                                                       num_factors = i,
                                                                                       cumulative_rsq = cumulative_rsquared_xm,
                                                                                       cumulative_adj_rsq = cumulative_adj_rsquared_xm))
        
      }
      

### how_many_factors

## M ~ X -- indiv


## M ~ X -- cumulative


## Y ~ M -- indiv


## Y ~ M -- cumulative


## Y ~ X*M -- indiv


## Y ~ X*M -- cumulative

