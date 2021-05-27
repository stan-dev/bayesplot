#' Find opportunities for model stacking 
#' 
#' The pancake plot shows how the predictive accuracy of two different
#' models varies as another variable changes. 
#' 
#' @param y A vector of observations. See Details.
#' @param psis_object_1,psis_object_2 If using loo version 2.0.0 or greater, 
#' an object returned by the `[loo::psis()]` function (or by the 
#' `[loo::loo()]` function with argument `save_psis` set to `TRUE`).
#' @param ... Currently unused.
#' @param group A grouping variable (a vector or factor) the same length 
#' as `y`. Each value in group is interpreted as the group level pertaining 
#' to the corresponding value of `y`. If `FALSE`, ignored.
#' @param size,alpha,jitter Passed to `[ggplot2::geom_point()]` to control 
#' aesthetics. `size` and `alpha` are passed to to the `size` and `alpha` 
#' arguments of `[ggplot2::geom_jitter()]` to control the appearance of 
#' points. `jitter` can be either a number or a vector of numbers.
#' Passing a single number will jitter variables along the x axis only, while 
#' passing a vector will jitter along both axes.
#' @param quantiles Boolean that determines whether to plot the quantiles of
#' `y` rather than `y` itself. Useful when `y` has a very irregular 
#' distribution.
#' @param sortByGroup Sort observations by `group`, then plot against an 
#' arbitrary index. Plotting by index can be useful when categories have 
#' very different sample sizes. 
#' 
#' 
#' @template return-ggplot
#' 
#' @template reference-vis-paper
#' 
#' @examples 
#' 
#' library(loo)
#' 
#' cbPalette <- c("#636363", "#E69F00", "#56B4E9", "#009E73", 
#'                "#F0E442", "#0072B2","#CC79A7")
#' 
#' # Plot using groups from WHO
#' 
#' pancake_plot(factor(GM@data$super_region_name), loo3, loo2, 
#'              group = GM@data$super_region_name, alpha = .5, 
#'              jitter = c(.45, .2)
#'              ) + 
#'              xlab("Region") + scale_colour_manual(values=cbPalette)
#' 
#' # Plot using groups identified with clustering
#' 
#' pancake_plot(factor(GM@data$cluster_region), loo3, loo2, 
#'              group = GM@data$super_region_name, alpha = .5, 
#'              jitter = c(.45, .2)
#'              ) + 
#'              xlab("Cluster Group") + scale_colour_manual(values=cbPalette)
#'              
#' # Plot using an index variable to reduce crowding
#' 
#' pancake_plot(1:2980, loo3, loo2, group = GM@data$super_region_name, 
#'              alpha = .5, sortByGroup = TRUE, 
#'              ) + 
#'              xlab("Index") + scale_colour_manual(values=cbPalette)
#' 
#' 
pancake_plot <- 
  function(y,
           psis_object_1,
           psis_object_2,
           ...,
           group = FALSE,
           outlier_thresh = FALSE,
           size = 1,
           alpha = 1,
           jitter = 0,
           quantiles = FALSE,
           sortByGroup = FALSE
  ){
 
    
    
    # I called it a pancake plot because stacks remind me of pancakes.
    # I did just eat pancakes. (Can be renamed)
    
    # Adding a 0 at the end lets users provide a single number as input.
    # In this case, only horizontal jitter is applied.
    jitter <- c(jitter, 0) 
    
    elpdDif <- psis_object_1$pointwise[, "elpd_loo"] - 
               psis_object_2$pointwise[, "elpd_loo"]
    

    if (quantiles){
      # If quantiles is set to true, replace all y values with their quantile
      y <- ecdf(y)(y)
    }

    
    if (sortByGroup){
      if (identical(group, FALSE) || !identical(y, 1:length(y))){
        stop("ERROR: sortByGroup should only be used for grouping categorical 
             variables, then plotting them with an arbitrary index. You can
             create such an index using `1:length(data)`.
             ")
      }
      
      values <- group_by(tibble(group, elpdDif), factor(group)) %>%
                    arrange(.by_group = TRUE)
      
      elpdDif <- pull(values, elpdDif)
      group <- pull(values, group)
      
    }
    
    
    plot <- ggplot(mapping=aes(y, elpdDif)) +
            geom_hline(yintercept=0) + 
            xlab(ifelse(sortByGroup, "y", "Index")) +
            ylab(expression(ELPD[i][1] - ELPD[i][2])) + 
            labs(color = "Groups")
    

    
    if (identical(group, FALSE)){ 
      # Don't color by group if no groups are passed
      plot <- plot +  
              geom_jitter(width = jitter[1], height = jitter[2], 
                          alpha = alpha, size = size
                          ) 
    }
    else{
      # If group is passed, use color
      plot <- plot +  
          geom_jitter(aes(color = factor(group)),
                      width = jitter[1], height = jitter[2], 
                      alpha = alpha, size = size
                      ) 
    }
    
    if (!identical(outlier_thresh, FALSE)){
      # Flag outliers
      is_outlier <- elpdDif > outlier_thresh
      index <- 1:length(y)
      outlier_labs <- index[is_outlier]
      
      plot <- plot + annotate("text",
                              x = y[is_outlier],
                              y = elpdDif[outlier_labs],
                              label = outlier_labs,
                              size = 4
                              )
              
      
    }
    
    return(plot)
  }

