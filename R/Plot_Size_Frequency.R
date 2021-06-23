#' Plot Size Frequency Distributions
#'
#' @param data a dataframe of calibrated size metrics, that includes user defined variables based on 1) the directory structure, and 2) the ROI labels used in image analyses. As output from Full_SizeExtractR_Workflow, with calibration lengths not included
#' @param size.metric character vector of length 1. Input the name of the size metric of interest. Must be an element of colnames(data). Case sensitive.
#' @param log_size logical. Whether to log transform the size metric or not.
#' @param nbins the number of bins to show on the histogram
#' @param group_by Set the grouping variable(s). character vector of length 1, 2 or 3, depending on the number of categorical grouping variables. Input the name of the variables c(). Case Sensitive. If no grouping variable is wanted, then leave empty.
#' @param fill_by Set the fill and colour variable. character vector of length 1. Input the name of the variable. Case Sensitive. If no grouping variable is wanted, then leave empty.
#' @param facetRow_by Set the facet row variable. character vector of length 1. Input the name of the variable. Case Sensitive. If no grouping variable is wanted, then leave empty.
#' @param facetCol_by Set the facet row column variable. character vector of length 1. Input the name of the variable. Case Sensitive. If no grouping variable is wanted, then leave empty.
#' @param scales_gg set facet variable of ggplot2::facet_wrap(). Either "free", "free_x", "free_y", or "fixed"
#'
#' @return Returns a ggplot object showing histograms of any size metric, by grouping variable, coupled with probability density curves scaled to the maximum histogram count per group.
#'
#' @export
#'
#' @examples
#' # load a dataset with three grouping variables: Year, Site, and Transect
#' data(Database.groups)
#'
#' # Make a plot with histograms by three groups
#' Plot_Size_Frequency(data = Database.groups,
#'                     size.metric = "Area",
#'                     log_size = TRUE,
#'                     nbins = 20,
#'                     group_by = c("Year","Site","Transect"),
#'                     fill_by = "Transect",
#'                     facetRow_by = "Site",
#'                     facetCol_by = "Year",
#'                     scales_gg = "fixed")
#'
Plot_Size_Frequency = function(data, size.metric, log_size = FALSE,
                               nbins = 15, group_by = FALSE, fill_by = FALSE,
                               facetRow_by = FALSE, facetCol_by = FALSE, scales_gg = "free"){
  if(is.na(match(size.metric, colnames(data)))){
    message("Error: size variable name does not match column names in dataframe")
  } else {
    yVar.vals = data[size.metric][[1]]
    if(is.numeric(yVar.vals) == FALSE){
      message("Error: size variable is not numeric")
    } else {
      # Continue with Script
      if(length(group_by) == 1 && group_by == FALSE){ group_by. = F } else { group_by. = T }
      if(fill_by     == FALSE){ fill_by.  = F } else { fill_by.  = T }
      if(facetRow_by == FALSE){ facetRow. = F } else { facetRow. = T }
      if(facetCol_by == FALSE){ facetCol. = F } else { facetCol. = T }

      if(group_by. == FALSE &&
         (fill_by != FALSE | facetRow_by != FALSE | facetCol_by != FALSE)
      ){
        message(paste("Error: no group_by variable(s) defined, yet either fill or facet variables defined.\n",
                      "If fill or facets are required then grouping variables must be defined (group_by)"))
      } else {

        if(group_by. == TRUE){
          data$group_byPaste = as.factor(apply(data[group_by], 1, paste, collapse = "_"))
          grplevs. = levels(data$group_byPaste)
        } else {
          data$group_byPaste = as.factor("One.Group")
          data$One.Group = "One.Group"
          grplevs. = FALSE
        }


        # Function to scale a density curve to ggplot histogram as counts
        density_counts2 <- function(x, x1, xall, from., to., n., log_size, nbins, grplevs.) {
          if(log_size == TRUE){
            x = log(x)
            x1[size.metric] = log(x1[size.metric])
            from. = log(from.)
            to. = log(to.)
          }

          if(length(grplevs.) == 1 &&  grplevs. == FALSE){
            grplevs. = "One.Group"
          }

          fit <- stats::density(x, from = from., to = to. , n = n., na.rm = TRUE)
          P = ggplot2::ggplot(xall, ggplot2::aes(.data[[size.metric]],
                                                 fill = .data[[group_byPastestr]],
                                                 group = .data[[group_byPastestr]])) +
            ggplot2::geom_histogram(bins=nbins, alpha = 0.5, position = 'identity')

          if(log_size == T){
            P = P + ggplot2::scale_x_log10()
          }

          P
          pb = ggplot2::ggplot_build(P)[["data"]][[1]]

          grpnum = which(grplevs. == unique(x1$group_byPaste))
          MaxCount = max(pb$count[which(pb$group == grpnum)],na.rm=T)

          fit$y = fit$y * MaxCount/ max(fit$y,na.rm=T) # bin number = bins

          if(log_size == TRUE){
            fit$x = exp(fit$x)
          }
          return(fit)
        }


        # Probability densities of areas scaled to counts using 10 bins.
        group_byPastestr = "group_byPaste"

        if(group_by. == TRUE){
          Allgroup_by = unique(c(group_by,group_byPastestr))
        } else {
          Allgroup_by = "One.Group"
        }

        data2 <- data %>%
          dplyr::group_by_at(Allgroup_by) %>%
          dplyr::do(dplyr::as_tibble(data.frame(yVar = density_counts2(x = .data[size.metric][[1]], x1 = .data, xall = data,
                                                                       from. = min(yVar.vals, na.rm=T), to. = max(yVar.vals,na.rm=T),
                                                                       n. = 512, log_size=log_size, nbins = nbins,
                                                                       grplevs. = grplevs.)$x,
                                                Count = density_counts2(x = .data[size.metric][[1]], x1 = .data, xall = data,
                                                                        from. = min(yVar.vals,na.rm=T), to. = max(yVar.vals,na.rm=T),
                                                                        n. = 512, log_size=log_size,  nbins = nbins,
                                                                        grplevs. = grplevs.)$y)))

        Count = "Count"
        yVar = "yVar"
        if(log_size == T){
          x.axis.name = paste(size.metric,"(log scale)")
        } else {
          x.axis.name = size.metric
        }


        P =
          ggplot2::ggplot()


        if(group_by. == F && fill_by. == F){
          P = P +
            ggplot2::geom_histogram(data = data,
                                    ggplot2::aes(x = .data[[size.metric]]),
                                    bins = nbins,
                                    position = 'identity', fill = "grey75",
                                    alpha = 0.2,
                                    colour = "black") +
            ggplot2::geom_line(data = data2,
                               ggplot2::aes(.data[[yVar]], .data[[Count]]),
                               size = 1)
        }

        if(group_by. == T && fill_by. == F){
          P = P +
            ggplot2::geom_histogram(data = data,
                                    ggplot2::aes(x = .data[[size.metric]],
                                                 group = .data[[group_byPastestr]]),
                                    bins = nbins,
                                    position = 'identity', fill = "grey75",
                                    alpha = 0.2,
                                    colour = "black") +
            ggplot2::geom_line(data = data2,
                               ggplot2::aes(.data[[yVar]], .data[[Count]],
                                            group = .data[[group_byPastestr]]),
                               size = 1)
        }

        if(group_by. == T && fill_by. == T){
          P = P +
            ggplot2::geom_histogram(data = data,
                                    ggplot2::aes(x = .data[[size.metric]],
                                                 group = .data[[group_byPastestr]],
                                                 fill = .data[[fill_by]]),
                                    bins = nbins,
                                    position = 'identity',
                                    alpha = 0.2,
                                    colour = "black") +
            ggplot2::geom_line(data = data2,
                               ggplot2::aes(.data[[yVar]], .data[[Count]],
                                            group = .data[[group_byPastestr]],
                                            colour = .data[[fill_by]]),
                               size = 1)
        }




        P = P +
          ggplot2::labs(x = x.axis.name, y = "Counts") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank())

        if(log_size == T){
          P = P + ggplot2::scale_x_log10(expand = c(0, 0),
                                         breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100,1000,10000),
                                         labels = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100,1000,10000))
        }

        if(facetRow. == T && facetCol. == F){
          P = P + ggplot2::facet_wrap(ggplot2::vars(.data[[facetRow_by]]),
                                      scales = scales_gg)
        }
        if(facetRow. == F && facetCol. == T){
          P = P + ggplot2::facet_grid(ggplot2::vars(.data[[facetCol_by]]),
                                      scales = scales_gg)
        }
        if(facetRow. == T && facetRow. == T){
          P = P + ggplot2::facet_grid(ggplot2::vars(.data[[facetCol_by]]),
                                      ggplot2::vars(.data[[facetRow_by]]),
                                      labeller = "label_both",
                                      scales = scales_gg)
        }


        P
        return(P)
      }
    }
  }
}

