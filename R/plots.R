#' Shortcut functions for commonly used plots in RP style
#'
#' \code{geom_tree_tot_by_split} creates a 'treemap' chart of the total of one outcome, split by another outcome.
#'
#' @param opts can be substituted with a (predefined or in-function) list of style parameters
#'
#'
#' @details in future we could try to make this more general, allowing other types of summary
#'
#' @examples
#' geom_treemap_opts_1 <- list(treemapify::geom_treemap(alpha = 0.5),
#'  treemapify::geom_treemap_text(fontface = "italic", colour = "blue", place = "centre",
#'                    grow = TRUE, min.size = 3 ),
#'  theme(legend.position = c(0.6, 0.2),
#'       plot.title = element_text(hjust = 0.5))
#'      )
#'
#'  midwest %>% geom_tree_tot_by_split(popamerindian, state, title = "Shares of midwestern 'American Indian residents' by state", opts= geom_treemap_opts_1)

#' @export

geom_tree_tot_by_split <- function(df, outcome, splitvar, title = "",
                                   opts = list(treemapify::geom_treemap(alpha = 0.7),
                                               treemapify::geom_treemap_text(
                                                 fontface = "italic",
                                                 colour = "white",
                                                 place = "centre",
                                                 grow = TRUE,
                                                 min.size = 1 ),
                                               theme(legend.position = "none",
                                                     plot.title = element_text(hjust = 0.5))
                                   )) {
  #TODO -- make a 'summary stat' argument
  df %>%
    dplyr::filter(!is.na({{splitvar}})) %>%
    select({{outcome}}, {{splitvar}}) %>%
    group_by({{splitvar}}) %>%
  summarise(total_outcome = sum({{outcome}}, na.rm=TRUE)) %>%
  mutate(outcome_share = round(total_outcome/sum(total_outcome)*100)) %>%
  ggplot(aes(area = total_outcome, fill= {{splitvar}},
             # Include percentage of total donation
             label = paste({{splitvar}}, paste0(outcome_share, "%"), sep = "\n"))) +
    opts +
  ggtitle(title)
}

