#' plot model coefficients for a single model
#'
#' @title plot_model_coefs
#'
#' @param model_output either model_list$all, model_list$bats, or model_list$rodents
#' @param model_type "glm" (all or rodents) or "glmm" (bats)
#' @param pretty_labels labels for model predictors
#' @param show_x_label boolean, whether "Odds ratios and 95% CIs" should appear
#' @return 
#' @export
plot_model_coefs <- function(model_output, model_type, pretty_labels,
                             show_x_label) {
  
  CIs <- confint(model_output)
  
  if(model_type == "glm"){
    model_coefs = data.frame(
      beta = model_output$coefficients, 
      ci.lb = CIs[, 1],
      ci.ub = CIs[, 2])
  }else if(model_type == "glmm"){
    model_coefs = data.frame(
      beta = summary(model_output)$coefficients$cond[, 1],
      ci.lb = CIs[1:7, 1],
      ci.ub = CIs[1:7, 2])
  }
  
  # exponentiate to get ORs
  model_coefs = exp(model_coefs)
  
  model_coefs$coef <- pretty_labels
  
  # set the order for plotting variables
  model_coefs$coef <- as.factor(model_coefs$coef)
  model_coefs$coef <- factor(
    model_coefs$coef,
    levels = rev(pretty_labels))
  
  # set color formatting based on whether CI crosses 1
  list <- c()
  for(i in 1:nrow(model_coefs)){
    if(model_coefs$ci.lb[i] < 1 & model_coefs$ci.ub[i] < 1){
      new_element <- "no"
      list <- c(list, new_element)
    }
    else if(model_coefs$ci.lb[i] > 1 & model_coefs$ci.ub[i] > 1){
      new_element <- "no"
      list <- c(list, new_element)
    }
    else{
      new_element <- "yes"
      list <- c(list, new_element)
    }
  }
  model_coefs$ci_crosses_one <- list
  model_coefs$ci_crosses_one <- as.factor(model_coefs$ci_crosses_one)
  
  coef_plot <- ggplot(model_coefs) + 
    geom_hline(yintercept = 1, linetype = "dashed", colour = "black") + 
    geom_errorbar(aes(x = coef, ymin = ci.lb, ymax = ci.ub, 
                      color = ci_crosses_one), 
                  width = 0, linewidth = 2.5) + 
    scale_color_manual(values = c("#5e4fa2", "darkgray"), guide = "none") + 
    geom_point(aes(x = coef, y = beta, color = ci_crosses_one), size = 5) + 
    coord_flip() + 
    theme_bw(base_size = 22) + 
    labs(x = NULL) + 
    ylab("Odds ratios and 95% CIs") + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(color = "black"),
          axis.line = element_line(),
          legend.position = "blank") + 
    scale_y_continuous(limits = c(0, 5))
  
  # helps for later plotting
  if(show_x_label == FALSE){
    coef_plot <- coef_plot + ylab("")
  }
  
  return(coef_plot)
  
}