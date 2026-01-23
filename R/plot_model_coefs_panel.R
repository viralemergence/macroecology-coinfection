#' plot model coefficients
#'
#' @title plot_model_coefs_panel
#'
#' @param model_list
#'
#' @return 
#' @export
plot_model_coefs_panel <- function(model_list){
  
  p1 <- plot_model_coefs(model_output = model_list$all, 
                         model_type = "glm", 
                         pretty_labels = c("intercept", "SES Asia", "birds", 
                                           "rodents", "shrews", "male", 
                                           "subadult", "juvenile", 
                                           "owned\ndomesticated", 
                                           "wild in\ncaptivity"),
                         show_x_label = FALSE)
  
  p2 <- plot_model_coefs(model_output = model_list$bats, 
                         model_type = "glmm", 
                         pretty_labels = c("intercept", "SES Asia", "male", 
                                           "subadult",  "juvenile",
                                           "cave-\nroosting"),
                         show_x_label = FALSE)
  
  p3 <- plot_model_coefs(model_output = model_list$rodents, 
                         model_type = "glm", 
                         pretty_labels = c("intercept", "male", 
                                           "subadult/\njuvenile"),
                         show_x_label = TRUE)
  
  coefs_panel <- (p1 / p2 / p3) #+
    #plot_layout(axes = "collect")
  
  return(coefs_panel)
}