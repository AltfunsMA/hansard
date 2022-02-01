css_marker <- c(".docDiv", "#documentContent") # selecting using SG online

poss_html_node <- possibly(html_node, otherwise = "No node")
poss_html_text <- possibly(html_text, otherwise = "No text")

extract_text <- function(html, title) {

  # For each html article, try to extract nodes with all css_markers
  # Then extract text from those that don't return NA and save in list
  
  text_temp <- list()
  
  for (i in 1:length(css_marker)) {
    
    node <- poss_html_node(html, css_marker[[i]])
    
    if(!is.na(node)) {
      
      text_temp[[i]]  <- poss_html_text(node) 
      
    } 
    
  }
  
  if (length(text_temp) == 0) {
    
    # message(paste0("No text for:", title)) 
    return(NA)
    
  }
  
  # Select those that have long enough text
  
  txt_selected <- unlist(text_temp[which(nchar(text_temp) > 100)])
  
  if(length(txt_selected) < 1) {
    return("Length 0 text")
  }
  
  # For several matches, pick one with the most characters
  else if (length(txt_selected) > 1) {
    
    txt_nchar <- nchar(txt_selected)
    
    return(txt_selected[which(txt_nchar == max(txt_nchar))])
    
  }
  
  # Only one returned
  txt_selected
  
}