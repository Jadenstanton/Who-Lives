

## Objective

# This creates wrapper functions around the CSS formatting tags in The Data Center website. 
# This assumes that knitr has been loaded since it is meant to be compiled with an Rmd document. 


## Function to put text in a blue box, class="highlight"

# Allows for md-style line breaks and headers.
# Note: headers will not display properly unless free of leading whitespace. 
# As a result, auto-indenting will likely have to be corrected.
makeCSS_highlight <- function(copyInMD) {
  knitr::raw_html(paste0("<div class=\"highlight\" markdown=\"1\"> \n",
                         "\n", 
                         copyInMD,
                         "\n",
                         "</div>",
                         "\n"))
}



## Function to put text in a source caption, class="source"

# Version 1: More like existing style: Paragraphs nested into a div.

# makeCSS_source <- function(copyInMD) {
#   knitr::raw_html(paste0("<div class=\"source\" style=\"clear: left;\" markdown=\"1\"> \n",
#                          "<p style=\"padding-left: 30px;\" markdown=\"1\">",
#                          "\n", 
#                          copyInMD,
#                          "</p>", 
#                          "\n",
#                          "</div>",
#                          "\n"))
# }

# Version 2: Controlling style in the div, leaving paragraphs controlled by markdown. 

makeCSS_source <- function(copyInMD) {
  knitr::raw_html(paste0("<div class=\"source\" style=\"clear: left; padding-left: 30px;\" markdown=\"1\"> \n",
                         "\n", 
                         copyInMD,
                         "</p>", 
                         "\n",
                         "</div>",
                         "\n"))
}