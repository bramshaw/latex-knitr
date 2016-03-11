## Example of a simple table from the Hake assessment.
## source this file to run the example

rm(list = ls(all=TRUE))
require(xtable)

fmt0 <- function(x, dec.points = 0){
  ## Format x to have supplied number of decimal points
  ## Make thousands seperated by commas and the number of decimal points given by
  ##  dec.points
  return(format(round(x,dec.points), big.mark = ",", nsmall = dec.points))
}

get.align <- function(num,
                      first.left = TRUE, ## Keep the first column left-justified
                                         ## If FALSE, it will be justified according to the 'just' argument
                      just = "r"         ## just is the justification to use for the columns, "r", "l", or "c"
                      ){
  ## Returns a character vector used in the align argument of the xtable command.
  ## e.g. posterior output tables, reference point tables. Most tables really.
  ## num is the number of columns in the table
  if(first.left){
    align <- c("l", "l")
  }else{
    align <- c(just, just)
  }
  for(i in 1:(num-1)){
    align <- c(align, just)
  }
  return(align)
}

make.cdfquant.table <- function(species,
                                digits = 2,           ## Number of decimal places
                                xcaption = "default", ## Caption to use
                                xlabel   = "default", ## Latex label to use
                                font.size = 9,        ## Size of the font for the table
                                space.size = 10,      ## Size of the spaces for the table
                                placement = "H"       ## Placement of table
                                ){
  ## Returns an xtable in the proper format for the executive summary catches

  ## If start.yr > 1991 then US foreign, US JV, and Canadian foreign will be removed since they are all zeroes.
  species <- t(species)
  row.names <- rownnames(species)
  row.names <- paste0("\\textbf{", gsub("\\.", " ", row.names), "}")
  species <- cbind(row.names, fmt0(species, digits))

  colnames(species) <- c("",
                         "\\textbf{2.5\\%}",
                         "\\textbf{25\\%}",
                         "\\textbf{50\\%}",
                         "\\textbf{75\\%}",
                         "\\textbf{97.5\\%}")

  ## Add the extra header spanning multiple columns (for Quantile header)
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- paste0("& \\multicolumn{", ncol(species) - 1, "}{c}{Quantile}")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(species,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(species))),
               caption.placement = "top",
               include.rownames = FALSE,
               add.to.row = addtorow,
               table.placement = placement,
               tabular.environment = "tabularx",
               width = "\\textwidth",
               sanitize.text.function = function(x){x},
               size = size.string))

}

species <- read.csv("C:/GitHub/SPERA-Maps/Results/Figures/Quantiles_Temperature.csv")
species <- na.omit(species)
make.cdfquant.table(species,
                    xcaption="What you want the table caption to be in the doc..",
                    xlabel="tab:cdfquant") ## This is the reference you will use in latex to point to this table


## Assumes you have the specialcell command in latex:
## \newcommand{\specialcell}[2][c]{\begin{tabular}[#1]{@{}c@{}}#2\end{tabular}}

## You would put a knitr code chunk in the RNW file that looks like this:
## <<total.catches.table, results='asis', echo=FALSE>>=
##   make.catches.table(catches,
##                      start.yr = 2006,
##                      end.yr = 2015,
##                      weight.factor = 1000,
##                      xcaption <- "Recent commercial fishery catch (t). Tribal catches are included where applicable.",
##                      xlabel <- "tab:es-catches",
##                      font.size = 9,
##                      space.size = 10,
##                      placement = "tbp")
## @
