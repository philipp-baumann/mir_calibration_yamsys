################################################################################
## Function to arrange plots in panels
## Source code from
## https://gist.github.com/baptiste/6652815
## idea found in
## http://stackoverflow.com/questions/18906439/add-annotation-box-to-grid-of-ggplot-objects
################################################################################
gtable_arrange <- function(..., grobs=list(), as.table=TRUE,
    top = NULL, bottom = NULL, 
    left = NULL, right = NULL, draw=TRUE){
  require(gtable)
  # alias
  gtable_add_grobs <- gtable_add_grob
  
  dots <- list(...)
  params <- c("nrow", "ncol", "widths", "heights",
    "respect", "just", "z") # TODO currently ignored
  
  layout.call <- intersect(names(dots), params)
  params.layout <- dots[layout.call]
  
  if(is.null(names(dots)))
    not.grobnames <- FALSE else
      not.grobnames <- names(dots) %in% layout.call
  
  if(!length(grobs))
    grobs <- dots[! not.grobnames ]
  
  ## figure out the layout
  n <- length(grobs)
  nm <- n2mfrow(n)
  
  if(is.null(params.layout$nrow) & is.null(params.layout$ncol)) 
  {
    params.layout$nrow = nm[1]
    params.layout$ncol = nm[2]
  }
  if(is.null(params.layout$nrow))
    params.layout$nrow = ceiling(n/params.layout$ncol)
  if(is.null(params.layout$ncol))
    params.layout$ncol = ceiling(n/params.layout$nrow)
  
  if(is.null(params.layout$widths))
    params.layout$widths <- unit(rep(1, params.layout$ncol), "null")
  if(is.null(params.layout$heights))
    params.layout$heights <- unit(rep(1,params.layout$nrow), "null")
  
  positions <- expand.grid(row = seq_len(params.layout$nrow), 
                           col = seq_len(params.layout$ncol))
  if(as.table) # fill table by rows
    positions <- positions[order(positions$row),]
  
  positions <- positions[seq_along(grobs), ] # n might be < ncol*nrow
  
  ## build the gtable, similar steps to gtable_matrix
  
  gt <- gtable(name="table")
  gt <- gtable_add_cols(gt, params.layout$widths)
  gt <- gtable_add_rows(gt, params.layout$heights)
  gt <- gtable_add_grobs(gt, grobs, t = positions$row, 
                         l = positions$col)
  
  ## titles given as strings are converted to text grobs
  if (is.character(top)) 
    top <- textGrob(top)
  if (is.character(bottom)) 
    bottom <- textGrob(bottom) # Check this line for border
  if (is.character(right)) 
    right <- textGrob(right, rot = -90)
  if (is.character(left)) 
    left <- textGrob(left, rot = 90)
  
  if(!is.null(top)){
    gt <- gtable_add_rows(gt, heights=grobHeight(top), 0)
    gt <- gtable_add_grobs(gt, top, t=1, l=1, r=ncol(gt))
  }
  if(!is.null(bottom)){
    gt <- gtable_add_rows(gt, heights=grobHeight(bottom), -1)
    gt <- gtable_add_grobs(gt, bottom, t=nrow(gt), l=1, r=ncol(gt))
  }
  if(!is.null(left)){
    gt <- gtable_add_cols(gt, widths=grobWidth(left), 0)
    gt <- gtable_add_grobs(gt, left, t=1, b=nrow(gt), l=1, r=1)
  }
  if(!is.null(right)){
    gt <- gtable_add_cols(gt, widths=grobWidth(right), -1)
    gt <- gtable_add_grobs(gt, right, t=1, b=nrow(gt), l=ncol(gt), r=ncol(gt))
  }
  
  if(draw){
    grid.newpage()
    grid.draw(gt)
  }
  invisible(gt)
}