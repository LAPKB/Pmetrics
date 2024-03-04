#' @title Plot PM_model objects
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' Plots a [PM_model] based on differential equations using network plots from tidygraph and ggraph packages.
#'
#' @details
#' This accepts a [PM_model] object and creates a network plot where nodes are compartments
#' and edges are arrows connecting compartments.
#' @method plot PM_model
#' @param x The name of an [PM_model] object.
#' @param marker Controls the characteristics of the compartments (nodes). 
#' It can be boolean or a list.
#' `TRUE` will plot the compartments with default characteristics.
#' `FALSE` will suppress compartment plotting.
#' If a list, can control some marker characteristics, including overriding defaults.
#' These include:
#' \itemize{
#' \item{`color`} Marker color (default: dodgerblue).
#' \item{`opacity`} Ranging between 0 (fully transparent) to 1 (fully opaque). Default is 0.5.
#' \item{`size`} Relative size of boxes, ranging from 0 to 1.  Default is 0.25.
#' \item{`line`} A list of  additional attributes governing the outline for filled shapes, most commonly
#' color (default: black) and width (default: 0.5).
#' }
#' <br>
#' <br>
#' Example: `marker = list(color = "red", opacity = 0.8, line = list(color = "black", width = 1))`
#' @param line Controls characteristics of arrows (edges).
#' `TRUE` will plot default lines. `FALSE` will suppress lines.
#' If a list, can control some line characteristics, including overriding defaults.
#' These include: 
#' \itemize{
#' \item{`color`} Line color (default: black)
#' \item{`width`} Thickness in points (default: 1).
#' } 
#' <br> 
#' <br>
#' Example: `line = list(color = "red", width = 2)`
#' @param explicit A data frame or tibble containing two columns named `from` and `to` 
#' to add additional connecting arrows to the plot indicating transfer between 
#' compartments. For each row, the `from` column contains the compartment number of the arrow origin, and the 
#' `to` column contains the compartment number of the arrow destination. Use 0 to indicate
#' a destination to the external sink. e.g., `explicit = data.frame(from = 3, to = 0)`
#' @param implicit Similar to `explicit`, used to add dashed connecting arrows 
#' to the plot indicating implicit transfer between 
#' compartments. For each row, the `from` column contains the compartment number of the arrow origin, and the 
#' `to` column contains the compartment number of the arrow destination. Use 0 to indicate
#' a destination to the external sink. e.g., `implicit = data.frame(from = 2, to = 4)`
#' @param ... Not used.
#' @return Plots the object.
#' @author Markus Hovd, Julian Otalvaro, Michael Neely
#' @seealso [PM_model], [ggraph::ggraph()], [ggplot2::ggplot()]
#' @export
#' @examples
#' NPex$model$plot()
#' @family PMplots

plot.PM_model <- function(x, marker = TRUE, line = TRUE, explicit, implicit,...) {
  
  model <- x
  marker <- if(is.list(marker) || marker){
    amendMarker(marker, default = list(color = "dodgerblue", size = 0.25, line = list(width = 0.5)))
  } else {FALSE}
  line <- if(is.list(line) || line){
    amendLine(line, default = list(color = "black"))
  } else {FALSE}
  
  #Add equations for algebraic models
  if(is.null(model$model_list$eqn)){
    key_vars <- c("ke", "v", "ka", "kcp", "kpc")
    pri <- names(model$model_list$pri)
    found_pri_keys <- key_vars %in% tolower(pri)
    
    if(!is.null(model$model_list$sec)){
      found_sec_keys <- purrr::map_lgl(key_vars, \(x) stringr::str_detect(model$model_list$sec,
                                                                          stringr::regex(x, ignore_case = TRUE)))
    } else {found_sec_keys <- rep(NA, 5)}
    found_keys <- key_vars[found_pri_keys | found_sec_keys] %>% na.exclude()
    model$model_list$eqn <- dplyr::case_when(
      all(found_keys %in% c("ke","v")) ~ c("dX[1] = RATEIV[1] - Ke*X[1]", 
                                           NA, 
                                           NA),
      
      all(found_keys %in% c("ke","v", "ka")) ~ c("dX[1] = BOLUS[1] - Ka*X[1]", 
                                                 "dX[2] = RATEIV[1] + Ka*X[1] - Ke*X[2]",
                                                 NA),
      
      all(found_keys %in% c("ke","v", "kcp", "kpc")) ~ c("dX[1] = RATEIV[1] - (Ke+KCP)*X[1] + KPC*X[2]", 
                                                         "dX[2] = KCP*X[1] - KPC*X[2]",
                                                         NA),
      
      all(found_keys %in% c("ke","v", "ka","kcp", "kpc")) ~ c("dX[1] = BOLUS[1] - Ka*X[1]", 
                                                              "dX[2] = RATEIV[1] + Ka*X[1] - (Ke+KCP)*X[2] + KPC*X[3]",
                                                              "dX[3] = KCP*X[2] - KPC*X[3]"),
      .size = 3
      
    ) %>% na.exclude()
  }
  
  #filter any equations that are not diffeq and make everything capital
  this_model <- model$model_list$eqn %>% 
    map(purrr::keep, stringr::str_detect, stringr::regex("dX\\[\\d+\\]|XP\\(\\d+\\)", ignore_case = TRUE)) %>% 
    unlist() 
  
  tree <- parse(text = this_model)
  if(length(tree)==0){stop("No differential equations detected. Use dX[i] for changes and X[i] for amounts (case insensitive).")}
  index <- 0
  
  parse_arrows <- function(tree, arrows = list()) {
    #browser()
    if (length(tree) == 3) {
      op <- tree[[1]]
      lhs <- tree[[2]]
      rhs <- tree[[3]]
    } else if (length(tree[[1]]) == 3) {
      op <- tree[[1]][[1]]
      lhs <- tree[[1]][[2]]
      rhs <- tree[[1]][[3]] 
    } else {
      return(arrows)
    }
    
    #check for distributions
    if(length(lhs)>1 && lhs[[1]] == "("){
      #expand distribution
      nterms <- length(lhs[[2]])
      lhs <- parse(text = paste(sapply(2:nterms,function(x) as.character(lhs[[2]][[x]])),
                                as.character(op),
                                deparse(rhs),
                                collapse = paste0(" ",as.character(lhs[[2]][[1]]), " ")))[[1]]
      rhs <- ""
    }
    
    if(length(rhs)>1 && rhs[[1]] == "("){
      #expand distribution
      nterms <- length(rhs[[2]])
      rhs <- parse(text = paste(deparse(lhs),
                                as.character(op),
                                sapply(2:nterms,function(x) as.character(rhs[[2]][[x]])),
                                collapse = paste0(" ",as.character(rhs[[2]][[1]]), " ")))[[1]]
      lhs <- ""
    }
    
    
    l = if (length(lhs) == 1) {
      lhs
    } else if(lhs[[1]] == "["){
      lhs[[2]]
    } else if (is.call(lhs) & length(lhs)==3){
      lhs[[3]]
    } else {
      lhs[[1]]
    }
    r = if (length(rhs) == 1) {
      rhs
    } else if(rhs[[1]] == "["){
      rhs[[2]]
    } else if (is.call(rhs) & length(rhs)==3){
      rhs[[3]]
    } else {
      rhs[[1]]
    }
    #cat("index", index,"\n\nlhs= ",deparse(lhs),"\nrhs = ",deparse(rhs),"\nl = ",deparse(l),"\nr = ",deparse(r),"\ntree = ",deparse(tree),"\n________________\n")
    
    
    if (l == "x" || r == "x" || l == "X" || r == "X") {
      #cat("arrows before: ",paste0(as.character(arrows),collapse = ", "),"\n")
      arrows = append(arrows, tree)
      #cat(deparse(tree), "appended\n")
      #cat("arrows after: ",paste0(as.character(arrows),collapse = ", "),"\n")
      
      return(arrows)
    }
    
    index <<- index + 1
    if(is.call(lhs)) {#cat("Calling from lhs...\n")
      arrows <- parse_arrows(lhs, arrows)}
    #cat("\nReturned lhs arrows: ", paste(as.character(arrows),collapse = ", "), "\n\n")
    
    if(is.call(rhs)) {#cat("Calling from rhs...\n")
      arrows <- parse_arrows(rhs, arrows)}
    #cat("\nReturned rhs arrows: ", paste(as.character(arrows),collapse = ", "), "\n\n")
    
    return(arrows)
  }
  
  parse_inputs <- function(input, itree){
    itree <- paste(itree, collapse = "")
    if(grepl(input, itree, ignore.case = TRUE)){
      type <- toupper(substr(input, 1, 1))
      number <- stringr::str_extract(itree,
                                     regex(paste0(input,"(\\(|\\[)\\d+(\\)|\\])"),
                                           ignore_case = TRUE)) %>%
        stringr::str_extract("\\d+")
      return(paste0(type,number))
    } else {return("")}
  }
  
  #process each compartment/equation
  parse_tree <- function(tree) {
    nodes = list()
    if (inherits(tree, "expression")) {
      for (itree in tree) {
        op = itree[[1]]
        lhs = itree[[2]]
        rhs = itree[[3]]
        if (op == "=") {
          if(lhs[[1]] == "[") {lhs <- lhs[-1]}
          nodes = append(nodes, list(node = list(
            node = as.character(lhs),
            arrows = as.character(parse_arrows(rhs)),
            bolus = parse_inputs("bolus", deparse(itree)),
            rateiv = parse_inputs("rateiv", deparse(itree))
          )))
        } else { #only one equation
          as.character(parse_arrows(tree))
        }
      }
    }
    return(nodes)
  }
  
  res <- parse_tree(tree)
  
  #clean up
  swap_if_needed <- function(obj){
    if(grepl("X\\[", obj[1], ignore.case = TRUE)){
      return(paste(obj[2], obj[1], sep = " * "))
    } else {
      return(paste(obj[1], obj[2], sep = " * "))
    }
  }
  #clean up
  
  #remove hanging arrows without "*"
  res <- purrr::map(res, function(x) list(node = x$node,
                                          arrows = x$arrows[grepl("\\*", x$arrows)],
                                          bolus = x$bolus,
                                          rateiv = x$rateiv)) %>%
    #ensure unique arrows in each node
    purrr::map(function(x) list(node = x$node,
                                arrows = unique(x$arrows),
                                bolus = x$bolus,
                                rateiv = x$rateiv)) %>%
    #ensure X terms come second
    purrr::map(function(x) list(node = x$node,
                                arrows = unlist(purrr::map(x$arrows, ~swap_if_needed(stringr::str_split_1(.x," \\* ")))),
                                bolus = x$bolus,
                                rateiv = x$rateiv))
  
  layout = res %>%
    lapply(., function(node) {
      data.frame(node = paste(node$node, collapse = ""),
                 arrow = node$arrow,
                 bolus = node$bolus,
                 rateiv = node$rateiv)
    }) %>%
    bind_rows() %>%
    dplyr::mutate(from = stringr::str_replace(node, stringr::regex("XP|dX", ignore_case = TRUE), "")) %>%
    dplyr::mutate(to = stringr::str_extract(string = arrow, pattern = "\\((\\d+)\\)|\\[(\\d+)\\]")) %>%
    dplyr::mutate(to = stringr::str_remove(to, pattern = "\\(|\\[")) %>%
    dplyr::mutate(to = stringr::str_remove(to, pattern = "\\)|\\]")) %>%
    dplyr::mutate(to = ifelse(from == to, "", to)) %>%
    dplyr::mutate(arrow = stringr::str_remove(string = arrow, pattern = "\\X\\((\\d+)\\)|\\X\\[(\\d+)\\]")) %>%
    dplyr::mutate(arrow = stringr::str_remove_all(string = arrow, pattern = " ")) %>%
    dplyr::mutate(arrow = stringr::str_remove_all(string = arrow, pattern = "^\\*|\\*\\w*$")) %>%
    dplyr::mutate(arrow = stringr::str_remove_all(string = arrow, pattern = "^\\-|\\-\\w*$")) %>%
    dplyr::relocate(node, arrow, to, from) %>%
    dplyr::rename(to = from,
                  from = to)
  
  #pause to define inputs
  input_cmt <- layout %>% dplyr::select(to, bolus, rateiv) %>%
    dplyr::filter(bolus != "" | rateiv != "") %>% distinct() %>%
    tidyr::pivot_longer(c(bolus, rateiv), names_to = "type", values_to = "input") %>%
    dplyr::select(-type) %>%
    dplyr::filter(input != "") %>% dplyr::rename(cmt = to)
  
  #resume layout
  layout <- layout %>% dplyr::select(-bolus, -rateiv) %>%
    dplyr::group_by(arrow) %>%
    dplyr::filter(n() == 1 | n() > 1 & from != "") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(from = ifelse(from == "", to, from)) %>%
    dplyr::mutate(to = ifelse(from == to, "", to)) %>%
    dplyr::mutate(to = ifelse(to == "", as.numeric(max(c(to, from), na.rm = T)) + 1, to)) %>%
    dplyr::mutate(to = ifelse(is.na(to), as.numeric(max(c(to, from), na.rm = T)) + 1, to)) %>%
    dplyr::filter(arrow != "") %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    dplyr::mutate(node = stringr::str_extract(node, "\\d+")) %>%
    dplyr::distinct(node, from, to) %>%
    dplyr::mutate(implicit = FALSE)
  
  #outputs
  if(!is.null(purrr::pluck(model,"model_list","out",1,"val"))){
    cmts <- map_chr(model$model_list$out, ~stringr::str_extract(.x$val, "\\d+"))
    output_cmt <- dplyr::tibble(out = paste0("Y",seq_along(cmts)), cmt = cmts)
  } else {
    output_cmt = dplyr::tibble(out = "", cmt = "1")
  }
  
  #add explicit arrows from user
  if(!missing(explicit)){
    
    max_to <- max(as.numeric(layout$to))
    
    if(!all(names(explicit) %in% c("from", "to"))){ stop("explicit should be a data frame with names from and to")}
    imp <- explicit %>% 
      dplyr::mutate(to = ifelse(to == 0, max_to, to)) %>%
      dplyr::mutate(node = from, implicit = FALSE) %>%
      dplyr::relocate(node, from, to, implicit) %>%
      dplyr::mutate(across(c(node, from, to), as.character)) 
    
    layout <- dplyr::bind_rows(layout, imp)
  }
  
  #add implicit arrows from user
  if(!missing(implicit)){
    
    max_to <- max(as.numeric(layout$to))
    
    if(!all(names(implicit) %in% c("from", "to"))){ stop("implicit should be a data frame with names from and to")}
    imp <- implicit %>% 
      dplyr::mutate(to = ifelse(to == 0, max_to, to)) %>%
      dplyr::mutate(node = from, implicit = TRUE) %>%
      dplyr::relocate(node, from, to,implicit) %>%
      dplyr::mutate(across(c(node, from, to), as.character)) 
    
    layout <- dplyr::bind_rows(layout, imp)
  }
  
  
  graph <- tidygraph::as_tbl_graph(layout) %>%
    dplyr::mutate(cmt = c(unique(layout$from),0)) %>%
    dplyr::mutate(position = ifelse(cmt == 0, "outside", "inside")) %>%
    dplyr::left_join(input_cmt, by = "cmt") %>%
    dplyr::mutate(input = ifelse(is.na(input),"",input)) %>%
    dplyr::left_join(output_cmt, by = "cmt") %>%
    dplyr::mutate(out = ifelse(is.na(out),"",out)) 
  
  
  
  
  g <- ggraph::ggraph(graph, layout = "tree")
  if(!is.logical(marker)){ #will only be logical if FALSE
    g <- g +
      ggraph::geom_node_tile(aes(fill = position, linetype = position), 
                             color = marker$line$color,
                             lwd = marker$line$width,
                             width = marker$size, height = marker$size, alpha = marker$opacity) +
      ggraph::geom_node_text(aes(label = input), nudge_x = .07, nudge_y = .05, color = "white") +
      ggraph::geom_node_text(aes(label = out), nudge_x = -.07, nudge_y = .05, color = "black") +
      ggplot2::scale_fill_manual(values = c(marker$color, "grey80"))
  }
  if(!is.logical(line)){ #will only be logical if FALSE
    g <- g +
      ggraph::geom_edge_fan(
        aes(linetype = as.numeric(implicit)+1),
        arrow = grid::arrow(angle = 15, type = "closed",
                            length = grid::unit(6, 'mm')),
        end_cap = ggraph::circle(3, 'mm'),
        start_cap = ggraph::circle(4, 'mm'),
        angle_calc = "across",
        edge_color = line$color,
        label_push = grid::unit(-4, 'mm'),
        edge_width = line$width) 
  }
  g <- g +
    ggraph::geom_node_label(aes(label = cmt), position = "identity") +
    ggraph::theme_graph() +
    ggplot2::theme(legend.position = "none")
  print(g)
  return(invisible(graph))
}

