#' Plot PM_model objects
#' 
#' Plots a [PM_model] based on differential equations using network plots from tidygraph and ggraph packages.
#' 
#' @details
#' This accepts a [PM_model] object with minimum of `$eqn` field and optionally an `$out` field within the 
#' `$model_list` field. 
#' 
#' @method plot PM_model
#' @param model The name of an [PM_model] object. 
#' 
#' @return Plots the object.
#' @author Markus Hovd, Julian Otalvaro, Michael Neely
#' @seealso [PM_model], [ggraph::ggraph()], [ggplot2::ggplot()]
#' @export
#' @examples
#' #to be implemented: NPex$model$plot()
#' @family PMplots

plot.PM_model = function(model) {
  
  #TO DO: add customizations
  
  #filter any equations that are not diffeq
  model$model_list$eqn <- map(purrr::keep, stringr::str_detect, "dX\\[\\d+\\]|XP\\(\\d+\\)") %>% unlist()
  tree <- parse(text = model$model_list$eqn)
  if(length(tree)==0){stop("No differential equations detected. Use dX[i] for changes and X[i] for amounts.")}
  
  parse_arrows <- function(tree, arrows = list()) {
    if (length(tree) == 3) {
      op = tree[[1]]
      lhs = tree[[2]]
      rhs = tree[[3]]
      
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
      } else {
        lhs[[1]]
      }
      r = if (length(rhs) == 1) {
        rhs
      } else if(rhs[[1]] == "["){
        rhs[[2]]
      } else {
        rhs[[1]]
      }
      
      
      if (l == "X" || r == "X") {
        arrows = append(arrows, tree)
        return(arrows)
      }
    }
    
    if(is.call(lhs)) arrows = parse_arrows(lhs, arrows)
    if(is.call(rhs)) arrows = parse_arrows(rhs, arrows)
    return(arrows)
  }
  
  parse_inputs <- function(input, itree){
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
    if (length(tree) > 1 && class(tree) == "expression") {
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
        } else {
          stop("")
        }
      }
    }
    return(nodes)
  }
  
  res <- parse_tree(tree)
  
  #clean up
  swap_if_needed <- function(obj){
    if(grepl("X\\[|x\\[", obj[1])){
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
    dplyr::mutate(from = stringr::str_replace(node, "XP|dX", "")) %>%
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
    dplyr::mutate(to = ifelse(to == "", as.numeric(max(to)) + 1,to)) %>%
    dplyr::filter(arrow != "")
  
  #outputs
  if(!is.null(purrr::pluck(model,"model_list","out",1,"val"))){
    output_cmt <- tibble::as_tibble(map(model$model_list$out, ~stringr::str_extract(.x$val, "\\d+")))
    output_cmt <- dplyr::bind_cols(out = names(output_cmt), as_tibble(t(output_cmt)))
    output_cmt$out <- toupper(output_cmt$out)
    output_cmt <- output_cmt %>%  dplyr::rename(cmt = 2)
  } else {
    output_cmt = tibble::tibble(out = "", cmt = "1")
  }
  
  
  graph <- tidygraph::as_tbl_graph(layout) %>%
    dplyr::mutate(cmt = c(unique(layout$from),0)) %>%
    dplyr::mutate(position = ifelse(cmt == 0, "outside", "inside")) %>%
    dplyr::left_join(input_cmt, by = "cmt") %>%
    dplyr::mutate(input = ifelse(is.na(input),"",input)) %>%
    dplyr::left_join(output_cmt, by = "cmt") %>%
    dplyr::mutate(out = ifelse(is.na(out),"",out))
  
  
  
  
  g <- ggraph::ggraph(graph, layout = "auto") +
    ggraph::geom_node_tile(aes(fill = position, linetype = position),
                           width = .25, height = .25, alpha = 0.5, size = 12) +
    ggraph::geom_node_text(aes(label = input), nudge_x = .07, nudge_y = .05, size = 6, color = "white") +
    ggraph::geom_node_text(aes(label = out), nudge_x = -.07, nudge_y = .05, size = 6, color = "black") +
    
    ggplot2::scale_fill_manual(values = c("dodgerblue", "grey80")) +
    ggraph::geom_edge_fan(
      arrow = grid::arrow(angle = 15, type = "closed",
                          length = unit(6, 'mm')),
      end_cap = circle(3, 'mm'),
      start_cap = circle(4, 'mm'),
      angle_calc = "across",
      label_push = unit(-4, 'mm'),
      edge_width = 1) +
    ggraph::geom_node_label(aes(label = cmt), position = "identity") +
    ggraph::theme_graph() +
    ggplot2::theme(legend.position = "none")
  print(g)
  return(graph)
}








