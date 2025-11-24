# R-to-Rust Transpiler for ODE Function
# Transforms an R ODE function into Rust closures with zero-based indexing,
# parameter/covariate fetching, and arithmetic operations.

# Convert an R expression to Rust code (recursive)
expr_to_rust <- function(expr, params = NULL, covs = NULL,
  declared = new.env(parent = emptyenv())) {
    declared_has <- function(name) isTRUE(get0(name, envir = declared, inherits = FALSE))
    declared_add <- function(name) assign(name, TRUE, envir = declared)
    
    # Base cases: numeric literals as floats, symbols
    if (is.numeric(expr) && length(expr) == 1) {
      val <- expr
      if (val == floor(val)) {
        return(sprintf("%d.0", as.integer(val)))
      } else {
        return(as.character(val))
      }
    }
    if (is.symbol(expr)) {
      return(as.character(expr))
    }
    
    # Handle indexing: var[idx] with zero-based adjustment
    if (is.call(expr) && as.character(expr[[1]]) == "[") {
      var <- as.character(expr[[2]])
      idx_raw <- expr[[3]]
      if (is.numeric(idx_raw) && length(idx_raw) == 1) {
        idx_val <- as.integer(idx_raw) - 1
        return(sprintf("%s[%d]", var, idx_val))
      } else {
        idx_code <- expr_to_rust(idx_raw, params, covs, declared)
        return(sprintf("%s[%s - 1]", var, idx_code))
      }
    }
    
    if (!is.call(expr)) stop("Unknown expression type")
    op <- as.character(expr[[1]])
    args <- as.list(expr[-1])
    rust_args <- lapply(args, expr_to_rust,
      params = params, covs = covs,
      declared = declared
    )
    switch(op,
      # Grouping
      "(" = sprintf("(%s)", rust_args[[1]]),
      "{" = {
        inner <- if (length(args) == 0) {
          character(0)
        } else {
          # turn each inner expr into a statement, joined by newlines
          inner_exprs <- as.list(args)
          paste(
            vapply(inner_exprs, function(e) {
              expr_to_rust(e, params, covs, declared)
            }, character(1)),
            collapse = "\n"
          )
        }
        inner
      },
      
      # Arithmetic
      "+" = if (length(rust_args) == 1) {
        sprintf("+(%s)", rust_args[[1]])
      } else {
        sprintf("%s + %s", rust_args[[1]], rust_args[[2]])
      },
      "-" = if (length(rust_args) == 1) {
        sprintf("-(%s)", rust_args[[1]])
      } else {
        sprintf("(%s) - (%s)", rust_args[[1]], rust_args[[2]])
      },
      "*" = sprintf("%s * %s", rust_args[[1]], rust_args[[2]]),
      "/" = sprintf("%s / %s", rust_args[[1]], rust_args[[2]]),
      "^" = if (suppressWarnings(!is.na(as.numeric(rust_args[[1]])))) {
        sprintf("(%sf64).powf(%s)", rust_args[[1]], rust_args[[2]])
      } else {
        sprintf("(%s).powf(%s)", rust_args[[1]], rust_args[[2]])
      },
      
      # Comparison
      "==" = sprintf("%s == %s", rust_args[[1]], rust_args[[2]]),
      "!=" = sprintf("%s != %s", rust_args[[1]], rust_args[[2]]),
      ">=" = sprintf("%s >= %s", rust_args[[1]], rust_args[[2]]),
      "<=" = sprintf("%s <= %s", rust_args[[1]], rust_args[[2]]),
      ">" = sprintf("%s > %s", rust_args[[1]], rust_args[[2]]),
      "<" = sprintf("%s < %s", rust_args[[1]], rust_args[[2]]),
      
      # Logical
      "&" = sprintf("%s && %s", rust_args[[1]], rust_args[[2]]),
      "|" = sprintf("%s || %s", rust_args[[1]], rust_args[[2]]),
      "!" = sprintf("!(%s)", rust_args[[1]]),
      
      # Math funcs
      "abs" = sprintf("(%s).abs()", rust_args[[1]]),
      "log" = sprintf("(%s).ln()", rust_args[[1]]),
      "log10" = sprintf("(%s).log10()", rust_args[[1]]),
      "exp" = sprintf("(%s).exp()", rust_args[[1]]),
      "sqrt" = sprintf("(%s).sqrt()", rust_args[[1]]),
      
      # Assignment
      "<-" = ,
      "=" = {
        lhs <- args[[1]]
        rhs_code <- rust_args[[2]]
        
        if (is.symbol(lhs)) {
          name <- as.character(lhs)
          if (declared_has(name)) {
            sprintf("%s = %s;", name, rhs_code)
          } else {
            declared_add(name)
            sprintf("let mut %s = %s;", name, rhs_code)
          }
        } else {
          # e.g., dx[1] = ...
          lhs_code <- expr_to_rust(lhs, params, covs, declared)
          sprintf("%s = %s;", lhs_code, rhs_code)
        }
      },
      
    
      # If
      "if" = {
        cond <- rust_args[[1]]
        then_code <- expr_to_rust(args[[2]], params, covs, declared)
        if (length(args) == 3) {
          else_code <- expr_to_rust(args[[3]], params, covs, declared)
          sprintf("if %s { %s } else { %s };", cond, then_code, else_code)
        } else {
          sprintf("if %s { %s };", cond, then_code)
        }
      },
      
      # For (left as-is; thread declared through)
      "for" = {
        var <- as.character(args[[1]])
        n_sym <- as.character(args[[2]][[3]])
        loop_exprs <- if (is.call(args[[3]]) && as.character(args[[3]][[1]]) == "{") {
          as.list(args[[3]][-1])
        } else {
          list(args[[3]])
        }
        body <- stmts_to_rust(loop_exprs, params, covs) # make sure stmts_to_rust gets declared too
        sprintf("for %s in 0..%s as usize {\n%s}\n", var, n_sym, indent(body))
      },
      
      # Pmetrics functions
      "get_e2" = {
        sprintf(
          "get_e2(%s, %s, %s, %s, %s, %s);",
          rust_args[[1]], rust_args[[2]], rust_args[[3]],
          rust_args[[4]], rust_args[[5]], rust_args[[6]]
        )
      },
      stop(sprintf("Unsupported operation: %s", op))
    )
  }
  
  
  # Helpers: convert list of statements to Rust and indent blocks
  stmts_to_rust <- function(exprs, params = NULL, covs = NULL) {
    lines <- vapply(exprs, expr_to_rust, character(1), params = params, covs = covs)
    tolower(paste(lines, collapse = "\n")) # rust is case sensitive, make everything lowercase
  }
  
  indent <- function(text, spaces = 4) {
    prefix <- strrep(" ", spaces)
    paste0(prefix, gsub("\n", paste0("\n", prefix), text))
  }
  
  # Transpile an R ODE function to Rust closure
  transpile_ode_eqn <- function(fun, params, covs, sec) {
    exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
    header <- sprintf(
      "|x, p, t, dx, b, rateiv, cov| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s); %s",
      paste(covs, collapse = ", "),
      paste(params, collapse = ", "),
      paste(sec, collapse = ", ")
    )
    body_rust <- stmts_to_rust(exprs, params, covs) %>%
    stringr::str_replace_all("bolus\\[", "b\\[") %>%
    stringr::str_replace_all("r\\[", "rateiv\\[")
    sprintf("%s\n%s\n }", header, indent(body_rust, spaces = 4))
  }
  
  
  
  transpile_analytic_eqn <- function(fun, params, covs) {
    if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") {
      found <- get_found_model(fun)
      if (length(found) == 1) { # NA
        cli::cli_abort(c(
          "x" = "No ODE and no library model templates found.",
          "i" = "EQN block must contain ODEs or a single library model template name."
        ))
      }
      tem <- found$name
      eqns <- as.list(body(fun)[-1])
      if (length(eqns) > 0) {
        eqns_char <- map_chr(eqns, deparse)
        # check for ODE, which should not be present
        if (any(stringr::str_detect(eqns_char, regex("dx\\[\\d+\\]", ignore_case = FALSE)))) {
          cli::cli_abort(c(
            "x" = "You appear to have included both a model library template and ODE.",
            "i" = "EQN block must contain ODEs or a single library model template name, not both."
          ))
        }
        exprs <- eqns[-which(eqns_char == found$name)]
      } else {
        exprs <- NULL
      }
    } else {
      list(body(fun))
    }
    
    # map model name from R to rust
    rust_tem <- dplyr::case_when(
      tem == "one_comp_iv" ~ "one_compartment",
      tem == "one_comp_iv_cl" ~ "", # TBD in rust
      tem == "two_comp_iv" ~ "two_compartments",
      tem == "two_comp_iv_cl" ~ "", # TBD in rust
      tem == "two_comp_bolus" ~ "one_compartment_with_absorption",
      tem == "two_comp_bolus_cl" ~ "", # TBD in rust
      tem == "three_comp_iv" ~ "three_compartments", # TBD in R
      tem == "three_comp_iv_cl" ~ "", # TBD in rust
      tem == "three_comp_bolus" ~ "two_compartments_with_absorption",
      tem == "three_comp_bolus_cl" ~ "", # TBD in rust
      tem == "four_comp_bolus" ~ "three_compartments_with_absorption", # TBD in R
      tem == "four_comp_bolus_cl" ~ "" # TBD in rust
    )
    
    header <- sprintf(
      " %s,\n |p, t, cov| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(&p, %s);",
      rust_tem,
      paste(covs, collapse = ", "),
      paste(params, collapse = ", ")
    )
    body_rust <- stmts_to_rust(exprs)
    # remap parameters
    # req_par <- get(tem)$parameters %>%
    #   tolower() %>%
    
    
    # this block needs to write the equations, e.g. p[0] = ke, based on the parameters in the model template,
    # not the parameters in the model. The model parameters may have different names, but are checked earlier.
    
    req_par <- model_lib(show = FALSE) %>%
    filter(Name == tem) %>%
    select(Parameters) %>%
    stringr::str_split(", ", simplify = TRUE) %>%
    unlist() %>%
    tolower() %>%
    purrr::discard(~ .x == "v" & !tem %in% c("one_comp_iv_cl", "two_comp_bolus_cl")) %>% # don't include V for models that don't need it in equations
    purrr::imap_chr(\(x, y){
      sprintf("p[%i] = %s;", y - 1, x)
    }) %>%
    paste(collapse = "\n")
    
    sprintf("%s\n%s\n%s\n }", header, indent(body_rust, spaces = 4), indent(req_par, spaces = 4))
  }
  
  transpile_sec <- function(fun) {
    exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
    body_rust <- stmts_to_rust(exprs)
    sprintf("%s\n", indent(body_rust, spaces = 4))
  }
  
  
  transpile_fa <- function(fun, params, covs, sec) {
    exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") {
      as.list(body(fun)[-1])
    } else {
      list(body(fun))
    }
    
    find_max_idx <- function(expr) {
      if (is.call(expr) && as.character(expr[[1]]) == "[" &&
      as.character(expr[[2]]) == "fa" &&
      is.numeric(expr[[3]])) {
        return(as.integer(expr[[3]]))
      }
      if (is.call(expr)) {
        return(max(sapply(as.list(expr), find_max_idx), 0L))
      }
      0L
    }
    max_fa <- max(sapply(exprs, find_max_idx), 0L)
    # If no fa[] found, we still need at least size 1
    arr_size <- if (max_fa > 0) max_fa else 1L
    
    header <- sprintf(
      "|p, t, cov| {\nfetch_params!(p, %s);\nfetch_cov!(cov, t, %s);\nlet mut fa: [f64; %d] = [0.0; %d];\n%s",
      paste(params, collapse = ", "),
      paste(covs, collapse = ", "),
      arr_size, arr_size,
      if (length(sec)) paste0("  ", paste(sec, collapse = "\n  "), "\n") else ""
    )
    
    body_lines <- stmts_to_rust(exprs, params = params, covs = covs)
    
    slots <- seq_len(arr_size) - 1L
    slot_args <- paste0(slots, "=> fa[", slots, "]", collapse = ", ")
    footer <- sprintf("  fa!{%s}}", slot_args)
    
    paste0(header, indent(body_lines, 2), "\n", footer)
  }
  
  
  transpile_lag <- function(fun, params, covs, sec) {
    exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") {
      as.list(body(fun)[-1])
    } else {
      list(body(fun))
    }
    
    find_max_idx <- function(expr) {
      if (is.call(expr) && as.character(expr[[1]]) == "[" &&
      as.character(expr[[2]]) == "lag" &&
      is.numeric(expr[[3]])) {
        return(as.integer(expr[[3]]))
      }
      if (is.call(expr)) {
        return(max(sapply(as.list(expr), find_max_idx), 0L))
      }
      0L
    }
    max_lag <- max(sapply(exprs, find_max_idx), 0L)
    # If no lag[] found, we still need at least size 1
    arr_size <- if (max_lag > 0) max_lag else 1L
    
    header <- sprintf(
      "|p, t, cov| {\nfetch_params!(p, %s);\nfetch_cov!(cov, t, %s);\nlet mut lag: [f64; %d] = [0.0; %d];\n%s",
      paste(params, collapse = ", "),
      paste(covs, collapse = ", "),
      arr_size, arr_size,
      if (length(sec)) paste0("  ", paste(sec, collapse = "\n  "), "\n") else ""
    )
    
    body_lines <- stmts_to_rust(exprs, params = params, covs = covs)
    
    slots <- seq_len(arr_size) - 1L
    slot_args <- paste0(slots, "=> lag[", slots, "]", collapse = ", ")
    footer <- sprintf("  lag!{%s}}", slot_args)
    
    paste0(header, indent(body_lines, 2), "\n", footer)
  }
  
  transpile_ini <- function(fun, params, covs, sec) {
    exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
    header <- sprintf(
      "|p, t, cov, _x| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s); %s",
      paste(covs, collapse = ", "),
      paste(params, collapse = ", "),
      paste(sec, collapse = ", ")
    )
    body_rust <- stmts_to_rust(exprs, params, covs)
    sprintf("%s\n%s\n }", header, indent(body_rust, spaces = 4))
  }
  
  transpile_out <- function(fun, params, covs, sec) {
    exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
    header <- sprintf(
      "|x, p, t, cov, y| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s);\n%s",
      paste(covs, collapse = ", "),
      paste(params, collapse = ", "),
      paste(sec, collapse = ", ")
    )
    body_rust <- stmts_to_rust(exprs, params, covs)
    sprintf("%s\n%s\n }", header, indent(body_rust, spaces = 4))
  }
  
  empty_sec <- function() {
    "|_p, _t, _cov| { }"
  }
  empty_fa <- function() {
    "|_p,_t,_cov| fa! {}"
  }
  empty_lag <- function() {
    "|_p,_t,_cov| lag! {}"
  }
  empty_ini <- function() {
    "|_p, _t, _cov, _x| { }"
  }
  
  empty_out <- function() {
    "|_x, _p, _t, _cov, _y| { }"
  }
  
  
  get_assignments <- function(fn, assign) {
    count_assignments <- function(expr) {
      if (is.call(expr)) {
        if (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) {
          lhs <- expr[[2]]
          # Check if lhs is an indexing call (i.e., assign[...] <- ...)
          if (is.call(lhs) && identical(lhs[[1]], as.name("["))) {
            target_name <- as.character(lhs[[2]])
            if (tolower(target_name) == tolower(assign)) {
              return(1 + count_assignments(expr[[3]]))
            }
          }
          return(count_assignments(expr[[2]]) + count_assignments(expr[[3]]))
        } else {
          return(sum(sapply(expr, count_assignments)))
        }
      }
      return(0)
    }
    
    body_expr <- body(fn)
    count_assignments(body_expr)
  }
  
  
  reserved_name_conflicts <- function(blocks){
    reserved <- c(
      "t",
      "x",
      "dx",
      "p",
      "b",
      "bolus",
      "r",
      "rateiv",
      "cov",
      "y"
    )

    conflicts <- purrr::map(blocks, \(b) {
      purrr::map_chr(reserved, \(r) {
        if (is.function(b)) { b <- func_to_char(b) }
        if (is.list(b)) { 
          b <- names(b) 
          con_match <- stringr::str_detect(tolower(b), glue::glue("^\\b{r}\\b")) # stand alone
        } else {
          con_match <- stringr::str_detect(tolower(b), glue::glue("^{r}\\s+[=<]")) # assignment in function
        }
        
        if (any(con_match)) {
          return(r)
        } else {
          return(NA_character_)
        }
      })
    })  %>% unlist() %>% purrr::discard(\(d) is.na(d)) %>% unique()
    
    return(conflicts)
  }