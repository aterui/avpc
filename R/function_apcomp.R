
  utils::globalVariables("where")

#' Average predictive comparisons
#'
#' @param m Model object. The function accepts model classes \code{"lm"}, \code{"rlm"}, \code{"glm"}, \code{"lmerMod"}, \code{"glmerMod"}
#' @param u Character string indicating input variable of interest.
#' @param v Character string indicating variables other than the input variable of interest. By default, the function uses all the variables except \code{u}.
#' @param var_transform Function transforming the scale of the response variable. If NULL (default), the function extracts an inverse link function from the model object \code{m}. Accepts either \code{"log"}, \code{"logit"}, or \code{"identity"}.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export


  apcomp <- function(m, u, v = NULL, var_transform = NULL) {

    # inverse logit function --------------------------------------------------

    ilogit <- function(x) {
      1 / (1 + exp(-x))
    }

    # model extraction --------------------------------------------------------

    ## validate model class
    if (!any(class(m) %in% c("lm", "rlm", "glm", "lmerMod", "glmerMod"))) {
      stop("the provided model class is not supported")
    }

    mod <- stats::model.matrix(m) %>% dplyr::as_tibble()
    v_var_name <- colnames(mod)

    # initial check -----------------------------------------------------------

    ## validate u input
    if (length(u) > 1) stop("u must be a single variable")

    if (!any(u %in% v_var_name)) {
      stop(paste("invalid variable input u. available variable names:",
                 paste(v_var_name, collapse = ", ")))
    }

    ## validate v input
    if (any(v %in% u)) stop("v must be different from u")

    if (is.null(v)) {
      v <- v_var_name[!(v_var_name %in% c("(Intercept)", u))]
    } else {
      if (!all(v %in% v_var_name)) {
        stop(paste("invalid variable input v. available variable names:",
                   paste(v_var_name, collapse = ", ")))
      }
    }


    # get pairs for u and v ---------------------------------------------------

    ## frame for v variables
    m_x1 <- m_x2 <- mod %>%
      dplyr::select(dplyr::all_of(v))

    m_x <- m_x1 %>%
      dplyr::mutate(id = as.numeric(rownames(m_x1)))

    ## mahalanobis distance for a set of v variables
    m_cov <- stats::cov(m_x1)
    m_dist <- apply(m_x1, 1, function(row_i) stats::mahalanobis(m_x2, row_i, m_cov))
    df_v <-  dplyr::tibble(sq_distance = c(m_dist),
                           row_id = rep(seq_len(nrow(m_dist)),
                                        times = ncol(m_dist)),
                           col_id = rep(seq_len(ncol(m_dist)),
                                        each = nrow(m_dist))) %>%
      dplyr::mutate(weight = 1 / (1 + .data$sq_distance))

    ## combine with input u
    m_u <- mod %>%
      dplyr::select(dplyr::all_of(u)) %>%
      dplyr::rename(u_input = dplyr::all_of(u)) %>%
      dplyr::mutate(id = as.numeric(rownames(mod)))

    df_uv <- df_v %>%
      dplyr::left_join(m_u, by = c("row_id" = "id")) %>%
      dplyr::rename(u1 = .data$u_input) %>%
      dplyr::left_join(m_u, by = c("col_id" = "id")) %>%
      dplyr::rename(u2 = .data$u_input) %>%
      dplyr::mutate(sign = ifelse(u2 - u1 >= 0, 1, -1)) %>%
      dplyr::left_join(m_x, by = c("row_id" = "id"), suffix = c("_v1", "_v2")) %>%
      dplyr::left_join(m_x, by = c("col_id" = "id"), suffix = c("_v1", "_v2"))


    # coef and matrix ---------------------------------------------------------

    ## input u and other variables v (note: v is v1 irrespective of input u)
    u1 <- df_uv %>% dplyr::pull(.data$u1)
    u2 <- df_uv %>% dplyr::pull(.data$u2)
    df_v1 <- df_uv %>% dplyr::summarize(dplyr::across(dplyr::ends_with("v1")))

    ## input low
    df_uv1 <- dplyr::tibble(u1 = u1, df_v1)
    colnames(df_uv1) <- c(u, v)
    df_uv1 <- dplyr::tibble("(Intercept)" = 1, df_uv1)

    ## input high
    df_uv2 <- dplyr::tibble(u2 = u2, df_v1)
    colnames(df_uv2) <- c(u, v)
    df_uv2 <- dplyr::tibble("(Intercept)" = 1, df_uv2)

    ## get link function from the model object if var_transform "null"
    if (is.null(var_transform)) {
      model_family <- stats::family(m)
      var_transform <- model_family$link
    }

    if (!any(var_transform %in% c("log", "log10", "logit", "identity"))) {
      stop("var_transform must be either log, log10, logit or identity")
    }

    ## get coefficients
    if (any(class(m) %in% c("lm", "rlm", "glm"))) {
      v_b <- m$coefficients
    } else {
      v_b <- stats::coef(summary(m))[, "Estimate"]
    }

    v_beta <- v_b[names(v_b) %in% c("(Intercept)", u, v)] %>%
      data.matrix()

    ## matrix of input and other variables; match variable order
    v_var_match_id <- match(names(v_b), colnames(df_uv1)) %>%
      stats::na.omit() %>%
      c()

    m_uv1 <- data.matrix(df_uv1[, v_var_match_id])
    m_uv2 <- data.matrix(df_uv2[, v_var_match_id])

    ## error check
    if (any(rownames(v_beta) != colnames(m_uv1))) {
       stop("error in matrix organization")
    }

    if (any(rownames(v_beta) != colnames(m_uv2))) {
       stop("error in matrix organization")
    }


    # average predictive comparison -------------------------------------------

    ## division by var_transform types
    if (var_transform == "identity") {
      e_y1 <- m_uv1 %*% v_beta
      e_y2 <- m_uv2 %*% v_beta
    }

    if (var_transform == "log") {
      e_y1 <- exp(m_uv1 %*% v_beta)
      e_y2 <- exp(m_uv2 %*% v_beta)
    }

    if (var_transform == "log10") {
      e_y1 <- 10 ^ (m_uv1 %*% v_beta)
      e_y2 <- 10 ^ (m_uv2 %*% v_beta)
    }

    if (var_transform == "logit") {
      e_y1 <- ilogit(m_uv1 %*% v_beta)
      e_y2 <- ilogit(m_uv2 %*% v_beta)
    }

    message(paste("link function used for variable transformation:",
                  var_transform,
                  "- an inverse function was used to estimate an average predictive comparison"))

    # estimate
    numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
    denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
    est <- numerator / denominator

    return(list(estimate = est, df_uv = df_uv))

  }
