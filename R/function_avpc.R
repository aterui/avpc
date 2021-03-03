
#' inverse logit function

  ilogit <- function(x) {
    1 / (1 + exp(-x))
  }

  utils::globalVariables('where')

#' Average predictive comparisons
#'
#' @param m model object. Accept model classes lm, rlm, glm, lmerMod
#' @param u a single character indicating input variable of interest.
#' @param v characters indicating other variables. select other than u by default
#' @param var_transform
#'
#' @importFrom dplyr %>%
#'
#' @export


  avpc <- function(m, u, v = NULL, var_transform = NULL) {

    # extract model data frame ------------------------------------------------

    if(!any(u %in% attributes(terms(m))$term.labels)) stop('invalid variable input u; check varaible name')

    if(is.null(v)) {

      v_var_names <- attributes(terms(m))$term.labels
      v <- v_var_names[!(v_var_names %in% u)]

    } else {

      if(!all(v %in% attributes(terms(m))$term.labels)) stop('invalid variable input v; check varaible name')

    }


    # division by model class
    if(any(class(m) %in% c('lm', 'rlm', 'glm'))) {

      m_frame <- m$model

    } else {

      if(any(class(m) %in% 'lmerMod')) {
        m_frame <- m@frame
      } else {
        stop('the provided model class is not supported')
      }

    }


    if(!all(unlist(lapply(m_frame, class)) %in% c('numeric', 'character'))) stop('variables contain classes of other than numeric or character')

    if(any(unlist(lapply(m_frame, class)) %in% c('character'))) {

      # character variable(s) exist

      ## extract character variables and convert them to numeric values
      ## stop if there are > 2 character levels in any character variable
      n_levels <- m_frame %>%
        dplyr::summarize(dplyr::across(.cols = where(is.character),
                                       .fns = ~ dplyr::n_distinct()(.x)))

      if(any(n_levels > 2)) stop('Currently, this function does not support categorical variables with > 2 levels. Consider converting the variable(s) to dummy binary variables (0, 1)')

      m_chr <- m_frame %>%
        dplyr::summarize(dplyr::across(.cols = where(is.character),
                                       .fns = ~ as.numeric(as.factor(.x)) - 1)) %>%
        dplyr::mutate(id = as.numeric(rownames(.)))

      ## extract numeric variables
      m_dbl <- m_frame %>%
        dplyr::summarize(dplyr::across(.cols = where(is.numeric))) %>%
        dplyr::mutate(id = as.numeric(rownames(.)))

      ## combine numeric and character variables
      mod <- m_chr %>%
        dplyr::left_join(m_dbl, by = 'id') %>%
        dplyr::select(-id) %>%
        dplyr::tibble()

      message('Character variable(s) detected in the data. These variables were coverted to dummy binary variables (0, 1)')

    } else {

      # no character variable

      mod <- dplyr::tibble(m_frame)

    }


    # get pairs for u and v ---------------------------------------------------

    # frame for v variables
    X1 <- X2 <- mod %>% dplyr::select(dplyr::all_of(v))
    X <- X1 %>% dplyr::mutate(id = as.numeric(rownames(.)))

    # mahalanobis distance for a set of v variables
    m_cov <- cov(X1)
    m_dist <- apply(X1, 1, function(row_i) mahalanobis(X2, row_i, m_cov))
    df_v <-  dplyr::tibble(sq_distance = c(m_dist),
                           row_id = rep(seq_len(nrow(m_dist)), times = ncol(m_dist)),
                           col_id = rep(seq_len(ncol(m_dist)), each = nrow(m_dist))) %>%
      dplyr::mutate(weight = 1/(1 + sq_distance))

    # combine with input u
    m_u <- mod %>%
      dplyr::select(dplyr::all_of(u)) %>%
      dplyr::rename(u_input = dplyr::all_of(u)) %>%
      dplyr::mutate(id = as.numeric(rownames(.)))

    df_uv <- df_v %>%
      dplyr::left_join(m_u, by = c('row_id' = 'id')) %>%
      dplyr::rename(u1 = u_input) %>%
      dplyr::left_join(m_u, by = c('col_id' = 'id')) %>%
      dplyr::rename(u2 = u_input) %>%
      dplyr::mutate(sign = ifelse(u2 - u1 >= 0, 1, -1)) %>%
      dplyr::left_join(X, by = c('row_id' = 'id'), suffix = c('_v1', '_v2')) %>%
      dplyr::left_join(X, by = c('col_id' = 'id'), suffix = c('_v1', '_v2'))


    # average predictive comparison -------------------------------------------

    # input u and other variables v (note: v is v1 irrespective of input u)
    u1 <- df_uv %>% dplyr::pull(u1)
    u2 <- df_uv %>% dplyr::pull(u2)
    df_v1 <- df_uv %>% dplyr::summarize(dplyr::across(ends_with('v1')))

    # input low
    df_uv1 <- dplyr::tibble(u1 = u1, df_v1)
    colnames(df_uv1) <- c(u, v)
    df_uv1 <- dplyr::tibble(Intercept = 1, df_uv1)

    # input high
    df_uv2 <- dplyr::tibble(u2 = u2, df_v1)
    colnames(df_uv2) <- c(u, v)
    df_uv2 <- dplyr::tibble(Intercept = 1, df_uv2)

    # get link function from the model object if var_transform 'null'
    if(is.null(var_transform)) {
      model_family <- family(m)
      var_transform <- model_family$link
    }

    if(!any(var_transform %in% c('log', 'logit', 'identity'))) stop('var_transform must be either log, logit or identity')


    # for model classes lm, rlm, glm
    if(any(class(m) %in% c('lm', 'rlm', 'glm'))) {

      names(m$coefficients) <- c('Intercept', attributes(terms(m))$term.labels)
      v_var_id <- match(names(m$coefficients), names(df_uv1)) %>%
        na.omit() %>%
        c()

      # vector of regression coefs
      v_b <- m$coefficients[names(m$coefficients) %in% c('Intercept', u, v)] %>%
        data.matrix()

      # matrix of input and other variables
      m_uv1 <- data.matrix(df_uv1[, v_var_id])
      m_uv2 <- data.matrix(df_uv2[, v_var_id])

      if(any(rownames(v_b) != colnames(m_uv1))) stop('error in matrix organization')
      if(any(rownames(v_b) != colnames(m_uv2))) stop('error in matrix organization')

    }

    # for model class lmerMod
    if(any(class(m) %in% 'lmerMod')) {

      names(m@beta) <- c('Intercept', attributes(terms(m))$term.labels)
      v_var_id <- match(names(m@beta), names(df_uv1)) %>%
        na.omit() %>%
        c()

      # vector of regression coefs
      v_b <- m@beta[names(m@beta) %in% c('Intercept', u, v)] %>%
        data.matrix()

      # matrix of input and other variables
      m_uv1 <- data.matrix(df_uv1[, v_var_id])
      m_uv2 <- data.matrix(df_uv2[, v_var_id])

      if(any(rownames(v_b) != colnames(m_uv1))) stop('error in matrix organization')
      if(any(rownames(v_b) != colnames(m_uv2))) stop('error in matrix organization')

    }

    # division by var_transform types
    if(var_transform == 'identity') {
      e_y1 <- m_uv1 %*% v_b
      e_y2 <- m_uv2 %*% v_b
    }

    if(var_transform == 'log') {
      e_y1 <- exp(m_uv1 %*% v_b)
      e_y2 <- exp(m_uv2 %*% v_b)
    }

    if(var_transform == 'logit') {
      e_y1 <- ilogit(m_uv1 %*% v_b)
      e_y2 <- ilogit(m_uv2 %*% v_b)
    }

    # estimate
    numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
    denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
    est <- numerator / denominator

    return(list(estimate = est, df_uv = df_uv))

  }

