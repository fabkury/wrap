build_rds_filepath <- function(rds_dir, varname, exec_label) {
  paste0(rds_dir, '/', make.names(varname),
    ifelse(nchar(exec_label), paste0(' (', exec_label, ')'), ''), '.rds')
}

#' @export
wrap <- function(var, exprs, by_name = FALSE, pass_val = NA, assign_val = NA, exec_label = '',
  rds_dir = './rds', ovr = FALSE, env = NULL, use_qs = TRUE, verbose = TRUE) {
  # This is a handy function to store variables between runs of the code and skip recreating them.
  # It checks if an RDS file for var already exists in rds_dir. If it does, read it from there. If
  # it does not, evaluates exprs and saves it to such RDS file.
  # var: The object itself, unquoted, or a character vector containing its name.
  # exprs: Expression to be evaluated if the RDS file doesn't already exist.
  # by_name: If true, var is interpreted as a character vector with the object name.
  # pass_val: If true, will return the object at the end.
  # assign_val: If true, will assign the value of the object to its name in the calling envirmt.
  # rds_dir: Directory to contain RDS files.
  # ovr: If true, will ignore existing objects and RDS files, and evaluate exprs.
  # pass_val	assign_val	passage	assignment
  # NA	NA	F	T
  # NA	T	F	T
  # NA	F	T	F
  # T	NA	T	F
  # F	NA	F	T
  # T	T	T	T
  # F	F	F	F
  # T	F	T	F
  # F	T	F	T

  if(by_name)
    varname <- var
  else
    varname <- deparse(substitute(var))

  if(length(env) > 0)
    target_envir <- env
  else
    target_envir <- parent.frame(n = 1)

  if(!ovr && exists(varname, envir = target_envir)) {
    var_val <- get(varname, envir = target_envir)
  } else {
    # Serializing file
    sz_file <- build_rds_filepath(rds_dir, varname, exec_label)
    if(use_qs) {
      sz_file <- sub('\\.rds$', '\\.qs', sz_file)
      read_fun <- qs::qread
    } else
      read_fun <- readRDS

    if(!ovr && file.exists(sz_file)) {
      if(verbose)
        message("Reading '", varname, "' from '", sz_file, "'.")
      var_val <- read_fun(sz_file)
    } else {
      var_val <- eval(substitute(exprs), envir = target_envir)

      if(use_qs)
        write_fun <- qs::qsave
      else
        write_fun <- readRDS

      if(verbose)
        message("Saving '", varname, "' to '", sz_file, "'.")
      if(!dir.exists(rds_dir))
        dir.create(rds_dir, recursive = T)
      if(use_qs)
        write_fun(var_val, sz_file, preset = 'balanced')
      else
        write_fun(var_val, sz_file)
    }

    if(isTRUE(assign_val) || (is.na(assign_val) && !isTRUE(pass_val)))
      assign(varname, var_val, envir = target_envir)
  }

  if(isTRUE(pass_val) || (is.na(pass_val) && isFALSE(assign_val)))
    return(var_val)
  else
    return(invisible(NA))
}

keepSZ <- function(var, by_name = FALSE, exec_label = '', rds_dir = './rds',
  verbose = FALSE, env = NULL, use_qs = TRUE) {
  # Helper function to create new RDS files, or update existing ones, with calling syntax and file
  # name compatibles with wrap.
  if(length(env) > 0)
    target_envir <- env
  else
    target_envir <- parent.frame(n = 1)

  if(by_name) {
    varname <- var
    var <- eval(parse(text=varname), envir = target_envir)
  } else
    varname <- deparse(substitute(var))

  sz_file <- build_rds_filepath(rds_dir, varname, exec_label)
  write_fun <- saveRDS
  if(use_qs) {
    write_fun <- qs::qsave
    sz_file <- sub('\\.rds$', '\\.qs', sz_file)
  }

  if(verbose)
    message("Saving '", varname, "' to file '", sz_file, "'.")

  if(!dir.exists(rds_dir))
    dir.create(rds_dir, recursive = T)

  tryCatch(write_fun(var, sz_file),
    error = function(e) {
      message('Error saving ', sz_file, ':', e)
    })
}

getSZ <- function(var, by_name = FALSE, pass_val = NA, assign_val = NA, ovr = FALSE,
  exec_label = '', rds_dir = './rds', env = NULL, use_qs = TRUE) {
  # This is equivalent to wrap, but only reads the RDS file, never evaluates any expression.
  if(by_name)
    varname <- var
  else
    varname <- deparse(substitute(var))

  if(length(env) > 0)
    target_envir <- env
  else
    target_envir <- parent.frame(n = 1)

  if(exists(varname, envir = target_envir) && !ovr) {
    val_val <- get(varname, envir = target_envir)
  } else {
    read_fun <- readRDS
    rds_file <- build_rds_filepath(rds_dir, varname, exec_label)
    qs_file <- sub('\\.rds$', '\\.qs', rds_file)
    if(use_qs && file.exists(qs_file)) {
      read_fun <- qs::qread
      sz_file <- qs_file
    } else {
      sz_file <- rds_file
    }

    if(file.exists(sz_file)) {
      message("Reading '", varname, "' from file '", sz_file, "'.")
      var_val <- read_fun(sz_file)

      if(isTRUE(assign_val) || (is.na(assign_val) && !isTRUE(pass_val)))
        assign(varname, var_val, envir = target_envir)

    } else {
      message("Unable to find SZ file for '", varname, "': '", sz_file, "'.")
      return(invisible(NULL))
    }
  }

  if(isTRUE(pass_val) || (is.na(pass_val) && isFALSE(assign_val)))
    return(var_val)
  else
    return(invisible(NA))
}

#' @export
wrap0 <- function(var, exprs, by_name = F, pass_val = NA, assign_val = NA, ovr = FALSE) {
  if(by_name)
    varname <- var
  else
    varname <- deparse(substitute(var))

  if(ovr || !exists(varname, envir = parent.frame(n = 1))) {
    var_val <- eval.parent(substitute(exprs), 1)
    if(isTRUE(assign_val) || (is.na(assign_val) && !isTRUE(pass_val)))
      assign(varname, var_val, envir = parent.frame(n = 1))
  }

  if(isTRUE(pass_val) || (is.na(pass_val) && isFALSE(assign_val)))
    return(var_val)
  else
    return(invisible(NA))
}

#' @export
remwrap <- function(var, by_name = FALSE, exec_label = '', rds_dir = './rds', use_qs = TRUE, verbose = TRUE) {
  if(by_name)
    varname <- var
  else
    varname <- deparse(substitute(var))

  # Serializing file
  sz_file <- build_rds_filepath(rds_dir, varname, exec_label)

  if(use_qs)
    sz_file <- sub('\\.rds$', '\\.qs', sz_file)

  if(file.exists(sz_file)) {
    if(verbose)
      message("Removing '", sz_file, "' of object '", varname, "'.")
    file.remove(sz_file)
  } else
    message("File '", sz_file, "' not found.")

  return(invisible(NULL))
}
