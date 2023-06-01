library(magrittr)

#' @title Check host availability
#'
#' @param host Hostname
#'
check_host <- function(host) {
  pingr::is_online(host)
}

#' @title Set a key
#'
#' @param service Name of the service
#' @param name    Name of the key
#'
set_key <- function(service, name) {
  
  msg <- glue::glue("Please enter value for {service}/{name} key:")
  
  value <- rstudioapi::askForPassword(prompt = msg)
  value <- stringr::str_trim(value, side = "both")
  
  if (base::nchar(value) == 0)
    base::stop("ERROR - Invalid value entered")
  
  keyring::key_set_with_value(service = service,
                              username = name,
                              password = value)
}


#' @title Get key value
#'
#' @param service Name of the service
#' @param name    Name of the key
#'
get_key <- function(service, name) {
  keyring::key_get(service, name)
}


#' @title Check Service
#'
#'
check_service <- function(service) {
  keys <- keyring::key_list()
  service %in% keys$service
}

#' @title Check Service
#'
#'
check_key <- function(service, name) {
  keys <- keyring::key_list()
  name %in% keys[keys$service == service,]$username
}

#' @title Get Services
#'
#'
get_services <- function() {
  keys <- keyring::key_list()
  unique(keys$service)
}

#' @title Get Service Keys
#'
#' @param service Account Service name
#'
get_keys <- function(service) {
  keys <- keyring::key_list()
  keys[keys$service == service,]$username
}

#' @title Get Service
#'
#' @param name Service name of the account
#'
get_account <- function(name) {
  package_check('keyring')
  
  if(!glamr::is_stored(name)) {
    
    if(!interactive())
      ui_stop("No {name} record found. Create a new account using {ui_code('set_key()')}")
  }
  
  accnt <- keyring::key_list(name)
  
  accnt %>%
    pull(username) %>%
    map(function(username) {
      get_key(service = name, name = username)
    }) %>%
    set_names(accnt$username) %>%
    invisible()
}

#' @title Get Service
#'
#' @param name    Service name of the account
#' @param keys    List of account key names
#' @param update  Should an existing account be overwriten
#'
set_account <- function(name,
                        keys = c("username", "password"),
                        update = FALSE) {
  
  package_check('keyring')
  
  # Keyring Service
  srv <- name
  
  if(glamr::is_stored(srv) & !update) {
    usethis::ui_stop("{srv} exists already. Set update to TRUE to overwrite")
  }
  
  # Prompt user to set value for account keys
  keys %>%
    walk(function(key){
      set_key(service = srv, name = key)
    })
}


#' @title Postgres Environment
#'
#' @param env Name of environment service
#'
pg_service <- function(env = "local") {
  base::paste0(env, "-postgres")
}


#' @title Postgres Database's host
#'
#' @param env Name of the environment, default set to local
#'
pg_host <- function(env = "local") {
  svc <- pg_service(env)
  get_key(service = svc, name = "host")
}


#' @title Postgres Database's port
#'
pg_port <- function(env = "local") {
  svc <- pg_service(env)
  get_key(service = svc, name = "port")
}


#' @title Postgres Database's name
#'
pg_database <- function(env = "local") {
  svc <- pg_service(env)
  get_key(service = svc, name = "database")
}


#' @title Postgres user's name
#'
pg_user <- function(env = "local") {
  svc <- pg_service(env)
  get_key(service = svc, name = "username")
}


#' @title Postgres user's password
#'
pg_pwd <- function(env = "local") {
  svc <- pg_service(env)
  get_key(service = svc, name = "password")
}

#' @title Postgres Connection
#'
#' @param db_host
#' @param db_port
#' @param db_database
#' @param db_user
#' @param db_pwd
#'
pg_connection <- function(db_driver = RPostgres::Postgres(),
                          db_host = pg_host(),
                          db_port = pg_port(),
                          db_name = pg_database(),
                          db_user = pg_user(),
                          db_pwd = pg_pwd()) {
  
  DBI::dbConnect(drv = db_driver,
                 host = db_host,
                 port = db_port,
                 dbname = db_name,
                 user = db_user,
                 password = db_pwd)
  
}


#' @title Establish a connection
#'
#' @param db_host
#' @param db_port
#' @param db_database
#' @param db_user
#' @param db_pwd
#' @param db_file
#'
db_connection <- function(db_driver = RPostgres::Postgres(),
                          db_host = pg_host(),
                          db_port = pg_port(),
                          db_name = pg_database(),
                          db_user = pg_user(),
                          db_pwd = pg_pwd(),
                          db_file = NULL) {
  
  conn <- NULL
  
  if (str_detect(db_name, ".*[.]db$|.*[.]sqlite$|.*[.]sqlite3$")) {
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  } else if("MySQLDriver" %in% class(db_driver)) {
    conn <- RMySQL::dbConnect(db_driver,
                              host = db_host,
                              port = db_port,
                              dbname = db_name,
                              user = db_user,
                              password = db_pwd)
  } else {
    conn <- pg_connection(db_driver, db_host, db_port, db_name, db_user, db_pwd)
  }
  
  return(conn)
}


#' @title Retrieve database name from a connection object
#'
#' @param conn DBI::dbConnect instance
#'
db_name <- function(conn) {
  
  conn_info <- DBI::dbGetInfo(conn)
  
  name <- conn_info$dbname
  
  return(name)
}


#' @title Connect to database
#'
#' @description TODO - Switch to a new database
#'
db_connect <- function(name, conn) {
  DBI::dbGetInfo(conn)$dbname <- name
  return(name)
}


#' @title List of Databases
#'
db_list <- function(conn) {
  
  sql_cmd <- '
    SELECT * FROM pg_database
    WHERE datallowconn = true
    AND datistemplate = false;
  '
  
  query <- DBI::dbGetQuery(conn, sql_cmd)
  
  query %>%
    tibble::as_tibble() %>%
    dplyr::pull(datname) %>%
    base::sort()
}


#' @title Get current database
#'
#'
db_in_use <- function(conn) {
  conn_info <- DBI::dbGetInfo(conn)
  db_name <- conn_info$dbname
  
  return(db_name)
}


#' @title List database schemas
#'
db_schemas <- function(conn) {
  
  db_name <- db_in_use(conn)
  
  usethis::ui_info(glue::glue("Current Database [{db_name}]"))
  
  sql_cmd <- "
    SELECT DISTINCT table_catalog, table_schema
    FROM information_schema.tables
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
    AND table_catalog = $1
    ORDER BY table_schema;
  "
  
  query <- DBI::dbGetQuery(conn, sql_cmd, params = base::list(db_name))
  
  schemas <- query %>%
    tibble::as_tibble() %>%
    dplyr::pull(table_schema) %>%
    base::sort()
  
  return(schemas)
}


#' @title Create new database schema
#'
db_schema <- function(conn, name) {
  
  db_name <- db_in_use(conn)
  
  usethis::ui_info(glue::glue("Current Database [{db_name}]"))
  usethis::ui_info(glue::glue("Adding new schema [{name}]"))
  
  sql_cmd <- glue::glue("CREATE SCHEMA IF NOT EXISTS {name};")
  
  tryCatch(
    expr = {
      DBI::dbSendQuery(conn, sql_cmd)
    },
    error = function(err) {
      print(err)
      usethis::ui_stop("ERROR")
    }
  )
}


#' @title List database and schema tables
#'
db_tables <- function(conn,
                      schema = NULL,
                      details = FALSE) {
  
  db_name <- db_in_use(conn)
  
  #usethis::ui_info(glue::glue("Current Database [{db_name}]"))
  
  sql_cmd <- "
    SELECT DISTINCT table_schema, table_name, table_type
    FROM information_schema.tables
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
    AND table_catalog = $1
    ORDER BY table_name;
  "
  
  query <- DBI::dbGetQuery(conn, sql_cmd, params = list(db_name))
  
  tbls <- query %>% tibble::as_tibble()
  
  if (!base::is.null(schema)) {
    tbls <- tbls %>% dplyr::filter(table_schema == schema)
  }
  
  if (details) {
    return(tbls)
  }
  
  tbls <- tbls %>%
    dplyr::mutate(
      name = dplyr::case_when(
        table_schema == "public" ~ table_name,
        TRUE ~ paste0(table_schema, ".", table_name)
      )
    ) %>%
    dplyr::pull(name)
  
  return(tbls)
}

#' @title Check if table exists
#'
#'
db_table_exists <- function(conn, tbl_name) {
  
  tbls <- db_tables(conn)
  
  tbl_name %in% tbls
}


#' @title Get Table full name
#'
#'
db_table_id <- function(tbl_name) {
  
  # Extract schema
  if(str_detect(tbl_name, ".*[.].*")) {
    schema <- str_extract(tbl_name, ".*(?=\\.)")
    name <- str_extract(tbl_name, "(?<=\\.).*")
  } else {
    schema <- "public"
    name <- tbl_name
  }
  
  DBI::Id(schema = schema, table = name)
}


#' @title
#'
db_create_table <- function(tbl_name, fields,
                            conn = NULL,
                            meta = NULL,
                            pkeys = NULL,
                            overwrite = FALSE,
                            load_data = TRUE) {
  # Check Connection
  if (is.null(conn))
    conn <- db_connection()
  
  db_name <- db_in_use(conn)
  
  usethis::ui_info(glue::glue("Current Database [{db_name}]"))
  
  # Extract schema
  if(str_detect(tbl_name, ".*[.].*")) {
    schema <- str_extract(tbl_name, ".*(?=\\.)")
    name <- str_extract(tbl_name, "(?<=\\.).*")
  } else {
    schema <- "public"
    name <- tbl_name
  }
  
  tbl_full_name <- db_table_id(tbl_name)
  
  # Check existing tables
  usethis::ui_info(glue::glue("Schema: {schema}"))
  usethis::ui_info(glue::glue("Table: {name}"))
  
  tbls <- db_tables(conn, schema)
  
  tbl_exists <- tbl_name %in% tbls
  
  usethis::ui_info(glue::glue("Exists: {tbl_exists}"))
  
  # Check / Set columns data types
  if (!base::is.null(meta)) {
    if (!all(names(fields) %in% names(meta))) {
      cols_missing <- setdiff(
        names(fields), names(meta[names(fields) %in% names(meta)])) %>%
        base::paste(collapse = ", ")
      
      usethis::ui_stop(glue::glue("Missing column(s) from {tbl_name}: {cols_missing}"))
    }
    
    cols <- meta[names(meta) %in% names(fields)]
  }
  
  # Drop Table
  if(overwrite & tbl_exists) {
    db_drop_table(tbl_name, conn)
  }
  
  # Create table
  usethis::ui_info("Creating table ...")
  
  if (!base::is.null(meta)) {
    DBI::dbCreateTable(conn, tbl_full_name, fields = cols)
    
  } else {
    DBI::dbCreateTable(conn, tbl_full_name, fields = fields)
  }
  
  # Add Primary Keys
  if (!base::is.null(pkeys)) {
    
    pkeys <- base::paste(pkeys, collapse = ", ")
    
    usethis::ui_info(glue::glue("Adding PKEY(s): {pkeys}"))
    
    cmd <- glue::glue_sql("ALTER TABLE {SQL(tbl_name)} ADD PRIMARY KEY ({SQL(pkeys)});",
                          .con = conn)
    
    res <- DBI::dbExecute(conn, cmd)
    
    base::stopifnot(res == 0)
  }
  
  # Insert data
  if(load_data & is.data.frame(fields) & nrow(fields) > 0) {
    usethis::ui_info(glue::glue("Appending data ... {nrow(fields)}"))
    DBI::dbAppendTable(conn, tbl_full_name, fields)
  }
  
  usethis::ui_done("Complete!")
}

#' @title Update / Alter Table Add PKEYs
#'
#'
db_update_table <- function(tbl_name,
                            conn = NULL,
                            pkeys = NULL,
                            fkeys = NULL) {
  
  if (base::is.null(pkeys) & base::is.null(fkeys)) {
    usethis::ui_stop("Missing one of the required keys: primary or foreign")
  }
  
  # Add Primary Keys
  if (!base::is.null(pkeys)) {
    
    pkeys <- base::paste(pkeys, collapse = ", ")
    
    usethis::ui_info(glue::glue("Adding PKEY(s): {pkeys}"))
    
    cmd <- glue::glue_sql("ALTER TABLE {DBI::SQL(tbl_name)} ADD PRIMARY KEY ({DBI::SQL(pkeys)});",
                          .con = conn)
    
    res <- DBI::dbExecute(conn, cmd)
    
    base::stopifnot(res == 0)
  }
  
  # Add Foreign Keys
  if (!base::is.null(fkeys)) {
    
    fkeys %>%
      names() %>%
      walk(function(.k) {
        
        ftbl <- .k
        pkeys <- base::paste(fkeys[[.k]], collapse = ", ")
        fkeys <- base::paste(fkeys[[.k]], collapse = ", ")
        
        usethis::ui_info(glue::glue("Adding Foreign Key(s): {tbl_name} ({pkeys}) => {ftbl} ({fkeys})"))
        
        # cmd <- glue::glue_sql("ALTER TABLE {DBI::SQL(tbl_name)} ADD FOREIGN KEY ({DBI::SQL(pkeys)}) REFERENCES {DBI::SQL(ftbl)} ({DBI::SQL(fkeys)});",
        #                       .con = conn)
        
        cmd <- DBI::SQL(glue::glue("ALTER TABLE {tbl_name} ADD FOREIGN KEY ({pkeys}) REFERENCES {ftbl} ({fkeys});",
                                   .con = conn))
        
        print(cmd)
        
        res <- DBI::dbExecute(conn, cmd)
        
        base::stopifnot(res == 0)
      })
  }
  
}

#' @title Drop Table from Database
#'
db_drop_table <- function(tbl_name, conn = NULL) {
  # Check Connection
  if (is.null(conn))
    conn <- db_connection()
  
  db_name <- db_in_use(conn)
  
  tbl_full_name <- db_table_id(tbl_name)
  
  # Check table exists before dropping
  if (db_table_exists(conn, tbl_name)) {
    
    usethis::ui_info(glue::glue("Dropping table [{tbl_name}] from [{db_name}] ..."))
    
    # Drop Table
    DBI::dbRemoveTable(conn, tbl_full_name)
    
    usethis::ui_done("Table Dropped!")
    
  } else {
    usethis::ui_stop(glue::glue("Table [{tbl_name}] does not exist in database [{db_name}]"))
  }
}

#' @title Create a new Database
#'
#' @param name
#' @param conn
#' @param owner
#'
db_create <- function(name,
                      conn = NULL,
                      owner = NULL) {
  # DB Connection
  if (is.null(conn))
    conn = db_connection()
  
  # Exclude SQLite/File based database
  if (stringr::str_detect(db_name(conn), ".db$|.sqlite$"))
    usethis::ui_stop("Invalid Input - File based connection do not support multiple databases")
  
  # Build query
  query <- glue::glue_sql("CREATE DATABASE {`name`}", .con = conn)
  
  if (!is.null(owner))
    query <- glue::glue_sql("CREATE DATABASE {`name`} WITH OWNER = {`owner`};",
                            .con = conn)
  
  # Execute query statement
  res <- tryCatch(
    expr = {DBI::dbExecute(conn, query)},
    warning = function(w){
      usethis::ui_warn("SQL warning: {query}")
      print(w)
    },
    error = function(e){
      usethis::ui_oops("SQL error: {query}")
      stop(e)
    }
  )
  
  return(res)
}

#' @title
db_munge <- function() {}

#' @title
db_query <- function() {}

#' @title
db_control <- function() {}

#' @title
#'
find_pkeys <- function(.df, colnames = FALSE) {
  
  combos <- .df %>%
    base::names() %>%
    purrr::accumulate(c)
  
  checks <- combos %>%
    map(syms) %>%
    map_lgl(~{
      .df %>%
        add_count(!!!.x) %>%
        filter(.data$n > 1) %>%
        nrow() %>%
        equals(0)
    })
  
  if (colnames)
    return(subset(combos, checks))
  
  which(checks)
}


#' @title Check if URL is valid
#'
host_ping <- function(base_url) {
  
  p_url <- httr::parse_url(base_url)
  host <- p_url$hostname
  res_p <- pingr::ping(host)
  check <- !base::any(base::is.na(res_p))
  
  if(!check) {
    msg <- stringr::str_c(base_url, " does not seem to be responding. Check your base url.")
    base::message(msg)
  }
  
  return(check)
}

#' @title Check is resource is online
#'
host_is_online <- function(base_url){
  pingr::is_online(base_url)
}

#' @title Clean Datim Modalities
#'
datim_clean_modalities <- function(dim_mods) {
  
  dim_mods %>%
    mutate(
      fiscal_year = str_extract(item, "(?<=[:space:]FY).*"),
      fiscal_year = paste0("FY", fiscal_year),
      sitetype = str_extract(item, ".*(?=[:space:]-)"),
      name = str_extract(item, ".*(?=[:space:]FY)"),
      short_name = str_extract(name, "(?<=-[:space:]).*"),
      short_name = case_when(
        short_name == "PMTCT ANC1 Only" ~ "PMTCT ANC",
        short_name == "PMTCT Post ANC1" ~ "Post ANC1",
        short_name == "Other Services" ~ "Other",
        short_name == "Home Based" ~ "Home",
        short_name %in% c("Other PITC", "TB Clinic") ~ str_replace(short_name, " ", ""),
        TRUE ~ short_name
      ),
      short_name = case_when(
        sitetype == "Community" ~ paste0(short_name, "Com"),
        TRUE ~ short_name
      )
    )
}


#' @title Datim Resources
#'
datim_resources <- function(...,
                            res_name = NULL,
                            dataset = FALSE,
                            username = NULL,
                            password = NULL,
                            base_url = NULL) {
  # datim credentials
  if (missing(username))
    username <- datim_user()
  
  if (missing(password))
    password <- datim_pwd()
  
  # Base url
  if (missing(base_url))
    base_url <- "https://final.datim.org"
  
  if(!host_is_online(base_url))
    base::stop(base::paste0("The resource seems to be offline: ", base_url))
  
  # URL Query Options
  options <- "?format=json&paging=false"
  
  # List of columns
  cols <- list(...)
  
  print(paste0(cols))
  
  # API URL
  api_url <- base_url %>%
    paste0("/api/resources", options)
  
  # Query data
  data <- api_url %>%
    datim_execute_query(username, password, flatten = TRUE) %>%
    purrr::pluck("resources") %>%
    tibble::as_tibble()
  
  data <- data %>% rename(name = displayName)
  
  # Filter if needed
  if (!base::is.null(res_name)) {
    data <- data %>%
      filter(name == res_name)
  }
  
  # Return only the url when results is just 1 row
  if(base::nrow(data) == 1 && dataset == FALSE) {
    return(data$href)
    
  } else if (base::nrow(data) == 1 && dataset == TRUE) {
    
    dta_url <- data$href
    print(dta_url)
    
    end_point <- dta_url %>%
      str_split("\\/") %>%
      unlist() %>%
      last()
    
    dta_url <- dta_url %>% paste0(options)
    
    print(length(cols))
    
    if (length(cols) > 0) {
      dta_url <- dta_url %>%
        paste0("&fields=", paste0(cols, collapse = ","))
    } else {
      dta_url <- dta_url %>%
        paste0("&fields=:nameable")
    }
    
    print(dta_url)
    
    data <- dta_url %>%
      datim_execute_query(username, password, flatten = TRUE) %>%
      purrr::pluck(end_point) %>%
      tibble::as_tibble()
  }
  
  return(data)
}

#' @title DATIM Data Elements
#'
#'
datim_dataements <- function() {
  datim_resources(res_name = "Data Elements", dataset = T) %>%
    select(uid = id, code, dataelement = name, shortname = shortName, description) %>%
    filter(str_detect(dataelement, "\\(|\\)", negate = F)) %>%
    mutate(
      dataelement_type = case_when(
        str_detect(dataelement, ".*\\)[:space:]TARGET") ~ "Targets",
        TRUE ~ "Results"
      ),
      indicator = str_extract(dataelement, ".*(?=[:space:]\\()"),
      disaggs = extract_text(dataelement, "()")
    ) %>%
    separate(col = disaggs,
             into = c("numeratordenom", "indicatortype", "disaggregate"),
             sep = ", ",
             remove = T) %>%
    relocate(description, .after = last_col())
}


#' @title Get Datim Data Elements SQLView
#' @note: TODO: Replace datasetuid with datasetname
#'
datim_deview <- function(datasetuid, base_url = NULL) {
  
  df_deview <- datim_sqlviews(
    view_name = "Data sets, elements and combos paramaterized",
    dataset = TRUE,
    vquery = list("dataSets" = datasetuid),
    base_url = base_url)
  
  df_deview %>%
    separate(dataset,
             into = c("source", "category"),
             sep = ": ",
             remove = F) %>%
    mutate(
      type = case_when(
        str_detect(source, " Targets$|.*Target.*") ~ "Targets",
        TRUE ~ "Results")) %>%
    select(dataset, source, category, type,
           dataelementuid, code, dataelement, shortname, dataelementdesc,
           categoryoptioncombouid, categoryoptioncombocode, categoryoptioncombo,
           everything())
}

#' @title Data Elements
#'
datim_data_elementsss <- function(username = NULL,
                                  password = NULL,
                                  base_url = NULL) {
  
  # datim credentials
  if (missing(username))
    username <- datim_user()
  
  if (missing(password))
    password <- datim_pwd()
  
  # Base url
  if (missing(base_url))
    base_url <- "https://final.datim.org"
  
  # API URL
  api_url <- base_url %>%
    paste0("/api/dataElements?format=json&paging=false&fields=:nameable")#:identifiable
  
  data <- api_url %>%
    datim_execute_query(username, password, flatten = TRUE) %>%
    purrr::pluck("dataElements") %>%
    tibble::as_tibble()
  
  print(data)
}


#' @title Datim SQLViews
#'
datim_sqlviews <- function(username,
                           password,
                           view_name = NULL,
                           dataset = FALSE,
                           datauid = NULL,
                           query = NULL,
                           base_url = NULL) {
  
  # Datim credentials
  accnt <- lazy_secrets("datim", username, password)
  
  # Base url
  if (missing(base_url))
    base_url <- "https://final.datim.org"
  
  # Other Options
  end_point <- "/api/sqlViews/"
  
  options <- "?format=json&paging=false"
  
  # API URL
  api_url <- base_url %>% paste0(end_point, options)
  
  # Query data
  data <- tryCatch(
    {
      api_url %>%
        grabr::datim_execute_query(
          username = accnt$username,
          password = accnt$password,
          flatten = TRUE
        )
    },
    error = function(err) {
      message(err)
      usethis::ui_stop("ERROR - Unable to excute datim query")
    },
    warning = function(warn) {
      message(warn)
    }
  )
  
  # Extract SQLview
  data <- data %>%
    purrr::pluck("sqlViews") %>%
    tibble::as_tibble() %>%
    dplyr::rename(uid = id, name = displayName)
  
  # Filter if needed
  if (!base::is.null(view_name)) {
    
    print(glue::glue("Searching for SQL View: {view_name} ..."))
    
    data <- data %>%
      dplyr::filter(stringr::str_to_lower(name) == str_to_lower(view_name))
  }
  
  # Number of rows
  rows = base::nrow(data)
  
  # Return only ID when results is just 1 row
  if(rows == 0) {
    base::warning("No match found for the requested SQL View")
    return(NULL)
  }
  # Flag non-unique sqlview names
  else if (rows > 1 && dataset == TRUE) {
    base::warning("There are more than 1 match for the requested SQL View data. Please try to be specific.")
    print(data)
    return(NULL)
  }
  # Return only ID when results is just 1 row
  else if (rows == 1 && dataset == FALSE) {
    return(data$uid)
  }
  # Return SQLVIEW data
  else if(base::nrow(data) == 1 && dataset == TRUE) {
    
    dta_uid <- data$uid
    
    dta_url <- base_url %>%
      paste0(end_point, dta_uid, "/data", options, "&fields=*") #:identifiable, :nameable
    
    # apply variable query if needed
    if (!is.null(query)) {
      q <- names(query$params) %>%
        map_chr(~paste0(.x, "=", query$params[.x])) %>%
        paste0(collapse = "&") %>%
        paste0("QUERY PARAMS: type=", query$type, "&", .)
      
      print(print(glue::glue("SQL View Params: {q}")))
      
      if (query$type == "variable") {
        vq <- names(query$params) %>%
          map_chr(~paste0(.x, ":", query$params[.x])) %>%
          paste0(collapse = "&") %>%
          paste0("&var=", .)
        
        print(glue::glue("SQL View variable query: {vq}"))
        
        dta_url <- dta_url %>% paste0(vq)
        
      } else if (query$type == "field") {
        fq <- names(query$params) %>%
          map_chr(~paste0("filter=", .x, ":eq:", query$params[.x])) %>%
          paste0(collapse = "&") %>%
          paste0("&", .)
        
        print(glue::glue("SQL View field query: {fq}"))
        
        dta_url <- dta_url %>% paste0(fq)
      }
      else {
        print(glue::glue("Error - Invalid query type: {query$type}"))
      }
    }
    
    print(glue::glue("SQL View url: {dta_url}"))
    
    # Query data
    data <- dta_url %>%
      grabr::datim_execute_query(username, password, flatten = TRUE)
    
    # Detect Errors
    if (!is.null(data$status)) {
      print(glue::glue("Status: {data$status}"))
      
      if(!is.null(data$message)) {
        print(glue::glue("Message: {data$message}"))
      }
      
      return(NULL)
    }
    
    # Headers
    headers <- data %>%
      purrr::pluck("listGrid") %>%
      purrr::pluck("headers") %>%
      dplyr::pull(column)
    
    # Data
    data <- data %>%
      purrr::pluck("listGrid") %>%
      purrr::pluck("rows") %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      janitor::clean_names() %>%
      magrittr::set_colnames(headers)
  }
  
  return(data)
}

#' @title Datim Period Information SQLView
#
#
datim_peview <- function(username, password,
                         base_url = NULL) {
  
  df_peview <- datim_sqlviews(
    username = username,
    password = password,
    view_name = "Period information",
    dataset = TRUE,
    base_url = base_url)
  
  df_peview %>%
    mutate(
      period = case_when(
        periodtype == "Daily" ~ paste0("FY", str_sub(iso, 3,4), "M", str_sub(iso, 5,6), "D", str_sub(iso, 7, 8)),
        str_detect(periodtype, "Weekly") ~ paste0("FY", str_sub(iso, 3,4), str_sub(iso, 5)),
        periodtype == "Monthly" ~ paste0("FY", str_sub(iso, 3,4), "M", str_sub(iso, 5)),
        TRUE ~ paste0("FY", str_sub(iso, 3,4), str_sub(iso, 5))
      )
    ) %>%
    rename(
      period_id = periodid,
      period_iso = iso,
      period_type = periodtype
    ) %>%
    select(starts_with("period"), everything())
}

#' @title Datim Mechanism SQLView
#'
#'
datim_mechview <- function(username, password,
                           query = NULL,
                           base_url = NULL) {
  
  # Extract OU Mechs
  df_mechview <- datim_sqlviews(
    username = username,
    password = password,
    view_name = "Mechanisms partners agencies OUS Start End",
    dataset = TRUE,
    query = query,
    base_url = base_url)
  
  # Reshape Results - mech code, award number, and name separations chars
  sep_chrs <- c("[[:space:]]+",
                "[[:space:]]+-",
                "[[:space:]]+-[[:space:]]+",
                "[[:space:]]+-[[:space:]]+-",
                "[[:space:]]+-[[:space:]]+-[[:space:]]+",
                "[[:space:]]+-[[:space:]]+-[[:space:]]+-",
                "-[[:space:]]+",
                "-[[:space:]]+-",
                "-[[:space:]]+-[[:space:]]+",
                "-[[:space:]]+-[[:space:]]+-",
                "-[[:space:]]+-[[:space:]]+-[[:space:]]+",
                "-[[:space:]]+-[[:space:]]+-[[:space:]]+-")
  
  # Reshape Results - separation
  df_mechview %>%
    rename(
      mech_code = code,
      operatingunit = ou,
      prime_partner = partner,
      prime_partner_id = primeid,
      funding_agency = agency,
      operatingunit = ou
    ) %>%
    # mutate(
    #   mech_name = str_remove(mechanism, mech_code),
    #   award_number = str_extract(mech_name, "(?<=-[:space:]).*(?=[:space:]-)"),
    #   mech_name = str_trim(mech_name),
    #   mech_name = str_remove(mech_name, "-[:space:].*[:space:]-[:space:]|-[:space:]")
    # ) %>%
    mutate(
      mech_name = str_remove(mechanism, mech_code),
      mech_name = str_replace_all(mech_name, "\n", ""),
      award_number = case_when(
        str_detect(prime_partner, "^TBD") ~ NA_character_,
        TRUE ~ str_extract(mech_name, "(?<=-[:space:])[A-Z0-9]+(?=[:space:]-[:space:])")
      ),
      mech_name = case_when(
        !is.na(award_number) ~ str_remove(mech_name, award_number),
        TRUE ~ mech_name
      ),
      mech_name = str_remove(mech_name, paste0("^", rev(sep_chrs), collapse = "|"))
    ) %>%
    select(uid, mech_code, mech_name, award_number, mechanism, everything())
}


#' @title Datim OU/Partners SQLView
#'
#'
datim_ppview <- function(username, password,
                         query = NULL,
                         base_url = NULL) {
  
  df_ppview <- datim_sqlviews(
    username = username,
    password = password,
    view_name = "Country, Partner, Agencies",
    dataset = TRUE,
    query = query,
    base_url = base_url)
  
  df_ppview %>%
    rename(
      operatingunit = ou,
      prime_partner = partner,
      funding_agencies = agencies
    )
}

#' @title Datim OU / Countries view
#'
#'
datim_cntryview <- function(username, password,
                            query = NULL,
                            base_url = NULL) {
  datim_sqlviews(
    username = username,
    password = password,
    view_name = "OU countries",
    dataset = TRUE,
    query = query,
    base_url = base_url)
}


#' @title Datim OU / Countries view
#'
#'
datim_orgview <- function(username, password,
                          query = NULL,
                          cntry_uid = NULL,
                          base_url) {
  
  df_cntries <- datim_cntryview(
    username = username,
    password = password,
    base_url = base_url
  )
  
  if (is.null(cntry_uid)) {
    df_orgview <- df_cntries %>%
      pmap_dfr(~datim_sqlviews(
        username = username,
        password = password,
        view_name = "Data Exchange: Organisation Units",
        dataset = TRUE,
        query = list("OU" = ..3),
        base_url = base_url
      ))
    
    return(df_orgview)
  }
  
  if (!cntry_uid %in% df_cntries$orgunit_uid)
    usethis::ui_stop("ERROR - Invalid country uid")
  
  cntry_code <- df_cntries %>%
    filter(orgunit_uid == cntry_uid) %>%
    pull(orgunit_code)
  
  datim_sqlviews(
    view_name = "Data Exchange: Organisation Units",
    dataset = TRUE,
    vquery = list("OU" = cntry_code),
    base_url = base_url
  )
}