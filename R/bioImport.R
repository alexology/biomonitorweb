#' bioImport
#'
#' @param x user `data.frame`.
#' @param group `mi` for macroinvertebrates and `mf` for macrophytes.
#' @param dfref custom reference database.
#'
#' @export


bioImport <- function( x , group = "mi" , dfref = NULL ){

  # set the reference database for the specified group
  if( identical( group , "mi" ) ){
    ref <- mi_ref
  }

  if( identical( group , "mf" ) ){
    ref <- mf_ref
  }

  if( is.data.frame( dfref ) ){
    ref <- dfref
  }

  x$Taxa <- trimws( sapply( x$Taxa , capWords , USE.NAMES = F ) )

  x <- aggregate( . ~ Taxa , x , FUN = sum )

  taxa_check <- unique( unlist( ref ) )
  taxa_check <- as.character( taxa_check[ taxa_check != "" ] )

  wrong_names <- x[ ! x[ , "Taxa" ] %in% taxa_check  , "Taxa" ]

  if( length( wrong_names ) == 0 ){
    wrong_names
  } else {
    custom <- FALSE
    if( is.data.frame( dfref ) ){
      custom <- TRUE
    }
    suggestNamesBio( wrong_names , custom = custom , group = group )
  }
}
