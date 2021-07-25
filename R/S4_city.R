###
#
# S4 class for storing a Townforge city
#
###

#' S4 class for a Townforge city
#'
#' Description
#' 
#' @export
setClass("tf_city",
	# Base class object representation
	representation(
		# Unique numeric identifier
		id = "numeric"
	),
	# Prototype S4 class object upon creation
	prototype(
		id = NA_real_
	),
	# Check if an object is valid S4 Townforge lot
	validity = function(object){
		TRUE	
	}
)

#' S4 creator for tf_city
#'
#' Description
#' 
#' @export
tf_city <- function(
	id,
	url = "http://127.0.0.1:28881/json_rpc"
){
	# Sanity checks
	if(!class(id)=="numeric" | missing(id)) stop("tf_city should be queried with an numeric 'id'")

	city <- TownforgeR::tf_rpc_curl(url = url, method="cc_get_city", params = list(id = id))$result

	# Creating tf_lot S4 object
	obj <- new("tf_city",
		id = id
	)
	# Return obj
	obj
}
