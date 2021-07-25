###
#
# S4 class for Townforge lots and its accompanying functions
#
##

#' S4 class for a Townforge lot
#'
#' Description
#' 
#' @export
setClass("tf_lot",
	# Base class object representation
	representation(
		# Unique numeric identifier
		id = "numeric",
		# {x0,y0,x1,y1} coordinates for the rectangular lot
		x0 = "numeric",
		y0 = "numeric",
		x1 = "numeric",
		y1 = "numeric",
		# Length in x-coordinate direction
		xlen = "numeric",
		# Length in y-coordinate direction
		ylen = "numeric",
		# Name of the lot
		name = "character",
		# Lot type (as an numeric code)
		type_int = "numeric",
		# Lot type (as a character string)
		type_chr = "character",
		# Which city lot is located in
		city = "numeric",
		# Economic power (100-300%)
		ep = "numeric",
		# Owner's player id (integer)
		owner_int = "numeric",
		# Owner's player name (character string)
		owner_chr = "character",
		# Area (xlen * ylen)
		area = "numeric",
		# Influence range
		influence = "numeric",
		# Materials located on the lot (named vector of material types and their quantity)
		materials = "list"
		# TODO:
		# Voxel representation of structures
	),
	# Prototype S4 class object upon creation
	prototype(
		id = NA_real_,
		x0 = NA_real_,
		y0 = NA_real_,
		x1 = NA_real_,
		y1 = NA_real_,
		xlen = NA_real_,
		ylen = NA_real_,
		name = NA_character_,
		type_int = NA_real_,
		type_chr = NA_character_,
		city = NA_real_,
		ep = NA_real_,
		owner_int = NA_real_,
		owner_chr = NA_character_,
		area = NA_real_,
		influence = NA_real_,
		materials = list()
	), 
	# Check if an object is valid S4 Townforge lot
	validity = function(object){
		if(is.integer(object@id) | round(object@id,0) == object@id){
			TRUE
		}else{
			FALSE
		}
	}
)

#' S4 creator for tf_lot
#'
#' Description
#' 
#' @export
tf_lot <- function(
	id,
	url = "http://127.0.0.1:28881/json_rpc"
){
	# Sanity checks
	if(!class(id)=="numeric" | missing(id)) stop("tf_lot should be queried with an numeric 'id'")
	# Additional fields would be available via RPC, taking the most important ones
	flag <- TownforgeR::tf_rpc_curl(url = url, method="cc_get_flag", params = list(id = id))$result
	# Creating tf_lot S4 object
	obj <- new("tf_lot",
		id = id,
		x0 = flag$x0,
		y0 = flag$y0,
		x1 = flag$x1,
		y1 = flag$y1,
		xlen = flag$x1 - flag$x0 + 1,
		ylen = flag$y1 - flag$y0 + 1,
		name = flag$name,
		type_int = flag$role,
		type_chr = NA_character_, # TODO
		city = flag$city,
		ep = flag$economic_power,
		owner_int = flag$owner,
		owner_chr = NA_character_, # TODO
		area = (flag$x1 - flag$x0 + 1)*(flag$y1 - flag$y0 + 1),
		influence = flag$influence,
		materials = list() # TODO
	)
	# Return obj
	obj
}

###
#
# Useful functions for the tf_lot objects
#
##

# Return a 4-coordinate representation of the bounding box of the lot's influence rectangle
setGeneric("influence_boundary",
	function(object){
		standardGeneric("influence_boundary")
	}
)
setMethod("influence_boundary", "tf_lot",
	function(object){
		c(object@x0 - object@influence, object@y0 - object@influence, object@x1 + object@influence, object@y1 + object@influence)
	}
)


# Override default show
setMethod("show", "tf_lot",
	function(object){
		cat("Townforge lot:\n")
		cat(paste0("Name (id): ", object@name, " (", object@id, ")\n"))
		cat(paste0("xlen, ylen, total area: ", object@xlen, ", ", object@ylen, ", ", object@area),"\n")
		cat(paste0("{x0=",object@x0,", y0=",object@y0,", x1=",object@x1,", y1=",object@y1,"}\n"))	
	}
)
