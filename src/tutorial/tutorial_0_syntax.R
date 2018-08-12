
# Everything after the pound sign (#) is a comment.

# The sequence <- is the left assigment operator.
x <- 8; # Assigns the value 8 to a variable x.
        # If x was already defined, it will be overwritten; otherwise it will be created.

# The function ls() returns the names of all objects in the current environment.
current_env <- ls(); # current_env now stores these names (except for the to-be-created <current_env>).

# The function rm() removes objects, by name, from the environment.
# It takes argument "list" that specifies a list of names of objects to be deleted.
# Arguments in functions can be passed by using the equality notation:
# function_name(..., argument_name = argument_value, ...)
rm(list = current_env); # Clear current environment prior to the declaration of <current_env>.
rm(current_env); # Clear <current_env>.
