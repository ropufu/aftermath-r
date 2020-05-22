
# Everything after the pound sign (#) is a comment.

# Left assigment operator: <-
x <- 8; # Assigns the value 8 to a variable x
        # (arrow points what gets sent where; in this case 8 to x).
        # If x was already defined, it will be overwritten; otherwise it will be created.
# Left assigment operator: <-
y <- x; # Assigns the value of x to variable y
        # (arrow indicates that x is sent to y).
        # y will then be a copy of x.
# Right assigment operator: ->
x -> z; # Assigns the value of x to variable z
        #(arrow indicates that x is sent to z, therefore creating/overwriting z).

# The function ls(...) returns the names of all objects in the current environment.
# In our case this will be the list "x", "y", "z".
current_env <- ls(); # current_env now stores these names
                     # (except for the to-be-created <current_env>).
# Now the environment contains the variable "current_env" in addition to "x", "y", "z".

# The function rm(...) removes objects, by name, from the environment.
# It takes argument named "list" that specifies a list of names of objects to be deleted.
# Arguments in functions can be passed by using the equality notation:
# function_name(..., argument_name = argument_value, ...)
rm(list = current_env); # Remove variables "x", "y", "z" from the environment.
rm(current_env); # Remove variables "current_env" from the environment.
